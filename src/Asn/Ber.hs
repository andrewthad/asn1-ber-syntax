{-# language BangPatterns #-}
{-# language BinaryLiterals #-}
{-# language NumericUnderscores #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language PatternSynonyms #-}
{-# language TypeApplications #-}
{-# language UnboxedTuples #-}

module Asn.Ber
  ( Value(..)
  , decode
  ) where

import Data.Bits ((.&.),testBit,unsafeShiftR)
import Data.Bytes (Bytes)
import Data.Bytes.Parser (Parser)
import Data.Primitive (SmallArray,PrimArray)
import Data.Word (Word8,Word32,Word64)
import Data.Int (Int64)
import Control.Monad (when)
import GHC.Exts (Int(I#))
import GHC.ST (ST(ST))
import qualified Data.Bytes.Parser as P
import qualified Data.Bytes.Parser.Leb128 as Leb128
import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts

data Value
  = Integer !Int64
    -- ^ Tag number: @0x02@
  | OctetString {-# UNPACK #-} !Bytes
    -- ^ Tag number: @0x04@
  | BitString !Word8 {-# UNPACK #-} !Bytes
    -- ^ Tag number: @0x03@. Has padding bit count and raw bytes.
  | Null
    -- ^ Tag number: @0x05@
  | ObjectIdentifier !(PrimArray Word32)
    -- ^ Tag number: @0x06@
  | PrintableString {-# UNPACK #-} !Bytes
    -- ^ Tag number: @0x13@
  | UtcTime
    -- ^ Tag number: @0x17@
  | Constructed !Class !Word32 !(SmallArray Value)
    -- ^ Constructed values. Includes the class, the tag, and the
    -- concatenated child values.

pattern Sequence :: Word64
pattern Sequence = 0x10

pattern Set :: Word64
pattern Set = 0x11

data Class
  = Universal
  | Application
  | ContextSpecific
  | Private

decode :: Bytes -> Either String Value
decode = P.parseBytesEither parser

takeLength :: Parser String s Word
takeLength = do
  w <- P.any "tried to take the length"
  case testBit w 7 of
    False -> pure (fromIntegral w)
    True -> do
      let go !n !acc = case n of
            0 -> pure acc
            _ -> if acc < 16_000_000
              then do
                x <- P.any "while taking length, ran out of bytes"
                let acc' = fromIntegral @Word8 @Word x + (acc * 256)
                go (n - 1) acc'
              else P.fail "that is a giant length, bailing out"
      go (fromIntegral @Word8 @Word w .&. 0b01111111) 0

objectIdentifier :: Parser String s Value
objectIdentifier = do
  n <- takeLength
  when (n < 1) (P.fail "oid must have length of at least 1")
  P.delimit "oid not enough bytes" "oid leftovers" (fromIntegral n) $ do
    w0 <- P.any "oid expecting first byte"
    let (v1, v2) = quotRem w0 40
        initialSize = 12
    buf0 <- P.effect (PM.newPrimArray initialSize)
    P.effect $ do
      PM.writePrimArray buf0 0 (fromIntegral @Word8 @Word32 v1)
      PM.writePrimArray buf0 1 (fromIntegral @Word8 @Word32 v2)
    let go !ix !sz !buf = P.isEndOfInput >>= \case
          True -> do
            res <- P.effect $ do
              PM.shrinkMutablePrimArray buf ix
              PM.unsafeFreezePrimArray buf
            pure (ObjectIdentifier res)
          False -> if ix < sz
            then do
              w <- Leb128.word32 "bad oid fragment"
              P.effect (PM.writePrimArray buf ix w)
              go (ix + 1) sz buf
            else do
              let newSz = sz * 2
              newBuf <- P.effect $ do
                newBuf <- PM.newPrimArray initialSize
                PM.copyMutablePrimArray newBuf 0 buf 0 sz
                pure newBuf
              go ix newSz newBuf
    go 0 initialSize buf0

-- TODO: support big tags (where base tag equals 31)
constructed :: Class -> Word8 -> Parser String s Value
constructed !clz !tag = do
  n <- takeLength
  P.delimit "constructed not enough bytes" "constructed leftovers" (fromIntegral n) $ do
    let initialSize = 8
    buf0 <- P.effect (PM.newSmallArray initialSize errorThunk)
    let go !ix !sz !buf = P.isEndOfInput >>= \case
          True -> do
            res <- P.effect $ do
              buf' <- resizeSmallMutableArray buf ix
              PM.unsafeFreezeSmallArray buf'
            pure (Constructed clz (fromIntegral @Word8 @Word32 tag) res)
          False -> if ix < sz
            then do
              v <- parser
              P.effect (PM.writeSmallArray buf ix v)
              go (ix + 1) sz buf
            else do
              let newSz = sz * 2
              newBuf <- P.effect $ do
                newBuf <- PM.newSmallArray initialSize errorThunk
                PM.copySmallMutableArray newBuf 0 buf 0 sz
                pure newBuf
              go ix newSz newBuf
    go 0 initialSize buf0

resizeSmallMutableArray :: PM.SmallMutableArray s a -> Int -> ST s (PM.SmallMutableArray s a)
resizeSmallMutableArray (PM.SmallMutableArray x) (I# i) =
  ST (\s -> (# Exts.shrinkSmallMutableArray# x i s, PM.SmallMutableArray x #))

errorThunk :: a
{-# noinline errorThunk #-}
errorThunk = errorWithoutStackTrace "Asn.Ber: implementation mistake"

printableString :: Parser String s Value
printableString = do
  n <- takeLength
  bs <- P.take "while decoding printable string, not enough bytes" (fromIntegral n)
  pure (PrintableString bs)

octetString :: Parser String s Value
octetString = do
  n <- takeLength
  bs <- P.take "while decoding octet string, not enough bytes" (fromIntegral n)
  pure (OctetString bs)

bitString :: Parser String s Value
bitString = do
  n <- takeLength
  when (n < 1) (P.fail "bitstring must have length of at least 1")
  padding <- P.any "expected a padding bit count"
  bs <- P.take "while decoding octet string, not enough bytes" (fromIntegral (n - 1))
  pure (BitString padding bs)

-- TODO: write this
integer :: Parser String s Value
integer = do
  n <- takeLength
  bs <- P.take "while decoding integer, not enough bytes" (fromIntegral n)
  pure (Integer 42)

-- TODO: write this
utcTime :: Parser String s Value
utcTime = do
  n <- takeLength
  bs <- P.take "while decoding utctime, not enough bytes" (fromIntegral n)
  pure UtcTime

nullParser :: Parser String s Value
nullParser = do
  n <- takeLength
  case n of
    0 -> pure Null
    _ -> P.fail ("expecting null contents to have length zero, got " ++ show n)

classFromUpperBits :: Word8 -> Class
classFromUpperBits w = case unsafeShiftR w 6 of
  0 -> Universal
  1 -> Application
  2 -> ContextSpecific
  _ -> Private

parser :: Parser String s Value
parser = P.any "expected tag byte" >>= \case
  0x13 -> printableString
  0x02 -> integer
  0x03 -> bitString
  0x04 -> octetString
  0x05 -> nullParser
  0x06 -> objectIdentifier
  0x17 -> utcTime
  b | testBit b 5 -> constructed (classFromUpperBits b) (b .&. 0b00011111)
    | otherwise -> P.fail ("unrecognized tag byte " ++ show b)
