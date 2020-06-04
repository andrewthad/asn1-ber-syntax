{-# language BangPatterns #-}
{-# language BinaryLiterals #-}
{-# language DerivingStrategies #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language NumericUnderscores #-}
{-# language PatternSynonyms #-}
{-# language TypeApplications #-}
{-# language UnboxedTuples #-}

module Asn.Ber
  ( Value(..)
  , decode
    -- * Constructed Patterns
  , pattern Set
  , pattern Sequence
  ) where

import Control.Monad (when)
import Data.Bits ((.&.),testBit,unsafeShiftR,unsafeShiftL,complement,clearBit)
import Data.Bytes (Bytes)
import Data.Bytes.Parser (Parser)
import Data.ByteString.Short.Internal (ShortByteString(SBS))
import Data.Int (Int64)
import Data.Primitive (SmallArray,PrimArray)
import Data.Word (Word8,Word32)
import GHC.Exts (Int(I#))
import GHC.ST (ST(ST))

import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Parser as P
import qualified Data.Bytes.Parser.Base128 as Base128
import qualified Data.Primitive as PM
import qualified Data.Text.Short as TS
import qualified Data.Text.Short.Unsafe as TS
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
  | Utf8String {-# UNPACK #-} !TS.ShortText
    -- ^ Tag number: @0x0C@
  | PrintableString {-# UNPACK #-} !TS.ShortText
    -- ^ Tag number: @0x13@
  | UtcTime
    -- ^ Tag number: @0x17@
  | Constructed !Class !Word32 !(SmallArray Value)
    -- ^ Constructed values. Includes the class, the tag, and the
    -- concatenated child values.
  deriving stock (Show)

pattern Sequence :: Word32
pattern Sequence = 0x10

pattern Set :: Word32
pattern Set = 0x11

data Class
  = Universal
  | Application
  | ContextSpecific
  | Private
  deriving stock (Show)

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
              w <- Base128.word32 "bad oid fragment"
              P.effect (PM.writePrimArray buf ix w)
              go (ix + 1) sz buf
            else do
              let newSz = sz * 2
              newBuf <- P.effect $ do
                newBuf <- PM.newPrimArray initialSize
                PM.copyMutablePrimArray newBuf 0 buf 0 sz
                pure newBuf
              go ix newSz newBuf
    go 2 initialSize buf0


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

utf8String :: Parser String s Value
utf8String = do
  n <- takeLength
  bs <- P.take "while decoding UTF-8 string, not enough bytes" (fromIntegral n)
  case TS.fromShortByteString (ba2sbs (Bytes.toByteArrayClone bs)) of
    Nothing -> P.fail "found non-UTF-8 byte sequences in printable string"
    Just r -> pure (Utf8String r)

printableString :: Parser String s Value
printableString = do
  n <- takeLength
  bs <- P.take "while decoding printable string, not enough bytes" (fromIntegral n)
  if Bytes.all isPrintable bs
    then pure $! PrintableString $! ba2stUnsafe $! Bytes.toByteArrayClone bs
    else P.fail "found non-printable characters in printable string"

isPrintable :: Word8 -> Bool
isPrintable = \case
  0x20 -> True
  0x27 -> True
  0x28 -> True
  0x29 -> True
  0x2B -> True
  0x2C -> True
  0x2D -> True
  0x2E -> True
  0x2F -> True
  0x3A -> True
  0x3D -> True
  0x3F -> True
  w | w >= 0x41 && w <= 0x5A -> True
  w | w >= 0x61 && w <= 0x7A -> True
  w | w >= 0x30 && w <= 0x39 -> True
  _ -> False

octetString :: Parser String s Value
octetString = do
  n <- takeLength
  bs <- P.take "while decoding octet string, not enough bytes" (fromIntegral n)
  pure (OctetString bs)

-- The whole bit string thing is kind of janky, but SNMP does not use
-- it, so it is not terribly important.
bitString :: Parser String s Value
bitString = do
  n <- takeLength
  when (n < 1) (P.fail "bitstring must have length of at least 1")
  padding <- P.any "expected a padding bit count"
  bs <- P.take "while decoding octet string, not enough bytes" (fromIntegral (n - 1))
  pure (BitString padding bs)

integer :: Parser String s Value
integer = takeLength >>= \case
  n | n <= 8 -> do
    -- there are not zero-length integer encodings, in BER,
    -- so a cons pattern will always succeed
    content <- P.take "while decoding integer, not enough bytes" (fromIntegral n)
    let isNegative = case Bytes.uncons content of
          Just (hd, _) -> testBit hd 7
          Nothing -> errorWithoutStackTrace "Asn.Ber.integer: programmer error"
        loopBody acc b = acc `unsafeShiftL` 8 + fromIntegral @Word8 @Int64 b
        unsigned = Bytes.foldl' loopBody 0 content
    pure $ Integer $ if isNegative
      then complement (clearBit unsigned (fromIntegral $ 8 * n - 1)) + 1
      else unsigned
    | otherwise -> do
      -- TODO parse bignums
      P.fail (show n ++ "-octet integer is too large to store in an Int64")

-- TODO: write this
utcTime :: Parser String s Value
utcTime = do
  n <- takeLength
  _ <- P.take "while decoding utctime, not enough bytes" (fromIntegral n)
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
  0x0C -> utf8String
  0x17 -> utcTime
  b | testBit b 5 -> constructed (classFromUpperBits b) (b .&. 0b00011111)
    | otherwise -> P.fail ("unrecognized tag byte " ++ show b)

ba2stUnsafe :: PM.ByteArray -> TS.ShortText
ba2stUnsafe (PM.ByteArray x) = TS.fromShortByteStringUnsafe (SBS x)

ba2sbs :: PM.ByteArray -> ShortByteString
ba2sbs (PM.ByteArray x) = SBS x
