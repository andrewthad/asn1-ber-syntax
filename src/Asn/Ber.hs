{-# language BangPatterns #-}
{-# language BinaryLiterals #-}
{-# language DerivingStrategies #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language MultiWayIf #-}
{-# language NamedFieldPuns #-}
{-# language NumericUnderscores #-}
{-# language PatternSynonyms #-}
{-# language RankNTypes #-}
{-# language TypeApplications #-}
{-# language UnboxedTuples #-}

module Asn.Ber
  ( Value(..)
  , Contents(..)
  , Class(..)
  , decode
    -- * Constructed Patterns
  , pattern Set
  , pattern Sequence
  ) where

import Asn.Oid (Oid(..))
import Control.Monad (when)
import Data.Bits ((.&.),(.|.),testBit,unsafeShiftR,unsafeShiftL,complement)
import Data.Bytes (Bytes)
import Data.Bytes.Parser (Parser)
import Data.ByteString.Short.Internal (ShortByteString(SBS))
import Data.Int (Int64)
import Data.Primitive (SmallArray)
import Data.Word (Word8,Word32)
import GHC.Exts (Int(I#))
import GHC.ST (ST(ST))

import qualified Chronos
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Parser as P
import qualified Data.Bytes.Parser.Latin as Latin
import qualified Data.Bytes.Parser.Base128 as Base128
import qualified Data.Primitive as PM
import qualified Data.Text.Short as TS
import qualified Data.Text.Short.Unsafe as TS
import qualified GHC.Exts as Exts

data Value = Value
  { tagClass :: !Class
  , tagNumber :: !Word32
  , contents :: !Contents
  }
  deriving stock (Show)
  deriving stock (Eq)

data Contents
  -- x = Boolean !Bool
  -- x   -- ^ Tag number: @0x01@
  -- x | Integer !Int64
  -- x   -- ^ Tag number: @0x02@
  -- x | OctetString {-# UNPACK #-} !Bytes
  -- x   -- ^ Tag number: @0x04@
  -- x | BitString !Word8 {-# UNPACK #-} !Bytes
  -- x   -- ^ Tag number: @0x03@. Has padding bit count and raw bytes.
  -- x | Null
  -- x   -- ^ Tag number: @0x05@
  -- x | ObjectIdentifier !Oid
  -- x   -- ^ Tag number: @0x06@
  -- x | Utf8String {-# UNPACK #-} !TS.ShortText
  -- x   -- ^ Tag number: @0x0C@
  -- x | PrintableString {-# UNPACK #-} !TS.ShortText
  -- x   -- ^ Tag number: @0x13@
  -- x | UtcTime !Int64
  -- x   -- ^ Tag number: @0x17@. Number of seconds since the epoch.
  -- x   -- The following guidance is inspired by RFC 5280:
  -- x   --
  -- x   -- * A two-digit year greater than or equal to 50 is interpreted
  -- x   --   as 19XX, and a two-digit year less than 50 is intepreted
  -- x   --   as 20XX.
  -- x   -- * Everything is converted to Zulu time zone. Unlike RFC 5280,
  -- x   --   we do not require Zulu, but we convert everything to it.
  -- x   -- * When seconds are absent, we treat the timestamp as one where
  -- x   --   the seconds are zero. That is, we understand 2303252359Z as
  -- x   --   2023-03-25T23:59:00Z.
  = Constructed {-# UNPACK #-} !(SmallArray Value)
    -- ^ Constructed value contents in concatenation order.
    -- The class and tag are held in `Value`.
  | Primitive {-# UNPACK #-} !Bytes
    -- ^ Values that require information about interpreting application,
    -- context-specific, or private tag.
  deriving stock (Show)
  deriving stock (Eq)

pattern Sequence :: Word32
pattern Sequence = 0x10

pattern Set :: Word32
pattern Set = 0x11

data Class
  = Universal
  | Application
  | ContextSpecific
  | Private
  deriving stock (Eq,Show)

decode :: Bytes -> Either String Value
decode = P.parseBytesEither parser

decodePayload :: (forall s. Word -> Parser String s a) -> Bytes -> Either String a
decodePayload k bs =
  let len = fromIntegral @Int @Word (Bytes.length bs)
   in P.parseBytesEither (k len) bs

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

primitive :: Parser String s Contents
primitive = do
  n <- takeLength
  bs <- P.take "while decoding unresolved contents, not enough bytes" (fromIntegral n)
  pure (Primitive bs)

constructed :: Parser String s Contents
constructed = do
  n <- takeLength
  P.delimit "constructed not enough bytes" "constructed leftovers" (fromIntegral n) $ do
    let initialSize = 8
    buf0 <- P.effect (PM.newSmallArray initialSize errorThunk)
    let go !ix !sz !buf = P.isEndOfInput >>= \case
          True -> do
            res <- P.effect $ do
              buf' <- resizeSmallMutableArray buf ix
              PM.unsafeFreezeSmallArray buf'
            pure (Constructed res)
          False -> if ix < sz
            then do
              v <- parser
              P.effect (PM.writeSmallArray buf ix v)
              go (ix + 1) sz buf
            else do
              let newSz = sz * 2
              newBuf <- P.effect $ do
                newBuf <- PM.newSmallArray newSz errorThunk
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


classFromUpperBits :: Word8 -> Class
classFromUpperBits w = case unsafeShiftR w 6 of
  0 -> Universal
  1 -> Application
  2 -> ContextSpecific
  _ -> Private

parser :: Parser String s Value
parser = do
  b <- P.any "expected tag byte"
  let tagClass = classFromUpperBits b
      isConstructed = testBit b 5
  tagNumber <- case b .&. 0b00011111 of
        31 -> Base128.word32 "bad big tag"
        num -> pure $ fromIntegral @Word8 @Word32 num
  contents <- if isConstructed then constructed else primitive
  pure Value{tagClass, tagNumber, contents}

ba2stUnsafe :: PM.ByteArray -> TS.ShortText
ba2stUnsafe (PM.ByteArray x) = TS.fromShortByteStringUnsafe (SBS x)

ba2sbs :: PM.ByteArray -> ShortByteString
ba2sbs (PM.ByteArray x) = SBS x
