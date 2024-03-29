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
  , decodeInteger
  , decodeOctetString
  , decodeNull
  , decodeObjectId
  , decodeUtf8String
  , decodePrintableString
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
  = Boolean !Bool
    -- ^ Tag number: @0x01@
  | Integer !Int64
    -- ^ Tag number: @0x02@
  | OctetString {-# UNPACK #-} !Bytes
    -- ^ Tag number: @0x04@
  | BitString !Word8 {-# UNPACK #-} !Bytes
    -- ^ Tag number: @0x03@. Has padding bit count and raw bytes.
  | Null
    -- ^ Tag number: @0x05@
  | ObjectIdentifier !Oid
    -- ^ Tag number: @0x06@
  | Utf8String {-# UNPACK #-} !TS.ShortText
    -- ^ Tag number: @0x0C@
  | PrintableString {-# UNPACK #-} !TS.ShortText
    -- ^ Tag number: @0x13@
  | UtcTime !Int64
    -- ^ Tag number: @0x17@. Number of seconds since the epoch.
    -- The following guidance is inspired by RFC 5280:
    --
    -- * A two-digit year greater than or equal to 50 is interpreted
    --   as 19XX, and a two-digit year less than 50 is intepreted
    --   as 20XX.
    -- * Everything is converted to Zulu time zone. Unlike RFC 5280,
    --   we do not require Zulu, but we convert everything to it.
    -- * When seconds are absent, we treat the timestamp as one where
    --   the seconds are zero. That is, we understand 2303252359Z as
    --   2023-03-25T23:59:00Z.
  | Constructed !(SmallArray Value)
    -- ^ Constructed value contents in concatenation order.
    -- The class and tag are held in `Value`.
  | Unresolved {-# UNPACK #-} !Bytes
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

decodeInteger :: Bytes -> Either String Int64
decodeInteger = decodePayload integerPayload

decodeOctetString :: Bytes -> Either String Bytes
decodeOctetString = decodePayload octetStringPayload

decodeNull :: Bytes -> Either String ()
decodeNull = decodePayload nullPayload

decodeObjectId :: Bytes -> Either String Oid
decodeObjectId = decodePayload objectIdentifierPayload

decodeUtf8String :: Bytes -> Either String TS.ShortText
decodeUtf8String = decodePayload utf8StringPayload

decodePrintableString :: Bytes -> Either String TS.ShortText
decodePrintableString = decodePayload printableStringPayload

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

objectIdentifier :: Parser String s Contents
objectIdentifier = fmap ObjectIdentifier . objectIdentifierPayload =<< takeLength

objectIdentifierPayload :: Word -> Parser String s Oid
objectIdentifierPayload len = do
  when (len < 1) (P.fail "oid must have length of at least 1")
  P.delimit "oid not enough bytes" "oid leftovers" (fromIntegral len) $ do
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
            pure (Oid res)
          False -> if ix < sz
            then do
              w <- Base128.word32 "bad oid fragment"
              P.effect (PM.writePrimArray buf ix w)
              go (ix + 1) sz buf
            else do
              let newSz = sz * 2
              newBuf <- P.effect $ do
                newBuf <- PM.newPrimArray newSz
                PM.copyMutablePrimArray newBuf 0 buf 0 sz
                pure newBuf
              go ix newSz newBuf
    go 2 initialSize buf0


unresolved :: Parser String s Contents
unresolved = do
  n <- takeLength
  bs <- P.take "while decoding unresolved contents, not enough bytes" (fromIntegral n)
  pure (Unresolved bs)

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

utf8String :: Parser String s Contents
utf8String = fmap Utf8String . utf8StringPayload =<< takeLength

utf8StringPayload :: Word -> Parser String s TS.ShortText
utf8StringPayload len = do
  bs <- P.take "while decoding UTF-8 string, not enough bytes" (fromIntegral len)
  case TS.fromShortByteString (ba2sbs (Bytes.toByteArrayClone bs)) of
    Nothing -> P.fail "found non-UTF-8 byte sequences in printable string"
    Just r -> pure r


printableString :: Parser String s Contents
printableString = fmap PrintableString . printableStringPayload =<< takeLength

printableStringPayload :: Word -> Parser String s TS.ShortText
printableStringPayload len = do
  bs <- P.take "while decoding printable string, not enough bytes" (fromIntegral len)
  if Bytes.all isPrintable bs
    then pure $! ba2stUnsafe $! Bytes.toByteArrayClone bs
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

octetString :: Parser String s Contents
octetString = fmap OctetString . octetStringPayload =<< takeLength

octetStringPayload :: Word -> Parser String s Bytes
octetStringPayload len = do
  P.take "while decoding octet string, not enough bytes" (fromIntegral len)

-- The whole bit string thing is kind of janky, but SNMP does not use
-- it, so it is not terribly important.
bitString :: Parser String s Contents
bitString = do
  n <- takeLength
  when (n < 1) (P.fail "bitstring must have length of at least 1")
  padding <- P.any "expected a padding bit count"
  if padding >= 8
    then P.fail "bitstring has more than 7 padding bits"
    else do
      bs <- P.take "while decoding octet string, not enough bytes" (fromIntegral (n - 1))
      pure (BitString padding bs)

boolean :: Parser String s Contents
boolean = takeLength >>= \case
  1 -> do
    w <- P.any "expected boolean payload"
    pure $ Boolean $ case w of
      0 -> False
      _ -> True
  _ -> P.fail "boolean length must be 1 byte"

integer :: Parser String s Contents
integer = takeLength >>= \case
  0 -> P.fail "integers must have non-zero length"
  n | n <= 8 -> Integer <$> integerPayload n
    | otherwise -> do
      -- TODO parse bignums
      P.fail (show n ++ "-octet integer is too large to store in an Int64")

integerPayload :: Word -> Parser String s Int64
integerPayload len = do
  content <- P.take "while decoding integer, not enough bytes" (fromIntegral len)
  -- There are not zero-length integer encodings in BER, and we guared
  -- against this above, so taking the head with unsafeIndex is safe.
  let isNegative = testBit (Bytes.unsafeIndex content 0) 7
      loopBody acc b = (acc `unsafeShiftL` 8) .|. fromIntegral @Word8 @Int64 b
  pure $ if isNegative
    then Bytes.foldl' loopBody (complement 0) content
    else Bytes.foldl' loopBody 0 content

-- TODO: write this
utcTime :: Parser String s Contents
utcTime = do
  len <- takeLength
  P.delimit "utctime not enough bytes" "utctime leftovers" (fromIntegral len) $ do
    !year0 <- twoDigits "utctime year digit problem"
    let !year = if year0 >= 50 then 1900 + year0 else 2000 + year0
    !month <- twoDigits "utctime month digit problem"
    !day <- twoDigits "utctime day digit problem"
    !hour <- twoDigits "utctime hour digit problem"
    !minute <- twoDigits "utctime minute digit problem"
    -- Offset must be provided in seconds.
    let finishWithoutSeconds !offset = case Chronos.timeFromYmdhms year month day hour minute 0 of
          Chronos.Time ns -> pure $! UtcTime (offset + div ns 1_000_000_000)
    let finishWithSeconds !offset !seconds = case Chronos.timeFromYmdhms year month day hour minute seconds of
          Chronos.Time ns -> pure $! UtcTime (offset + div ns 1_000_000_000)
    Latin.peek >>= \case
      Nothing -> finishWithoutSeconds 0
      Just c -> case c of
        'Z' -> do
          _ <- P.any "utctime impossible"
          finishWithoutSeconds 0
        '+' -> do
          _ <- P.any "utctime impossible"
          !offsetHour <- twoDigits "utctime offset hour digit problem"
          !offsetMinute <- twoDigits "utctime offset minute digit problem"
          let !offset = fromIntegral @Int @Int64 (negate (60 * (60 * offsetHour + offsetMinute)))
          finishWithoutSeconds offset
        '-' -> do
          _ <- P.any "utctime impossible"
          !offsetHour <- twoDigits "utctime offset hour digit problem"
          !offsetMinute <- twoDigits "utctime offset minute digit problem"
          let !offset = fromIntegral @Int @Int64 (60 * (60 * offsetHour + offsetMinute))
          finishWithoutSeconds offset
        _ | c >= '0', c <= '9' -> do
              seconds <- twoDigits "utctime seconds digit problem"
              Latin.peek >>= \case
                Nothing -> finishWithSeconds 0 seconds
                Just d -> case d of
                  'Z' -> do
                    _ <- P.any "utctime impossible"
                    finishWithSeconds 0 seconds
                  '+' -> do
                    _ <- P.any "utctime impossible"
                    !offsetHour <- twoDigits "utctime offset hour digit problem"
                    !offsetMinute <- twoDigits "utctime offset minute digit problem"
                    let !offset = fromIntegral @Int @Int64 (negate (60 * (60 * offsetHour + offsetMinute)))
                    finishWithSeconds offset seconds
                  '-' -> do
                    _ <- P.any "utctime impossible"
                    !offsetHour <- twoDigits "utctime offset hour digit problem"
                    !offsetMinute <- twoDigits "utctime offset minute digit problem"
                    let !offset = fromIntegral @Int @Int64 (60 * (60 * offsetHour + offsetMinute))
                    finishWithSeconds offset seconds
                  _ -> P.fail "utctime unexpected byte after seconds"
        _ -> P.fail "utctime unexpected byte after minute"

twoDigits :: e -> Parser e s Int
{-# inline twoDigits #-}
twoDigits e = do
  w0 <- P.any e
  w0' <- if w0 >= 0x30 && w0 <= 0x39
    then pure (fromIntegral @Word8 @Int w0 - 0x30)
    else P.fail e
  w1 <- P.any e
  w1' <- if w1 >= 0x30 && w1 <= 0x39
    then pure (fromIntegral @Word8 @Int w1 - 0x30)
    else P.fail e
  pure (w0' * 10 + w1')

nullParser :: Parser String s Contents
nullParser = fmap (const Null) . nullPayload =<< takeLength

nullPayload :: Word -> Parser String s ()
nullPayload 0 = pure ()
nullPayload len = P.fail ("expecting null contents to have length zero, got " ++ show len)


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
  contents <- if
    | Universal <- tagClass
    , not isConstructed
    -> case tagNumber of
      0x01 -> boolean
      0x13 -> printableString
      0x02 -> integer
      0x03 -> bitString
      0x04 -> octetString
      0x05 -> nullParser
      0x06 -> objectIdentifier
      0x0C -> utf8String
      0x17 -> utcTime
      _ -> P.fail ("unrecognized universal primitive tag number " ++ show tagNumber)
    | isConstructed -> constructed
    | otherwise -> unresolved
  pure Value{tagClass, tagNumber, contents}

ba2stUnsafe :: PM.ByteArray -> TS.ShortText
ba2stUnsafe (PM.ByteArray x) = TS.fromShortByteStringUnsafe (SBS x)

ba2sbs :: PM.ByteArray -> ShortByteString
ba2sbs (PM.ByteArray x) = SBS x
