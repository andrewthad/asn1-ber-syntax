{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language LambdaCase #-}
{-# language NumericUnderscores #-}
{-# language TypeApplications #-}

module Asn.Ber.Primitive.Decode
  ( int64
  , nonnegativeIntegerBytes20
  , utcTime
  , oid
  , null
  , boolean
  , bitString
  , bitStringBytes
  , octetString
  , utf8String
  , printableString
  ) where

import Prelude hiding (null)

import Asn.Oid (Oid(Oid))
import Data.Bytes (Bytes)
import Data.Int (Int64)
import Data.Bits (unsafeShiftL,complement,testBit,(.|.))
import Data.Word (Word8,Word32)
import Data.Bytes.Types (ByteArrayN(ByteArrayN))
import Data.ByteString.Short.Internal (ShortByteString(SBS))
import Control.Monad.ST.Run (runByteArrayST)
import Data.Bytes.Parser (Parser)
import Data.Text.Short (ShortText)

import qualified Data.Bytes.Parser.Base128 as Base128
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Parser.Latin as Latin
import qualified Data.Bytes.Parser as P
import qualified Data.Primitive as PM
import qualified Data.Text.Short as TS
import qualified Data.Text.Short.Unsafe as TS
import qualified Chronos

-- | Universal tag number: @0x01@. Expects boolean encoding.
boolean :: Bytes -> Maybe Bool 
boolean !b = case Bytes.length b of
  1 -> Just $! Bytes.unsafeIndex b 0 /= 0
  _ -> Nothing

-- | Universal tag number: @0x04@. Expects octet string encoding.
octetString :: Bytes -> Maybe Bytes
octetString = Just

-- | Universal tag number: @0x0C@. Expects utf8 string encoding.
utf8String :: Bytes -> Maybe ShortText
utf8String = TS.fromShortByteString . Bytes.toShortByteString

-- | Universal tag number: @0x13@. Expects printable string encoding.
printableString :: Bytes -> Maybe ShortText
printableString bs =
  if Bytes.all isPrintable bs
    then pure $! ba2stUnsafe $! Bytes.toByteArrayClone bs
    else Nothing

ba2stUnsafe :: PM.ByteArray -> TS.ShortText
ba2stUnsafe (PM.ByteArray x) = TS.fromShortByteStringUnsafe (SBS x)

-- | Universal tag number: @0x02@. Expects integer encoding.
int64 :: Bytes -> Maybe Int64
int64 !content = case Bytes.length content of
  -- Empty input is not an acceptable way to encode the number zero.
  0 -> Nothing
  _ ->
    let isNegative = testBit (Bytes.unsafeIndex content 0) 7
        loopBody acc b = (acc `unsafeShiftL` 8) .|. fromIntegral @Word8 @Int64 b
        seed = if isNegative then complement 0 else (0 :: Int64)
     in Just $! Bytes.foldl' loopBody seed content

-- | Universal tag number: @0x02@. Expects integer encoding. If the
-- integer is negative, this rejects it.
-- If it is a number less than @256^20@, accept it. Integers with a
-- small number of bytes are padded with zeros on the left.
nonnegativeIntegerBytes20 :: Bytes -> Maybe (ByteArrayN 20)
nonnegativeIntegerBytes20 !b = case Bytes.length b of
  -- Empty input is not an acceptable way to encode the number zero.
  0 -> Nothing
  len
    | w0 <- Bytes.unsafeIndex b 0 -> case len of
        1 -> if testBit w0 7
          then Nothing -- reject negative number
          else Just $! ByteArrayN $ runByteArrayST $ do
            dst <- PM.newByteArray 20
            PM.setByteArray dst 0 20 (0 :: Word8)
            PM.writeByteArray dst 19 w0
            PM.unsafeFreezeByteArray dst
        _ | w1 <- Bytes.unsafeIndex b 1 -> if testBit w0 7
              then Nothing -- reject negative number
              else case w0 of
                0 -> if testBit w1 7
                  then if len <= 21
                    then Just $! ByteArrayN $ runByteArrayST $ do
                      dst <- PM.newByteArray 20
                      PM.setByteArray dst 0 20 (0 :: Word8)
                      Bytes.unsafeCopy dst (21 - len) (Bytes.unsafeDrop 1 b)
                      PM.unsafeFreezeByteArray dst
                    else Nothing -- more than 20 bytes of data
                  else Nothing -- reject nine leading zero bits
                _ -> if len <= 20
                  then Just $! ByteArrayN $ runByteArrayST $ do
                    dst <- PM.newByteArray 20
                    PM.setByteArray dst 0 20 (0 :: Word8)
                    Bytes.unsafeCopy dst (20 - len) b
                    PM.unsafeFreezeByteArray dst
                  else Nothing -- more than 20 bytes of data

-- | Universal tag number: @0x17@. Expects @utctime@ encoding.
-- If the seconds part is missing, treating it as zero seconds past the
-- minute. A timestamp with a non-Zulu time zone is normalized by
-- converting to Zulu time. Returns the number of seconds since the epoch.
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
utcTime :: Bytes -> Maybe Int64
utcTime = P.parseBytesMaybe (utcTimeParser <* P.endOfInput "utctime expected eof")

utcTimeParser :: Parser String s Int64
utcTimeParser = do
  !year0 <- twoDigits "utctime year digit problem"
  let !year = if year0 >= 50 then 1900 + year0 else 2000 + year0
  !month <- twoDigits "utctime month digit problem"
  !day <- twoDigits "utctime day digit problem"
  !hour <- twoDigits "utctime hour digit problem"
  !minute <- twoDigits "utctime minute digit problem"
  -- Offset must be provided in seconds.
  let finishWithoutSeconds !offset = case Chronos.timeFromYmdhms year month day hour minute 0 of
        Chronos.Time ns -> pure $! offset + div ns 1_000_000_000
  let finishWithSeconds !offset !seconds = case Chronos.timeFromYmdhms year month day hour minute seconds of
        Chronos.Time ns -> pure $! offset + div ns 1_000_000_000
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

-- | Universal tag number: @0x06@. Expects object identifier encoding.
oid :: Bytes -> Maybe Oid
oid = P.parseBytesMaybe objectIdentifierParser

-- | Universal tag number: @0x05@. Expects null encoding.
null :: Bytes -> Maybe ()
null b = case Bytes.length b of
  0 -> Just ()
  _ -> Nothing

-- This consumes all the way to EOF.
objectIdentifierParser :: Parser String s Oid
objectIdentifierParser = do
  P.isEndOfInput >>= \case
    True -> P.fail "oid must have length of at least 1"
    False -> pure ()
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

-- | Universal tag number: @0x04@. Expects bitstring format.
bitString :: Bytes -> Maybe (Word8,Bytes)
bitString = P.parseBytesMaybe bitStringParser

-- | Universal tag number: @0x04@. Expects bitstring format. Expects the number of
-- padding bits to be zero.
bitStringBytes :: Bytes -> Maybe Bytes
bitStringBytes !b
  | Bytes.length b >= 1, 0 <- Bytes.unsafeHead b = Just $! Bytes.unsafeDrop 1 b
  | otherwise = Nothing

-- Always consumes all the bytes.
bitStringParser :: Parser String s (Word8,Bytes)
bitStringParser = do
  padding <- P.any "expected a padding bit count"
  if padding >= 8
    then P.fail "bitstring has more than 7 padding bits"
    else do
      bs <- P.remaining
      pure (padding,bs)

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
