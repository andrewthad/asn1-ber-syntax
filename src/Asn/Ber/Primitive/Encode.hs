{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language DuplicateRecordFields #-}
{-# language NumericUnderscores #-}
{-# language TypeApplications #-}

module Asn.Ber.Primitive.Encode
  ( int64
  , utcTime
  , oid
  ) where

import Asn.Oid (Oid(Oid))
import Data.Word (Word8,Word32)
import Data.Int (Int64)
import Data.Bytes (Bytes)
import Data.Bits (testBit,unsafeShiftR,(.&.),(.|.))
import Data.Bytes.Builder (Builder)

import qualified Chronos
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Chunks as Chunks
import qualified Data.Bytes.Builder as Builder
import qualified GHC.Exts as Exts
import qualified Data.Bytes.Builder.Bounded as BB
import qualified Arithmetic.Nat as Nat
import qualified Data.Primitive as PM
import qualified Data.Primitive.Contiguous as C

-- | Encode 64-bit integer using primitive @integer@ format.
int64 :: Int64 -> Bytes
int64 n = Exts.fromList minimized
  where
  byteList =
    [ fromIntegral @Int64 @Word8 $ 0xFF .&. (n `unsafeShiftR` bits)
    | bits <- [56,48..0]
    ]
  minimized :: [Word8]
  minimized
    | n < 0 =
      case dropWhile (==0xFF) byteList of
        bs'@(hd:_) | hd `testBit` 7 -> bs'
        bs' -> 0xFF:bs'
    | otherwise =
      case dropWhile (==0x00) byteList of
        bs'@(hd:_) | not (hd `testBit` 7) -> bs'
        bs' -> 0x00:bs'

-- | Encode seconds since the epoch using primitive @utctime@ format.
utcTime :: Int64 -> Bytes
utcTime !epochSeconds = 
  let t = Chronos.Time (epochSeconds * 1_000_000_000)
   in case Chronos.timeToDatetime t of
        Chronos.Datetime
          { datetimeDate = Chronos.Date
            { dateYear = Chronos.Year year
            , dateMonth = Chronos.Month month
            , dateDay = Chronos.DayOfMonth day
            }
          , datetimeTime = Chronos.TimeOfDay
            { timeOfDayHour = hour
            , timeOfDayMinute = minute
            , timeOfDayNanoseconds = nanoseconds
            }
          } -> Bytes.fromByteArray $ BB.run Nat.constant $
            encodeTwoDigit (rem year 100)
            `BB.append`
            encodeTwoDigit (month + 1)
            `BB.append`
            encodeTwoDigit day
            `BB.append`
            encodeTwoDigit hour
            `BB.append`
            encodeTwoDigit minute
            `BB.append`
            encodeTwoDigit (fromIntegral @Int64 @Int (quot nanoseconds 1_000_000_000))
            `BB.append`
            BB.ascii 'Z'

encodeTwoDigit :: Int -> BB.Builder 2
encodeTwoDigit !n =
  BB.word8 (fromIntegral @Int @Word8 (0x30 + quot n 10))
  `BB.append`
  BB.word8 (fromIntegral @Int @Word8 (0x30 + rem n 10))

oid :: Oid -> Bytes
oid = Chunks.concat . Builder.run 128 . oidBuilder

oidBuilder :: Oid -> Builder
oidBuilder (Oid arr)
  | PM.sizeofPrimArray arr < 2 = errorWithoutStackTrace
      "Asn.Ber.Primitive.Encode.oid: Object Identifier must have at least two components"
  | otherwise =
      Builder.word8
        ( fromIntegral @Word @Word8
          ( (fromIntegral @Word32 @Word (PM.indexPrimArray arr 0)) * 40 +
            (fromIntegral @Word32 @Word (PM.indexPrimArray arr 1))
          )
        )
      <>
      C.foldr
        (\w acc -> Builder.word32Vlq w <> acc
        ) mempty (C.slice arr 2 (C.size arr - 2))
