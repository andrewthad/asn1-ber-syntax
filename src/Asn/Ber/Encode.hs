{-# language BangPatterns #-}
{-# language LambdaCase #-}
{-# language MultiWayIf #-}
{-# language NamedFieldPuns #-}
{-# language TypeApplications #-}

module Asn.Ber.Encode
  ( encode
  ) where

import Asn.Ber (Value(..),Contents(..),Class(..))
import Data.Bits ((.&.),(.|.),unsafeShiftL,unsafeShiftR,bit,testBit)
import Data.ByteArray.Builder (Builder)
import Data.Bytes (Bytes)
import Data.Int (Int64)
import Data.Primitive.ByteArray (byteArrayFromList)
import Data.Word (Word8)
import Data.Primitive (SmallArray)

import qualified Data.ByteArray.Builder as B
import qualified Data.Bytes as Bytes

data Encoder = E
  { builder :: !Builder
  , size :: !Int
  }

instance Semigroup Encoder where
  a <> b = E
    { builder = builder a <> builder b
    , size = size a + size b
    }
instance Monoid Encoder where
  mempty = E
    { builder = mempty
    , size = 0
    }

encode :: Value -> Builder
encode = builder . encodeValue

encodeValue :: Value -> Encoder
encodeValue v@Value{contents} =
  let theContent = encodeContents contents
   in valueHeader v <> encodeLength (size theContent) <> theContent
   -- in valueHeader v <> base128 (size theContent) <> theContent

valueHeader :: Value -> Encoder
valueHeader Value{tagClass,tagNumber,contents} = byte1 <> extTag
  where
  byte1 = E
    { builder = B.byteArray $ byteArrayFromList [clsBits .|. pcBits .|. tagBits]
    , size = 1
    }
  clsBits = (`unsafeShiftL` 5) $ case tagClass of
    Universal -> 0
    Application -> 1
    ContextSpecific -> 2
    Private -> 3
  pcBits = case contents of
    Constructed _ -> bit 5
    _ -> 0x00
  tagBits = min (fromIntegral @_ @Word8 tagNumber) 31
  extTag
    | tagNumber < 31 = mempty
    | otherwise = undefined -- TODO

encodeLength :: Int -> Encoder
encodeLength n
  | n < 128 = E
    { builder = B.byteArray $ byteArrayFromList [fromIntegral @Int @Word8 n]
    , size = 1
    }
  | otherwise =
    let len = base256 (fromIntegral n)
        lenSize = E
          { builder = B.byteArray $ byteArrayFromList [fromIntegral @Int @Word8 $ size len]
          , size = 1
          }
     in lenSize <> len

encodeContents :: Contents -> Encoder
encodeContents = \case
  Integer n -> base128 n
  OctetString bs -> bytes bs

------------------ Content Encoders ------------------

base128 :: Int64 -> Encoder
base128 = go False 0 []
  where
  go !lastNeg !size acc n =
    let content = fromIntegral @Int64 @Word8 (n .&. 0x7F)
        rest = n `unsafeShiftR` 7
        thisNeg = testBit content 6
        atEnd = (content == 0 && rest == 0 && not lastNeg)
              || (content == 0x7F && rest == (-1) && lastNeg)
     in if size /= 0 && atEnd
        then stop size acc
        else
          let content' = (if size == 0 then 0 else 0x80) .|. content
           in go thisNeg (size + 1) (content' : acc) rest
  stop !size acc = E
    { builder = (B.byteArray . byteArrayFromList $ acc)
    , size
    }

base256 :: Int64 -> Encoder
base256 n = E
    { builder = B.byteArray (byteArrayFromList minimized)
    , size = length minimized
    }
  where
  byteList = [fromIntegral @Int64 @Word8 (n `unsafeShiftR` bits) | bits <- reverse [0,8..56]]
  minimized
    | n < 0 =
      case dropWhile (==0xFF) byteList of
        bs'@(hd:_) | hd `testBit` 7 -> bs'
        bs' -> 0xFF:bs'
    | otherwise =
      case dropWhile (==0x00) byteList of
        bs'@(hd:_) | not (hd `testBit` 7) -> bs'
        bs' -> 0x00:bs'

bytes :: Bytes -> Encoder
bytes bs = E
  { builder = B.bytes bs
  , size = Bytes.length bs
  }
