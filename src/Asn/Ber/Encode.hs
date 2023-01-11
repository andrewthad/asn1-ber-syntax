{-# language BangPatterns #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language MultiWayIf #-}
{-# language NamedFieldPuns #-}
{-# language TypeApplications #-}

module Asn.Ber.Encode
  ( encode
  ) where

import Prelude hiding (length)

import Asn.Ber (Value(..),Contents(..),Class(..))
import Asn.Oid (Oid(..))
import Control.Monad.ST (runST)
import Data.Bits ((.&.),(.|.),unsafeShiftL,unsafeShiftR,bit,testBit)
import Data.Bytes.Types (Bytes(Bytes))
import Data.ByteString.Short.Internal (ShortByteString(SBS))
import Data.Foldable (foldMap',foldlM)
import Data.Int (Int64)
import Data.Primitive (SmallArray,PrimArray)
import Data.Primitive.ByteArray (byteArrayFromList,ByteArray(ByteArray))
import Data.Word (Word8,Word32)

import qualified Data.Primitive as Prim
import qualified Data.Primitive.Contiguous as C
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Types
import qualified Data.Text.Short as TS

data Encoder
  = Leaf {-# UNPACK #-} !Bytes
  | Node
    { _length :: !Int
    , _children :: !(SmallArray Encoder)
    }
  deriving(Show)

length :: Encoder -> Int
length (Leaf bs) = Bytes.length bs
length a@(Node _ _) = _length a

instance Semigroup Encoder where
  a <> b
    | length a == 0 = b
    | length b == 0 = a
  a <> b = Node
    { _length = length a + length b
    , _children = C.doubleton a b
    }

instance Monoid Encoder where
  mempty = Node 0 mempty

append3 :: Encoder -> Encoder -> Encoder -> Encoder
append3 a b c = Node
  { _length = length a + length b + length c
  , _children = C.tripleton a b c
  }

word8 :: Word8 -> Encoder
word8 = Leaf . Bytes.singleton

singleton :: Bytes -> Encoder
singleton = Leaf

run :: Encoder -> Bytes
run (Leaf bs) = bs
run Node{_length=len0,_children=children0} = runST $ do
  dst <- Prim.newByteArray len0
  let go !ixA eA = case eA of
        Leaf bs -> do
          Bytes.unsafeCopy dst ixA bs
          pure (Bytes.length bs + ixA)
        Node{_length,_children} -> do
          foldlM (\ixB eB -> go ixB eB) ixA _children
  ixC <- foldlM (\ixA e -> go ixA e) 0 children0
  if ixC /= len0
    then errorWithoutStackTrace "Asn.Ber.Encode.run: implementation mistake"
    else do
      dst' <- Prim.unsafeFreezeByteArray dst
      pure Bytes{array=dst',offset=0,length=len0}

encode :: Value -> Bytes
encode = run . encodeValue

encodeValue :: Value -> Encoder
encodeValue v@Value{contents} =
  let theContent = encodeContents contents
   in append3 (valueHeader v) (encodeLength (length theContent)) theContent

valueHeader :: Value -> Encoder
valueHeader Value{tagClass,tagNumber,contents} = byte1 <> extTag
  where
  byte1 = word8 (clsBits .|. pcBits .|. tagBits)
  clsBits = (`unsafeShiftL` 6) $ case tagClass of
    Universal -> 0
    Application -> 1
    ContextSpecific -> 2
    Private -> 3
  pcBits = case contents of
    Constructed _ -> bit 5
    _ -> 0x00
  tagBits = fromIntegral @Word32 @Word8 $ min tagNumber 31
  extTag
    | tagNumber < 31 = mempty
    | otherwise = base128 (fromIntegral @Word32 @Int64 tagNumber) -- FIXME use an unsigned base128 encoder

encodeLength :: Int -> Encoder
encodeLength n
  | n < 128 = word8 $ fromIntegral @Int @Word8 n
  | otherwise =
    let len = base256 (fromIntegral n)
        lenHeader = word8 $ bit 7 .|. (fromIntegral @Int @Word8 (length len))
     in lenHeader <> len

-- Note: UtcTime is missing and will crash the program
encodeContents :: Contents -> Encoder
encodeContents = \case
  Integer n -> base256 n
  OctetString bs -> bytes bs
  BitString padBits bs -> word8 padBits <> bytes bs
  Null -> mempty
  ObjectIdentifier (Oid arr)
    | Prim.sizeofPrimArray arr < 2 -> error "Object Identifier must have at least two components"
    | otherwise -> objectIdentifier arr
  Utf8String str -> utf8String str
  PrintableString str -> printableString str
  Constructed arr -> constructed arr
  Unresolved raw -> bytes raw

------------------ Content Encoders ------------------

base128 :: Int64 -> Encoder
base128 = go False (0 :: Int) []
  where
  go !lastNeg !size acc n =
    let content = fromIntegral @Int64 @Word8 (n .&. 0x7F)
        rest = n `unsafeShiftR` 7
        thisNeg = testBit content 6
        atEnd = (content == 0 && rest == 0 && not lastNeg)
              || (content == 0x7F && rest == (-1) && lastNeg)
     in if size /= 0 && atEnd
        then stop acc
        else
          let content' = (if size == 0 then 0 else 0x80) .|. content
           in go thisNeg (size + 1) (content' : acc) rest
  stop acc = singleton . Bytes.fromByteArray . byteArrayFromList $ acc

base256 :: Int64 -> Encoder
base256 n = singleton $ Bytes.fromByteArray (byteArrayFromList minimized)
  where
  byteList =
    [ fromIntegral @Int64 @Word8 $ 0xFF .&. (n `unsafeShiftR` bits)
    | bits <- [56,48..0]
    ]
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
bytes = singleton

objectIdentifier :: PrimArray Word32 -> Encoder
objectIdentifier arr = firstComps <> mconcat restComps
  where
  firstComps = word8 $ fromIntegral @Word32 @Word8 $
    (40 * Prim.indexPrimArray arr 0) + (Prim.indexPrimArray arr 1)
  restComps = [base128 $ fromIntegral @Word32 @Int64 $ Prim.indexPrimArray arr i
              | i <- [2..Prim.sizeofPrimArray arr - 1]]

utf8String :: TS.ShortText -> Encoder
utf8String str = singleton $ shortTextToBytes $ str

printableString :: TS.ShortText -> Encoder
printableString str = singleton $ shortTextToBytes $ str
  -- utf8 is backwards-compatible with ascii, so just hope that the input text is actually printable ascii

constructed :: SmallArray Value -> Encoder
constructed = foldMap' encodeValue

shortTextToBytes :: TS.ShortText -> Bytes
shortTextToBytes str = case TS.toShortByteString str of
  -- ShortText is already utf8-encoded, so just re-wrap it
  SBS arr -> Bytes.fromByteArray (ByteArray arr)
