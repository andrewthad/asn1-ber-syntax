{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Asn.Oid
  ( Oid(..)
  , toShortText
  , fromShortTextDot
  , size
  , index
  , take
  , isPrefixOf
  ) where

import Prelude hiding (take)

import Control.Monad.ST (runST)
import Data.List (intercalate)
import Data.Primitive (PrimArray)
import Data.Text.Short (ShortText)
import Data.Word (Word32)
import Data.ByteString.Short.Internal (ShortByteString(SBS))

import qualified Arithmetic.Nat as Nat
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Chunks as Chunks
import qualified Data.Bytes.Builder as Builder
import qualified Data.Bytes.Builder.Bounded as Bounded
import qualified Data.Primitive as Prim
import qualified Data.Primitive.Contiguous as C
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as ST
import qualified Data.Bytes.Parser as Parser
import qualified Data.Bytes.Parser.Latin as Latin


newtype Oid = Oid { getOid :: PrimArray Word32 }
  deriving newtype (Show)
  deriving newtype (Semigroup)
  deriving newtype (Monoid)
  deriving newtype (Eq)
  deriving newtype (Ord)

-- | Encode an OID. Encodes the empty OID as empty text even though
-- this is not a valid encoded OID.
toShortText :: Oid -> ShortText
toShortText (Oid arr) = case sz of
  0 -> ST.empty
  _ -> ba2st $ Chunks.concatU $ Builder.run 256
    ( Builder.word32Dec (Prim.indexPrimArray arr 0)
      <>
      C.foldMap
        (\w -> Builder.fromBounded Nat.constant
          (Bounded.append (Bounded.ascii '.') (Bounded.word32Dec w))
        ) (C.slice arr 1 (sz - 1))
    )
  where
  !sz = Prim.sizeofPrimArray arr

-- | Decode an OID. Returns Nothing if the text is empty.
fromShortTextDot :: ShortText -> Maybe Oid
fromShortTextDot !str =
  let !b = Bytes.fromShortByteString (ST.toShortByteString str)
      !maxPossibleParts = div (Bytes.length b) 2 + 1
   in Parser.parseBytesMaybe
        ( do w0 <- Latin.decWord32 ()
             dst <- Parser.effect $ do
               dst <- Prim.newPrimArray maxPossibleParts
               Prim.writePrimArray dst 0 w0
               pure dst
             let go !ix = Parser.isEndOfInput >>= \case
                   True -> Parser.effect $ do
                     Prim.shrinkMutablePrimArray dst ix
                     dst' <- Prim.unsafeFreezePrimArray dst
                     pure (Oid dst')
                   False -> do
                     Latin.char () '.'
                     w <- Latin.decWord32 ()
                     Parser.effect (Prim.writePrimArray dst ix w)
                     go (ix + 1)
             go 1
        ) b

size :: Oid -> Int
size = Prim.sizeofPrimArray . getOid

index :: Oid -> Int -> Word32
index (Oid arr) = Prim.indexPrimArray arr

take :: Oid -> Int -> Oid
take (Oid preArr) len
  | len >= Prim.sizeofPrimArray preArr = Oid preArr
  | otherwise = runST $ do
    dst <- Prim.newPrimArray len
    Prim.copyPrimArray dst 0 preArr 0 len
    Oid <$> Prim.unsafeFreezePrimArray dst

isPrefixOf :: Oid -> Oid -> Bool
isPrefixOf (Oid preArr) (Oid arr)
  | preSize > theSize = False
  | otherwise = go 0
  where
  go !i
    | i >= preSize = True
    | Prim.indexPrimArray preArr i /= Prim.indexPrimArray arr i = False
    | otherwise = go (i + 1)
  preSize = Prim.sizeofPrimArray preArr
  theSize = Prim.sizeofPrimArray arr

ba2st :: Prim.ByteArray -> ShortText
{-# inline ba2st #-}
ba2st (Prim.ByteArray x) = ST.fromShortByteStringUnsafe (SBS x)
