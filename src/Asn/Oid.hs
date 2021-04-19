{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}

module Asn.Oid
  ( Oid(..)
  , toShortText
  , fromShortTextDot
  , size
  , index
  , isPrefixOf
  ) where

import Data.List (intercalate)
import Data.Primitive (PrimArray)
import Data.Text.Short (ShortText)
import Data.Word (Word32)
import Text.Read (readMaybe)

import qualified Data.Primitive as Prim
import qualified Data.Primitive.Contiguous as C
import qualified Data.Text.Short as ST


newtype Oid = Oid { getOid :: PrimArray Word32 }
  deriving stock (Show)
  deriving stock (Eq)

toShortText :: Oid -> ShortText
toShortText (Oid arr) = ST.pack $ intercalate "." $ show <$> C.toList arr

fromShortTextDot :: ShortText -> Maybe Oid
fromShortTextDot str = fmap (Oid . Prim.primArrayFromList) $
  mapM (readMaybe . ST.unpack) $ ST.split (== '.') str

size :: Oid -> Int
size = Prim.sizeofPrimArray . getOid

index :: Oid -> Int -> Word32
index (Oid arr) = Prim.indexPrimArray arr

isPrefixOf :: Oid -> Oid -> Bool
isPrefixOf (Oid preArr) (Oid arr)
  | preSize > theSize = False
  | otherwise = go 0
  where
  go !i
    | i >= theSize = True
    | Prim.indexPrimArray preArr i /= Prim.indexPrimArray arr i = False
    | otherwise = go (i + 1)
  preSize = Prim.sizeofPrimArray preArr
  theSize = Prim.sizeofPrimArray arr
