{-# LANGUAGE DerivingStrategies #-}

module Asn.Oid
  ( Oid(..)
  ) where

import Data.Primitive (PrimArray)
import Data.Word (Word32)


newtype Oid = Oid { getOid :: PrimArray Word32 }
  deriving stock (Show)
  deriving stock (Eq)
