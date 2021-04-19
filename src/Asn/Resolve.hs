{-# language BangPatterns #-}
{-# language DeriveFunctor #-}
{-# language DerivingStrategies #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}

-- | Transform between Haskell values and the 'Value' type. The instance you
-- write for 'ToAsn' and 'FromAsn' assume a schema. I (Eric) think this is
-- reasonable because I expect each schema to be one-to-one with data types.
module Asn.Resolve
  ( Parser
  , run
  , MemberParser
  -- * Combinators
  , fail
  , integer
  -- TODO bitString
  , octetString
  , null
  , oid
  , utf8String
  , printableString
  , sequence
  , index
  , sequenceOf
  , withTag
  , chooseTag
  -- * Error Breadcrumbs
  , Path(..)
  -- * Re-Exports
  , Value
  , Contents
  , Class(..)
  ) where

import Prelude hiding (fail,null,reverse,null,sequence)

import Asn.Ber (Value(..), Contents(..), Class(..))
import Asn.Oid (Oid)
import Control.Applicative (Alternative(..))
import Control.Monad.ST (ST, runST)
import Data.Bifunctor (first)
import Data.Bytes (Bytes)
import Data.Int (Int64)
import Data.Primitive (SmallArray,SmallMutableArray)
import Data.Text.Short (ShortText)
import Data.Word (Word32)

import qualified Data.Primitive as PM
import qualified Asn.Ber as Ber


newtype Parser a = P { unP :: Path -> Either Path a }
  deriving stock (Functor)

instance Applicative Parser where
  pure x = P $ \_ -> Right x
  a <*> b = P $ \p -> unP a p <*> unP b p

instance Monad Parser where
  a >>= k = P $ \p -> unP a p >>= \x -> unP (k x) p

instance Alternative Parser where
  empty = fail
  a <|> b = P $ \p -> case unP a p of
    Right val -> Right val
    Left err1 -> case unP b p of
      Right val -> Right val
      Left err2 -> Left $ longerPath err1 err2

run :: Parser a -> Either Path a
run r = first reverse $ unP r Nil

newtype MemberParser a = MP
  { unMP :: SmallArray Value -> Path -> Either Path a }
  deriving stock Functor

instance Applicative MemberParser where
  pure a = MP (\_ _ -> Right a)
  MP f <*> MP g = MP $ \p mbrs ->
    f p mbrs <*> g p mbrs


fail :: Parser a
fail = P $ Left

unresolved :: (Bytes -> Either String a) -> Bytes -> Parser a
unresolved f bytes = either (const fail) pure (f bytes)

integer :: Value -> Parser Int64
integer = \case
  Value{contents=Integer n} -> pure n
  Value{contents=Unresolved bytes} -> unresolved Ber.decodeInteger bytes
  _ -> fail

octetString :: Value -> Parser Bytes
octetString = \case
  Value{contents=OctetString bs} -> pure bs
  Value{contents=Unresolved bytes} -> unresolved Ber.decodeOctetString bytes
  _ -> fail

null :: Value -> Parser ()
null = \case
  Value{contents=Null} -> pure ()
  Value{contents=Unresolved bytes} -> unresolved Ber.decodeNull bytes
  _ -> fail

oid :: Value -> Parser Oid
oid = \case
  Value{contents=ObjectIdentifier objId} -> pure objId
  Value{contents=Unresolved bytes} -> unresolved Ber.decodeObjectId bytes
  _ -> fail

utf8String :: Value -> Parser ShortText
utf8String = \case
  Value{contents=Utf8String str} -> pure str
  Value{contents=Unresolved bytes} -> unresolved Ber.decodeUtf8String bytes
  _ -> fail

printableString :: Value -> Parser ShortText
printableString = \case
  Value{contents=PrintableString str} -> pure str
  Value{contents=Unresolved bytes} -> unresolved Ber.decodePrintableString bytes
  _ -> fail

sequenceOf :: forall a. (Value -> Parser a) -> Value -> Parser (SmallArray a)
sequenceOf k = \case
  Value{tagNumber=16, contents=Constructed vals} -> P $ \p -> runST $ do
    dst <- PM.newSmallArray (PM.sizeofSmallArray vals) undefined
    go vals dst p 0
  _ -> fail
  where
  go :: forall s.
       SmallArray Value
    -> SmallMutableArray s a
    -> Path
    -> Int
    -> ST s (Either Path (SmallArray a))
  go src dst p0 ix
    | ix < PM.sizeofSmallArray src = do
      let val = PM.indexSmallArray src ix
      case unP (k val) (Index ix p0) of
        Left err -> pure $ Left err
        Right rval -> do
          PM.writeSmallArray dst ix rval
          go src dst p0 (ix + 1)
    | otherwise = Right <$> PM.unsafeFreezeSmallArray dst

sequence :: MemberParser a -> Value -> Parser a
sequence k = \case
  Value{contents=Constructed vals} -> P (unMP k vals)
  _ -> fail

index :: Int -> (Value -> Parser a) -> MemberParser a
index ix k = MP $ \vals p ->
  let p' = Index ix p in
  if ix < PM.sizeofSmallArray vals
    then unP (k $ PM.indexSmallArray vals ix) p'
    else Left p'

withTag :: Class -> Word32 -> (Value -> Parser a) -> Value -> Parser a
withTag cls num k v = case v of
  Value{tagClass,tagNumber}
    | tagClass == cls && tagNumber == num ->
      P $ \p -> unP (k v) (Tag cls num p)
  _ -> fail

chooseTag :: [(Class, Word32, Value -> Parser a)] -> Value -> Parser a
chooseTag tab0 v@Value{tagClass,tagNumber} = go tab0
  where
  go [] = fail
  go ((cls, num, k) : rest)
    | cls == tagClass && num == tagNumber
      = P $ \p -> unP (k v) (Tag cls num p)
    | otherwise = go rest

data Path
  = Nil
  | Index {-# UNPACK #-} !Int !Path
  -- ^ into the nth field of a constructed type
  | Tag !Class !Word32 !Path
  -- ^ into a specific tag
  deriving stock (Eq, Show)

longerPath :: Path -> Path -> Path
longerPath a b = if pathSize 0 a < pathSize 0 b then b else a
  where
  pathSize :: Int -> Path -> Int
  pathSize !acc Nil = acc
  pathSize !acc (Index _ rest) = pathSize (1 + acc) rest
  pathSize !acc (Tag _ _ rest) = pathSize (1 + acc) rest

reverse :: Path -> Path
reverse = go Nil
  where
  go !acc Nil = acc
  go !acc (Index ix rest) = go (Index ix acc) rest
  go !acc (Tag cls num rest) = go (Tag cls num acc) rest
