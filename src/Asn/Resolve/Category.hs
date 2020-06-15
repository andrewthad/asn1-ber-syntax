{-# language BangPatterns #-}
{-# language DeriveFunctor #-}
{-# language DerivingStrategies #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TupleSections #-}

-- | Transform between Haskell values and the 'Value' type. The instance you
-- write for 'ToAsn' and 'FromAsn' assume a schema. I (Eric) think this is
-- reasonable because I expect each schema to be one-to-one with data types.
module Asn.Resolve.Category
  ( Parser
  , run
  -- * Combinators
  , (>->)
  , fail
  , integer
  -- TODO bitString
  , octetString
  , null
  , oid
  , utf8String
  , printableString
  , sequenceOf
  , sequence
  , index
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
import Control.Applicative (Alternative(..))
import Control.Monad.ST (ST, runST)
import Data.Bifunctor (bimap,second)
import Data.Bytes (Bytes)
import Data.Int (Int64)
import Data.Primitive (PrimArray,SmallArray,SmallMutableArray)
import Data.Text.Short (ShortText)
import Data.Word (Word32)

import qualified Data.Primitive as PM
import qualified Asn.Ber as Ber


newtype Parser a b = P { unP :: a -> Path -> Either Path (b, Path) }

instance Functor (Parser a) where
  fmap f (P k) = P $ \v p -> case k v p of
    Right (x, p') -> Right (f x, p')
    Left err -> Left err

instance Applicative (Parser a) where
  pure x = P $ \_ p -> Right (x, p)
  (P g) <*> (P h) = P $ \v p -> case g v p of
    Right (f, _) -> case h v p of
      Right (x, p') -> Right (f x, p')
      Left err -> Left err
    Left err -> Left err

(>->) :: Parser a b -> Parser b c -> Parser a c
(P f) >-> (P g) = P $ \v p -> case f v p of
  Right (v', p') -> g v' p'
  Left err -> Left err

-- instance Monad Parser where
--   a >>= k = P $ \p -> unP a p >>= \x -> unP (k x) p

instance Alternative (Parser a) where
  empty = fail
  P f <|> (P g) = P $ \v p -> case f v p of
    Right r -> Right r
    Left err1 -> case g v p of
      Right r -> Right r
      Left err2 -> Left $ longerPath err1 err2

run :: Parser a b -> a -> Either Path b
run r v = bimap reverse fst $ unP r v Nil

-- newtype MemberParser a = MP
--   { unMP :: SmallArray Value -> Path -> Either Path a }
--   deriving stock Functor

-- instance Applicative MemberParser where
--   pure a = MP (\_ _ -> Right a)
--   MP f <*> MP g = MP $ \p mbrs ->
--     f p mbrs <*> g p mbrs


fail :: Parser a b
fail = P $ const Left

unresolved :: (Bytes -> Either String a) -> Bytes -> Path -> Either Path (a, Path)
unresolved f bs p = bimap (const p) (,p) (f bs)

integer :: Parser Value Int64
integer = P $ \v p -> case v of
  Value{contents=Integer n} -> Right (n, p)
  Value{contents=Unresolved bytes} -> unresolved Ber.decodeInteger bytes p
  _ -> Left p

octetString :: Parser Value Bytes
octetString = P $ \v p -> case v of
  Value{contents=OctetString bs} -> Right (bs, p)
  Value{contents=Unresolved bytes} -> unresolved Ber.decodeOctetString bytes p
  _ -> Left p

null :: Parser Value ()
null = P $ \v p -> case v of
  Value{contents=Null} -> Right ((), p)
  Value{contents=Unresolved bytes} -> unresolved Ber.decodeNull bytes p
  _ -> Left p

oid :: Parser Value (PrimArray Word32)
oid = P $ \v p -> case v of
  Value{contents=ObjectIdentifier objId} -> Right (objId, p)
  Value{contents=Unresolved bytes} -> unresolved Ber.decodeObjectId bytes p
  _ -> Left p

utf8String :: Parser Value ShortText
utf8String = P $ \v p -> case v of
  Value{contents=Utf8String str} -> Right (str, p)
  Value{contents=Unresolved bytes} -> unresolved Ber.decodeUtf8String bytes p
  _ -> Left p

printableString :: Parser Value ShortText
printableString = P $ \v p -> case v of
  Value{contents=PrintableString str} -> Right (str, p)
  Value{contents=Unresolved bytes} -> unresolved Ber.decodePrintableString bytes p
  _ -> Left p

sequenceOf :: forall a. Parser Value a -> Parser Value (SmallArray a)
sequenceOf k = P $ \v p -> case v of
  Value{tagNumber=16, contents=Constructed vals} -> runST $ do
    dst <- PM.newSmallArray (PM.sizeofSmallArray vals) undefined
    second (,p) <$> go vals dst p 0
  _ -> Left p
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
      case unP k val (Index ix p0) of
        Left err -> pure $ Left err
        Right (rval, _) -> do
          PM.writeSmallArray dst ix rval
          go src dst p0 (ix + 1)
    | otherwise = Right <$> PM.unsafeFreezeSmallArray dst

sequence :: Parser Value (SmallArray Value)
sequence = P $ \v p -> case v of
  Value{contents=Constructed vals} -> Right (vals, p)
  _ -> Left p

index :: Int -> (Parser Value a) -> Parser (SmallArray Value) a
index ix k = P $ \vals p ->
  let p' = Index ix p in
  if ix < PM.sizeofSmallArray vals
    then unP k (PM.indexSmallArray vals ix) p'
    else Left p'

withTag :: Class -> Word32 -> Parser Value Value
withTag cls num = P $ \v p -> case v of
  Value{tagClass,tagNumber}
    | tagClass == cls && tagNumber == num ->
      Right (v, Tag cls num p)
  _ -> Left p

chooseTag :: [(Class, Word32, Parser Value a)] -> Parser Value a
chooseTag tab = foldr (<|>) fail (adapt <$> tab)
  where
  adapt (cls, num, k) = withTag cls num >-> k


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
