{-# language BangPatterns #-}
{-# language DeriveFunctor #-}
{-# language DerivingStrategies #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language NamedFieldPuns #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TupleSections #-}
{-# language TypeApplications #-}
{-# language UnboxedTuples #-}

-- | Transform between Haskell values and the 'Value' type. The instance you
-- write for 'ToAsn' and 'FromAsn' assume a schema. I (Eric) think this is
-- reasonable because I expect each schema to be one-to-one with data types.
module Asn.Resolve.Category
  ( Parser
  , run
  -- * Combinators
  , explicit
  , arr
  , (>->)
  , fail
  , primitive
  , int64
  , utcTime
  , word8
  -- TODO bitString
  , octetString
  , octetStringSingleton
  , null
  , oid
  , utf8String
  , printableString
  , sequenceOf
  , sequence
  , set
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
import Asn.Oid (Oid)
import Control.Applicative (Alternative(..))
import Control.Monad.ST (ST, runST)
import Data.Bifunctor (bimap,second)
import Data.Bytes (Bytes)
import Data.Int (Int64)
import Data.Primitive (SmallArray,SmallMutableArray)
import Data.Text.Short (ShortText)
import Data.Word (Word32,Word8)

import qualified Data.Primitive as PM
import qualified Data.Bytes as Bytes
import qualified Asn.Ber.Primitive.Decode as Decode

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

-- | Similar to the @arr@ method of @Arrow@.
arr :: (a -> Maybe b) -> Parser a b
arr f = P $ \v p -> case f v of
  Just v' -> Right (v', p)
  Nothing -> Left p

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

fail :: Parser a b
fail = P $ const Left

liftDecode :: (Bytes -> Maybe a) -> Bytes -> Path -> Either Path (a, Path)
liftDecode f bs p = maybe (Left p) (\x -> Right (x,p)) (f bs)

-- | Expects primitive encoded data. Do not use this for string-like
-- types since it will reject constructed encodings.
primitive :: Parser Value Bytes
primitive = P $ \v p -> case v of
  Value{contents=Primitive bytes} -> Right (bytes,p)
  _ -> Left p

-- | Same as integer but restricts the range.
word8 :: Parser Value Word8
word8 = P $ \v p -> case v of
  Value{contents=Primitive bytes} -> do
    (n,p') <- liftDecode Decode.int64 bytes p
    if n >= 0 && n < 256
      then Right (fromIntegral @Int64 @Word8 n, p')
      else Left p'
  _ -> Left p

utcTime :: Parser Value Int64
utcTime = P $ \v p -> case v of
  Value{contents=Primitive bytes} -> liftDecode Decode.utcTime bytes p
  _ -> Left p

int64 :: Parser Value Int64
int64 = P $ \v p -> case v of
  Value{contents=Primitive bytes} -> liftDecode Decode.int64 bytes p
  _ -> Left p

octetString :: Parser Value Bytes
octetString = P $ \v p -> case v of
  Value{contents=Primitive bytes} -> liftDecode Decode.octetString bytes p
  _ -> Left p

-- | Variant of 'octetString' that expects the @OctetString@ to have
-- exactly one byte. Returns the value of the byte.
octetStringSingleton :: Parser Value Word8
octetStringSingleton = P $ \v p -> case v of
  Value{contents=Primitive bytes} -> do
    (bs,p') <- liftDecode Decode.octetString bytes p
    case Bytes.length bs of
      1 -> Right (Bytes.unsafeIndex bs 0, p')
      _ -> Left p'
  _ -> Left p

null :: Parser Value ()
null = P $ \v p -> case v of
  Value{contents=Primitive bytes} -> liftDecode Decode.null bytes p
  _ -> Left p

oid :: Parser Value Oid
oid = P $ \v p -> case v of
  Value{contents=Primitive bytes} -> liftDecode Decode.oid bytes p
  _ -> Left p

utf8String :: Parser Value ShortText
utf8String = P $ \v p -> case v of
  Value{contents=Primitive bytes} -> liftDecode Decode.utf8String bytes p
  _ -> Left p

printableString :: Parser Value ShortText
printableString = P $ \v p -> case v of
  Value{contents=Primitive bytes} -> liftDecode Decode.printableString bytes p
  _ -> Left p

-- We do not check the tag so that we can reuse this for set.
sequenceOf :: forall a. Parser Value a -> Parser Value (SmallArray a)
sequenceOf k = P $ \v p -> case v of
  Value{contents=Constructed vals} -> runST $ do
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

-- TODO: check the tag
sequence :: Parser Value (SmallArray Value)
sequence = P $ \v p -> case v of
  Value{contents=Constructed vals} -> Right (vals, p)
  _ -> Left p

-- TODO: check the tag
set :: forall a. Parser Value a -> Parser Value (SmallArray a)
set = sequenceOf

index :: Int -> Parser (SmallArray a) a
index ix = P $ \vals p ->
  let p' = Index ix p in
  if ix < PM.sizeofSmallArray vals
    then Right (PM.indexSmallArray vals ix, p')
    else Left p'

withTag :: Class -> Word32 -> Parser Value Value
withTag cls num = P $ \v p -> case v of
  Value{tagClass,tagNumber}
    | tagClass == cls && tagNumber == num ->
      Right (v, Tag cls num p)
  _ -> Left p

explicit :: Word32 -> Parser Value Value
explicit w = P $ \v p -> case v of
  Value{tagClass,tagNumber,contents}
    | tagClass == ContextSpecific
    , tagNumber == w
    , Constructed children <- contents
    , PM.sizeofSmallArray children == 1
    , (# ch #) <- PM.indexSmallArray## children 0 ->
      Right (ch, Tag ContextSpecific w p)
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
