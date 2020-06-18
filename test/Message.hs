{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}

module Message
  ( resolveMessage
  , Message(..)
  ) where

import Prelude hiding (sequence)

import Asn.Resolve

import Data.Bytes (Bytes)
import Data.Int (Int64)
import Data.Primitive (PrimArray,SmallArray)
import Data.Word (Word32)

type ObjectId = PrimArray Word32 -- FIXME there really should be an ObjectId type in Asn module

data Message = Message
  { version :: Int64
  , community :: Bytes
  , pdu :: Pdu
  }
  deriving(Show)
data Pdu
  = GetRequest APdu
  | GetNextRequest APdu
  -- | GetBulkRequest BulkPdus -- TODO
  | Response APdu
  | SetRequest APdu
  | InformRequest APdu
  | SnmpV2Trap APdu
  | Report APdu
  deriving(Show)
data APdu = Pdu
  { requestId :: Int64
  , errorStatus :: Int64
  , errorIndex :: Int64
  , varBinds :: SmallArray VarBind
  }
  deriving(Show)
data VarBind = VarBind
  { name :: ObjectId
  , result :: VarBindResult
  }
  deriving(Show)
data VarBindResult
  = Value ObjectSyntax
  | Unspecified
  | NoSuchObject
  | NoSuchInstance
  | EndOfMibView
  deriving(Show)
data ObjectSyntax
  = IntegerValue Int64
  | StringValue Bytes
  | ObjectIdValue ObjectId
  | IpAddressValue Bytes
  | CounterValue Int64
  | TimeticksValue Int64
  | ArbitraryValue Bytes
  | BigCounterValue Int64
  | UnsignedIntegerValue Int64
  deriving(Show)

resolveMessage :: Value -> Either Path Message
resolveMessage = run . message

message :: Value -> Parser Message
message = sequence $ do
  version <- index 0 integer
  community <- index 1 octetString
  pdu <- index 2 resolvePdu
  pure Message{version,community,pdu}
  where
  resolvePdu = chooseTag
    [ (ContextSpecific, 0, fmap GetRequest . aPdu)
    , (ContextSpecific, 1, fmap GetNextRequest . aPdu)
    -- , (ContextSpecific, 2, fmap GetBulkRequest . bulkPdus) -- TODO
    , (ContextSpecific, 3, fmap Response . aPdu)
    , (ContextSpecific, 4, fmap SetRequest . aPdu)
    , (ContextSpecific, 5, fmap InformRequest . aPdu)
    , (ContextSpecific, 6, fmap SnmpV2Trap . aPdu)
    , (ContextSpecific, 7, fmap Report . aPdu)
    ]
  aPdu :: Value -> Parser APdu
  aPdu = sequence $ do
    requestId <- index 0 integer
    errorStatus <- index 1 integer
    errorIndex <- index 2 integer
    varBinds <- index 3 $ sequenceOf $ sequence $ do
      name <- index 0 oid
      result <- index 1 varBind
      pure VarBind{name,result}
    pure Pdu{requestId,errorStatus,errorIndex,varBinds}
  varBind :: Value -> Parser VarBindResult
  varBind = chooseTag
    [(Application, 1, fmap (Value . CounterValue) . integer)]
    -- TODO
