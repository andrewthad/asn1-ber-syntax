{-# language NamedFieldPuns #-}
{-# language NumericUnderscores #-}
{-# language TypeApplications #-}

import Asn.Oid (Oid(Oid))
import Asn.Ber (Value(..),Class(..),Contents(..))
import Data.Word (Word32)
import Data.Int (Int64)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (Gen)
import Test.Tasty (TestTree,defaultMain,testGroup)
import Test.Tasty.QuickCheck ((===))
import Test.Tasty.QuickCheck (testProperty)

import qualified Asn.Ber as Ber
import qualified Asn.Ber.Primitive.Decode as Decode
import qualified Asn.Ber.Primitive.Encode as Encode
import qualified Asn.Oid as Oid
import qualified Asn.Ber.Encode as Ber
import qualified Data.Bytes as Bytes
import qualified Data.Primitive as Prim
import qualified Data.Primitive.SmallArray as SA
import qualified Data.Text.Short as TS
import qualified GHC.Exts as Exts
import qualified Test.QuickCheck.Gen as Gen
import qualified Test.QuickCheck as QC


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "roundtrip"
  [ testProperty "value" $ \val ->
    let bs = Ber.encode val
        val' = Ber.decode bs
     in val' === Right val
  , testProperty "int64" $ QC.forAll (QC.chooseAny @Int64) $ \i -> do
      Just i === Decode.int64 (Encode.int64 i)
  , testProperty "utctime" $ QC.forAll (QC.chooseInt (-100_000_000,1_500_000_000)) $ \i -> do
      let i' = fromIntegral @Int @Int64 i
      Just i' === Decode.utcTime (Encode.utcTime i')
  , testProperty "oid" $ QC.forAll arbitraryOid $ \oid -> do
      Just oid === Decode.oid (Encode.oid oid)
  ]

arbitraryOid :: Gen Oid
arbitraryOid = do
  a <- Gen.chooseBoundedIntegral (0,2)
  b <- Gen.chooseBoundedIntegral (0,39)
  rest <- QC.listOf (QC.chooseAny @Word32)
  pure (Oid.Oid (Exts.fromList (a:b:rest)))

instance Arbitrary Value where
  arbitrary = 
    Gen.oneof [primitive] -- TODO: add constructed
    where
    primitive = do
      tagClass <- Gen.oneof $ pure <$> [Application, ContextSpecific, Private]
      tagNumber <- Gen.oneof $ Gen.chooseBoundedIntegral <$> [(0,30), (31,65535)]
      contents <- (Primitive . Encode.int64) <$> arbitrary
      pure Value{tagClass,tagNumber,contents}

-- x    aUniversal = do
-- x      let tagClass = Universal
-- x      Gen.oneof
-- x        [ do
-- x            let tagNumber = 0x02
-- x            contents <- Integer <$> arbitrary
-- x            pure Value{tagClass,tagNumber,contents}
-- x        , do
-- x            let tagNumber = 0x17
-- x            contents <- UtcTime <$> QC.choose
-- x              (-100_000_000,1_500_000_000)
-- x            pure Value{tagClass,tagNumber,contents}
-- x        , do
-- x            let tagNumber = 0x01
-- x            contents <- Boolean <$> arbitrary
-- x            pure Value{tagClass,tagNumber,contents}
-- x        , do
-- x            contents <- OctetString <$> arbitrary
-- x            let tagNumber = 0x04
-- x            pure Value{tagClass,tagNumber,contents}
-- x        , do
-- x            let tagNumber = 0x03
-- x            contents <- BitString <$> Gen.chooseBoundedIntegral (0,7) <*> arbitrary
-- x            pure Value{tagClass,tagNumber,contents}
-- x        , do
-- x            let tagNumber = 0x05
-- x            let contents = Null
-- x            pure Value{tagClass,tagNumber,contents}
-- x        , do
-- x            let tagNumber = 0x06
-- x            a <- Gen.chooseBoundedIntegral (0,2)
-- x            b <- Gen.chooseBoundedIntegral (0,39)
-- x            rest <- arbitrary @[Word32]
-- x            let contents = ObjectIdentifier (Oid.Oid (Exts.fromList (a:b:rest)))
-- x            pure Value{tagClass,tagNumber,contents}
-- x        , do
-- x            let tagNumber = 0x0C
-- x            contents <- Utf8String <$> arbitrary
-- x            pure Value{tagClass,tagNumber,contents}
-- x        , do
-- x            let tagNumber = 0x13
-- x            chars <- Gen.listOf $ Gen.elements $ concat
-- x              [ ['A'..'Z']
-- x              , ['a'..'z']
-- x              , ['0'..'9']
-- x              , " '()+,-./:=?"
-- x              ]
-- x            let contents = PrintableString $ TS.fromString chars
-- x            pure Value{tagClass,tagNumber,contents}
-- x        -- UtcTime -- TODO
-- x        ]
-- x    aConstructed = do
-- x      tagClass <- arbitrary
-- x      tagNumber <- arbitrary
-- x      contents <- Constructed . SA.smallArrayFromList <$> Gen.sized go
-- x      pure Value{tagClass,tagNumber,contents}
-- x      where
-- x      go :: Int -> Gen [Value]
-- x      go 0 = pure []
-- x      go n = Gen.resize (n `div` 10) $ Gen.listOf arbitrary

instance Arbitrary Class where
  arbitrary = Gen.oneof $ pure <$>
    [ Universal
    , Application
    , ContextSpecific
    , Private
    ]

instance Arbitrary Bytes.Bytes where
  arbitrary = Bytes.fromLatinString <$> arbitrary

instance Arbitrary TS.ShortText where
  arbitrary = TS.fromString <$> arbitrary
