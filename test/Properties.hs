{-# language NamedFieldPuns #-}
{-# language NumericUnderscores #-}
{-# language TypeApplications #-}

import Asn.Ber (Value(..),Class(..),Contents(..))
import Data.Word (Word32)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (Gen)
import Test.Tasty (TestTree,defaultMain,testGroup)
import Test.Tasty.QuickCheck ((===))
import Test.Tasty.QuickCheck (testProperty)

import qualified Asn.Ber as Ber
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
tests = testGroup "encoder-decoder"
  [ testProperty "encoded value is decodable" $ \val ->
    let bs = Ber.encode val
        val' = Ber.decode bs
     in val' === Right val
  ]

instance Arbitrary Value where
  arbitrary = do
    Gen.oneof [aUniversal, anUnresolved] -- TODO
    where
    anUnresolved = do
      tagClass <- Gen.oneof $ pure <$> [Application, ContextSpecific, Private]
      tagNumber <- Gen.oneof $ Gen.chooseBoundedIntegral <$> [(0,30), (31,65535)]
      contents <- Unresolved <$> arbitrary
      pure Value{tagClass,tagNumber,contents}
    aUniversal = do
      let tagClass = Universal
      Gen.oneof
        [ do
            let tagNumber = 0x02
            contents <- Integer <$> arbitrary
            pure Value{tagClass,tagNumber,contents}
        , do
            let tagNumber = 0x17
            contents <- UtcTime <$> QC.choose
              (-100_000_000,1_500_000_000)
            pure Value{tagClass,tagNumber,contents}
        , do
            let tagNumber = 0x01
            contents <- Boolean <$> arbitrary
            pure Value{tagClass,tagNumber,contents}
        , do
            contents <- OctetString <$> arbitrary
            let tagNumber = 0x04
            pure Value{tagClass,tagNumber,contents}
        , do
            let tagNumber = 0x03
            contents <- BitString <$> Gen.chooseBoundedIntegral (0,7) <*> arbitrary
            pure Value{tagClass,tagNumber,contents}
        , do
            let tagNumber = 0x05
            let contents = Null
            pure Value{tagClass,tagNumber,contents}
        , do
            let tagNumber = 0x06
            a <- Gen.chooseBoundedIntegral (0,2)
            b <- Gen.chooseBoundedIntegral (0,39)
            rest <- arbitrary @[Word32]
            let contents = ObjectIdentifier (Oid.Oid (Exts.fromList (a:b:rest)))
            pure Value{tagClass,tagNumber,contents}
        , do
            let tagNumber = 0x0C
            contents <- Utf8String <$> arbitrary
            pure Value{tagClass,tagNumber,contents}
        , do
            let tagNumber = 0x13
            chars <- Gen.listOf $ Gen.elements $ concat
              [ ['A'..'Z']
              , ['a'..'z']
              , ['0'..'9']
              , " '()+,-./:=?"
              ]
            let contents = PrintableString $ TS.fromString chars
            pure Value{tagClass,tagNumber,contents}
        -- UtcTime -- TODO
        ]
    aConstructed = do
      tagClass <- arbitrary
      tagNumber <- arbitrary
      contents <- Constructed . SA.smallArrayFromList <$> Gen.sized go
      pure Value{tagClass,tagNumber,contents}
      where
      go :: Int -> Gen [Value]
      go 0 = pure []
      go n = Gen.resize (n `div` 10) $ Gen.listOf arbitrary

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
