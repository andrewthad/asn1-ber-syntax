{-# language DerivingStrategies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}

import Asn.Ber (decode)
import Asn.Oid (Oid)
import Data.Bytes (Bytes)

import qualified Asn.Oid as Oid
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit
import qualified Test.Tasty.QuickCheck as QC
import qualified Asn.Ber as Ber
import qualified GHC.Exts as Exts

main :: IO ()
main = Tasty.defaultMain $ Tasty.testGroup "tests"
  [ HUnit.testCase "signed-data" $ case decode exampleSignedData of
      Left e -> fail ("Decoding signed data failed with: " ++ show e)
      Right _ -> pure ()
  , HUnit.testCase "bad-integer-zero" $ case decode badIntegerZero of
      Left _ -> pure ()
      Right _ -> fail ("Decoding bad integer zero succeeded unexpectedly.")
  , HUnit.testCase "good-integer-zero" $ case decode goodIntegerZero of
      Left _ -> fail "Decoding good integer zero failed unexpectedly."
      Right Ber.Value{Ber.contents = Ber.Integer 0} -> pure ()
      Right _ -> fail "Decoding good integer zero gave bad result."
  , QC.testProperty "oid-encode-decode" $ \oid -> case Oid.size oid of
      0 -> True
      _ -> case Oid.fromShortTextDot (Oid.toShortText oid) of
        Nothing -> False
        Just r -> r == oid
  ]

badIntegerZero :: Bytes
badIntegerZero = Exts.fromList [0x02,0x00]

goodIntegerZero :: Bytes
goodIntegerZero = Exts.fromList [0x02,0x01,0x00]

-- Taken from https://www.di-mgt.com.au/docs/examplesPKCS.txt
exampleSignedData :: Bytes
exampleSignedData = Exts.fromList
  [0x30,0x82,0x02,0x50,0x06,0x09,0x2a,0x86,0x48,0x86,0xf7,0x0d,0x01,0x07,0x02,0xa0
  ,0x82,0x02,0x41,0x30,0x82,0x02,0x3d,0x02,0x01,0x01,0x31,0x0e,0x30,0x0c,0x06,0x08
  ,0x2a,0x86,0x48,0x86,0xf7,0x0d,0x02,0x02,0x05,0x00,0x30,0x28,0x06,0x09,0x2a,0x86
  ,0x48,0x86,0xf7,0x0d,0x01,0x07,0x01,0xa0,0x1b,0x04,0x19,0x45,0x76,0x65,0x72,0x79
  ,0x6f,0x6e,0x65,0x20,0x67,0x65,0x74,0x73,0x20,0x46,0x72,0x69,0x64,0x61,0x79,0x20
  ,0x6f,0x66,0x66,0x2e,0xa0,0x82,0x01,0x5e,0x30,0x82,0x01,0x5a,0x30,0x82,0x01,0x04
  ,0x02,0x04,0x14,0x00,0x00,0x29,0x30,0x0d,0x06,0x09,0x2a,0x86,0x48,0x86,0xf7,0x0d
  ,0x01,0x01,0x02,0x05,0x00,0x30,0x2c,0x31,0x0b,0x30,0x09,0x06,0x03,0x55,0x04,0x06
  ,0x13,0x02,0x55,0x53,0x31,0x1d,0x30,0x1b,0x06,0x03,0x55,0x04,0x0a,0x13,0x14,0x45
  ,0x78,0x61,0x6d,0x70,0x6c,0x65,0x20,0x4f,0x72,0x67,0x61,0x6e,0x69,0x7a,0x61,0x74
  ,0x69,0x6f,0x6e,0x30,0x1e,0x17,0x0d,0x39,0x32,0x30,0x39,0x30,0x39,0x32,0x32,0x31
  ,0x38,0x30,0x36,0x5a,0x17,0x0d,0x39,0x34,0x30,0x39,0x30,0x39,0x32,0x32,0x31,0x38
  ,0x30,0x35,0x5a,0x30,0x42,0x31,0x0b,0x30,0x09,0x06,0x03,0x55,0x04,0x06,0x13,0x02
  ,0x55,0x53,0x31,0x1d,0x30,0x1b,0x06,0x03,0x55,0x04,0x0a,0x13,0x14,0x45,0x78,0x61
  ,0x6d,0x70,0x6c,0x65,0x20,0x4f,0x72,0x67,0x61,0x6e,0x69,0x7a,0x61,0x74,0x69,0x6f
  ,0x6e,0x31,0x14,0x30,0x12,0x06,0x03,0x55,0x04,0x03,0x13,0x0b,0x54,0x65,0x73,0x74
  ,0x20,0x55,0x73,0x65,0x72,0x20,0x31,0x30,0x5b,0x30,0x0d,0x06,0x09,0x2a,0x86,0x48
  ,0x86,0xf7,0x0d,0x01,0x01,0x01,0x05,0x00,0x03,0x4a,0x00,0x30,0x47,0x02,0x40,0x0a
  ,0x66,0x79,0x1d,0xc6,0x98,0x81,0x68,0xde,0x7a,0xb7,0x74,0x19,0xbb,0x7f,0xb0,0xc0
  ,0x01,0xc6,0x27,0x10,0x27,0x00,0x75,0x14,0x29,0x42,0xe1,0x9a,0x8d,0x8c,0x51,0xd0
  ,0x53,0xb3,0xe3,0x78,0x2a,0x1d,0xe5,0xdc,0x5a,0xf4,0xeb,0xe9,0x94,0x68,0x17,0x01
  ,0x14,0xa1,0xdf,0xe6,0x7c,0xdc,0x9a,0x9a,0xf5,0x5d,0x65,0x56,0x20,0xbb,0xab,0x02
  ,0x03,0x01,0x00,0x01,0x30,0x0d,0x06,0x09,0x2a,0x86,0x48,0x86,0xf7,0x0d,0x01,0x01
  ,0x02,0x05,0x00,0x03,0x41,0x00,0x45,0x1a,0xa1,0xe1,0xaa,0x77,0x20,0x4a,0x5f,0xcd
  ,0xf5,0x76,0x06,0x9d,0x02,0xf7,0x32,0xc2,0x6f,0x36,0x7b,0x0d,0x57,0x8a,0x6e,0x64
  ,0xf3,0x9a,0x91,0x1f,0x47,0x95,0xdf,0x09,0x94,0x34,0x05,0x11,0xa0,0xd1,0xdf,0x4a
  ,0x20,0xb2,0x6a,0x77,0x4c,0xca,0xef,0x75,0xfc,0x69,0x2e,0x54,0xc2,0xa1,0x93,0x7c
  ,0x07,0x11,0x26,0x9d,0x9b,0x16,0x31,0x81,0x9b,0x30,0x81,0x98,0x02,0x01,0x01,0x30
  ,0x34,0x30,0x2c,0x31,0x0b,0x30,0x09,0x06,0x03,0x55,0x04,0x06,0x13,0x02,0x55,0x53
  ,0x31,0x1d,0x30,0x1b,0x06,0x03,0x55,0x04,0x0a,0x13,0x14,0x45,0x78,0x61,0x6d,0x70
  ,0x6c,0x65,0x20,0x4f,0x72,0x67,0x61,0x6e,0x69,0x7a,0x61,0x74,0x69,0x6f,0x6e,0x02
  ,0x04,0x14,0x00,0x00,0x29,0x30,0x0c,0x06,0x08,0x2a,0x86,0x48,0x86,0xf7,0x0d,0x02
  ,0x02,0x05,0x00,0x30,0x0d,0x06,0x09,0x2a,0x86,0x48,0x86,0xf7,0x0d,0x01,0x01,0x01
  ,0x05,0x00,0x04,0x40,0x05,0xfa,0x6a,0x81,0x2f,0xc7,0xdf,0x8b,0xf4,0xf2,0x54,0x25
  ,0x09,0xe0,0x3e,0x84,0x6e,0x11,0xb9,0xc6,0x20,0xbe,0x20,0x09,0xef,0xb4,0x40,0xef
  ,0xbc,0xc6,0x69,0x21,0x69,0x94,0xac,0x04,0xf3,0x41,0xb5,0x7d,0x05,0x20,0x2d,0x42
  ,0x8f,0xb2,0xa2,0x7b,0x5c,0x77,0xdf,0xd9,0xb1,0x5b,0xfc,0x3d,0x55,0x93,0x53,0x50
  ,0x34,0x10,0xc1,0xe1]

instance QC.Arbitrary Oid where
  arbitrary = Oid.Oid . Exts.fromList <$> QC.arbitrary
