import Test.QuickCheck.Arbitrary.ADT
import Test.SmallCheck.Series
import Test.SmallCheck.Series.Instances
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC

import Data.List
import Data.Ord
import Data.Set (Set, fromList)

import Immortelle.CMS

import Debug.Trace

instance (Serial m a, Ord a) => Serial m (Set a) where
  series = fromList <$> series

instance Monad m => Serial m BraceletType
instance Monad m => Serial m HairType
instance Monad m => Serial m BroochType
instance Monad m => Serial m ProductCategory
instance Monad m => Serial m AuthorCode
instance Monad m => Serial m Color
instance Monad m => Serial m Patination
instance Monad m => Serial m Stone
instance Monad m => Serial m Incrustation
instance Monad m => Serial m VendorCode

instance ToADTArbitrary BraceletType
instance ToADTArbitrary HairType
instance ToADTArbitrary BroochType
instance ToADTArbitrary ProductCategory
instance ToADTArbitrary AuthorCode
instance ToADTArbitrary Color
instance ToADTArbitrary Patination
instance ToADTArbitrary Stone
instance ToADTArbitrary Incrustation
instance ToADTArbitrary VendorCode

instance Arbitrary BraceletType where
  arbitrary = genericArbitrary
instance Arbitrary HairType where
  arbitrary = genericArbitrary
instance Arbitrary BroochType where
  arbitrary = genericArbitrary
instance Arbitrary ProductCategory where
  arbitrary = genericArbitrary
instance Arbitrary AuthorCode where
  arbitrary = genericArbitrary
instance Arbitrary Color where
  arbitrary = genericArbitrary
instance Arbitrary Patination where
  arbitrary = genericArbitrary
instance Arbitrary Stone where
  arbitrary = genericArbitrary
instance Arbitrary Incrustation where
  arbitrary = genericArbitrary
instance Arbitrary VendorCode where
  arbitrary = genericArbitrary

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

vendorEncodeDecode :: VendorCode -> Bool
vendorEncodeDecode vc = decodeVendorCode (encodeVendorCode vc) == Right vc

scProps = adjustOption (const $ SmallCheckDepth 3) $ testGroup "(checked by SmallCheck)"
  [ SC.testProperty "decodeVendorCode . encodeVendorCode == Right" vendorEncodeDecode
  ]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "decodeVendorCode . encodeVendorCode == Right" vendorEncodeDecode
  ]

unitTests = testGroup "Unit tests"
  [
  ]
