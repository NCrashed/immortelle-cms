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
import Data.Text (unpack)

import Immortelle.CMS

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps = adjustOption (const $ SmallCheckDepth 3) $ testGroup "(checked by SmallCheck)"
  [
  ]

qcProps = testGroup "(checked by QuickCheck)"
  [ 
  ]

unitTests = testGroup "Unit tests"
  [
  ]
