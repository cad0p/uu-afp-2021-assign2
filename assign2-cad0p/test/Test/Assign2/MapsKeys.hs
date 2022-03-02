module Test.Assign2.MapsKeys (qcLookup, huLookup) where


import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map as Map

import Assign2.MapsKeys (lookupAll, lookupSome)

qcLookup :: TestTree
qcLookup =  testGroup "Lookup" []


huLookup :: TestTree
huLookup =  testGroup "Lookup"  [ huLookupSome
                                , huLookupAll ]

huLookupSome :: TestTree
huLookupSome =  testGroup "lookupSome"
  [ testCase "1" (
      lookupSome [1 :: Int, 3] (Map.fromList[(1, "found 1"), (2, "found 2")])
    @?=
      ["found 1"]
  )
  ]

huLookupAll :: TestTree
huLookupAll =  testGroup "lookupAll"
  [ testCase "1" (
      lookupAll [1 :: Int, 3] (Map.fromList[(1, "found 1"), (2, "found 2")])
    @?=
      Nothing
  )
  , testCase "2" (
      lookupAll [1 :: Int, 2] (Map.fromList[(1, "found 1"), (2, "found 2")])
    @?=
      Just ["found 1", "found 2"]
  )
  , testCase "3" (
      lookupAll [1 :: Int, 2, 3] (Map.fromList[(1, "found 1"), (2, "found 2")])
    @?=
      Just ["found 1", "found 2"]
  )]