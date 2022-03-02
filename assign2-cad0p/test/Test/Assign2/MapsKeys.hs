module Test.Assign2.MapsKeys (qcLookup, huLookup) where


import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map as Map

import Assign2.MapsKeys (lookupSome)

qcLookup :: TestTree
qcLookup =  testGroup "Lookup" []


huLookup :: TestTree
huLookup =  testGroup "Lookup" [ huLookupSome ]

huLookupSome :: TestTree
huLookupSome =  testGroup "lookupSome"
  [ testCase "1" (
      lookupSome [1 :: Int, 3] (Map.fromList[(1, "found 1"), (2, "found 2")])
    @?=
      ["found 1"]
  )
  ]