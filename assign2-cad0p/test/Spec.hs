import Test.Tasty

import Test.Assign2.Tree      ( qcTree    , huTree     )
import Test.Assign2.RoseTree  ( qcRoseTree, huRoseTree )
import Test.Assign2.Teletype  ( qcTeletype, huTeletype )
import Test.Assign2.MapsKeys  ( qcLookup  , huLookup   )



main :: IO ()
main = defaultMain tests

tests       ::  TestTree
tests       =   testGroup "Tests"       [ properties, unitTests ]

properties  ::  TestTree
properties  =   testGroup "Properties"  [ qcProps ]

qcProps     ::  TestTree
qcProps     =   testGroup "QuickCheck"  [ qcTree
                                        , qcRoseTree
                                        , qcTeletype
                                        , qcLookup ]



unitTests   ::  TestTree
unitTests   =   testGroup "Unit tests"  [ hUnit ]

hUnit       ::  TestTree
hUnit       =   testGroup "HUnit"       [ huTree
                                        , huRoseTree
                                        , huTeletype
                                        , huLookup ]

