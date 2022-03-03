import           Test.Tasty

import           Test.Assign2.Filter   (huFilter, qcFilter)
import           Test.Assign2.MapsKeys (huLookup, qcLookup)
import           Test.Assign2.RoseTree (huRoseTree, qcRoseTree)
import           Test.Assign2.Teletype (huTeletype, qcTeletype)
import           Test.Assign2.Tree     (huTree, qcTree)



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
                                        , qcLookup
                                        , qcFilter ]



unitTests   ::  TestTree
unitTests   =   testGroup "Unit tests"  [ hUnit ]

hUnit       ::  TestTree
hUnit       =   testGroup "HUnit"       [ huTree
                                        , huRoseTree
                                        , huTeletype
                                        , huLookup
                                        , huFilter ]

