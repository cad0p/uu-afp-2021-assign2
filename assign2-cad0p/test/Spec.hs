import          Test.Tasty
import Test.Assign2.RoseTree 
                      ( qcRoseTree, huRoseTree )



main :: IO ()
main = defaultMain tests

tests       ::  TestTree
tests       =   testGroup "Tests"       [ properties, unitTests ]

properties  ::  TestTree
properties  =   testGroup "Properties"  [ qcProps ]

qcProps     ::  TestTree
qcProps     =   testGroup "QuickCheck"  [ qcTree
                                        , qcRoseTree
                                        , qcTeletype ]

qcTree      ::  TestTree
qcTree      =   testGroup "Tree"        []

qcTeletype  ::  TestTree
qcTeletype  =   testGroup "Teletype"    []



unitTests   ::  TestTree
unitTests   =   testGroup "Unit tests"  [ hUnit ]

hUnit       ::  TestTree
hUnit       =   testGroup "HUnit"       [ huTree
                                        , huRoseTree
                                        , huTeletype ]

huTree      ::  TestTree
huTree      =   testGroup "Tree"        []



huTeletype  ::  TestTree
huTeletype  =   testGroup "Teletype" []