import          Test.Tasty
import          Test.Tasty.QuickCheck         as QC
import          Test.Tasty.QuickCheck.Laws.Functor (testFunctorLaws)

-- import qualified Assign2 as A2

main :: IO ()
main = defaultMain tests

tests       ::  TestTree
tests       =   testGroup "Tests"       [ properties ]

properties  ::  TestTree
properties  =   testGroup "Properties"  [ qcProps ]

qcProps     ::  TestTree
qcProps     =   testGroup "QuickCheck"  [ qcTree
                                        , qcRoseTree
                                        , qcTeletype ]

qcTree      ::  TestTree
qcTree      =   testGroup "Tree"
  [ -- QC.testProperty "Functor Laws" []
      -- (testFunctorLaws A2.Functor A2.Tree)
  ]

qcRoseTree  ::  TestTree
qcRoseTree  =   testGroup "RoseTree" []

qcTeletype  ::  TestTree
qcTeletype  =   testGroup "Teletype" []