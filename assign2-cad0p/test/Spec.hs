import Assign2        ( Functor
                      , Applicative
                      , Monad
                      , Foldable
                      , Traversable
                      , fmap 
                      , pure
                      , (<*>)
                      , return
                      , (>>=)
                      , foldMap
                      , traverse
                      )


import Prelude hiding ( Functor
                      , Applicative
                      , Monad
                      , Foldable
                      , Traversable
                      , fmap 
                      , pure
                      , (<*>)
                      , return
                      , (>>=)
                      , foldMap
                      , traverse
                      )

import          Test.Tasty
import          Test.Tasty.QuickCheck         as QC
import          Test.Tasty.HUnit              as HU
import Assign2.RoseTree (RoseTree(..), Applicative)
-- import          Test.Tasty.QuickCheck.Laws.Functor (testFunctorLaws)

-- import qualified Assign2 as A2

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
qcTree      =   testGroup "Tree"
  [ -- QC.testProperty "Functor Laws" []
      -- (testFunctorLaws A2.Functor A2.Tree)
  ]

qcRoseTree  ::  TestTree
qcRoseTree  =   testGroup "RoseTree" []

qcTeletype  ::  TestTree
qcTeletype  =   testGroup "Teletype" []



unitTests   ::  TestTree
unitTests   =   testGroup "Unit tests"  [ hUnit ]

hUnit       ::  TestTree
hUnit       =   testGroup "HUnit"   [ huTree
                                    , huRoseTree
                                    , huTeletype ]

huTree      ::  TestTree
huTree      =   testGroup "Tree"
  [ -- hu.testProperty "Functor Laws" []
      -- (testFunctorLaws A2.Functor A2.Tree)
  ]

huRoseTree  ::  TestTree
huRoseTree  =   testGroup "RoseTree" [ huRoseTreeApplicative ]

huRoseTreeApplicative
            :: TestTree
huRoseTreeApplicative
            =   testGroup "Applicative"
              [ testCase  "RoseLeaf r" ( assertEqual ""
                  ((RoseNode (+1) :: [RoseTree (Int -> Int)] -> RoseTree (Int -> Int)) 
                    [RoseNode (*2) [] :: RoseTree (Int -> Int)] <*> RoseLeaf)
                  RoseLeaf)
              ]

huTeletype  ::  TestTree
huTeletype  =   testGroup "Teletype" []