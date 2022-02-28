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
import          Test.Tasty.HUnit              as HU
-- import          Test.Tasty.QuickCheck         as QC
-- import          Test.Tasty.QuickCheck.Laws
-- import Data.Typeable (Proxy(..))

import          Assign2.Tree     (Tree(..))
import          Assign2.RoseTree (RoseTree(..))

-- import          ArbitraryTest


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
  [ testGroup "Functor Laws" [
      -- testFunctorLaws 
      --   (Proxy :: Proxy Tree) (Proxy :: Proxy ())
      --   (Proxy :: Proxy Bool) (Proxy :: Proxy Int) (Proxy :: Proxy Char)
      --   (const (==))
    ]
    -- QC.testProperty "Functor Laws" []
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
            -- test cases taken from here:
            -- https://stackoverflow.com/questions/57950226/how-do-i-map-functions-over-a-rosetree-in-applicative-haskell
              [ testCase  "RoseLeaf r" (
                -- https://kseo.github.io/posts/2017-01-04-type-defaulting-in-haskell.html
                  ((RoseNode (+1) :: [RoseTree (Int -> Int)] -> RoseTree (Int -> Int))
                    [RoseNode (*2) [] :: RoseTree (Int -> Int)] <*> RoseLeaf)
                  @?= RoseLeaf)
              , testCase "RoseLeaf l" (
                (RoseLeaf <*>
                  RoseNode (7 :: Int) [RoseNode 1 [], RoseNode 2 [], RoseNode 3 [RoseNode 4 []]]
                  :: RoseTree Int)
                @?= RoseLeaf)
              , testCase "RoseNode 1" (
                  (RoseNode (+1) [] <*>
                    RoseNode (7 :: Int) [RoseNode 1 [], RoseNode 2 [], RoseNode 3 [RoseNode 4 []]])
                  @?= RoseNode 8 [RoseNode 2 [],RoseNode 3 [],RoseNode 4 [RoseNode 5 []]]
                )
              , testCase "RoseNode 2" (
                  (RoseNode (+1) [RoseNode (*2) []] <*> 
                    RoseNode (5 :: Int) [RoseNode 2 [], RoseNode 8 [RoseNode 1 []]])
                  @?= RoseNode 6 [RoseNode 3 [],RoseNode 9 [RoseNode 2 []],
                                  RoseNode 10 [RoseNode 4 [],RoseNode 16 [RoseNode 2 []]]]
                )
              ]

huTeletype  ::  TestTree
huTeletype  =   testGroup "Teletype" []