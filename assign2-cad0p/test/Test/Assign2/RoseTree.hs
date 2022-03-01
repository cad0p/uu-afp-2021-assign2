module Test.Assign2.RoseTree 
                      ( qcRoseTree
                      , huRoseTree
                      ) where

import Assign2        ( Functor
                      , Applicative
                      , Monad
                      , Foldable
                      , Traversable
                      , fmap
                      , (<$>)
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
                      , (<$>)
                      , pure
                      , (<*>)
                      , return
                      , (>>=)
                      , foldMap
                      , traverse
                      )

import          Test.Tasty
import          Test.Tasty.HUnit
import          Test.Assign2.Helpers (decApp)


import          Assign2.RoseTree (RoseTree(..))


qcRoseTree  ::  TestTree
qcRoseTree  =   testGroup "RoseTree"    []


huRoseTree  ::  TestTree
huRoseTree  =   testGroup "RoseTree"    [ huRoseTreeApplicative ]

huRoseTreeApplicative :: TestTree
huRoseTreeApplicative = testGroup "Applicative"
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