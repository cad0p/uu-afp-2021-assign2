module Test.Assign2.RoseTree
                      ( qcRoseTree
                      , huRoseTree
                      ) where

import Assign2        ( fmap
                      -- , (<$>)
                      , pure
                      , (<*>)
                      -- , return
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
huRoseTree  =   testGroup "RoseTree"    [ huRoseTreeFunctor
                                        , huRoseTreeApplicative
                                        , huRoseTreeMonad
                                        , huRoseTreeFoldable
                                        , huRoseTreeTraversable ]


huRoseTreeFunctor     :: TestTree
huRoseTreeFunctor     = testGroup "Functor"
  [ testCase "1" (
      fmap (+1) (RoseNode 1 [RoseNode 3 [RoseLeaf]])
    @?=
      (RoseNode 2 [RoseNode 4 [RoseLeaf]] :: RoseTree Int)
  )]

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

{-| 'decFun' is a test function to decrease RoseNodes by 1
-}
decFun :: Int -> RoseTree Int
decFun n = if n > 0 then pure (n - 1) else RoseLeaf

huRoseTreeMonad     :: TestTree
huRoseTreeMonad     = testGroup "Monad"
  [ testCase "1" (
      RoseNode 17 [RoseNode 23 [RoseLeaf], RoseNode 29 [RoseLeaf]] >>= decFun
    @?=
      (RoseNode 16 [RoseNode 22 [RoseLeaf],RoseNode 28 [RoseLeaf]] :: RoseTree Int)
  )]


huRoseTreeFoldable     :: TestTree
huRoseTreeFoldable     = testGroup "Foldable"
  [ testCase "1" (
      foldMap show (RoseNode (1 :: Int) [RoseNode 2 [RoseNode 3 [RoseLeaf]], RoseNode 4 [RoseLeaf], RoseNode 5 [RoseNode 6 [RoseLeaf]]])
    @?=
      ("123456" :: String)
  )]


huRoseTreeTraversable     :: TestTree
huRoseTreeTraversable     = testGroup "Traversable"
  [ testCase "1" (
      traverse decApp ( RoseNode 1 [RoseLeaf, RoseNode 2 [RoseLeaf], RoseNode 3 [RoseLeaf]] )
    @?=
      (Just (RoseNode 0 [RoseLeaf,RoseNode 1 [RoseLeaf],RoseNode 2 [RoseLeaf]]) :: Maybe (RoseTree Int))
  )]

