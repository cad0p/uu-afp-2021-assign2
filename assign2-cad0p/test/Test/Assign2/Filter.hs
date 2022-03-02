module Test.Assign2.Filter (qcFilter, huFilter) where


import Test.Tasty
import Test.Tasty.HUnit

import Assign2.Filter (gfilter)
import Assign2.Tree   (Tree(..))



qcFilter :: TestTree
qcFilter = testGroup "Filter" []

huFilter :: TestTree
huFilter = testGroup "Filter" [ huGfilter ]

{-| if I add this orphan instance everything works..
-}
instance Foldable Tree where
  foldMap f (Leaf a)    = f a
  foldMap f (Node a b)  = foldMap f a <> foldMap f b

{-|
  Error: No instance for (Foldable Tree) arising from a use of ‘gfilter’
-}
huGfilter :: TestTree
huGfilter = testGroup "gfilter" 
  [ testCase "1" (
      gfilter odd (Node (Leaf (1 :: Int)) (Leaf 2))
    @?=
      [1]
  )
  , testCase "2" (
      gfilter odd (Node (Leaf (1 :: Int)) (Node (Leaf 2) (Leaf 3)))
    @?=
      [1, 3]
  )
  ]