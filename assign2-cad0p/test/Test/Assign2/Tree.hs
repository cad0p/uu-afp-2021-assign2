module Test.Assign2.Tree 
                      ( qcTree
                      , huTree
                      ) where

import Assign2        ( fmap
                      -- , (<$>)
                      , pure
                      -- , (<*>)
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


import          Assign2.Tree (Tree(..))


qcTree  ::  TestTree
qcTree  =   testGroup "Tree"    []


huTree  ::  TestTree
huTree  =   testGroup "Tree"    [ huTreeFunctor 
                                , huTreeApplicative
                                , huTreeMonad
                                , huTreeFoldable
                                , huTreeTraversable ]

huTreeFunctor     :: TestTree
huTreeFunctor     = testGroup "Functor"
  [ testCase "1" 
      (fmap (+1) ( Node (Leaf 1) (Leaf 2) )
    @?= 
      (Node (Leaf 2) (Leaf 3) :: Tree Int)
  )]

huTreeApplicative :: TestTree
huTreeApplicative = testGroup "Applicative"
  [

  ]

{-| 'decFun' is a test function to decrease Leaves by 1
-}
decFun :: Int -> Tree Int
decFun n = if n > 0 then pure (n - 1) else Leaf 0

huTreeMonad       :: TestTree
huTreeMonad       = testGroup "Monad"
  [ testCase "1" (
      Node (Leaf 1) (Leaf 2) >>= decFun
    @?=
      (Node (Leaf 0) (Leaf 1) :: Tree Int)
  )]

huTreeFoldable    :: TestTree
huTreeFoldable    = testGroup "Foldable"
  [ testCase "1" (
      foldMap show ( Node (Leaf 1) (Leaf (2 :: Int)) )
    @?=
      ("12" :: String)
  )]

huTreeTraversable :: TestTree
huTreeTraversable = testGroup "Traversable"
  [ testCase "1" (
      traverse decApp ( Node (Leaf 1) (Leaf 2) )
    @?=
      (Just (Node (Leaf 0) (Leaf 1)) :: Maybe (Tree Int))
  )]
