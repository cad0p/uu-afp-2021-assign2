module Test.Assign2.Tree 
                      ( qcTree
                      , huTree
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

import          Assign2.Tree (Tree(..))


qcTree  ::  TestTree
qcTree  =   testGroup "Tree"    []


huTree  ::  TestTree
huTree  =   testGroup "Tree"    [ huTreeApplicative ]

huTreeApplicative
            :: TestTree
huTreeApplicative
            =   testGroup "Applicative"
            []