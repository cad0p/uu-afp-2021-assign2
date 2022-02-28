module ArbitraryTest where

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

import Test.QuickCheck

import Assign2.Tree (Tree(..))
import Control.Monad (liftM2, liftM)

instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = sized tree' where
    tree' 0 = liftM Leaf arbitrary
    tree' n =
      oneof [ liftM Leaf arbitrary
            , liftM2 Node subtree subtree ]
      where subtree = tree' (n `div` 2)
      

instance (CoArbitrary a) => CoArbitrary (Tree a) where
  coarbitrary (Leaf n) =
    variant 0 . coarbitrary n
  coarbitrary (Node t1 t2) =
    variant 1 . coarbitrary t1 . coarbitrary t2