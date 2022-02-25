module Assign2.RoseTree where

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


data RoseTree a = RoseNode a [RoseTree a] | RoseLeaf
  deriving Show


{-|
  https://stackoverflow.com/questions/69695834/defining-fmap-for-a-rosetree

  >>> *Assign2.RoseTree> fmap (+1) (RoseNode 1 [(RoseNode 3 [])])
  >>  RoseNode 2 [RoseNode 4 []]
-}
instance Functor RoseTree where
  fmap _ RoseLeaf = RoseLeaf
  fmap f (RoseNode a rs) =
    RoseNode (f a) ((fmap . fmap) f rs)

instance Functor [] where
  fmap _ []       = []
  fmap f (x : xs) = f x : fmap f xs

