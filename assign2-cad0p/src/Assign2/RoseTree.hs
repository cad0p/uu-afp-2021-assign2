{-|
Module      : Assign2.RoseTree
Description : RoseTree instatiation of the classes
Copyright   : (c) Pier Carlo Cadoppi, 2022
License     : MIT License
Maintainer  : p.c.cadoppi@students.uu.nl
Stability   : experimental


-}

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

{-|
  https://stackoverflow.com/questions/57950226/how-do-i-map-functions-over-a-rosetree-in-applicative-haskell
-}
instance Applicative RoseTree where
  pure a = RoseNode a []
  RoseLeaf <*> _  = RoseLeaf
  _ <*> RoseLeaf  = RoseLeaf
  (RoseNode f rs) <*> r'@(RoseNode v rs') = 
    RoseNode (f v) (map (fmap f) rs' ++ map (<*> r') rs)


