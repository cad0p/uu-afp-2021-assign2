{-|
Module      : Assign2.RoseTree
Description : RoseTree instatiation of the classes
Copyright   : (c) Pier Carlo Cadoppi, 2022
License     : MIT License
Maintainer  : p.c.cadoppi@students.uu.nl
Stability   : experimental


-}

module Assign2.RoseTree
                      ( Applicative
                      , RoseTree (..)
                      ) where

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
import Data.Typeable (Typeable)


data RoseTree a = RoseNode a [RoseTree a] | RoseLeaf
  deriving (Eq, Show, Typeable)


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


{-| 'dec' is a test function to decrease RoseNodes by 1
-}
dec :: Int -> RoseTree Int
dec n = if n > 0 then RoseNode (n - 1) [] else RoseLeaf

{-|
  >>> *Assign2.RoseTree> RoseNode 17 [RoseNode 23 [], RoseNode 29 []] >>= dec
  >> RoseNode 16 [RoseNode 22 [],RoseNode 28 []]
-}
instance Monad RoseTree where
  return = pure
  RoseLeaf >>= _ = RoseLeaf
  RoseNode a l >>= f = case f a of
    RoseLeaf -> RoseLeaf
    RoseNode a' l' -> RoseNode a' (l' ++ fmap (>>= f) l)


{-|
  >>> foldMap (show) (RoseNode 1 [RoseNode 2 [RoseNode 3 [RoseLeaf]], RoseNode 4 [RoseLeaf], RoseNode 5 [RoseNode 6 [RoseLeaf]]])
  >>  "123456"
-}
instance Foldable RoseTree where
  foldMap _ RoseLeaf        = mempty
  foldMap f (RoseNode a rs) = f a <> mconcat (map (foldMap f) rs)


{-|
  According to here: 
  https://mail.haskell.org/pipermail/haskell-cafe/2007-December/036616.html

  This shoud work but it doesn't
-}
instance Traversable RoseTree where
  traverse _ RoseLeaf         = pure RoseLeaf
  traverse f (RoseNode a rs)  =
    pure RoseNode <*> f a <*> traverse (traverse f) rs

