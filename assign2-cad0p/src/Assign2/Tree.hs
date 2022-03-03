{-|
Module      : Assign2.Tree
Description : Tree instatiation of the classes
Copyright   : (c) Pier Carlo Cadoppi, 2022
License     : MIT License
Maintainer  : p.c.cadoppi@students.uu.nl
Stability   : experimental


-}

module Assign2.Tree   ( Tree(..) )
                      where

import           Assign2       (Applicative, Foldable, Functor, Monad,
                                Traversable, fmap, foldMap, pure, return,
                                traverse, (<$>), (<*>), (>>=))

import           Prelude       hiding (Applicative, Foldable, Functor, Monad,
                                Traversable, fmap, foldMap, pure, return,
                                traverse, (<$>), (<*>), (>>=))

import           Data.Typeable (Typeable)

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Eq, Show, Typeable)


{-|
  >>> fmap (+1) ( Node (Leaf 1) (Leaf 2) )
  >> Node (Leaf 2) (Leaf 3)
-}
instance Functor Tree where
  fmap f (Leaf a)   = Leaf (f a)
  fmap f (Node a b) = Node (fmap f a) (fmap f b)


instance Applicative Tree where
  pure = Leaf
  Leaf f      <*> v             = f <$> v
  Node a b    <*> Leaf v        =
    Node (a <*> pure v) (b <*> pure v)
  (Node a b)  <*> (Node a' b')  =
    Node (a <*> a') (b <*> b')



{-|
  >>> *Assign2> ( Node (Leaf 1) (Leaf 2) ) >>= decFun
  >>  Node (Leaf 0) (Leaf 1)
-}
instance Monad Tree where
  return = pure
  (Leaf a)    >>= f = f a
  (Node a b)  >>= f = Node (a >>= f) (b >>= f)


{-|
  >>> *Assign2> foldMap (show) (Node  (Leaf 1) (Leaf 2)  )
  >>  "12"
-}
instance Foldable Tree where
  foldMap f (Leaf a)   = f a
  foldMap f (Node a b) = foldMap f a <> foldMap f b


{-|
  >>> traverse decApp ( Node (Leaf 1) (Leaf 2) )
  >>  Just (Node (Leaf 0) (Leaf 1))
-}
instance Traversable Tree where
  traverse f (Leaf a)   = Leaf <$> f a
  traverse f (Node a b) =
    Node <$> traverse f a <*> traverse f b

