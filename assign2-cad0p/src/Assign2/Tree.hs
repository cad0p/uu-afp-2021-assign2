{-|
Module      : Assign2.Tree
Description : Tree instatiation of the classes
Copyright   : (c) Pier Carlo Cadoppi, 2022
License     : MIT License
Maintainer  : p.c.cadoppi@students.uu.nl
Stability   : experimental


-}

module Assign2.Tree where

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

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Eq, Show, Typeable)


{-|
  >>> fmap (+1) ( Node (Leaf 1) (Leaf 2) )
  >> Node (Leaf 2) (Leaf 3)
-}
instance Functor Tree where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node a b) = Node (fmap f a) (fmap f b)


instance Applicative Tree where
  pure = Leaf
  Leaf f      <*> v             =
    fmap f v
  Node a b    <*> Leaf v        =
    Node (a <*> pure v) (b <*> pure v)
  (Node a b)  <*> (Node a' b')  =
    Node (a <*> a') (b <*> b')


{-| 'dec' is a test function to decrease Leaves by 1
-}
dec :: Int -> Tree Int
dec n = if n > 0 then Leaf (n - 1) else Leaf 0  



{-|
  >>> *Assign2> ( Node (Leaf 1) (Leaf 2) ) >>= dec
  >>Node (Leaf 0) (Leaf 1)
-}
instance Monad Tree where
  return = pure
  (Leaf a)    >>= f = f a
  (Node a b)  >>= f = Node (a >>= f) (b >>= f)


{-|
  >>> *Assign2> foldMap (show) (Node  (Leaf 1) (Leaf 2)  )
  >> "12"
-}
instance Foldable Tree where
  foldMap f (Leaf a)    = f a
  foldMap f (Node a b)  = foldMap f a <> foldMap f b


{-|
  >>> traverse dec ( Node (Leaf 1) (Leaf 2) )
  >>> Leaf (Node (Leaf 0) (Leaf 1))
-}
instance Traversable Tree where
  traverse f (Leaf a)   = Leaf <$> f a
  traverse f (Node a b) = 
    pure Node <*> traverse f a <*> traverse f b

