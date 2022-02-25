module Assign2
                      ( Functor
                      , Applicative
                      , Traversable
                      , fmap 
                      , pure
                      , (<*>)
                      , traverse
                      , Tree
                      ) where

import Prelude hiding ( Functor
                      , Applicative
                      , Traversable
                      , fmap 
                      , pure
                      , (<*>)
                      , traverse
                      )


-- Given the standard type classes for functors, applicative functors and monads:

class Functor f where
  fmap :: (a -> b) -> f a -> f b

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

class Applicative f => Monad f where
  return :: a -> f a
  (>>=) :: f a -> (a -> f b) -> f b

class Foldable t where
  foldMap :: Monoid m => (a -> m) -> t a -> m

class Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)


-- Given the standard type classes for functors, applicative functors and monads:

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Show

data RoseTree a = RoseNode a [RoseTree a] | RoseLeaf
  deriving Show

data Teletype a = Get (Char -> Teletype a)
                | Put Char (Teletype a)
                | Return a


{-|
  Example: 
  >fmap (+1) ( Node (Leaf 1) (Leaf 2) )
-}
instance Functor Tree where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node a b) = Node (fmap f a) (fmap f b)


instance Applicative Tree where
  pure = Leaf
  Leaf f      <*> Leaf v        =
    pure (f v)
  Node a b    <*> Leaf v        =
    Node (a <*> pure v) (b <*> pure v)
  Leaf f      <*> (Node a' b')  =
    Node (pure f <*> a') (pure f <*> b')
  (Node a b)  <*> (Node a' b')  =
    Node (a <*> a') (b <*> b')



dec :: Int -> Maybe Int
dec n = if n > 0 then Just (n - 1) else Nothing  

instance Traversable Tree where
  traverse f (Leaf a)   = pure Leaf <*> f a
  traverse f (Node a b) = 
    pure Node <*> traverse f a <*> traverse f b
