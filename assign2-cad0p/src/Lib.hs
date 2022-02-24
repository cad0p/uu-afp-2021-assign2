module Lib
    ( someFunc
    ) where

import Prelude hiding ( Functor, Applicative, fmap )

someFunc :: IO ()
someFunc = putStrLn "someFunc"


-- Given the standard type classes for functors, applicative functors and monads:

class Functor f where
  fmap :: (a -> b) -> f a -> f b

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

class Applicative f => Monad f where
  return :: a -> f a
  (>>=) :: f a -> (a -> f b) -> f b


-- Given the standard type classes for functors, applicative functors and monads:

data Tree a = Leaf a | Node (Tree a) (Tree a)

data RoseTree a = RoseNode a [RoseTree a] | RoseLeaf

data Teletype a = Get (Char -> Teletype a)
                | Put Char (Teletype a)
                | Return a

instance Functor Tree where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node a b) = Node (fmap f a) (fmap f b)


