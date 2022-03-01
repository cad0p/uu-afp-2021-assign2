{-|
Module      : Assign2
Description : Second assignment on Functor, Applicative, Monad, Foldable, and Traversable
Copyright   : (c) Pier Carlo Cadoppi, 2022
License     : MIT License
Maintainer  : p.c.cadoppi@students.uu.nl
Stability   : experimental


-}

module Assign2
                      ( Functor
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
                      , decApp
                      , Teletype (..)
                      ) where

-- https://stackoverflow.com/questions/34349072/importing-a-data-type-in-haskell

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

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap


instance Functor [] where
  fmap _ []       = []
  fmap f (x : xs) = f x : fmap f xs

instance Traversable [] where
  traverse _ []     = pure []
  traverse f (x:xs) = pure (:) <*> f x <*> traverse f xs


instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just (f x)

instance Applicative Maybe where
  pure = Just
  Nothing <*> _ = Nothing
  (Just f) <*> something = fmap f something


-- Given the standard type classes for functors, applicative functors and monads:




data Teletype a = Get (Char -> Teletype a)
                | Put Char (Teletype a)
                | Return a

{-| 'decApp' is a test function to decrease objects by 1
-}
decApp :: Int -> Maybe Int
decApp n = if n > 0 then Just (n - 1) else Nothing 
