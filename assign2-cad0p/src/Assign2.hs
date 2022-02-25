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
                      , pure
                      , (<*>)
                      , return
                      , (>>=)
                      , foldMap
                      , traverse
                      , Teletype (..)
                      ) where

-- https://stackoverflow.com/questions/34349072/importing-a-data-type-in-haskell

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




data Teletype a = Get (Char -> Teletype a)
                | Put Char (Teletype a)
                | Return a

