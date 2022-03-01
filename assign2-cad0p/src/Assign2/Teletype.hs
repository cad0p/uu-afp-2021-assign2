{-|
Module      : Assign2.Teletype
Description : Teletype instatiation of the classes
Copyright   : (c) Pier Carlo Cadoppi, 2022
License     : MIT License
Maintainer  : p.c.cadoppi@students.uu.nl
Stability   : experimental


-}

module Assign2.Teletype
  ( Teletype(..)
  ) where

import Assign2        ( Functor
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
                      )

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
import Data.Typeable (Typeable)


data Teletype a = Get (Char -> Teletype a)
                | Put Char (Teletype a)
                | Return a
  deriving (Typeable)

instance Functor Teletype where
  fmap f (Get tt)     = Get (fmap f . tt)
  fmap f (Put c tt)   = Put c (f <$> tt)
  fmap f (Return tt)  = Return (f tt)
