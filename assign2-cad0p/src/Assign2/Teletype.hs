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

{-|
  Found the solution to `show a`:

  No instance for (Show a) arising from a use of..
  https://stackoverflow.com/a/28840366/5029932
-}
instance (Show a, Num a) => Show (Teletype a) where
  show (Get _) = "Get g"
  show (Put c tt) = "Put " ++ [c] ++ " (" ++ show tt ++ ")"
  show (Return a) = "Return " ++ show a


instance (Eq a, Num a) => Eq (Teletype a) where
  (Get g) == (Get g') = g 'c' == g' 'c'
  (Put c tt) == (Put c' tt') = c == c' && tt == tt'
  (Return a) == (Return a') = a == a'
  _ == _ = False


{-|
  >>> fmap (+1) ( Return (2 :: Int) )
  >>  Return 3
-}
instance Functor Teletype where
  fmap f (Get tt)     = Get (fmap f . tt)
  fmap f (Put c tt)   = Put c (f <$> tt)
  fmap f (Return tt)  = Return (f tt)

{-|
  >>> Return (+1) <*> Return 5
  >>  Return 6
-}
instance Applicative Teletype where
  pure = Return
  (Get g) <*> tt' = Get(\c -> g c <*> tt')
  (Put c tt) <*> tt' = Put c (tt <*> tt')
  (Return a) <*> tt' = a <$> tt'

{-|
  >>> Return 5 >>= (Put 'c' . Return) -- \x -> Put 'c' (Return x)
  >>  Put 'c' (Return 5)
-}
instance Monad Teletype where
  return = pure
  (Get g) >>= g' = Get (\x -> g x >>= g')
  (Put _ tt) >>= f = tt >>= f
  (Return a) >>= f = f a

{-|
  >>> 
  >>  
-}
-- instance Foldable Teletype where


