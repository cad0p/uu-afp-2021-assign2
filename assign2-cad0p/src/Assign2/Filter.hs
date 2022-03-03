{-|
Module      : Assign2.Filter
Description : general filter
Copyright   : (c) Pier Carlo Cadoppi, 2022
License     : MIT License
Maintainer  : p.c.cadoppi@students.uu.nl
Stability   : experimental
-}


module Assign2.Filter ( gfilter ) where

import           Assign2 (Applicative, Foldable, Functor, Monad, Traversable,
                          fmap, foldMap, pure, return, traverse, (<$>), (<*>),
                          (>>=))

import           Prelude hiding (Applicative, Foldable, Functor, Monad,
                          Traversable, fmap, foldMap, pure, return, traverse,
                          (<$>), (<*>), (>>=))


{-|
  'gfilter' is a generic filter function
  that uses 'foldMap'

  >>> gfilter odd (Node (Leaf 1) (Leaf 2))
  >>  [1]
-}
gfilter :: Foldable f => (a -> Bool) -> f a -> [a]
gfilter test = foldMap (\x -> [ x | test x])
