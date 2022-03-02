{-|
Module      : Assign2.MapsKeys
Description : lookupAll and lookupSome
Copyright   : (c) Pier Carlo Cadoppi, 2022
License     : MIT License
Maintainer  : p.c.cadoppi@students.uu.nl
Stability   : experimental
-}

module Assign2.MapsKeys (lookupAll, lookupSome) where
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

-- Data.Map is part of containers, and containers package does use `Internal` naming :)
-- https://www.stackage.org/package/containers

{-|
  'lookupAll' returns Just vs if all the argument keys occur in the map,
   and Nothing otherwise
  
  >>> lookupAll [1, 2] (Map.fromList[(1, "found 1"), (2, "found 2")])
  >>  Just ["found 1", "found 2"]
-}
lookupAll :: Ord k => [k] -> Map.Map k v -> Maybe [v]
lookupAll ns m
 | length(lookupSome ns m) == length m = Just (lookupSome ns m)
 | otherwise = Nothing

{-| 'lookupSome' returns the list of values for which the keys exist

  >>> lookupSome [1, 3] (Map.fromList([(1, "found 1"), (2, "found 2")]))
  >>  ["found 1"]
-}
lookupSome :: Ord k => [k] -> Map.Map k v -> [v]
lookupSome ns m =  mapMaybe (`Map.lookup` m) ns -- \k -> Map.lookup k m