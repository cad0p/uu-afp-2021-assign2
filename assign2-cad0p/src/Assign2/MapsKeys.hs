module Assign2.MapsKeys (lookupSome) where
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

-- Data.Map is part of containers, and containers package does use `Internal` naming :)
-- https://www.stackage.org/package/containers

{-|
  'lookupAll' returns Just vs if all the argument keys occur in the map,
   and Nothing otherwise
-}
-- lookupAll :: Ord k => [k] -> Map.Map k v -> Maybe [v]
-- lookupAll ns m = map (\k -> Map.lookup k m) ns

{-| 'lookupSome' returns the list of values for which the keys exists

  >>> lookupSome [1, 3] (Map.fromList([(1, "found 1"), (2, "found 2")]))
  >>  ["found 1"]
-}
lookupSome :: Ord k => [k] -> Map.Map k v -> [v]
lookupSome ns m =  mapMaybe (`Map.lookup` m) ns -- \k -> Map.lookup k m