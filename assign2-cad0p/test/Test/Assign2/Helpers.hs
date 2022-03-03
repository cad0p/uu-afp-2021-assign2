module Test.Assign2.Helpers (decApp) where

{-| is a test Functor function to decrease 'b' instances by 1
  (should this be a monad?)
-}
-- decFun      ::  (Eq a b) => a -> b a


{-| 'decApp' is a test Applicative function to decrease object instances by 1
-}
decApp :: Int -> Maybe Int
decApp n = if n > 0 then Just (n - 1) else Nothing
