module Test.Assign2.Teletype 
                      ( qcTeletype
                      , huTeletype
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

import          Test.Tasty
import          Test.Tasty.HUnit
import          Test.Assign2.Helpers (decApp)

import          Assign2.Teletype (Teletype(..))


qcTeletype  ::  TestTree
qcTeletype  =   testGroup "Teletype"    []


huTeletype  ::  TestTree
huTeletype  =   testGroup "Teletype"    [ huTeletypeFunctor
                                        , huTeletypeApplicative ]


huTeletypeFunctor     :: TestTree
huTeletypeFunctor     = testGroup "Functor"
  [ testCase "Return" (
      fmap (+1) ( Return (2 :: Int) )
    @?=
      Return 3
  )
  , testCase "Put"    (
      fmap (+1) ( Put 'c' (Return 5) )
    @?=
       Put 'c' (Return 6)
  )]


huTeletypeApplicative :: TestTree
huTeletypeApplicative = testGroup "Applicative"
  []

