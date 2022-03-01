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
import Data.Char (digitToInt)


qcTeletype  ::  TestTree
qcTeletype  =   testGroup "Teletype"    []


huTeletype  ::  TestTree
huTeletype  =   testGroup "Teletype"    [ huTeletypeFunctor
                                        , huTeletypeApplicative
                                        , huTeletypeMonad ]


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
       Put 'c' (Return (6 :: Int))
  )]


huTeletypeApplicative :: TestTree
huTeletypeApplicative = testGroup "Applicative"
  [ testCase "Return" (
      Return (+1) <*> Return 5
    @?=
      Return (6 :: Int)
  )]

huTeletypeMonad       :: TestTree
huTeletypeMonad       = testGroup "Monad"
  [ testCase "Return" (
      Return 5 >>= (Put 'c' . Return) -- \x -> Put 'c' (Return x)
    @?=
      Put 'c' (Return (5 :: Int))
  )
  , testCase "RetPut" (
      Return 5 >>= (Put 'c' . Put 'd' . Return) -- \x -> Put 'c' (Put 'd' (Return x)
    @?=
      Put 'c' (Put 'd' (Return (5 :: Int)))
  )
  , testCase "Put" (
      Put 'c' (Return 5) >>= Return
    @?=
      Return (5 :: Int)
  )
  , testCase "Get" (
      Get (\c -> Put c (Return (digitToInt c))) >>= Return
    @?=
      Get (Return . digitToInt)
  )
  ]

