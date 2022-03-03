module Test.Assign2.Teletype
                      ( qcTeletype
                      , huTeletype
                      ) where

import           Assign2              (Applicative, Foldable, Functor, Monad,
                                       Traversable, fmap, foldMap, pure, return,
                                       traverse, (<$>), (<*>), (>>=))


import           Prelude              hiding (Applicative, Foldable, Functor,
                                       Monad, Traversable, fmap, foldMap, pure,
                                       return, traverse, (<$>), (<*>), (>>=))

import           Test.Assign2.Helpers (decApp)
import           Test.Tasty
import           Test.Tasty.HUnit

import           Assign2.Teletype     (Teletype (..))
import           Data.Char            (digitToInt)


qcTeletype  ::  TestTree
qcTeletype  =   testGroup "Teletype"    []


huTeletype  ::  TestTree
huTeletype  =   testGroup "Teletype"    [ huTeletypeFunctor
                                        , huTeletypeApplicative
                                        , huTeletypeMonad
                                        , huTeletypeFoldable
                                        , huTeletypeTraversable ]


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

huTeletypeFoldable    :: TestTree
huTeletypeFoldable    = testGroup "Foldable"
  [ testCase "Return" (
      foldMap show (Return (5 :: Int))
    @?=
      "5"
  )
  , testCase "RetPut" (
      foldMap show (Put 'c' (Put 'd' (Return (5 :: Int))))
    @?=
      "5"
  )
  , testCase "Get" (
      foldMap show (Get Return)
    @?=
      mempty
  )]

huTeletypeTraversable :: TestTree
huTeletypeTraversable = testGroup "Traversable"
  [ testCase "Return" (
      traverse decApp (Return 5)
    @?=
      Just (Return (4 :: Int))
  )]

