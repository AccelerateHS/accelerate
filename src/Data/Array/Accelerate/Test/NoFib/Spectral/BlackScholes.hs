{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Spectral.BlackScholes
-- Copyright   : [2009..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Test.NoFib.Spectral.BlackScholes (

  test_blackscholes,

) where

import Data.Proxy
import Data.Typeable
import Prelude                                                      as P

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Array.Sugar                            as S
import Data.Array.Accelerate.Test.NoFib.Base
import Data.Array.Accelerate.Test.NoFib.Config
import Data.Array.Accelerate.Test.Similar

import Hedgehog
import qualified Hedgehog.Gen                                       as Gen
import qualified Hedgehog.Range                                     as Range

import Test.Tasty
import Test.Tasty.Hedgehog


test_blackscholes :: RunN -> TestTree
test_blackscholes runN =
  testGroup "blackscholes"
    [ at (Proxy::Proxy TestHalf)   $ testElt (Gen.realFloat :: Range Half -> Gen Half)
    , at (Proxy::Proxy TestFloat)  $ testElt Gen.float
    , at (Proxy::Proxy TestDouble) $ testElt Gen.double
    ]
  where
    testElt
        :: forall a. (P.Floating a, P.Ord a, A.Floating a, A.Ord a , Similar a)
        => (Range a -> Gen a)
        -> TestTree
    testElt e =
      testProperty (show (typeOf (undefined :: a))) $ test_blackscholes' runN e


test_blackscholes'
    :: (P.Floating a, P.Ord a, A.Floating a, A.Ord a, Similar a)
    => RunN
    -> (Range a -> Gen a)
    -> Property
test_blackscholes' runN e =
  property $ do
    sh  <- forAll ((Z :.) <$> Gen.int (Range.linear 0 16384))
    psy <- forAll (array sh ((,,) <$> e (Range.linearFrac 5 30)
                                  <*> e (Range.linearFrac 1 100)
                                  <*> e (Range.linearFrac 0.25 10)))
    --
    let !go = runN blackscholes in go psy ~~~ blackscholesRef psy


riskfree, volatility :: P.Floating a => a
riskfree   = 0.02
volatility = 0.30

horner :: P.Num a => [a] -> a -> a
horner coeff x = x * foldr1 madd coeff
  where
    madd a b = a + x*b

cnd' :: P.Floating a => a -> a
cnd' d =
  let poly     = horner coeff
      coeff    = [0.31938153,-0.356563782,1.781477937,-1.821255978,1.330274429]
      rsqrt2pi = 0.39894228040143267793994605993438
      k        = 1.0 / (1.0 + 0.2316419 * abs d)
  in
  rsqrt2pi * exp (-0.5*d*d) * poly k


blackscholes :: (P.Floating a, A.Floating a, A.Ord a) => Acc (Vector (a, a, a)) -> Acc (Vector (a, a))
blackscholes = A.map go
  where
  go (A.unlift -> (price,strike,years)) =
    let
        r       = A.constant riskfree
        v       = A.constant volatility
        v_sqrtT = v * sqrt years
        d1      = (log (price / strike) + (r + 0.5 * v * v) * years) / v_sqrtT
        d2      = d1 - v_sqrtT
        cnd d   = let c = cnd' d in d A.> 0 ? (1.0 - c, c)
        cndD1   = cnd d1
        cndD2   = cnd d2
        x_expRT = strike * exp (-r * years)
    in
    A.lift ( price * cndD1 - x_expRT * cndD2
           , x_expRT * (1.0 - cndD2) - price * (1.0 - cndD1))


blackscholesRef :: (P.Floating a, P.Ord a, Elt a) => Vector (a, a, a) -> Vector (a, a)
blackscholesRef psy = fromFunction (S.shape psy) (go . indexArray psy)
  where
    go (price, strike, years) =
      let
          r       = riskfree
          v       = volatility
          v_sqrtT = v * sqrt years
          d1      = (log (price / strike) + (r + 0.5 * v * v) * years) / v_sqrtT
          d2      = d1 - v_sqrtT
          cnd d   = let c = cnd' d in if d P.> 0
                                        then 1.0 - c
                                        else c
          cndD1   = cnd d1
          cndD2   = cnd d2
          x_expRT = strike * exp (-r * years)
      in
      ( price * cndD1 - x_expRT * cndD2
      , x_expRT * (1.0 - cndD2) - price * (1.0 - cndD1)
      )

