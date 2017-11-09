{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}

module Test.Spectral.BlackScholes (

  test_blackscholes

) where

import Control.Applicative
import Data.Label
import Data.Maybe
import Data.Typeable
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import System.Random
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Prelude                                                  as P

import Config
import QuickCheck.Arbitrary.Array
import Data.Array.Accelerate                                    as A
import Data.Array.Accelerate.Array.Sugar                        as A
import Data.Array.Accelerate.Array.Data                         as A
import Data.Array.Accelerate.Examples.Internal                  as A
import Data.Array.Accelerate.IO.Foreign.ForeignPtr              as A


test_blackscholes :: Backend -> Config -> Test
test_blackscholes backend opt = testGroup "black-scholes" $ catMaybes
  [ testElt configFloat  c_BlackScholes_f
  , testElt configDouble c_BlackScholes_d
  ]
  where
    testElt :: forall a. (P.Floating a, A.Floating a, A.Ord a, Similar a, Arbitrary a, Random a, Storable a, ForeignPtrs (EltRepr a) ~ ForeignPtr a)
            => (Config :-> Bool)
            -> BlackScholes a
            -> Maybe Test
    testElt ok cfun
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just
      $ testProperty (show (typeOf (undefined :: a))) (run_blackscholes cfun)

    opts :: (P.Floating a, Random a) => Gen (a,a,a)
    opts = (,,) <$> choose (5,30) <*> choose (1,100) <*> choose (0.25,10)

    run_blackscholes :: forall a. (P.Floating a, A.Floating a, A.Ord a, Similar a, Storable a, Random a, ForeignPtrs (EltRepr a) ~ ForeignPtr a)
                     => BlackScholes a
                     -> Property
    run_blackscholes cfun =
      forAll (sized return)                     $ \nmax ->
      forAll (choose (0,nmax))                  $ \n ->
      forAll (arbitraryArrayOf (Z:.n) opts)     $ \psy -> ioProperty $ do
        let actual = run1 backend blackscholes psy
        expected  <- blackScholesRef cfun psy
        return     $ expected ~?= actual


--
-- Black-Scholes option pricing ------------------------------------------------
--

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
  go x =
    let (price, strike, years) = A.unlift x
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


-- Reference implementation, stolen from the CUDA SDK examples reference
-- implementation and modified for our purposes.
--
type BlackScholes a = Ptr a -> Ptr a -> Ptr a -> Ptr a -> Ptr a -> a -> a -> Int32 -> IO ()

blackScholesRef
    :: forall a. (Storable a, P.Floating a, A.Floating a, ForeignPtrs (EltRepr a) ~ ForeignPtr a)
    => BlackScholes a
    -> Vector (a,a,a)
    -> IO (Vector (a,a))
blackScholesRef cfun xs = do
  let Z :. n = arrayShape xs
  --
  r_adata <- newArrayData n
  let res                                   = Array ((), n) r_adata
      ((((), f_price), f_strike), f_years)  = toForeignPtrs xs
      (((), f_call), f_put)                 = toForeignPtrs res
  --
  withForeignPtr f_price   $ \p_price  ->
   withForeignPtr f_strike $ \p_strike ->
    withForeignPtr f_years $ \p_years  ->
     withForeignPtr f_call $ \p_call   ->
      withForeignPtr f_put $ \p_put    ->
       cfun p_call p_put p_price p_strike p_years riskfree volatility (P.fromIntegral n)
  --
  return res


foreign import ccall unsafe "BlackScholes_f" c_BlackScholes_f :: Ptr Float  -> Ptr Float  -> Ptr Float  -> Ptr Float  -> Ptr Float  -> Float  -> Float  -> Int32 -> IO ()
foreign import ccall unsafe "BlackScholes_d" c_BlackScholes_d :: Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Double -> Double -> Int32 -> IO ()

