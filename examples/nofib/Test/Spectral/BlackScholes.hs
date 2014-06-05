{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Spectral.BlackScholes (

  test_blackscholes

) where

import Prelude                                                  as P
import Control.Applicative
import Data.Label
import Data.Maybe
import Data.Typeable
import Test.QuickCheck
import Test.QuickCheck.Property                                 ( morallyDubiousIOProperty )
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import System.Random

import Config
import ParseArgs
import Test.Base
import QuickCheck.Arbitrary.Array
import Data.Array.Accelerate                                    as A
import Data.Array.Accelerate.Array.Sugar                        as A
import Data.Array.Accelerate.IO                                 as A


test_blackscholes :: Config -> Test
test_blackscholes opt = testGroup "black-scholes" $ catMaybes
  [ testElt configFloat  c_BlackScholes_f
  , testElt configDouble c_BlackScholes_d
  ]
  where
    backend = get configBackend opt

    testElt :: forall a. ( Elt a, IsFloating a, Similar a, Arbitrary a, Random a, Storable a
                         , BlockPtrs (EltRepr a) ~ ((), Ptr a), BlockPtrs (EltRepr' a) ~ Ptr a)
            => (Config :-> Bool)
            -> BlackScholes a
            -> Maybe Test
    testElt ok cfun
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just
      $ testProperty (show (typeOf (undefined :: a))) (run_blackscholes cfun)

    opts :: (Floating a, Random a) => Gen (a,a,a)
    opts = (,,) <$> choose (5,30) <*> choose (1,100) <*> choose (0.25,10)

    run_blackscholes :: forall a. ( Elt a, IsFloating a, Similar a, Storable a, Random a, Arbitrary a
                                  , BlockPtrs (EltRepr a) ~ ((), Ptr a), BlockPtrs (EltRepr' a) ~ Ptr a)
                     => BlackScholes a -> Property
    run_blackscholes cfun =
      forAll (sized $ \nmax -> choose (0,nmax)) $ \n ->
      forAll (arbitraryArrayOf (Z:.n) opts)     $ \psy -> morallyDubiousIOProperty $ do
        let actual = run1 backend blackscholes psy
        expected  <- blackScholesRef cfun psy
        return     $ expected ~?= actual


--
-- Black-Scholes option pricing ------------------------------------------------
--

riskfree, volatility :: Floating a => a
riskfree   = 0.02
volatility = 0.30

horner :: Num a => [a] -> a -> a
horner coeff x = x * foldr1 madd coeff
  where
    madd a b = a + x*b

cnd' :: Floating a => a -> a
cnd' d =
  let poly     = horner coeff
      coeff    = [0.31938153,-0.356563782,1.781477937,-1.821255978,1.330274429]
      rsqrt2pi = 0.39894228040143267793994605993438
      k        = 1.0 / (1.0 + 0.2316419 * abs d)
  in
  rsqrt2pi * exp (-0.5*d*d) * poly k


blackscholes :: (Elt a, IsFloating a) => Acc (Vector (a, a, a)) -> Acc (Vector (a, a))
blackscholes = A.map go
  where
  go x =
    let (price, strike, years) = A.unlift x
        r       = A.constant riskfree
        v       = A.constant volatility
        v_sqrtT = v * sqrt years
        d1      = (log (price / strike) + (r + 0.5 * v * v) * years) / v_sqrtT
        d2      = d1 - v_sqrtT
        cnd d   = let c = cnd' d in d >* 0 ? (1.0 - c, c)
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
    :: forall a. (Storable a, Floating a, Elt a, BlockPtrs (EltRepr a) ~ ((), Ptr a), BlockPtrs (EltRepr' a) ~ Ptr a)
    => BlackScholes a
    -> Vector (a,a,a)
    -> IO (Vector (a,a))
blackScholesRef cfun xs =
  let (Z :. n)  = arrayShape xs
  in
  allocaArray n $ \p_call ->
  allocaArray n $ \p_put ->
  allocaArray n $ \p_price ->
  allocaArray n $ \p_strike ->
  allocaArray n $ \p_years -> do
    toPtr xs ((((), p_price), p_strike), p_years)
    cfun p_call p_put p_price p_strike p_years riskfree volatility (P.fromIntegral n)
    fromPtr (Z :. n) (((), p_call), p_put)


foreign import ccall unsafe "BlackScholes_f" c_BlackScholes_f :: Ptr Float  -> Ptr Float  -> Ptr Float  -> Ptr Float  -> Ptr Float  -> Float  -> Float  -> Int32 -> IO ()
foreign import ccall unsafe "BlackScholes_d" c_BlackScholes_d :: Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Double -> Double -> Int32 -> IO ()

