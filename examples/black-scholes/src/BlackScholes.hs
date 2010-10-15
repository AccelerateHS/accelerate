{-# LANGUAGE ForeignFunctionInterface #-}

module BlackScholes where

import Foreign
import Foreign.C
import Data.Array.Accelerate (Exp, Acc, Vector, (>*), (?))
import qualified Data.Vector.Storable           as S
import qualified Data.Array.Accelerate          as Acc


riskfree, volatility :: Float
riskfree   = 0.02
volatility = 0.30

--------------------------------------------------------------------------------
-- Accelerate
--------------------------------------------------------------------------------

horner :: Num a => [a] -> a -> a
horner coeff x = foldr1 madd coeff
  where
    madd a b = b*x + a

cnd :: Exp Float -> Exp Float
cnd d =
  let poly     = horner coeff
      coeff    = [0.0,0.31938153,-0.356563782,1.781477937,-1.821255978,1.330274429]
      rsqrt2pi = 0.39894228040143267793994605993438
      k        = 1.0 / (1.0 + 0.2316419 * abs d)
      cnd'     = rsqrt2pi * exp (-0.5*d*d) * poly k
  in  d >* 0 ? (1.0 - cnd', cnd')

blackscholes :: Vector (Float, Float, Float) -> Acc (Vector (Float, Float))
blackscholes xs = Acc.map go (Acc.use xs)
  where
  go x =
    let (price, strike, years) = Acc.untuple x
        r     = Acc.constant riskfree
        v     = Acc.constant volatility
        sqrtT = sqrt years
        d1    = (log (price / strike) + (r + 0.5 * v * v) * years) / (v * sqrtT)
        d2    = d1 - v * sqrtT
        cndD1 = cnd d1
        cndD2 = cnd d2
        expRT = exp (-r * years)
    in
    Acc.tuple ( price * cndD1 - strike * expRT * cndD2
              , strike * expRT * (1.0 - cndD2) - price * (1.0 - cndD1))

--------------------------------------------------------------------------------
-- CUDA SDK
--------------------------------------------------------------------------------

blackscholes_sdk :: S.Vector Float -> S.Vector Float -> S.Vector Float -> IO ()
blackscholes_sdk price strike years =
  let nopt = S.length price
  in
  S.unsafeWith price  $ \p_price  ->
  S.unsafeWith strike $ \p_strike ->
  S.unsafeWith years  $ \p_years  ->
  allocaArray nopt    $ \p_call   ->
  allocaArray nopt    $ \p_put    ->
    blackscholes_cuda p_call p_put p_price p_strike p_years (realToFrac riskfree) (realToFrac volatility) (fromIntegral nopt)

foreign import ccall unsafe "blackscholes"
  blackscholes_cuda :: Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float -> CFloat -> CFloat -> CInt -> IO ()

