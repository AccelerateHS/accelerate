
module BlackScholes where

import Random

import System.Random.MWC
import Data.Array.IArray     as IArray
import Data.Array.Accelerate as Acc


riskfree, volatility :: Float
riskfree   = 0.02
volatility = 0.30

-- Black-Scholes option pricing
-------------------------------

horner :: Num a => [a] -> a -> a
horner coeff x = foldr1 madd coeff
  where
    madd a b = b*x + a

cnd' :: Floating a => a -> a
cnd' d =
  let poly     = horner coeff
      coeff    = [0.0,0.31938153,-0.356563782,1.781477937,-1.821255978,1.330274429]
      rsqrt2pi = 0.39894228040143267793994605993438
      k        = 1.0 / (1.0 + 0.2316419 * abs d)
  in
  rsqrt2pi * exp (-0.5*d*d) * poly k


blackscholesAcc :: Vector (Float, Float, Float) -> Acc (Vector (Float, Float))
blackscholesAcc xs = Acc.map go (Acc.use xs)
  where
  go x =
    let (price, strike, years) = Acc.unlift x
        r     = Acc.constant riskfree
        v     = Acc.constant volatility
        sqrtT = sqrt years
        d1    = (log (price / strike) + (r + 0.5 * v * v) * years) / (v * sqrtT)
        d2    = d1 - v * sqrtT
        cnd d = d >* 0 ? (1.0 - cnd' d, cnd' d)
        cndD1 = cnd d1
        cndD2 = cnd d2
        expRT = exp (-r * years)
    in
    Acc.lift ( price * cndD1 - strike * expRT * cndD2
             , strike * expRT * (1.0 - cndD2) - price * (1.0 - cndD1))


blackscholesRef :: IArray.Array Int (Float,Float,Float) -> IArray.Array Int (Float,Float)
blackscholesRef xs = listArray (bounds xs) [ go x | x <- elems xs ]
  where
  go (price, strike, years) =
    let r     = riskfree
        v     = volatility
        sqrtT = sqrt years
        d1    = (log (price / strike) + (r + 0.5 * v * v) * years) / (v * sqrtT)
        d2    = d1 - v * sqrtT
        cnd d = if d > 0 then 1.0 - cnd' d else cnd' d
        cndD1 = cnd d1
        cndD2 = cnd d2
        expRT = exp (-r * years)
    in
    ( price * cndD1 - strike * expRT * cndD2
    , strike * expRT * (1.0 - cndD2) - price * (1.0 - cndD1))


-- Main
-- ----

run :: Int -> IO (() -> IArray.Array Int (Float,Float), () -> Acc (Vector (Float,Float)))
run n = withSystemRandom $ \gen -> do
  v_sp <- randomUArrayR (5,30)    gen n
  v_os <- randomUArrayR (1,100)   gen n
  v_oy <- randomUArrayR (0.25,10) gen n

  let v_psy = listArray (0,n-1) $ zip3 (elems v_sp) (elems v_os) (elems v_oy)
      a_psy = Acc.fromIArray v_psy
  --
  return (run_ref v_psy, run_acc a_psy)
  where
    {-# NOINLINE run_ref #-}
    run_ref psy () = blackscholesRef psy
    run_acc psy () = blackscholesAcc psy

