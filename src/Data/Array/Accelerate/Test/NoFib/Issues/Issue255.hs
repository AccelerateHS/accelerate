{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Issues.Issue255
-- Copyright   : [2009..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- https://github.com/AccelerateHS/accelerate/issues/255
--

module Data.Array.Accelerate.Test.NoFib.Issues.Issue255 (

  test_issue255

) where

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Test.NoFib.Base
import Data.Array.Accelerate.Test.NoFib.Config

import Test.Tasty
import Test.Tasty.HUnit

import Data.List                                                    as P
import Prelude                                                      as P


test_issue255 :: RunN -> TestTree
test_issue255 runN =
  askOption $ \(Interpreter slow) ->
    if slow
      then testGroup "255 (skipped due to interpreter backend)" []
      else testGroup "255"
            [ within lIMIT $ testCase "0"   (force $ total (as P.!! 0))
            , within lIMIT $ testCase "2"   (force $ total (as P.!! 2))
            , within lIMIT $ testCase "4"   (force $ total (as P.!! 4))
            , within lIMIT $ testCase "20"  (force $ total (as P.!! 20))
            , within lIMIT $ testCase "100" (force $ total (as P.!! 100))
            -- , within lIMIT $ testCase "200" (force $ total (as P.!! 200))
            -- , within lIMIT $ testCase "300" (force $ total (as P.!! 300))
            ]
  where
    lIMIT = 30 * 1000 * 1000  -- microseconds
    n     = 20 * 1024 * 1024  -- 160 * MiB (8 bytes per Double)

    as :: [A.Vector Double]
    as = sums (A.fromList (Z:.n) (repeat 0)) (A.fromList (Z:.n) (repeat 1))

    scalar :: Elt e => e -> Scalar e
    scalar x = fromFunction Z (const x)

    sums :: A.Vector Double -> A.Vector Double -> [A.Vector Double]
    sums a0 b
      = a0
      : ( P.snd
        $ P.mapAccumL
            (\a' i -> let !go = runN step
                          a   = go (a', b, scalar i)
                      in (a, a))
            a0
            [0 .. 500]
        )

    step :: Acc (A.Vector Double, A.Vector Double, A.Scalar Int) -> Acc (A.Vector Double)
    step (unlift -> (a, b, A.the -> _i::Exp Int)) = A.zipWith (+) a b

    total :: A.Vector Double -> A.Scalar Double
    total a = go a
      where
        !go = runN (A.sum . A.map id)
        -- runN $ A.sum $ A.map id $ A.use a

    force :: A.Scalar Double -> Assertion
    force x = () @=? (indexArray x Z `seq` ())

  -- print $ total $ as P.!! 0
  -- print $ total $ as P.!! 2
  -- print $ total $ as P.!! 4
  -- print $ total $ as P.!! 20
  -- print $ total $ as P.!! 100
  -- print $ total $ as P.!! 200
  -- print $ total $ as P.!! 300


within :: Integer -> TestTree -> TestTree
within usec = localOption (mkTimeout usec)

-- within :: Int -> Scalar Double -> Assertion
-- within n arr = do
--   r <- timeout n $ evaluate (indexArray arr Z `seq` ())
--   case r of
--     Nothing -> assertFailure "timeout: backend is too slow or memory manager stuck?"
--     Just () -> return ()

