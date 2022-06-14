{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Issues.Issue102
-- Copyright   : [2009..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- https://github.com/AccelerateHS/accelerate/issues/102
--

module Data.Array.Accelerate.Test.NoFib.Issues.Issue102 (

  test_issue102

) where

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Test.NoFib.Base

import Test.Tasty
import Test.Tasty.HUnit


test_issue102 :: RunN -> TestTree
test_issue102 runN =
  testCase "102"  $ ref1 @=? runN test1


ref1 :: Array DIM3 Int
ref1 = fromList (Z :. 1 :. 3 :. 1) [4,4,4]

test1 :: Acc (Array DIM3 Int)
test1 =
  let p         = 3
      lts       = 1
      rts       = 1
      rustride  = 1

      v         = fill (I1 (p-1)) 2
      ru'       = fill (I1 (p-1)) 1

      -- generate a vector with phi(p)=p-1 elements
      x'        = reshape (I3 lts (p-1) rts) v

      --embed into a vector of length p
      y         = generate (I3 lts p rts)
                           (\ix -> let I3 l i r = ix
                                   in  i A.== 0 ? (0, x' ! (I3 l (i-1) r)))

      -- do a DFT_p
      y'        = reshape (I3 lts p rts) (flatten y)
      dftrus    = generate (I2 p p)
                           (\ix -> let I2 i j = ix
                                   in ru' ! (I1 (i*j*rustride `mod` p)))

      tensorDFTCoeffs   = A.replicate @(Z :. Int :. All :. Int :. All) (I4 lts All rts All) dftrus
      tensorInputCoeffs = generate (shape tensorDFTCoeffs)
                                   (\ix -> let I4 l _ r col = ix
                                           in  y' ! (I3 l col r))

      dftans    = flatten $ fold (+) 0 $ A.zipWith (*) tensorDFTCoeffs tensorInputCoeffs

      --continue the alternate transform, but this line breaks
      dfty      = reshape (shape y) $ dftans
  in
  dfty

