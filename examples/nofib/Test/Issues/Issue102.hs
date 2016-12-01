{-# LANGUAGE TypeOperators  #-}

module Test.Issues.Issue102 (test_issue102)
  where

import Config
import Test.Framework
import Test.Framework.Providers.HUnit

import Prelude                                                  as P
import Data.Array.Accelerate                                    as A
import Data.Array.Accelerate.Examples.Internal                  as A


test_issue102 :: Backend -> Config -> Test
test_issue102 backend _conf =
  testCase "102" (assertEqual ref1 $ run backend test1)


ref1 :: Array DIM3 Int
ref1 = fromList (Z :. 1 :. 3 :. 1) [4,4,4]

test1 :: Acc (Array DIM3 Int)
test1 =
  let p         = 3
      lts       = 1
      rts       = 1
      rustride  = 1

      v         = fill (constant (Z:.(p-1))) (constant 2)
      ru'       = fill (constant (Z:.(p-1))) (constant 1)

      -- generate a vector with phi(p)=p-1 elements
      x'        = reshape (constant (Z :. lts :. (p-1) :. rts)) v

      --embed into a vector of length p
      y         = generate (constant (Z :. lts :. p :. rts))
                           (\ix -> let (Z :. l :. i :. r) = unlift ix :: Z :. Exp Int :. Exp Int :. Exp Int
                                   in  i A.== 0 ? (0, x' ! (lift $ Z :. l :. i-1 :. r)))

      -- do a DFT_p
      y'        = reshape (constant (Z :. lts :. p :. rts)) (flatten y)
      dftrus    = generate (constant (Z :. p :. p))
                           (\ix -> let (Z :. i :. j) = unlift ix :: Z :. Exp Int :. Exp Int
                                   in ru' ! (lift (Z :. (i*j*rustride `mod` (constant p)))))

      tensorDFTCoeffs   = A.replicate (lift (Z:.lts:.All:.rts:.All)) dftrus
      tensorInputCoeffs = generate (shape tensorDFTCoeffs)
                                   (\ix -> let (Z:.l:._:.r:.col) = unlift ix :: Z :. Exp Int :. Exp Int :. Exp Int :. Exp Int
                                           in  y' ! (lift $ Z:.l:.col:.r))

      dftans    = flatten $ fold (+) (constant 0) $ A.zipWith (*) tensorDFTCoeffs tensorInputCoeffs

      --continue the alternate transform, but this line breaks
      dfty      = reshape (shape y) $ dftans
  in
  dfty

