
module Test.Issues.Issue168 (test_issue168)
  where

import Config
import ParseArgs
import Test.Base
import Test.Framework
import Test.Framework.Providers.HUnit

import Prelude                                  as P
import Data.Array.Accelerate                    as A
import Data.Label


test_issue168 :: Config -> Test
test_issue168 conf = testGroup "168"
  [ testCase "A" (assertEqual ref1 $ run backend (A.fill sh test1))
  , testCase "B" (assertEqual ref1 $ run backend (A.fill sh test2))
  , testCase "C" (assertEqual ref2 $ run backend (A.fill sh test3))
  ]
  where
    backend     = get configBackend conf
    sh          = index1 (constant 1) :: Exp DIM1

    -- Test 1
    -- ------
    ref1 = fromList (Z:.1) [(3.0,2.0)]

    dqc1 :: (Exp Float, Exp Float)
    dqc1 = (2,1)

    qMult1 :: (Exp Float, Exp Float) -> (Exp Float, Exp Float)
    qMult1 (a1,_) = (3, a1)

    test1 :: Exp (Float, Float)
    test1 = P.iterate (lift1 qMult1) (lift dqc1) P.!! 1

    test2 :: Exp (Float, Float)
    test2 = A.iterate (constant 1) (lift1 qMult1) (lift dqc1)

    -- Test 2
    -- ------
    ref2 = fromList (Z:.1) [(1.0,2.0,3.0,4.0,5.0,6.0)]

    dqc2 :: (Exp Float, Exp Float, Exp Float, Exp Float, Exp Float, Exp Float)
    dqc2 = (6,5,4,3,2,1)

    qMult2 :: (Exp Float, Exp Float, Exp Float, Exp Float, Exp Float, Exp Float)
           -> (Exp Float, Exp Float, Exp Float, Exp Float, Exp Float, Exp Float)
    qMult2 (a1,b1,c1,d1,e1,f1) = (f1,e1,d1,c1,b1,a1)

    test3 :: Exp (Float,Float,Float,Float,Float,Float)
    test3 = A.iterate (constant 1) (lift1 qMult2) (lift dqc2)

