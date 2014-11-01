
module Test.Issues.Issue114 (test_issue114)
  where

import Config
import Test.Framework
import Test.Framework.Providers.HUnit

import Prelude                                                  as P
import Data.Array.Accelerate                                    as A
import Data.Array.Accelerate.Examples.Internal                  as A


test_issue114 :: Backend -> Config -> Test
test_issue114 backend _conf = testGroup "114"
  [ testCase "A" (assertEqual ref1 $ run backend test1)
  , testCase "B" (assertEqual ref2 $ run backend test2)
  ]


type EFloat = (Float, Float) -- Represents a real number with a value and error

fromFloat :: Float -> EFloat
fromFloat x = (x, 0)

-- toFloat :: EFloat -> Float
-- toFloat (val, err) = val + err

add :: Exp EFloat -> Exp EFloat -> Exp EFloat
add = lift2 f
    where
        f :: (Exp Float, Exp Float) -> (Exp Float, Exp Float) -> (Exp Float, Exp Float)
        f (aval, aerr) (bval, berr) = (val, err)
            where
                val = aval + bval
                err = aval - (val - det) + (bval - det) + aerr + berr
                det = val - aval

esum :: Acc (Vector EFloat) -> Acc (Scalar EFloat)
esum  x = A.fold1  add x

epsum :: Acc (Vector EFloat) -> Acc (Vector EFloat)
epsum x = A.scanl1 add x

xs :: Acc (Vector EFloat)
xs = use $ fromFunction (Z :. 100) (\_ -> fromFloat 1.01)


ref1 :: Scalar EFloat
ref1 = fromList Z [(101,0)]

test1 :: Acc (Scalar EFloat)
test1 = esum xs


ref2 :: Vector EFloat
ref2 = fromList (Z :. 100) [ (1.01 * i, 0) | i <- [1..]]

test2 :: Acc (Vector EFloat)
test2 = epsum xs

