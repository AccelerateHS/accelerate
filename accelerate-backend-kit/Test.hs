{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Array.Accelerate.SimpleConverter (convert)
import qualified Data.Array.Accelerate.SimpleAST as S
-- import qualified Data.Array.Accelerate.Smart       as Sugar
-- import qualified Data.Array.Accelerate.Array.Sugar as Sugar

import Data.Int
-- import Data.Array.Accelerate (use,Z,(:.))
-- import qualified Data.Array.Accelerate as Acc
import Data.Array.Accelerate

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

-- TEMP:
-- import qualified Data.Array.Accelerate.Language as Lang
-- TEMP:
-- import qualified Data.Array.Accelerate.Interpreter as Interp
import Text.PrettyPrint.GenericPretty (doc)
import Prelude hiding (zipWith,replicate,map)

p0 = use $ fromList (Z :. (10::Int)) [1..10::Int64]
t0 :: S.AExp
t0 = convert p0

p1 :: Acc (Scalar Float)
p1 = let xs = generate (constant (Z :. (10::Int))) (\ (i) -> 3.3 )
         ys = xs
     in  fold (+) 0 (zipWith (*) xs ys)
t1 :: S.AExp
t1 = convert p1


p2 :: Acc (Vector Int32)
p2 = let xs = replicate (constant (Z :. (4::Int))) (unit 4)
     in map (+ 10) xs
t2 = convert p2


p3 :: Acc (Array DIM3 Int32)
p3 = let arr = generate  (constant (Z :. (5::Int))) (\_ -> 33)
         xs  = replicate (constant$ Z :. (2::Int) :. All :. (3::Int)) arr
     in xs 
t3 = convert p3

-- Test 4, a program that creates an IndexScalar:
p4 :: Acc (Scalar Int64)
p4 = let arr = generate (constant (Z :. (5::Int))) (\_ -> 33) in
     unit $ arr ! (index1 2)
        -- (Lang.constant (Z :. (3::Int)))  
t4 = convert p4         


-- This one generates EIndex:
p5 :: Acc (Scalar (((Z :. All) :. Int) :. All))
p5 = unit$ lift $ Z :. All :. (2::Int) :. All
t5 = convert p5

-- This one generates ETupProjectFromRight:
p6 :: Acc (Vector Float)
p6 = map go (use xs)
  where
    xs :: Vector (Float, Float)
    xs = fromList sh [(1,1),(2,2)]
    sh = Z :. (2::Int)
    go x = let (a,b) = unlift x   in a*b
t6 = convert p6


transposeAcc :: Array DIM2 Float -> Acc (Array DIM2 Float)
transposeAcc mat =
  let mat' = use mat
      swap = lift1 $ \(Z:.x:.y) -> Z :.      y  :.      x 
                                :: Z :. Exp Int :. Exp Int
  in
  backpermute (swap $ shape mat') swap mat'

-- This one uses dynamic index head/tail (but not cons):
p7 :: Acc (Array DIM2 Float)
p7 = transposeAcc (fromList (Z :. (2::Int) :. (2::Int)) [1..4])
t7 = convert p7
-- Evaluating "doc t7" prints:
-- Let a0
--     (TArray TFloat)
--     (Use "Array (Z :. 2) :. 2 [1.0,2.0,3.0,4.0]")
--     (Backpermute (EIndex [EIndexHeadDynamic (EIndexTailDynamic (EShape (Vr a0))),
--                           EIndexHeadDynamic (EShape (Vr a0))])
--                  (Lam [(v1, TTuple [TInt,TInt])]
--                       (EIndex [EIndexHeadDynamic (EIndexTailDynamic (EVr v1)),
--                                EIndexHeadDynamic (EVr v1)]))
--                  (Vr a0))


-- TODO -- still need to generate an IndexCons node.




--------------------------------------------------------------------------------

main = defaultMain tests
tests = [ testCase "use/fromList" (print$ doc t0)
	, testCase "fold/zipwith"  (print$ doc t1)
	, testCase "map/replicate"  (print$ doc t2)
	, testCase "generate/replicate" (print$ doc t3)
	, testCase "index scalar"  (print$ doc t4)
	, testCase "lift/index"    (print$ doc t5)
	, testCase "project tuple" (print$ doc t6)
	, testCase "index test"    (print$ doc t7)
	]
