{-# LANGUAGE TypeOperators, ScopedTypeVariables #-}
module SliceExamples where

import Data.Array.Accelerate as Acc
import qualified Data.Array.Unboxed as UA

--    y
--    ^
--    |  3   4 
--    |  1   2
--     -------> x
--
arr :: Acc (Array DIM2 Int)
arr = use $ fromList (Z:.2:.2) [1,2,3,4]

slice1 :: Exp (Z:.Int:.All:.All)
slice1 = lift $ Z:.(2::Int):.All:.All

slice2 :: Exp (Z:.All:.Int:.All)
slice2 = lift $ Z:.All:.(2::Int):.All

slice3 :: Exp (Z:.All:.All:.Int)
slice3 = lift $ Z:.All:.All:.(2::Int)

-- Replicate into z-axis
-- should produce [1,2,3,4,1,2,3,4]
test1 :: Acc (Array DIM3 Int)
test1 = Acc.replicate slice1 arr

-- Replicate into y-axis
-- should produce [1,2,1,2,3,4,3,4]
test2 :: Acc (Array DIM3 Int)
test2 =  Acc.replicate slice2 arr

-- Replicate into x-axis
-- should produce [1,1,2,2,3,3,4,4]
test3 :: Acc (Array DIM3 Int)
test3 =  Acc.replicate slice3 arr

--
-- repN. Replicates an array into the rightmost dimension of
-- the result array.
--
repN :: forall sh e. (Shape sh, Elt e)
     => Int 
     -> Acc (Array sh e)
     -> Acc (Array (sh:.Int) e)
repN n a = Acc.replicate (lift (Any:.n :: Any sh:.Int)) a

repExample :: Acc (Array DIM2 Int) -> Acc (Array DIM3 Int)
repExample = repN 2

repExample' :: Acc (Array DIM2 Int) -> Acc (Array DIM3 Int)
repExample' = Acc.replicate (lift (Z:.All:.All:.(2::Int)))

slice1' :: Any (Z:.Int:.Int) :. Int
slice1' = Any:.2

slice2' :: Z:.All:.All:.Int
slice2' = Z:.All:.All:.2

run1 :: IO (() -> UA.UArray (Int,Int,Int) Int, () -> Acc (Array DIM3 Int))
run1 = return (\() -> UA.array ((0,0,0),(1,1,1)) [ ((0,0,0), 1)
                                                 , ((0,0,1), 2)
                                                 , ((0,1,0), 3)
                                                 , ((0,1,1), 4)
                                                 , ((1,0,0), 1)
                                                 , ((1,0,1), 2)
                                                 , ((1,1,0), 3)
                                                 , ((1,1,1), 4) ]
              ,\() -> test1)

run2 :: IO (() -> UA.UArray (Int,Int,Int) Int, () -> Acc (Array DIM3 Int))
run2 = return (\() -> UA.array ((0,0,0),(1,1,1)) [ ((0,0,0), 1)
                                                 , ((0,0,1), 2)
                                                 , ((0,1,0), 1)
                                                 , ((0,1,1), 2)
                                                 , ((1,0,0), 3)
                                                 , ((1,0,1), 4)
                                                 , ((1,1,0), 3)
                                                 , ((1,1,1), 4) ]
              ,\() -> test2)

run3 :: IO (() -> UA.UArray (Int,Int,Int) Int, () -> Acc (Array DIM3 Int))
run3 = return (\() -> UA.array ((0,0,0),(1,1,1)) [ ((0,0,0), 1)
                                                 , ((0,0,1), 1)
                                                 , ((0,1,0), 2)
                                                 , ((0,1,1), 2)
                                                 , ((1,0,0), 3)
                                                 , ((1,0,1), 3)
                                                 , ((1,1,0), 4)
                                                 , ((1,1,1), 4) ]
              ,\() -> test3)
run4 :: IO (() -> UA.UArray (Int,Int,Int) Int, () -> Acc (Array DIM3 Int))
run4 = return (\() -> UA.array ((0,0,0),(1,1,1)) [ ((0,0,0), 1)
                                                 , ((0,0,1), 1)
                                                 , ((0,1,0), 2)
                                                 , ((0,1,1), 2)
                                                 , ((1,0,0), 3)
                                                 , ((1,0,1), 3)
                                                 , ((1,1,0), 4)
                                                 , ((1,1,1), 4) ]
              ,\() -> repExample arr)

run5 :: IO (() -> UA.UArray (Int,Int,Int) Int, () -> Acc (Array DIM3 Int))
run5 = return (\() -> UA.array ((0,0,0),(1,1,1)) [ ((0,0,0), 1)
                                                 , ((0,0,1), 1)
                                                 , ((0,1,0), 2)
                                                 , ((0,1,1), 2)
                                                 , ((1,0,0), 3)
                                                 , ((1,0,1), 3)
                                                 , ((1,1,0), 4)
                                                 , ((1,1,1), 4) ]
              ,\() -> repExample' arr)
