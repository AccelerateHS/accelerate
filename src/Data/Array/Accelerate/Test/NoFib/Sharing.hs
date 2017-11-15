{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Sharing
-- Copyright   : [2009..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Test.NoFib.Sharing (

  test_sharing

) where

import Data.Array.Accelerate                                        as A hiding ( exp )
import Data.Array.Accelerate.Trafo.Sharing
import Data.Array.Accelerate.Data.Bits                              as A

import Control.DeepSeq
import Control.Exception
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit
import Prelude                                                      as P hiding ( exp )


test_sharing :: TestTree
test_sharing =
  testGroup "sharing"
    [ testCase  "simple"                $ sharingAcc test_simple
    , testCase  "ordering"              $ sharingAcc test_ordering
    , testCase  "sort"                  $ sharingAcc test_sort
    , testCase  "blowup"                $ sharingAcc (test_blowup 20)
    , testCase  "bfs"                   $ sharingAcc test_bfs
    , testGroup "same level"
      [ testCase "1"                    $ sharingAcc test_two_lets_same_level_1
      , testCase "2"                    $ sharingAcc test_two_lets_same_level_2
      ]
    , testGroup "empty top"
      [ testCase "1"                    $ sharingAcc test_no_let_at_top_1
      , testCase "2"                    $ sharingAcc test_no_let_at_top_2
      ]
    , testCase  "pipe"                  $ sharingAcc test_pipe
    , testCase  "bound variables"       $ sharingAcc test_bound_variables
    , testCase  "big tuple"             $ sharingExp test_big_tuple
    , testGroup "iteration"
      [ testCase "simple"               $ sharingAcc test_iteration_simple
      , testCase "outside"              $ sharingAcc test_iteration_outside
      , testCase "body and condition"   $ sharingAcc test_iteration_body_condition
      , testCase "awhile"               $ sharingAcc test_awhile
      , testCase "iterate"              $ sharingAcc test_iterate
      , testCase "nested"               $ sharingExp test_nested_iteration
      , testCase "unused"               $ sharingExp test_unused_iteration
      ]
    , testGroup "nested data-parallelism"
      [ expectFail $ testCase "mvm"     $ sharingAcc test_nested_data_praallelism
      ]
    ]
  where
    sharingAcc :: Arrays a => Acc a -> Assertion
    sharingAcc acc =
      catch (rnf (convertAcc True True True True acc) `seq` return ())
            (\(e :: SomeException) -> assertFailure (show e))

    sharingExp :: Elt e => Exp e -> Assertion
    sharingExp exp =
      catch (rnf (convertExp True exp) `seq` return ())
            (\(e :: SomeException) -> assertFailure (show e))


--------------------------------------------------------------------------------
--
-- Some tests to make sure that sharing recovery is working.
--

mkArray :: Int -> Acc (Array DIM1 Int)
mkArray n = use $ fromList (Z:.1) [n]

test_blowup :: Int -> Acc (Array DIM1 Int)
test_blowup 0 = (mkArray 0)
test_blowup n = A.map (\_ -> newArr ! (lift (Z:.(0::Int))) +
                             newArr ! (lift (Z:.(1::Int)))) (mkArray n)
  where
    newArr = test_blowup (n-1)

idx :: Int -> Exp DIM1
idx i = lift (Z:.i)

test_bfs :: Acc (Array DIM1 Int)
test_bfs = A.map (\x -> (map2 ! (idx 1)) +  (map1 ! (idx 2)) + x) arr
  where
    map1 :: Acc (Array DIM1 Int)
    map1 =  A.map (\y -> (map2 ! (idx 3)) + y) arr

    map2 :: Acc (Array DIM1 Int)
    map2 =  A.map (\z -> z + 1) arr

    arr :: Acc (Array DIM1 Int)
    arr =  mkArray 666

test_two_lets_same_level_1 :: Acc (Array DIM1 Int)
test_two_lets_same_level_1 =
  let arr1 = mkArray 1
  in let arr2 = mkArray 2
     in  A.map (\_ -> arr1!(idx 1) + arr1!(idx 2) + arr2!(idx 3) + arr2!(idx 4)) (mkArray 3)

test_two_lets_same_level_2 :: Acc (Array DIM1 Int)
test_two_lets_same_level_2 =
 let arr2 = mkArray 2
 in let arr1 = mkArray 1
    in  A.map (\_ -> arr1!(idx 1) + arr1!(idx 2) + arr2!(idx 3) + arr2!(idx 4)) (mkArray 3)


-- These two programs test that lets can be introduced not just at the top of a AST
-- but in intermediate nodes.
--
test_no_let_at_top_1 :: Acc (Array DIM1 Int)
test_no_let_at_top_1 = A.map (\x -> x + 1) test_bfs

test_no_let_at_top_2 :: Acc (Array DIM1 Int)
test_no_let_at_top_2
  = A.map (\x -> x + 2)
  $ A.map (\x -> x + 1) test_bfs

--
--
--
test_simple :: Acc (Array DIM1 (Int,Int))
test_simple = A.map (\_ -> a ! (idx 1))  d
  where
    c = use $ A.fromList (Z :. 3) [1..]
    d = A.map (+1) c
    a = A.zip d c


-- sortKey is a real program that Ben Lever wrote. It has some pretty interesting
-- sharing going on.
--
sortKey :: (Elt e)
        => (Exp e -> Exp Int)         -- ^mapping function to produce key array from input array
        -> Acc (Vector e)
        -> Acc (Vector e)
sortKey keyFun arr =  foldl sortOneBit arr (P.map lift ([0..31] :: [Int]))
  where
    sortOneBit inArr bitNum = outArr
      where
        keys    = A.map keyFun inArr

        bits    = A.map (\a -> (A.testBit a bitNum) ? (1, 0)) keys
        bitsInv = A.map (\b -> (b A.== 0) ? (1, 0)) bits

        (falses, numZeroes) = unlift (A.scanl' (+) 0 bitsInv)
        trues               = A.map (\x -> (A.the numZeroes) + (A.fst x) - (A.snd x))
                            $ A.zip ixs falses

        dstIxs = A.map (\x -> let (b, t, f) = unlift x  in (b A.== (constant (0::Int))) ? (f, t))
               $ A.zip3 bits trues falses
        outArr = scatter dstIxs inArr inArr -- just use input as default array
                                            --(we're writing over everything anyway)
    --
    ixs   = enumeratedArray (shape arr)

-- Create an array where each element is the value of its corresponding
-- row-major index.
--
enumeratedArray :: Exp DIM1 -> Acc (Array DIM1 Int)
enumeratedArray sh = A.generate sh unindex1

test_sort :: Acc (Vector Int)
test_sort = sortKey id $ use $ fromList (Z:.10) [9,8,7,6,5,4,3,2,1,0]

-- map1 has children map3 and map2.
-- map2 has child map3.
-- Back when we still used a list for the NodeCounts data structure this mean that
-- you would be merging [1,3,2] with [2,3] which violated precondition of (+++).
-- This tests that the new algorithm works just fine on this.
--
test_ordering :: Acc (Array DIM1 Int)
test_ordering = A.map (\_ -> map1 ! (idx 1) + map2 ! (idx 1)) arr
  where
    map1 = A.map (\_ -> map3 ! (idx 1) + map2 ! (idx 2)) arr
    map2 = A.map (\_ -> map3 ! (idx 3)) arr
    map3 = A.map (+1) arr
    arr  = mkArray 42


-- Tests array-valued lambdas in conjunction with sharing recovery.
--
test_pipe :: Acc (Vector Int)
test_pipe = (acc1 >-> acc2) xs
  where
    z :: Acc (Scalar Int)
    z = unit 0

    xs :: Acc (Vector Int)
    xs = use $ fromList (Z:.10) [0..]

    acc1 :: Acc (Vector Int) -> Acc (Vector Int)
    acc1 = A.map (\_ -> the z)

    acc2 :: Acc (Vector Int) -> Acc (Vector Int)
    acc2 arr = let arr2 = use $ fromList (Z:.10) [10..]
               in  A.map (\_ -> arr2!constant (Z:.(0::Int))) (A.zip arr arr2)


-- Test for bound variables
--
test_bound_variables :: Acc (Array DIM2 Int, Array DIM2 Float, Array DIM2 Float)
test_bound_variables = lift (first, both, second)
  where
    is :: Array DIM2 Int
    is = fromList (Z:.10:.10) [0..]

    fs :: Array DIM2 Float
    fs = fromList (Z:.10:.10) [0..]

    -- Ignoring the first parameter
    first = stencil2 centre clamp (use fs) clamp (use is)
      where
        centre :: Stencil3x3 Float -> Stencil3x3 Int -> Exp Int
        centre _ (_,(_,y,_),_)  = y

    -- Using both
    both = stencil2 centre clamp (use fs) clamp (use is)
      where
        centre :: Stencil3x3 Float -> Stencil3x3 Int -> Exp Float
        centre (_,(_,x,_),_) (_,(_,y,_),_)  = x + A.fromIntegral y

    -- Not using the second parameter
    second = stencil2 centre clamp (use fs) clamp (use is)
      where
        centre :: Stencil3x3 Float -> Stencil3x3 Int -> Exp Float
        centre (_,(_,x,_),_) _  = x

-- Test for 8 and 9 tuples
--
test_big_tuple :: Exp ((Int,Int,Int,Int,Int,Int,Int,Int), (Int,Int,Int,Int,Int,Int,Int,Int,Int))
test_big_tuple = lift (A.constant (0,0,0,0,0,0,0,0), A.constant (0,0,0,0,0,0,0,0,0))

{--
-- Tests for sharing recovery of iteration
--
iteration :: Test
iteration = testGroup "iteration"
  [
    iter "simple"             test1
  , iter "outside"            test2
  , iter "body and condition" test3
  , iter "awhile"             awhile_test
  , iter "iterate"            iterate_test
  , iter "nested"             nested
  , iter "unused"             unused
  ]
  where
    iter :: Show a => TestName -> a -> Test
    iter name acc = testCase name (P.length (show acc) `seq` return ())
--}

v1 :: Acc (Vector Float)
v1 = use $ fromList (Z:.10) [0..]

test_iteration_simple :: Acc (Vector Float)
test_iteration_simple
  = flip A.map v1
  $ \x -> A.while (A.< x) (+1) 0

test_iteration_outside :: Acc (Vector Float)
test_iteration_outside
  = flip A.map v1
  $ \x -> let y = 2*pi
          in  y + A.while (A.< 10) (+y) x

test_iteration_body_condition :: Acc (Vector Float)
test_iteration_body_condition
  = flip A.map v1
  $ \x -> A.while (A.< x) (+x) 0

test_awhile :: Acc (Vector Float)
test_awhile = A.awhile (\a -> A.unit (the (A.sum a) A.< 200)) (A.map (+1)) v1

test_iterate :: Acc (Vector Float)
test_iterate
  = flip A.map v1
  $ \x -> let y = 2*x
          in  y + A.iterate (constant 10) (\x' -> y + x' + 10) x

test_for :: Elt a => Exp Int -> (Exp Int -> Exp a -> Exp a) -> Exp a -> Exp a
test_for n f seed
  = A.snd
  $ A.iterate n (\v -> let (i, x) = unlift v
                       in  lift (i+1, f i x))
                (lift (constant 0, seed))

test_nested_iteration :: Exp Int
test_nested_iteration =
  test_for 64 (\i _ ->
    test_for 64 (\j acc' -> i + j + acc') 0) 0

test_unused_iteration :: Exp Int
test_unused_iteration =
  A.while (A.== 10) (const 10) 5

----------------------------------------------------------------------

-- This program contains nested data-parallelism and thus sharing recovery
-- will fail.
--
test_nested_data_praallelism :: Acc (Vector Float)
test_nested_data_praallelism =
  mvm (use $ fromList (Z:.10:.10) [0..]) (use $ fromList (Z:.10) [0..])
  where
    dotp :: A.Num e => Acc (Vector e) -> Acc (Vector e) -> Acc (Scalar e)
    dotp xs ys = A.fold (+) 0 $ A.zipWith (*) xs ys

    takeRow :: Elt e => Exp Int -> Acc (Array DIM2 e) -> Acc (Vector e)
    takeRow n mat =
      let Z :. _ :. cols = unlift (shape mat) :: Z:. Exp Int :. Exp Int
      in backpermute (index1 cols)
                     (\ix -> index2 n (unindex1 ix))
                     mat

    mvm :: A.Num e => Acc (Array DIM2 e) -> Acc (Vector e) -> Acc (Vector e)
    mvm mat vec =
      let Z :. rows :. _ = unlift (shape mat) :: Z :. Exp Int :. Exp Int
      in generate (index1 rows)
                  (\ix -> the (vec `dotp` takeRow (unindex1 ix) mat))

