{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Test.Sharing (

  test_sharing

) where

import Config

import Prelude                                  as P
import Data.Array.Accelerate                    as A
import Data.Array.Accelerate.Data.Bits          as A
import Test.Framework
import Test.Framework.Providers.HUnit


test_sharing :: Config -> Test
test_sharing _ =
  testGroup "sharing recovery"
    [
      sharing "simple"                  simple
    , sharing "order fail"              orderFail
    , sharing "test sort"               testSort
    , sharing "much sharing"            (muchSharing 20)
    , sharing "bfs fail"                bfsFail
    , sharing "two lets same level"     twoLetsSameLevel
    , sharing "two lets same level"     twoLetsSameLevel2
    , sharing "no let at top"           noLetAtTop
    , sharing "no let at top"           noLetAtTop2
    , sharing "pipe"                    pipe
    , sharing "bound variables"         varUse
    , sharing "big tuple"               bigTuple
    , iteration
    ]
    where
      sharing :: Show a => TestName -> a -> Test
      sharing name acc =
        testCase name (P.length (show acc) `seq` return ())



--------------------------------------------------------------------------------
--
-- Some tests to make sure that sharing recovery is working.
--

mkArray :: Int -> Acc (Array DIM1 Int)
mkArray n = use $ fromList (Z:.1) [n]

muchSharing :: Int -> Acc (Array DIM1 Int)
muchSharing 0 = (mkArray 0)
muchSharing n = A.map (\_ -> newArr ! (lift (Z:.(0::Int))) +
                             newArr ! (lift (Z:.(1::Int)))) (mkArray n)
  where
    newArr = muchSharing (n-1)

idx :: Int -> Exp DIM1
idx i = lift (Z:.i)

bfsFail :: Acc (Array DIM1 Int)
bfsFail = A.map (\x -> (map2 ! (idx 1)) +  (map1 ! (idx 2)) + x) arr
  where
    map1 :: Acc (Array DIM1 Int)
    map1 =  A.map (\y -> (map2 ! (idx 3)) + y) arr

    map2 :: Acc (Array DIM1 Int)
    map2 =  A.map (\z -> z + 1) arr

    arr :: Acc (Array DIM1 Int)
    arr =  mkArray 666

twoLetsSameLevel :: Acc (Array DIM1 Int)
twoLetsSameLevel =
  let arr1 = mkArray 1
  in let arr2 = mkArray 2
     in  A.map (\_ -> arr1!(idx 1) + arr1!(idx 2) + arr2!(idx 3) + arr2!(idx 4)) (mkArray 3)

twoLetsSameLevel2 :: Acc (Array DIM1 Int)
twoLetsSameLevel2 =
 let arr2 = mkArray 2
 in let arr1 = mkArray 1
    in  A.map (\_ -> arr1!(idx 1) + arr1!(idx 2) + arr2!(idx 3) + arr2!(idx 4)) (mkArray 3)


-- These two programs test that lets can be introduced not just at the top of a AST
-- but in intermediate nodes.
--
noLetAtTop :: Acc (Array DIM1 Int)
noLetAtTop = A.map (\x -> x + 1) bfsFail

noLetAtTop2 :: Acc (Array DIM1 Int)
noLetAtTop2
  = A.map (\x -> x + 2)
  $ A.map (\x -> x + 1) bfsFail

--
--
--
simple :: Acc (Array DIM1 (Int,Int))
simple = A.map (\_ -> a ! (idx 1))  d
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
--enumeratedArray :: (Shape sh) => Exp sh -> Acc (Array sh Int)
--enumeratedArray sh = A.reshape sh
--                   $ A.generate (index1 $ shapeSize sh) unindex1

enumeratedArray :: Exp DIM1 -> Acc (Array DIM1 Int)
enumeratedArray sh = A.generate sh unindex1

testSort :: Acc (Vector Int)
testSort = sortKey id $ use $ fromList (Z:.10) [9,8,7,6,5,4,3,2,1,0]


-- map1 has children map3 and map2.
-- map2 has child map3.
-- Back when we still used a list for the NodeCounts data structure this mean that
-- you would be merging [1,3,2] with [2,3] which violated precondition of (+++).
-- This tests that the new algorithm works just fine on this.
--
orderFail :: Acc (Array DIM1 Int)
orderFail = A.map (\_ -> map1 ! (idx 1) + map2 ! (idx 1)) arr
  where
    map1 = A.map (\_ -> map3 ! (idx 1) + map2 ! (idx 2)) arr
    map2 = A.map (\_ -> map3 ! (idx 3)) arr
    map3 = A.map (+1) arr
    arr  = mkArray 42


-- Tests array-valued lambdas in conjunction with sharing recovery.
--
pipe :: Acc (Vector Int)
pipe = (acc1 >-> acc2) xs
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
varUse :: (Acc (Array DIM2 Int), Acc (Array DIM2 Float), Acc (Array DIM2 Float))
varUse = (first, both, second)
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
bigTuple :: (Exp (Int,Int,Int,Int,Int,Int,Int,Int), Exp (Int,Int,Int,Int,Int,Int,Int,Int,Int))
bigTuple = (A.constant (0,0,0,0,0,0,0,0), A.constant (0,0,0,0,0,0,0,0,0))

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

    vec :: Acc (Vector Float)
    vec = use $ fromList (Z:.10) [0..]

    test1 :: Acc (Vector Float)
    test1 = flip A.map vec
      $ \x -> A.while (A.< x) (+1) 0

    test2 :: Acc (Vector Float)
    test2 = flip A.map vec
      $ \x -> let y = 2*pi
              in  y + A.while (A.< 10) (+y) x

    test3 :: Acc (Vector Float)
    test3 = flip A.map vec
      $ \x -> A.while (A.< x) (+x) 0

    awhile_test :: Acc (Vector Float)
    awhile_test = A.awhile (\a -> A.unit (the (A.sum a) A.< 200)) (A.map (+1)) vec

    iterate_test :: Acc (Vector Float)
    iterate_test = flip A.map vec
        $ \x -> let y = 2*x
                in  y + A.iterate (constant 10) (\x' -> y + x' + 10) x

    for :: Elt a => Exp Int -> (Exp Int -> Exp a -> Exp a) -> Exp a -> Exp a
    for n f seed
      = A.snd
      $ A.iterate n (\v -> let (i, x) = unlift v
                           in  lift (i+1, f i x))
                    (lift (constant 0, seed))

    nested :: Exp Int
    nested
      = for 64 (\i _ ->
          for 64 (\j acc' -> i + j + acc') 0) 0

    unused :: Exp Int
      = A.while (A.== 10) (const 10) 5

----------------------------------------------------------------------

-- This program contains nested data-parallelism and thus sharing recovery
-- will fail.
--
_shouldFail :: Acc (Vector Float)
_shouldFail = mvm (use $ fromList (Z:.10:.10) [0..]) (use $ fromList (Z:.10) [0..])
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



