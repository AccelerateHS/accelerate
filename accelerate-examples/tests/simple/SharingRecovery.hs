{-# LANGUAGE TypeOperators, ScopedTypeVariables #-}


--
-- Some tests to make sure that sharing recovery is working.
--
module SharingRecovery where

import Prelude hiding (zip3)

import Data.Array.Accelerate as Acc


mkArray :: Int -> Acc (Array DIM1 Int)
mkArray n = use $ fromList (Z:.1) [n]

muchSharing :: Int -> Acc (Array DIM1 Int)
muchSharing 0 = (mkArray 0)
muchSharing n = Acc.map (\_ -> newArr ! (lift (Z:.(0::Int))) +
                               newArr ! (lift (Z:.(1::Int)))) (mkArray n)
  where
    newArr = muchSharing (n-1)

idx :: Int -> Exp DIM1
idx i = lift (Z:.i)

bfsFail :: Acc (Array DIM1 Int)
bfsFail = Acc.map (\x -> (map2 ! (idx 1)) +  (map1 ! (idx 2)) + x) arr
  where
    map1 :: Acc (Array DIM1 Int)
    map1 =  Acc.map (\y -> (map2 ! (idx 3)) + y) arr
    map2 :: Acc (Array DIM1 Int)
    map2 =  Acc.map (\z -> z + 1) arr
    arr  :: Acc (Array DIM1 Int)
    arr =  mkArray 666

twoLetsSameLevel :: Acc (Array DIM1 Int)
twoLetsSameLevel =
  let arr1 = mkArray 1
  in let arr2 = mkArray 2
     in  Acc.map (\_ -> arr1!(idx 1) + arr1!(idx 2) + arr2!(idx 3) + arr2!(idx 4)) (mkArray 3)

twoLetsSameLevel2 :: Acc (Array DIM1 Int)
twoLetsSameLevel2 =
 let arr2 = mkArray 2
 in let arr1 = mkArray 1
    in  Acc.map (\_ -> arr1!(idx 1) + arr1!(idx 2) + arr2!(idx 3) + arr2!(idx 4)) (mkArray 3)

--
-- These two programs test that lets can be introduced not just at the top of a AST
-- but in intermediate nodes.
--
noLetAtTop :: Acc (Array DIM1 Int)
noLetAtTop = Acc.map (\x -> x + 1) bfsFail

noLetAtTop2 :: Acc (Array DIM1 Int)
noLetAtTop2 = Acc.map (\x -> x + 2) $ Acc.map (\x -> x + 1) bfsFail

--
--
--
simple :: Acc (Array DIM1 (Int,Int))
simple = Acc.map (\_ -> a ! (idx 1))  d
  where
    c = use $ Acc.fromList (Z :. 3) [1..]
    d = Acc.map (+1) c
    a = Acc.zip d c

--------------------------------------------------------------------------------
--
-- sortKey is a real program that Ben Lever wrote. It has some pretty interesting
-- sharing going on.
--
sortKey :: (Elt e)
        => (Exp e -> Exp Int)         -- ^mapping function to produce key array from input array
        -> Acc (Vector e)
        -> Acc (Vector e)
sortKey keyFun arr =  foldl sortOneBit arr (Prelude.map lift ([0..31] :: [Int]))
  where
    sortOneBit inArr bitNum = outArr
      where
        keys    = Acc.map keyFun inArr

        bits    = Acc.map (\a -> (Acc.testBit a bitNum) ? (1, 0)) keys
        bitsInv = Acc.map (\b -> (b ==* 0) ? (1, 0)) bits

        (falses, numZeroes) = Acc.scanl' (+) 0 bitsInv
        trues               = Acc.map (\x -> (Acc.the numZeroes) + (Acc.fst x) - (Acc.snd x)) $
                               Acc.zip ixs falses

        dstIxs = Acc.map (\x -> let (b, t, f) = unlift x  in (b ==* (constant (0::Int))) ? (f, t)) $
                   zip3 bits trues falses
        outArr = scatter dstIxs inArr inArr -- just use input as default array
                                            --(we're writing over everything anyway)
    --
    ixs   = enumeratedArray (shape arr)

-- | Copy elements from source array to destination array according to a map. For
--   example:
--
--    default = [0, 0, 0, 0, 0, 0, 0, 0, 0]
--    map     = [1, 3, 7, 2, 5, 8]
--    input   = [1, 9, 6, 4, 4, 2, 5]
--
--    output  = [0, 1, 4, 9, 0, 4, 0, 6, 2]
--
--   Note if the same index appears in the map more than once, the result is
--   undefined. The map vector cannot be larger than the input vector.
--
scatter :: (Elt e)
        => Acc (Vector Int)      -- ^map
        -> Acc (Vector e)        -- ^default
        -> Acc (Vector e)        -- ^input
        -> Acc (Vector e)        -- ^output
scatter mapV defaultV inputV = Acc.permute (const) defaultV pF inputV
  where
    pF ix = lift (Z :. (mapV ! ix))


-- | Create an array where each element is the value of its corresponding row-major
--   index.
--
--enumeratedArray :: (Shape sh) => Exp sh -> Acc (Array sh Int)
--enumeratedArray sh = Acc.reshape sh
--                   $ Acc.generate (index1 $ shapeSize sh) unindex1

enumeratedArray :: Exp DIM1 -> Acc (Array DIM1 Int)
enumeratedArray sh = Acc.generate sh unindex1

zip3 :: forall sh e1 e2 e3. (Shape sh, Elt e1, Elt e2, Elt e3)
     => Acc (Array sh e1)
     -> Acc (Array sh e2)
     -> Acc (Array sh e3)
     -> Acc (Array sh (e1, e2, e3))
zip3 as bs cs = Acc.zipWith (\a bc -> let (b, c) = unlift bc :: (Exp e2, Exp e3)
                                      in lift (a, b, c)) as $ Acc.zip bs cs

unzip3 :: forall sh. forall e1. forall e2. forall e3. (Shape sh, Elt e1, Elt e2, Elt e3)
       => Acc (Array sh (e1, e2, e3))
       -> (Acc (Array sh e1), Acc (Array sh e2), Acc (Array sh e3))
unzip3 abcs = (as, bs, cs)
  where
    (bs, cs)  = Acc.unzip bcs
    (as, bcs) = Acc.unzip
              $ Acc.map (\abc -> let (a, b, c) = unlift abc :: (Exp e1, Exp e2, Exp e3)
                                 in lift (a, lift (b, c))) abcs

testSort = sortKey id $ use $ fromList (Z:.10) [9,8,7,6,5,4,3,2,1,0]

----------------------------------------------------------------------

--
-- map1 has children map3 and map2.
-- map2 has child map3.
-- Back when we still used a list for the NodeCounts data structure this mean that
-- you would be merging [1,3,2] with [2,3] which violated precondition of (+++).
-- This tests that the new algorithm works just fine on this.
--
orderFail :: Acc (Array DIM1 Int)
orderFail = Acc.map (\_ -> map1 ! (idx 1) + map2 ! (idx 1)) arr
  where
    map1 = Acc.map (\_ -> map3 ! (idx 1) + map2 ! (idx 2)) arr
    map2 = Acc.map (\_ -> map3 ! (idx 3)) arr
    map3 = Acc.map (+1) arr
    arr = mkArray 42