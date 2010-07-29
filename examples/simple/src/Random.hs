{-# LANGUAGE BangPatterns, FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE ParallelListComp #-}

module Random where

import System.Random.MWC
import Data.Array.IArray
import Data.Array.Unboxed               (UArray)
import Data.Array.IO                    (MArray, IOUArray)
import Control.Exception                (evaluate)
import qualified Data.Array.MArray      as M
import qualified Data.Array.Accelerate  as Acc


-- Convert an unboxed array to an Accelerate array
--
convertVector :: (IArray UArray e, Acc.Elem e) => UArray Int e -> IO (Acc.Vector e)
convertVector v =
  let arr = Acc.fromIArray v
  in  evaluate (arr `Acc.indexArray` 0) >> return arr


-- Generate a random, uniformly distributed vector of specified size. The second
-- argument can be used to modify the generated value (e.g. map into a certain
-- range)
--
randomVector
  :: (Variate a, MArray IOUArray e IO, IArray UArray e)
  => GenIO -> (a -> e) -> Int -> IO (UArray Int e)

randomVector gen f n = do
  mu  <- M.newArray_ (0,n-1) :: MArray IOUArray e IO => IO (IOUArray Int e)
  let go !i | i < n     = uniform gen >>= (\e -> M.writeArray mu i (f e)) >> go (i+1)
            | otherwise = M.unsafeFreeze mu
  go 0


-- Compare two vectors element-wise for equality, for a given measure of
-- similarity. The index and values are printed for pairs that fail.
--
validate
  :: (IArray UArray e, Ix ix, Show e, Show ix)
  => (e -> e -> Bool) -> UArray ix e -> UArray ix e -> IO Bool

validate f ref arr =
  let sim = filter (not . null) [ if f x y then [] else ">>> " ++ shows i ": " ++ show (x,y)
                                  | (i,x) <- assocs ref
                                  | y     <- elems arr ]
  in if null sim
        then putStrLn "Valid"                  >> return True
        else mapM_ putStrLn ("INVALID!" : sim) >> return False


-- Floating point equality with relative tolerance
--
similar :: (Fractional a, Ord a) => a -> a -> Bool
similar x y =
  let epsilon = 0.0001
  in  abs ((x-y) / (x+y+epsilon)) < epsilon

