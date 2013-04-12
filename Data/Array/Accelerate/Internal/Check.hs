{-# LANGUAGE CPP #-}
-- |
-- Module      : Data.Array.Accelerate.Internal.Check
-- Copyright   : [2009..2011] Roman Lechinskiy, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Bounds checking infrastructure
--
-- Stolen from the Vector package by Roman Leshchinskiy. This code has a
-- BSD-style license. <http://hackage.haskell.org/package/vector>
--

module Data.Array.Accelerate.Internal.Check (

  -- * Bounds checking and assertion infrastructure
  Checks(..), doChecks,
  error, check, warning, assert, checkIndex, checkLength, checkSlice

) where

import Prelude                          hiding ( error )
import Debug.Trace
import qualified Prelude                as P

data Checks = Bounds | Unsafe | Internal deriving( Eq )

doBoundsChecks :: Bool
#ifdef ACCELERATE_BOUNDS_CHECKS
doBoundsChecks = True
#else
doBoundsChecks = False
#endif

doUnsafeChecks :: Bool
#ifdef ACCELERATE_UNSAFE_CHECKS
doUnsafeChecks = True
#else
doUnsafeChecks = False
#endif

doInternalChecks :: Bool
#ifdef ACCELERATE_INTERNAL_CHECKS
doInternalChecks = True
#else
doInternalChecks = False
#endif


doChecks :: Checks -> Bool
{-# INLINE doChecks #-}
doChecks Bounds   = doBoundsChecks
doChecks Unsafe   = doUnsafeChecks
doChecks Internal = doInternalChecks

message :: String -> Int -> Checks -> String -> String -> String
{-# INLINE message #-}
message file line kind loc msg
  = unlines
  $ (if kind == Internal
       then ([""
             ,"*** Internal error in package accelerate ***"
             ,"*** Please submit a bug report at https://github.com/AccelerateHS/accelerate/issues"]++)
       else id)
    [ file ++ ":" ++ show line ++ " (" ++ loc ++ "): " ++ msg ]

error :: String -> Int -> Checks -> String -> String -> a
{-# INLINE error #-}
error file line kind loc msg
  = P.error (message file line kind loc msg)

check :: String -> Int -> Checks -> String -> String -> Bool -> a -> a
{-# INLINE check #-}
check file line kind loc msg cond x
  | not (doChecks kind) || cond = x
  | otherwise                   = error file line kind loc msg

warning :: String -> Int -> Checks -> String -> String -> Bool -> a -> a
{-# INLINE warning #-}
warning file line kind loc msg cond x
  | not (doChecks kind) || cond = x
  | otherwise                   = trace (message file line kind loc msg) x

assert_msg :: String
assert_msg = "assertion failure"

assert :: String -> Int -> Checks -> String -> Bool -> a -> a
{-# INLINE assert #-}
assert file line kind loc = check file line kind loc assert_msg

checkIndex_msg :: Int -> Int -> String
{-# NOINLINE checkIndex_msg #-}
checkIndex_msg i n = "index out of bounds " ++ show (i,n)

checkIndex :: String -> Int -> Checks -> String -> Int -> Int -> a -> a
{-# INLINE checkIndex #-}
checkIndex file line kind loc i n x
  = check file line kind loc (checkIndex_msg i n) (i >= 0 && i<n) x


checkLength_msg :: Int -> String
{-# NOINLINE checkLength_msg #-}
checkLength_msg n = "negative length " ++ show n

checkLength :: String -> Int -> Checks -> String -> Int -> a -> a
{-# INLINE checkLength #-}
checkLength file line kind loc n x
  = check file line kind loc (checkLength_msg n) (n >= 0) x


checkSlice_msg :: Int -> Int -> Int -> String
{-# NOINLINE checkSlice_msg #-}
checkSlice_msg i m n = "invalid slice " ++ show (i,m,n)

checkSlice :: String -> Int -> Checks -> String -> Int -> Int -> Int -> a -> a
{-# INLINE checkSlice #-}
checkSlice file line kind loc i m n x
  = check file line kind loc (checkSlice_msg i m n)
                             (i >= 0 && m >= 0 && i+m <= n) x

