{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE TypeOperators            #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Issues.Issue437
-- Copyright   : [2009..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- https://github.com/AccelerateHS/accelerate/issues/437
--

module Data.Array.Accelerate.Test.NoFib.Issues.Issue437 (

  test_issue437

) where

import Data.Atomic                                        as Atomic
import Data.Array.Accelerate                              as A
import Data.Array.Accelerate.Test.NoFib.Base

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.ExpectedFailure

import Text.Printf
import Prelude                                            as P


test_issue437 :: RunN -> TestTree
#ifndef ACCELERATE_DEBUG
test_issue437 _
  = expectFail
  $ testCase "437"
  $ assertFailure "This test requires building with -fdebug"
#else
test_issue437 runN
  = testCase "437"
  $ do
    a0 <- Atomic.read __total_bytes_allocated_remote
    b0 <- Atomic.read __total_bytes_copied_to_remote
    c0 <- Atomic.read __total_bytes_copied_from_remote

    let (a,_) = go xs

    -- check the final result is actually transferred back
    a @?= fromList Z [42]

    a1 <- Atomic.read __total_bytes_allocated_remote
    b1 <- Atomic.read __total_bytes_copied_to_remote
    c1 <- Atomic.read __total_bytes_copied_from_remote

    let alloc = a1-a0
        to    = b1-b0
        from  = c1-c0

    -- Either:
    --  a) this is a local memory backend; no transfer takes place
    --
    --  b) this is a remote memory backend; we should only transfer the 4 bytes
    --     to the device for the 'unit', but since the data is already on the
    --     host we can avoid the transfer back
    --
    assertBool (printf "bytes_allocated_remote=%d, bytes_copied_to_remote=%d, bytes_copied_from_remote=%d" alloc to from)
      $ (alloc P.== 0 P.&& to P.== 0 P.&& from P.== 0) P.||   -- local memory space
        (alloc P.>  0 P.&& to P.== 4 P.&& from P.== 0)        -- remote memory space
  where
    xs :: (Scalar Float, Matrix Float)
    xs = runN $ T2 (unit 42) (fill (constant $ Z:.10000:.10000) 1)

    go :: Arrays a => a -> a
    go = runN f
      where
        f :: Arrays a => Acc a -> Acc a
        f = id

-- internals
foreign import ccall "&__total_bytes_allocated_remote"    __total_bytes_allocated_remote    :: Atomic
foreign import ccall "&__total_bytes_copied_to_remote"    __total_bytes_copied_to_remote    :: Atomic
foreign import ccall "&__total_bytes_copied_from_remote"  __total_bytes_copied_from_remote  :: Atomic

#endif

