{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Issues.Issue286
-- Copyright   : [2009..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- https://github.com/AccelerateHS/accelerate/issues/286
--

module Data.Array.Accelerate.Test.NoFib.Issues.Issue286 (

  test_issue286

) where

import Data.Array.Accelerate                                        as A hiding ( (>->), (==) )
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Sugar                            as Sugar
import Data.Array.Accelerate.Array.Unique
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Lifetime
import Data.Array.Accelerate.Test.NoFib.Base
import Data.Array.Accelerate.Test.NoFib.Config
import qualified Data.Array.Accelerate.Array.Representation         as R

import Test.Tasty
import Test.Tasty.HUnit

import Pipes
import System.IO.Unsafe
import Prelude                                                      as P
import Data.Vector.Storable                                         as S


test_issue286 :: RunN -> TestTree
test_issue286 runN =
  askOption $ \(Interpreter slow) ->
    if slow
      then testGroup "286 (skipped due to interpreter backend)" []
      else testGroup "286 (check heap profile)"
            [ testCase "hs.hs"    (void $ runEffect $ hs_producer sh  >-> hs_consume_sv)
            , testCase "hs.acc"   (void $ runEffect $ hs_producer sh  >-> acc_consume_sv runN sh)
            , testCase "acc.hs"   (void $ runEffect $ acc_producer sh >-> hs_consume_acc)
            , testCase "acc.acc"  (void $ runEffect $ acc_producer sh >-> acc_consume_acc runN)
            ]
    where
      sh :: DIM2
      sh = Z :. 120 :. 659

hs_producer :: DIM2 -> Producer (S.Vector Word8) IO ()
hs_producer sh = producer' 0
  where
    producer' :: Int -> Producer (S.Vector Word8) IO ()
    producer' i = do
      yield $ S.replicate (arraySize sh) 1
      if i == 5
        then producer' 0
        else producer' (i+1)

hs_consume_sv :: Consumer (S.Vector Word8) IO ()
hs_consume_sv = consumer' 0 0
  where
    consumer' :: Int -> Float -> Consumer (S.Vector Word8) IO ()
    consumer' n acc = do
      v <- await
      let
          i  = S.sum $ S.map P.fromIntegral v
          i' = i + acc
          n' = n + 1
      if i' `seq` n' `seq` (n == lIMIT)
        then acc `seq` return ()
        else consumer' n' i'

hs_consume_acc :: Consumer (Array DIM2 Word8) IO ()
hs_consume_acc = consumer' 0 0
  where
    consumer' :: Int -> Float -> Consumer (Array DIM2 Word8) IO ()
    consumer' n acc = do
      a <- await
      let
          v  = toVectors a      :: S.Vector Word8
          i  = S.sum $ S.map P.fromIntegral v
          i' = i + acc
          n' = n + 1
      if i' `seq` n' `seq` (n == lIMIT)
        then acc `seq` return ()
        else consumer' n' i'

acc_producer :: DIM2 -> Producer (Array DIM2 Word8) IO ()
acc_producer sh = producer' 0
  where
    producer' :: Int -> Producer (Array DIM2 Word8) IO ()
    producer' i =
      do
        yield $ A.fromFunction sh (\_ -> 1)
        if i == 5
          then producer' 0
          else producer' (i+1)

acc_consume_sv :: RunN -> DIM2 -> Consumer (S.Vector Word8) IO ()
acc_consume_sv runN sh = consumer' 0 0
  where
    !go = runN (A.sum . A.flatten . A.map A.fromIntegral)

    consumer' :: Int -> Float -> Consumer (S.Vector Word8) IO ()
    consumer' n acc = do
      v <- await
      let
          a  = fromVectors sh v   :: Array DIM2 Word8
          i  = the' $ go a
          i' = i + acc
          n' = n + 1
      if i' `seq` n' `seq` (n == lIMIT)
        then acc `seq` return ()
        else consumer' n' i'

acc_consume_acc :: RunN -> Consumer (Array DIM2 Word8) IO ()
acc_consume_acc runN = consumer' 0 0
  where
    !go = runN (A.sum . A.flatten . A.map A.fromIntegral)

    consumer' :: Int -> Float -> Consumer (Array DIM2 Word8) IO ()
    consumer' n acc = do
      a <- await
      let
          i  = the' $ go a
          i' = i + acc
          n' = n + 1
      if i' `seq` n' `seq` (n == lIMIT)
        then acc `seq` return ()
        else consumer' n' i'

lIMIT :: Int
lIMIT = 50000

the' :: Scalar a -> a
the' x = indexArray x Z


-- Minimally stolen from accelerate-io
--
toVectors :: Shape sh => A.Array sh Word8 -> S.Vector Word8
toVectors (Array sh (AD_Word8 ua)) =
  unsafeFromForeignPtr0 (unsafeGetValue (uniqueArrayData ua)) (R.size sh)

fromVectors :: Shape sh => sh -> S.Vector Word8 -> A.Array sh Word8
fromVectors sh v = Array (fromElt sh) adata
  where
    (fp,vsize)  = unsafeToForeignPtr0 v
    adata       = $boundsCheck "fromVectors" "shape mismatch" (vsize P.== Sugar.size sh)
                $ AD_Word8 (unsafePerformIO $ newUniqueArray fp)

