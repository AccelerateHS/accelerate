-- https://github.com/AccelerateHS/accelerate/issues/286
--

module Test.Issues.Issue286 (test_issue286)
  where

import Config
import Test.Framework
import Test.Framework.Providers.HUnit

import Prelude                                                  as P
import Data.Array.Accelerate                                    as A hiding ( (>->), (==) )
import Data.Array.Accelerate.IO.Data.Vector.Storable            as A
import Data.Array.Accelerate.Examples.Internal                  as A

import Pipes
import Data.Vector.Storable                                     as S


test_issue286 :: Backend -> Config -> Test
test_issue286 backend _conf =
  testGroup "286 (run with +RTS -M4M -RTS or check heap profile in EKG or similar)"
    [
      testCase "hs.hs"    (void $ runEffect $ hs_producer sh  >-> hs_consume_sv)
    , testCase "hs.acc"   (void $ runEffect $ hs_producer sh  >-> acc_consume_sv sh backend)
    , testCase "acc.hs"   (void $ runEffect $ acc_producer sh >-> hs_consume_acc)
    , testCase "acc.acc"  (void $ runEffect $ acc_producer sh >-> acc_consume_acc backend)
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
      v <- await
      let
          a  = toVectors v      :: S.Vector Word8
          i  = S.sum $ S.map P.fromIntegral a
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

acc_consume_sv :: DIM2 -> Backend -> Consumer (S.Vector Word8) IO ()
acc_consume_sv sh backend = consumer' 0 0
  where
    go = run1 backend (A.sum . A.flatten . A.map A.fromIntegral)

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

acc_consume_acc :: Backend -> Consumer (Array DIM2 Word8) IO ()
acc_consume_acc backend = consumer' 0 0
  where
    go = run1 backend (A.sum . A.flatten . A.map A.fromIntegral)

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

