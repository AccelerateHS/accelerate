{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Test.Prelude.Sequences (

  test_sequences

) where

import Prelude                                                  as P
import Data.Label
import Data.Maybe
import Data.Typeable
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck                                          hiding ( generate, collect )

import Config
import QuickCheck.Arbitrary.Array                               ()
import Data.Array.Accelerate.Examples.Internal
import Data.Array.Accelerate                                    as A
import Data.Array.Accelerate.Array.Sugar                        as Sugar

toSeq' :: (Shape sh, Elt a)
       => Acc (Array (sh :. Int) a)
       -> Seq [Array sh a]
toSeq' = toSeq (Any :. Split)

iota :: Int -> Acc (Vector Int)
iota n = generate (index1 (constant n)) unindex1

iota' :: Acc (Scalar Int) -> Acc (Vector Int)
iota' n = generate (index1 (the n)) unindex1

--iotaChunk :: Int -> Int -> Acc (Array (Z :. Int :. Int) Int)
--iotaChunk n b = reshape (constant (Z :. b :. n)) $ generate (index1 (constant (n * b))) unindex1

-- idSequence
--     :: forall sh a. (Shape sh, Slice sh, Elt a)
--     => Acc (Array (sh :. Int) a)
--     -> Acc (Array (sh :. Int) a)
-- idSequence xs
--   = reshape (A.shape xs) . asnd . collect
--   . fromSeq
--   $ toSeq Divide xs

-- idSequenceRef :: (Shape sh, Elt a) => (Array (sh :. Int) a) -> (Array (sh :. Int) a)
-- idSequenceRef = id

sumMaxSequence :: (A.Num a, A.Ord a, A.Bounded a) => Acc (Vector a) -> Acc (Scalar a, Scalar a)
sumMaxSequence xs = collect $
  let xs' = toSeq' xs
  in lift ( foldSeq (+) 0 xs'
          , foldSeq A.max minBound xs')

sumMaxSequenceRef :: (Elt a, P.Ord a, P.Bounded a, P.Num a) => Vector a -> (Scalar a, Scalar a)
sumMaxSequenceRef xs = ( fromList Z . (:[]) . P.sum                    . toList $ xs
                       , fromList Z . (:[]) . P.foldl (P.max) minBound . toList $ xs
                       )

scatterSequence :: A.Num a => Acc (Vector a, Vector (Int, a)) -> Acc (Vector a)
scatterSequence input = collect
  $ foldSeqFlatten f (afst input)
  $ toSeq' (asnd input)
  where
    f xs' _ upd =
      let (to, ys) = A.unzip upd
      in permute (+) xs' (index1 . (`mod` A.size (afst input)) . (to A.!)) ys

scatterSequenceRef :: (Elt a, P.Num a) => (Vector a, Vector (Int, a)) -> Vector a
scatterSequenceRef (vec, vec_upd) =
  let xs                = toList vec
      updates           = toList vec_upd
      n                 = P.length xs
      ys                = P.foldl f xs updates
      f xs' (i, x)      = [ if j == i `P.mod` n then x P.+ y else y | (j, y) <- P.zip [0..] xs']
  in
  fromList (Z :. n) ys

logsum :: (A.Floating a, A.FromIntegral Int a) => Int -> Acc (Scalar a)
logsum n = collect
  $ foldSeq (+) 0.0
  $ mapSeq (A.map (log . A.fromIntegral . (+1)))
  $ toSeq' (iota n)

logsumRef :: (Elt a, P.Floating a) => Int -> Scalar a
logsumRef n = fromList Z [P.sum [log (P.fromIntegral i) | i <- [1..n]]]

--logsumChunk :: A.Floating a => Int -> Int -> Acc (Scalar a)
--logsumChunk n b = sum $ collect
--  $ foldSeq (+) (rep (Z :. b) 0.0)
--  $ mapSeq (map (log . fromIntegral . (+1)))
--  $ toSeq' (iotaChunk n b)

--logsumChunkRef :: A.Floating a => Int -> Int -> Scalar a
--logsumChunkRef n b = logsumRef (n * b)

nestedSequence :: Int -> Int -> Acc (Vector Int)
nestedSequence n m = asnd . collect
  $ fromSeq
  $ mapSeq
  (\ i -> collect
          $ foldSeq (+) 0
          $ mapSeq (A.zipWith (+) i)
          $ toSeq' (iota m)
  )
  $ toSeq' (iota n)

nestedSequenceRef :: Int -> Int -> Vector Int
nestedSequenceRef n m = fromList (Z :. n) [P.sum [i + j | j <- [0..m-1]] | i <- [0..n-1]]

nestedIrregularSequence :: Int -> Acc (Vector Int)
nestedIrregularSequence n = asnd . collect
  $ fromSeq
  $ mapSeq
  (\ i -> collect
        $ foldSeq (+) 0
        $ mapSeq (A.zipWith (+) i)
        $ toSeq' (iota' i)
  )
  $ toSeq' (iota n)

nestedIrregularSequenceRef :: Int -> Vector Int
nestedIrregularSequenceRef n = fromList (Z :. n) [P.sum [i + j | j <- [0..i-1]] | i <- [0..n-1]]

deepNestedSequence :: Int -> Acc (Vector Int)
deepNestedSequence n = asnd . collect
  $ fromSeq
  $ mapSeq
  (\ i -> asnd . collect
        $ fromSeq
        $ mapSeq
        (\ j -> collect
              $ foldSeq (+) 0
              $ mapSeq
              (\ k -> collect
                    $ foldSeq (+) 0
                    $ toSeq' (iota' k)
              )
              $ toSeq' (iota' j)
        )
        $ toSeq' (iota' i)
  )
  $ toSeq' (iota n)

deepNestedSequenceRef :: Int -> Vector Int
deepNestedSequenceRef n = fromList (Z :. P.length xs) xs
  where xs = [P.sum [x | k <- [0..j-1], x <- [0..k-1]] | i <- [0..n-1], j <- [0..i-1]]

chunking1 :: Int -> Acc (Scalar Int)
chunking1 n = collect
  $ foldSeq (+) 0
  $ let s = toSeq' (iota n)
    in zipWithSeq (\ x y -> A.zipWith (-) (A.sum x) (A.product y))
         (mapSeq iota' s)
         (mapSeq (iota' . A.map (constant n -)) s)


chunking2 :: Acc (Array (Z :. Int :. Int) Int, Array (Z :. Int :. Int) Int) -> Acc (Vector Int)
chunking2 input = asnd $ collect
  $ fromSeq
  $ zipWithSeq (A.++)
      (toSeq (Z :. Split :. All) (afst input))
      (toSeq (Z :. Split :. All) (asnd input))

chunking2b :: (Array (Z :. Int :. Int) Int, Array (Z :. Int :. Int) Int) -> Acc (Vector Int)
chunking2b input = asnd $ collect
  $ fromSeq
  $ zipWithSeq (A.++)
      (toSeq (Z :. Split :. All) $ use (P.fst input))
      (toSeq (Z :. Split :. All) $ use (P.snd input))

chunking2Ref :: (Array (Z :. Int :. Int) Int, Array (Z :. Int :. Int) Int) -> Vector Int
chunking2Ref (a, b) =
  let
    (Z:. n :. m) = Sugar.shape a
    (Z:. r :. c) = Sugar.shape b
    xs = [ [ a Sugar.! (Z :. i :. j) | j <- [0..m-1]] | i <- [0..n-1]]
    ys = [ [ b Sugar.! (Z :. i :. j) | j <- [0..c-1]] | i <- [0..r-1]]
    zs = P.zipWith (P.++) xs ys
    res = concat zs
  in fromList (Z :. P.length res) res

chunking1Ref :: Int -> Scalar Int
chunking1Ref n = fromList Z [x]
  where x = P.sum [ P.sum [0..i-1] - P.product [0..n-i-1] | i <- [0..n-1] ]


test_sequences :: Backend -> Config -> Test
test_sequences backend opt
  = testGroup "sequences"
  $ if backend `elem` supportedBackends
      then test_sequences' backend opt
      else []
  where
    supportedBackends =
      [ Interpreter
#ifdef ACCELERATE_CUDA_BACKEND
      , CUDA
#endif
      ]

test_sequences' :: Backend -> Config -> [Test]
test_sequences' backend opt =
  [ testGroup "id" $ catMaybes
    [
    --   testIdSequence configInt8   (undefined :: Int8)
    -- , testIdSequence configInt16  (undefined :: Int16)
    -- , testIdSequence configInt32  (undefined :: Int32)
    -- , testIdSequence configInt64  (undefined :: Int64)
    -- , testIdSequence configWord8  (undefined :: Word8)
    -- , testIdSequence configWord16 (undefined :: Word16)
    -- , testIdSequence configWord32 (undefined :: Word32)
    -- , testIdSequence configWord64 (undefined :: Word64)
    -- , testIdSequence configFloat  (undefined :: Float)
    -- , testIdSequence configDouble (undefined :: Double)
    ]
  , testGroup "sum_max" $ catMaybes
    [ testSumMaxSequence configInt8   (undefined :: Int8)
    , testSumMaxSequence configInt16  (undefined :: Int16)
    , testSumMaxSequence configInt32  (undefined :: Int32)
    , testSumMaxSequence configInt64  (undefined :: Int64)
    , testSumMaxSequence configWord8  (undefined :: Word8)
    , testSumMaxSequence configWord16 (undefined :: Word16)
    , testSumMaxSequence configWord32 (undefined :: Word32)
    , testSumMaxSequence configWord64 (undefined :: Word64)
    ]
  , testGroup "scatter" $ catMaybes
    [ testScatterSequence configInt8   (undefined :: Int8)
    , testScatterSequence configInt16  (undefined :: Int16)
    , testScatterSequence configInt32  (undefined :: Int32)
    , testScatterSequence configInt64  (undefined :: Int64)
    , testScatterSequence configWord8  (undefined :: Word8)
    , testScatterSequence configWord16 (undefined :: Word16)
    , testScatterSequence configWord32 (undefined :: Word32)
    , testScatterSequence configWord64 (undefined :: Word64)
    , testScatterSequence configFloat  (undefined :: Float)
    , testScatterSequence configDouble (undefined :: Double)
    ]
  , testGroup "logsum" $ catMaybes
    [ testLogsum configFloat  (undefined :: Float)
    , testLogsum configDouble (undefined :: Double)
    ]
  --, testGroup "logsum_chunked" $ catMaybes
  --  [ testLogsumChunked configFloat  (undefined :: Float)
  --  , testLogsumChunked configDouble (undefined :: Double)
  --  ]
  , testGroup "nested"
    [ testNestedSequence
    , testNestedIrregularSequence
    , testDeepNestedSequence
    ]
  , testGroup "chunking"
    [ testChunking1
    , testChunking2
    , testChunking2b
    ]
  ]
  where
    -- testIdSequence
    --     :: forall a. (Similar a, A.Num a, Arbitrary a)
    --     => (Config :-> Bool)
    --     -> a
    --     -> Maybe Test
    -- testIdSequence ok _
    --   | P.not (get ok opt)      = Nothing
    --   | otherwise               = Just $ testGroup (show (typeOf (undefined :: a)))
    --       [ testDim dim1
    --       , testDim dim2
    --       ]
    --   where
    --     testDim :: forall sh. (sh ~ FullShape sh, Slice sh, Shape sh, P.Eq sh, Arbitrary sh, Arbitrary (Array (sh :. Int) a)) => (sh :. Int) -> Test
    --     testDim sh = testProperty ("DIM" P.++ show (rank sh))
    --       ((\ xs -> run1 backend idSequence xs ~?= idSequenceRef xs) :: Array (sh :. Int) a -> Property)


    testSumMaxSequence
        :: forall a. (P.Num a, P.Bounded a, P.Ord a, A.Num a, A.Bounded a, A.Ord a, Similar a, Arbitrary a)
        => (Config :-> Bool)
        -> a
        -> Maybe Test
    testSumMaxSequence ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testProperty (show (typeOf (undefined :: a)))
          ((\xs -> run1 backend sumMaxSequence xs ~?= sumMaxSequenceRef xs) :: Vector a -> Property)


    testScatterSequence
        :: forall a. (P.Num a, A.Num a, Similar a, Arbitrary a)
        => (Config :-> Bool)
        -> a
        -> Maybe Test
    testScatterSequence ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testProperty (show (typeOf (undefined :: a)))
          ((\input -> run1 backend scatterSequence input ~?= scatterSequenceRef input) :: (Vector a, Vector (Int, a)) -> Property)


    testLogsum
        :: forall a. (P.Floating a, A.Floating a, A.FromIntegral Int a, Similar a, Arbitrary a)
        => (Config :-> Bool)
        -> a
        -> Maybe Test
    testLogsum ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testProperty (show (typeOf (undefined :: a)))
          (\ (NonNegative n) -> (run backend (logsum n) :: Scalar a) ~?= logsumRef n)

    --testLogsumChunked :: forall a. (A.Floating a, Similar a, Arbitrary a)
    --           => (Config :-> Bool)
    --           -> a
    --           -> Maybe Test
    --testLogsumChunked ok _
    --  | P.not (get ok opt)      = Nothing
    --  | otherwise               = Just $ testProperty (show (typeOf (undefined :: a)))
    --      (\ (NonNegative n) (Positive b) -> (run backend (logsumChunk n b) :: Scalar a) ~?= logsumChunkRef n b)

    testNestedSequence :: Test
    testNestedSequence =
      testProperty "regular"
        (\ (NonNegative n) (NonNegative m) -> (run backend (nestedSequence n m) ~?= nestedSequenceRef n m))

    testNestedIrregularSequence :: Test
    testNestedIrregularSequence =
      testProperty "irregular"
        (\ (NonNegative n) -> (run backend (nestedIrregularSequence n) ~?= nestedIrregularSequenceRef n))

    testDeepNestedSequence :: Test
    testDeepNestedSequence =
      testProperty "deep"
        (\ (NonNegative n) -> (run backend (deepNestedSequence n) ~?= deepNestedSequenceRef n))

    testChunking1 :: Test
    testChunking1 =
      testProperty "chunking1"
        (\ (NonNegative n) -> (run backend (chunking1 n) ~?= chunking1Ref n))

    testChunking2 :: Test
    testChunking2 =
      testProperty "chunking2" $ verbose
        (\ input -> (run1 backend chunking2 input ~?= chunking2Ref input))

    testChunking2b :: Test
    testChunking2b =
      testProperty "chunking2b"
        (\ input -> (run backend (chunking2b input) ~?= chunking2Ref input))
