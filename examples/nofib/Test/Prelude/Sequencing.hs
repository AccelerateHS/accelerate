{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeFamilies        #-}

module Test.Prelude.Sequencing (
  
  test_sequences

) where

import Prelude hiding ( zip, zipWith, fst, snd, map, fromIntegral, sum, product, replicate, (++) )
import qualified Prelude                                        as P
import Data.Label
import Data.Maybe
import Data.Typeable
import Test.QuickCheck hiding ( generate )
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Config
import ParseArgs
import Test.Base
import QuickCheck.Arbitrary.Array                               ()
import Data.Array.Accelerate                                    as Acc
import Data.Array.Accelerate.Array.Sugar                        as Sugar
import Data.Array.Accelerate.AST ( Idx(..) )

toSeq' :: (Shape sh, Elt a, Arrays arrs)
          => Acc (Array (sh :. Int) a)
          -> AccSequence (lenv, Array sh a) arrs
          -> AccSequence lenv arrs
toSeq' acc l = toSeq (constant (Any :. stream)) acc l
  where 
    stream :: Int
    stream = maxBound

take2 :: (Arrays a, Arrays b, Arrays c) => Acc ((a,b),c) -> Acc (b,c)
take2 x = lift (asnd (afst x), asnd x)

iota :: Int -> Acc (Vector Int)
iota n = generate (index1 (constant n)) unindex1

iota' :: Acc (Scalar Int) -> Acc (Vector Int)
iota' n = generate (index1 (the n)) unindex1

rep :: (Shape sh, Elt a) => sh -> a -> Acc (Array sh a)
rep sh a = fill (constant sh) (constant a)

iotaChunk :: Int -> Int -> Acc (Array (Z :. Int :. Int) Int)
iotaChunk n b = reshape (constant (Z :. b :. n)) $ generate (index1 (constant (n * b))) unindex1

idSequence :: (sh ~ FullShape sh, Slice sh, Shape sh, Elt a) => Acc (Array (sh :. Int) a) -> Acc (Array (sh :. Int) a)
idSequence xs = reshape (Acc.shape xs) $ asnd . asnd $ runSequence
  $ toSeq (Acc.shape xs) xs
  $ fromSeq ZeroIdx
  $ emptySeq
  where

idSequenceRef :: (Shape sh, Elt a) => (Array (sh :. Int) a) -> (Array (sh :. Int) a)
idSequenceRef = id

sumMaxSequence :: (Elt a, IsBounded a, IsNum a) => Acc (Vector a) -> Acc (Scalar a, Scalar a)
sumMaxSequence xs = take2 $ runSequence 
  $ toSeq' xs
  $ foldSeq (zipWith max) (unit minBound) ZeroIdx
  $ foldSeq (zipWith (+)) (unit 0) ZeroIdx
  $ emptySeq
  
sumMaxSequenceRef :: (Elt a, Ord a, Bounded a, Num a) => Vector a -> (Scalar a, Scalar a)
sumMaxSequenceRef xs = ( fromList Z . (:[]) . P.sum     . toList $ xs
                   , fromList Z . (:[]) . P.foldl (P.max) minBound . toList $ xs)

scatterSequence :: (Elt a, IsNum a) => Acc (Vector a, Vector (Int, a)) -> Acc (Vector a)
scatterSequence input = asnd $ runSequence
  $ toSeq' (asnd input)
  $ foldSeqFlatten f (afst input) ZeroIdx
  $ emptySeq
  where
    f xs' _ upd = 
      let (to, ys) = Acc.unzip upd
      in permute (+) xs' (index1 . (`mod` Acc.size (afst input)) . (to Acc.!)) ys

scatterSequenceRef :: (Elt a, IsNum a) => (Vector a, Vector (Int, a)) -> Vector a
scatterSequenceRef (vec, vec_upd) =
  let xs = toList vec
      n = P.length xs
      updates = toList vec_upd
      f xs' (i, x) = [ if j == i `P.mod` n then x P.+ y else y | (j, y) <- P.zip [0..] xs']
      ys = P.foldl f xs updates
  in fromList (Z :. n) ys

logsum :: (Elt a, IsFloating a) => Int -> Acc (Scalar a)
logsum n = asnd $ runSequence
  $ toSeq' (iota n)
  $ mapSeq (map (log . fromIntegral . (+1))) ZeroIdx
  $ foldSeq (zipWith (+)) (unit 0.0) ZeroIdx
  $ emptySeq

logsumRef :: (Elt a, IsFloating a) => Int -> Scalar a
logsumRef n = fromList Z [P.sum [log (P.fromIntegral i) | i <- [1..n]]]

logsumChunk :: (Elt a, IsFloating a) => Int -> Int -> Acc (Scalar a)
logsumChunk n b = sum $ asnd  $ runSequence
  $ toSeq' (iotaChunk n b)
  $ mapSeq (map (log . fromIntegral . (+1))) ZeroIdx
  $ foldSeq (zipWith (+)) (rep (Z :. b) 0.0) ZeroIdx
  $ emptySeq

logsumChunkRef :: (Elt a, IsFloating a) => Int -> Int -> Scalar a
logsumChunkRef n b = logsumRef (n * b)

nestedSequence :: Int -> Int -> Acc (Vector Int)
nestedSequence n m = asnd . asnd $ runSequence
  $ toSeq' (iota n)
  $ mapSeq
  (\ i -> asnd $ runSequence 
          $ toSeq' (iota m)
          $ mapSeq (zipWith (+) i) ZeroIdx
          $ foldSeq (zipWith (+)) (rep Z 0) ZeroIdx
          $ emptySeq
  ) ZeroIdx
  $ fromSeq ZeroIdx
  $ emptySeq

nestedSequenceRef :: Int -> Int -> Vector Int
nestedSequenceRef n m = fromList (Z :. n) [P.sum [i + j | j <- [0..m-1]] | i <- [0..n-1]]

nestedIrregularSequence :: Int -> Acc (Vector Int)
nestedIrregularSequence n = asnd . asnd $ runSequence
  $ toSeq' (iota n)
  $ mapSeq
  (\ i -> asnd $ runSequence 
        $ toSeq' (iota' i)
        $ mapSeq (zipWith (+) i) ZeroIdx
        $ foldSeq (zipWith (+)) (rep Z 0) ZeroIdx
        $ emptySeq
  ) ZeroIdx
  $ fromSeq ZeroIdx
  $ emptySeq
  
nestedIrregularSequenceRef :: Int -> Vector Int
nestedIrregularSequenceRef n = fromList (Z :. n) [P.sum [i + j | j <- [0..i-1]] | i <- [0..n-1]]

deepNestedSequence :: Int -> Acc (Vector Int)
deepNestedSequence n = asnd . asnd $ runSequence
  $ toSeq' (iota n)
  $ mapSeq
  (\ i -> asnd . asnd $ runSequence 
        $ toSeq' (iota' i)
        $ mapSeq 
        (\ j -> asnd $ runSequence
              $ toSeq' (iota' j)
              $ mapSeq
              (\ k -> asnd $ runSequence
                    $ toSeq' (iota' k)
                    $ foldSeq (zipWith (+)) (rep Z 0) ZeroIdx
                    $ emptySeq
              ) ZeroIdx
              $ foldSeq (zipWith (+)) (rep Z 0) ZeroIdx
              $ emptySeq
        ) ZeroIdx
        $ fromSeq ZeroIdx
        $ emptySeq
  ) ZeroIdx
  $ fromSeq ZeroIdx
  $ emptySeq
  
deepNestedSequenceRef :: Int -> Vector Int
deepNestedSequenceRef n = fromList (Z :. P.length xs) xs
  where xs = [P.sum [x | k <- [0..j-1], x <- [0..k-1]] | i <- [0..n-1], j <- [0..i-1]]

chunking1 :: Int -> Acc (Scalar Int)
chunking1 n = asnd $ runSequence
  $ toSeq' (iota n)
  $ mapSeq iota' ZeroIdx
  $ mapSeq (iota' . map (constant n -)) (SuccIdx ZeroIdx)
  $ zipWithSeq (\ x y -> zipWith (-) (sum x) (product y)) (SuccIdx ZeroIdx) ZeroIdx
  $ foldSeq (zipWith (+)) (rep Z 0) (ZeroIdx)
  $ emptySeq
  

chunking2 :: Acc (Array (Z :. Int :. Int) Int, Array (Z :. Int :. Int) Int) -> Acc (Vector Int)
chunking2 input = asnd $ asnd $ runSequence
  $ toSeq (constant (Z :. stream :. All)) (afst input)
  $ toSeq (constant (Z :. stream :. All)) (asnd input)
  $ zipWithSeq (++) (SuccIdx ZeroIdx) ZeroIdx
  $ fromSeq ZeroIdx
  $ emptySeq
  where
    stream = maxBound :: Int

chunking2b :: (Array (Z :. Int :. Int) Int, Array (Z :. Int :. Int) Int) -> Acc (Vector Int)
chunking2b input = asnd $ asnd $ runSequence
  $ useLazy (constant (Z :. stream :. All)) (P.fst input)
  $ useLazy (constant (Z :. stream :. All)) (P.snd input)
  $ zipWithSeq (++) (SuccIdx ZeroIdx) ZeroIdx
  $ fromSeq ZeroIdx
  $ emptySeq
  where
    stream = maxBound :: Int

chunking2Ref :: (Array (Z :. Int :. Int) Int, Array (Z :. Int :. Int) Int) -> Vector Int
chunking2Ref (a, b) =
  let 
    (Z:. n :. m) = Sugar.shape a
    (Z:. r :. c) = Sugar.shape b
    xs = [ [ a Sugar.! (Z :. i :. j) | j <- [0..m-1]] | i <- [0..n-1]]
    ys = [ [ b Sugar.! (Z :. i :. j) | j <- [0..c-1]] | i <- [0..r-1]]
    zs = P.zipWith (P.++) xs ys
    res = concat zs
  in fromList (Z :. length res) res

chunking1Ref :: Int -> Scalar Int
chunking1Ref n = fromList Z [x]
  where x = P.sum [ P.sum [0..i-1] - P.product [0..n-i-1] | i <- [0..n-1] ]

test_sequences :: Config -> Test
test_sequences opt = testGroup "sequences"
  [ testGroup "id" $ catMaybes 
    [ testIdSequence configInt8   (undefined :: Int8)
    , testIdSequence configInt16  (undefined :: Int16)
    , testIdSequence configInt32  (undefined :: Int32)
    , testIdSequence configInt64  (undefined :: Int64)
    , testIdSequence configWord8  (undefined :: Word8)
    , testIdSequence configWord16 (undefined :: Word16)
    , testIdSequence configWord32 (undefined :: Word32)
    , testIdSequence configWord64 (undefined :: Word64)
    , testIdSequence configFloat  (undefined :: Float)
    , testIdSequence configDouble (undefined :: Double)
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
  , testGroup "logsum_chunked" $ catMaybes
    [ testLogsumChunked configFloat  (undefined :: Float)
    , testLogsumChunked configDouble (undefined :: Double)
    ]
  , testGroup "nested"
    [ testNestedSequence
    , testNestedIrregularSequence
--    , testDeepNestedSequence
    ]
  , testGroup "chunking"
    [ testChunking1
    , testChunking2
    , testChunking2b
    ]
  ]
  where
    backend = get configBackend opt

    testIdSequence :: forall a. (Elt a, Similar a, IsNum a, Arbitrary a)
            => (Config :-> Bool)
            -> a
            -> Maybe Test
    testIdSequence ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testGroup (show (typeOf (undefined :: a)))
          [ testDim dim1
          , testDim dim2
          ]
      where
        testDim :: forall sh. (sh ~ FullShape sh, Slice sh, Shape sh, Eq sh, Arbitrary sh, Arbitrary (Array (sh :. Int) a)) => (sh :. Int) -> Test
        testDim sh = testProperty ("DIM" P.++ show (dim sh))
          ((\ xs -> run1 backend idSequence xs ~?= idSequenceRef xs) :: Array (sh :. Int) a -> Property)
          

    testSumMaxSequence :: forall a. (Elt a, Similar a, IsNum a, IsBounded a, Bounded a, Ord a, Arbitrary a)
            => (Config :-> Bool)
            -> a
            -> Maybe Test
    testSumMaxSequence ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testProperty (show (typeOf (undefined :: a)))
          ((\xs -> run1 backend sumMaxSequence xs ~?= sumMaxSequenceRef xs) :: Vector a -> Property)


    testScatterSequence :: forall a. (Elt a, Similar a, IsNum a, Arbitrary a)
            => (Config :-> Bool)
            -> a
            -> Maybe Test
    testScatterSequence ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testProperty (show (typeOf (undefined :: a)))
          ((\input -> run1 backend scatterSequence input ~?= scatterSequenceRef input) :: (Vector a, Vector (Int, a)) -> Property)


    testLogsum :: forall a. (Elt a, Similar a, IsFloating a, Arbitrary a)
               => (Config :-> Bool)
               -> a
               -> Maybe Test
    testLogsum ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testProperty (show (typeOf (undefined :: a)))
          (\ (NonNegative n) -> (run backend (logsum n) :: Scalar a) ~?= logsumRef n)

    testLogsumChunked :: forall a. (Elt a, Similar a, IsFloating a, Arbitrary a)
               => (Config :-> Bool)
               -> a
               -> Maybe Test
    testLogsumChunked ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testProperty (show (typeOf (undefined :: a)))
          (\ (NonNegative n) (Positive b) -> (run backend (logsumChunk n b) :: Scalar a) ~?= logsumChunkRef n b)

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
      testProperty "chunking2"
        (\ input -> (run1 backend chunking2 input ~?= chunking2Ref input))

    testChunking2b :: Test
    testChunking2b =
      testProperty "chunking2b"
        (\ input -> (run backend (chunking2b input) ~?= chunking2Ref input))
