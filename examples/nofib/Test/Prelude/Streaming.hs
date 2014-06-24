{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeFamilies        #-}

module Test.Prelude.Streaming (
  
  test_loops

) where

import Prelude hiding ( zip, zipWith, fst, snd, map, fromIntegral, sum, replicate )
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

toStream' :: (Shape sh, Elt a, Arrays arrs)
          => Acc (Array (sh :. Int) a)
          -> AccLoop (lenv, Array sh a) arrs
          -> AccLoop lenv arrs
toStream' acc l = toStream (constant (Any :. stream)) acc l
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

idLoop :: (sh ~ FullShape sh, Slice sh, Shape sh, Elt a) => Acc (Array (sh :. Int) a) -> Acc (Array (sh :. Int) a)
idLoop xs = reshape (Acc.shape xs) $ asnd . asnd $ loop
  $ toStream (Acc.shape xs) xs
  $ fromStream ZeroIdx
  $ emptyLoop
  where

idLoopRef :: (Shape sh, Elt a) => (Array (sh :. Int) a) -> (Array (sh :. Int) a)
idLoopRef = id

sumMaxLoop :: (Elt a, IsBounded a, IsNum a) => Acc (Vector a) -> Acc (Scalar a, Scalar a)
sumMaxLoop xs = take2 $ loop 
  $ toStream' xs
  $ foldStream (zipWith max) (unit minBound) ZeroIdx
  $ foldStream (zipWith (+)) (unit 0) ZeroIdx
  $ emptyLoop
  
sumMaxLoopRef :: (Elt a, Ord a, Bounded a, Num a) => Vector a -> (Scalar a, Scalar a)
sumMaxLoopRef xs = ( fromList Z . (:[]) . P.sum     . toList $ xs
                   , fromList Z . (:[]) . P.foldl (P.max) minBound . toList $ xs)

scatterLoop :: (Elt a, IsNum a) => Acc (Vector a) -> Acc (Vector (Int, a)) -> Acc (Vector a)
scatterLoop xs updates = asnd $ loop
  $ toStream' updates
  $ foldStreamFlatten f xs ZeroIdx
  $ emptyLoop
  where
    f xs' _ upd = 
      let (to, ys) = Acc.unzip upd
      in permute (+) xs' (index1 . (`mod` Acc.size xs) . (to Acc.!)) ys

scatterLoopRef :: (Elt a, IsNum a) => Vector a -> Vector (Int, a) -> Vector a
scatterLoopRef vec vec_upd =
  let xs = toList vec
      n = P.length xs
      updates = toList vec_upd
      f xs' (i, x) = [ if j == i `P.mod` n then x P.+ y else y | (j, y) <- P.zip [0..] xs']
      ys = P.foldl f xs updates
  in fromList (Z :. n) ys

logsum :: (Elt a, IsFloating a) => Int -> Acc (Scalar a)
logsum n = asnd $ loop
  $ toStream' (iota n)
  $ mapStream (map (log . fromIntegral . (+1))) ZeroIdx
  $ foldStream (zipWith (+)) (unit 0.0) ZeroIdx
  $ emptyLoop

logsumRef :: (Elt a, IsFloating a) => Int -> Scalar a
logsumRef n = fromList Z [P.sum [log (P.fromIntegral i) | i <- [1..n]]]

logsumChunk :: (Elt a, IsFloating a) => Int -> Int -> Acc (Scalar a)
logsumChunk n b = sum $ asnd  $ loop
  $ toStream' (iotaChunk n b)
  $ mapStream (map (log . fromIntegral . (+1))) ZeroIdx
  $ foldStream (zipWith (+)) (rep (Z :. b) 0.0) ZeroIdx
  $ emptyLoop

logsumChunkRef :: (Elt a, IsFloating a) => Int -> Int -> Scalar a
logsumChunkRef n b = logsumRef (n * b)

nestedLoop :: Int -> Int -> Acc (Vector Int)
nestedLoop n m = asnd . asnd $ loop
  $ toStream' (iota n)
  $ mapStream
  (\ i -> asnd $ loop 
          $ toStream' (iota m)
          $ mapStream (zipWith (+) i) ZeroIdx
          $ foldStream (zipWith (+)) (rep Z 0) ZeroIdx
          $ emptyLoop
  ) ZeroIdx
  $ fromStream ZeroIdx
  $ emptyLoop

nestedLoopRef :: Int -> Int -> Vector Int
nestedLoopRef n m = fromList (Z :. n) [P.sum [i + j | j <- [0..m-1]] | i <- [0..n-1]]

nestedIrregularLoop :: Int -> Acc (Vector Int)
nestedIrregularLoop n = asnd . asnd $ loop
  $ toStream' (iota n)
  $ mapStream
  (\ i -> asnd $ loop 
        $ toStream' (iota' i)
        $ mapStream (zipWith (+) i) ZeroIdx
        $ foldStream (zipWith (+)) (rep Z 0) ZeroIdx
        $ emptyLoop
  ) ZeroIdx
  $ fromStream ZeroIdx
  $ emptyLoop
  
nestedIrregularLoopRef :: Int -> Vector Int
nestedIrregularLoopRef n = fromList (Z :. n) [P.sum [i + j | j <- [0..i-1]] | i <- [0..n-1]]

deepNestedLoop :: Int -> Acc (Vector Int)
deepNestedLoop n = asnd . asnd $ loop
  $ toStream' (iota n)
  $ mapStream
  (\ i -> asnd . asnd $ loop 
        $ toStream' (iota' i)
        $ mapStream 
        (\ j -> asnd $ loop
              $ toStream' (iota' j)
              $ mapStream
              (\ k -> asnd $ loop
                    $ toStream' (iota' k)
                    $ foldStream (zipWith (+)) (rep Z 0) ZeroIdx
                    $ emptyLoop
              ) ZeroIdx
              $ foldStream (zipWith (+)) (rep Z 0) ZeroIdx
              $ emptyLoop
        ) ZeroIdx
        $ fromStream ZeroIdx
        $ emptyLoop
  ) ZeroIdx
  $ fromStream ZeroIdx
  $ emptyLoop
  
deepNestedLoopRef :: Int -> Vector Int
deepNestedLoopRef n = fromList (Z :. P.length xs) xs
  where xs = [P.sum [x | k <- [0..j-1], x <- [0..k-1]] | i <- [0..n-1], j <- [0..i-1]]

test_loops :: Config -> Test
test_loops opt = testGroup "loops"
  [ testGroup "id" $ catMaybes 
    [ testIdLoop configInt8   (undefined :: Int8)
    , testIdLoop configInt16  (undefined :: Int16)
    , testIdLoop configInt32  (undefined :: Int32)
    , testIdLoop configInt64  (undefined :: Int64)
    , testIdLoop configWord8  (undefined :: Word8)
    , testIdLoop configWord16 (undefined :: Word16)
    , testIdLoop configWord32 (undefined :: Word32)
    , testIdLoop configWord64 (undefined :: Word64)
    , testIdLoop configFloat  (undefined :: Float)
    , testIdLoop configDouble (undefined :: Double)
    ]  
  , testGroup "sum_max" $ catMaybes
    [ testSumMaxLoop configInt8   (undefined :: Int8)
    , testSumMaxLoop configInt16  (undefined :: Int16)
    , testSumMaxLoop configInt32  (undefined :: Int32)
    , testSumMaxLoop configInt64  (undefined :: Int64)
    , testSumMaxLoop configWord8  (undefined :: Word8)
    , testSumMaxLoop configWord16 (undefined :: Word16)
    , testSumMaxLoop configWord32 (undefined :: Word32)
    , testSumMaxLoop configWord64 (undefined :: Word64)
    ]
  , testGroup "scatter" $ catMaybes 
    [ testScatterLoop configInt8   (undefined :: Int8)
    , testScatterLoop configInt16  (undefined :: Int16)
    , testScatterLoop configInt32  (undefined :: Int32)
    , testScatterLoop configInt64  (undefined :: Int64)
    , testScatterLoop configWord8  (undefined :: Word8)
    , testScatterLoop configWord16 (undefined :: Word16)
    , testScatterLoop configWord32 (undefined :: Word32)
    , testScatterLoop configWord64 (undefined :: Word64)
    , testScatterLoop configFloat  (undefined :: Float)
    , testScatterLoop configDouble (undefined :: Double)
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
    [ testNestedLoop
    , testNestedIrregularLoop
    , testDeepNestedLoop
    ]
  ]
  where
    backend = get configBackend opt

    testIdLoop :: forall a. (Elt a, Similar a, IsNum a, Arbitrary a)
            => (Config :-> Bool)
            -> a
            -> Maybe Test
    testIdLoop ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testGroup (show (typeOf (undefined :: a)))
          [ testDim dim1
          , testDim dim2
          , testDim dim3
          ]
      where
        testDim :: forall sh. (sh ~ FullShape sh, Slice sh, Shape sh, Eq sh, Arbitrary sh, Arbitrary (Array (sh :. Int) a)) => (sh :. Int) -> Test
        testDim sh = testProperty ("DIM" P.++ show (dim sh))
          ((\ xs -> run1 backend idLoop xs ~?= idLoopRef xs) :: Array (sh :. Int) a -> Property)
          

    testSumMaxLoop :: forall a. (Elt a, Similar a, IsNum a, IsBounded a, Bounded a, Ord a, Arbitrary a)
            => (Config :-> Bool)
            -> a
            -> Maybe Test
    testSumMaxLoop ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testProperty (show (typeOf (undefined :: a)))
          ((\xs -> run1 backend sumMaxLoop xs ~?= sumMaxLoopRef xs) :: Vector a -> Property)


    testScatterLoop :: forall a. (Elt a, Similar a, IsNum a, Arbitrary a)
            => (Config :-> Bool)
            -> a
            -> Maybe Test
    testScatterLoop ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testProperty (show (typeOf (undefined :: a)))
          ((\xs updates -> run backend (scatterLoop (use xs) (use updates)) ~?= scatterLoopRef xs updates) :: Vector a -> Vector (Int, a) -> Property)


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
          (\ (NonNegative n) (NonZero b) -> (run backend (logsumChunk n b) :: Scalar a) ~?= logsumChunkRef n b)

    testNestedLoop :: Test
    testNestedLoop =
      testProperty "regular"
        (\ (NonNegative n) (NonNegative m) -> (run backend (nestedLoop n m) ~?= nestedLoopRef n m))
      
    testNestedIrregularLoop :: Test
    testNestedIrregularLoop =
      testProperty "irregular"
        (\ (NonNegative n) -> (run backend (nestedIrregularLoop n) ~?= nestedIrregularLoopRef n))
      
    testDeepNestedLoop :: Test
    testDeepNestedLoop =
      testProperty "deep"
        (\ (NonNegative n) -> (run backend (deepNestedLoop n) ~?= deepNestedLoopRef n))
      
