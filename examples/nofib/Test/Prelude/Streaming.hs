{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Test.Prelude.Streaming (

  test_fromS,
  test_toS,
  test_mapS,
  test_foldS,
--  toSRef,
--  fromSRef,
  mapSRef,
--  foldSRef,

) where

-- TODO remov --------------
import Debug.Trace ( trace )
----------------------------

import Prelude                                                  as P
import Data.Label
import Data.Maybe
import Data.Typeable
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Prelude.Mapping ( mapRef, zipWithRef )

import Config
import ParseArgs
import Test.Base
import QuickCheck.Arbitrary.Array                               ()
import Data.Array.Accelerate                                    as A hiding (indexHead, indexTail)
import Data.Array.Accelerate.Array.Sugar                        as Sugar

instance Similar DIM0
instance Similar DIM1
instance Similar DIM2

test_fromS :: Config -> Test
test_fromS opt = testGroup "fromStream" $ catMaybes
  [ testElt configInt8   (undefined :: Int8)
  , testElt configInt16  (undefined :: Int16)
  , testElt configInt32  (undefined :: Int32)
  , testElt configInt64  (undefined :: Int64)
  , testElt configWord8  (undefined :: Word8)
  , testElt configWord16 (undefined :: Word16)
  , testElt configWord32 (undefined :: Word32)
  , testElt configWord64 (undefined :: Word64)
  , testElt configFloat  (undefined :: Float)
  , testElt configDouble (undefined :: Double)
  ]
  where    
    backend         = get configBackend opt
    test_from  xs = run1 backend (A.fromStream) xs ~?= fromStreamRef xs
    testElt :: forall a. (Elt a, IsNum a, Similar a, Arbitrary a, Ord a)
            => (Config :-> Bool)
            -> a
            -> Maybe Test
    testElt ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testGroup (show (typeOf (undefined :: a)))
          [ testDim dim0
          , testDim dim1
          , testDim dim2
          ]
      where
        testDim :: forall sh. (Similar sh, Shape sh, Eq sh, Arbitrary sh, Arbitrary [Array sh a]) => sh -> Test
        testDim sh = testGroup ("DIM" P.++ show (dim sh))
          [ testProperty "fromStream" (  test_from   :: [Array sh a] -> Property)
          ]


test_toS :: Config -> Test
test_toS opt = testGroup "toStream" $ catMaybes
  [ testElt configInt8   (undefined :: Int8)
  , testElt configInt16  (undefined :: Int16)
  , testElt configInt32  (undefined :: Int32)
  , testElt configInt64  (undefined :: Int64)
  , testElt configWord8  (undefined :: Word8)
  , testElt configWord16 (undefined :: Word16)
  , testElt configWord32 (undefined :: Word32)
  , testElt configWord64 (undefined :: Word64)
  , testElt configFloat  (undefined :: Float)
  , testElt configDouble (undefined :: Double)
  ]
  where    
    backend         = get configBackend opt
    test_to  xs = run1 backend (A.toStream) xs ~?= toStreamRef xs
    test_iso  xs = run1 backend (A.reshape (constant (arrayShape xs)) . A.snd . A.fromStream . A.toStream) xs ~?= xs
    testElt :: forall a. (Elt a, IsNum a, Similar a, Arbitrary a, Ord a)
            => (Config :-> Bool)
            -> a
            -> Maybe Test
    testElt ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testGroup (show (typeOf (undefined :: a)))
          [ testDim dim1
          , testDim dim2
          ]
      where
        testDim :: forall sh. (Shape sh, Eq sh, Arbitrary sh, Arbitrary (Array (sh :. Int) a)) => (sh :. Int) -> Test
        testDim sh = testGroup ("DIM" P.++ show (dim sh))
          [ testProperty "toStream" (   test_to    :: (Array (sh :. Int) a) -> Property)
          , testProperty "to- fromStream iso" (   test_iso    :: (Array (sh :. Int) a) -> Property)
          ]


test_mapS :: Config -> Test
test_mapS opt = testGroup "mapStream" $ catMaybes
  [ testElt configInt8   (undefined :: Int8)
  , testElt configInt16  (undefined :: Int16)
  , testElt configInt32  (undefined :: Int32)
  , testElt configInt64  (undefined :: Int64)
  , testElt configWord8  (undefined :: Word8)
  , testElt configWord16 (undefined :: Word16)
  , testElt configWord32 (undefined :: Word32)
  , testElt configWord64 (undefined :: Word64)
  , testElt configFloat  (undefined :: Float)
  , testElt configDouble (undefined :: Double)
  , testElt' configInt8   (undefined :: Int8)
  , testElt' configInt16  (undefined :: Int16)
  , testElt' configInt32  (undefined :: Int32)
  , testElt' configInt64  (undefined :: Int64)
  , testElt' configWord8  (undefined :: Word8)
  , testElt' configWord16 (undefined :: Word16)
  , testElt' configWord32 (undefined :: Word32)
  , testElt' configWord64 (undefined :: Word64)
  , testElt' configFloat  (undefined :: Float)
  , testElt' configDouble (undefined :: Double)
  ]
  where
    backend         = get configBackend opt
    test_square  xs = run1 backend (A.mapStream (A.map (\x -> x*x))) xs ~?= mapSRef (mapRef (\x -> x*x)) xs
    test_foldAll xs = run1 backend (A.mapStream (A.foldAll (+) 0))   xs ~?= mapSRef (foldAllRef (+) 0) xs
    test_fold1   xs = run1 backend (A.mapStream (A.fold1 (+)))       xs ~?= mapSRef (fold1Ref (+)) xs
    test_scanl   xs = run1 backend (A.mapStream (A.scanl (+) 0))     xs ~?= mapSRef (scanlRef (+) 0) xs
    test_repl    xs sl = run1 backend (A.mapStream (A.replicate (constant sl))) xs ~?= mapSRef (replRef sl) xs
    testElt :: forall a. (Elt a, IsNum a, Similar a, Arbitrary a, Ord a)
            => (Config :-> Bool)
            -> a
            -> Maybe Test
    testElt ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testGroup (show (typeOf (undefined :: a)))
          [ testDim dim0
          , testDim dim1
          , testDim dim2
          ]
      where
        testDim :: forall sh. (Shape sh, Eq sh, Arbitrary sh, Arbitrary [Array sh a]) => sh -> Test
        testDim sh = testGroup ("DIM" P.++ show (dim sh))
          [ testProperty "square" (  test_square   :: [Array sh a] -> Property), 
            testProperty "foldAll" ( test_foldAll  :: [Array sh a] -> Property),
            testProperty "scanl" (   test_scanl    :: [Vector a] -> Property)
          ]
    testElt' :: forall a. (Elt a, IsNum a, Similar a, Arbitrary a, Ord a)
            => (Config :-> Bool)
            -> a
            -> Maybe Test
    testElt' ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testGroup (show (typeOf (undefined :: a)))
          [ testDim' dim1
          , testDim' dim2
          ]
      where
        testDim' :: forall sh. (Shape sh, Eq sh, Arbitrary sh, Arbitrary [Array (sh :. Int) a]) => (sh :. Int) -> Test
        testDim' sh = testGroup ("DIM" P.++ show (dim sh))
          [ testProperty "fold1" (   test_fold1    :: [Array (sh :. Int) a] -> Property)
          ]

test_foldS :: Config -> Test
test_foldS opt = testGroup "foldStream" $ catMaybes
  [ testElt configInt8   (undefined :: Int8)
  , testElt configInt16  (undefined :: Int16)
  , testElt configInt32  (undefined :: Int32)
  , testElt configInt64  (undefined :: Int64)
  , testElt configWord8  (undefined :: Word8)
  , testElt configWord16 (undefined :: Word16)
  , testElt configWord32 (undefined :: Word32)
  , testElt configWord64 (undefined :: Word64)
  , testElt configFloat  (undefined :: Float)
  , testElt configDouble (undefined :: Double)
  , testElt' configInt8   (undefined :: Int8)
  , testElt' configInt16  (undefined :: Int16)
  , testElt' configInt32  (undefined :: Int32)
  , testElt' configInt64  (undefined :: Int64)
  , testElt' configWord8  (undefined :: Word8)
  , testElt' configWord16 (undefined :: Word16)
  , testElt' configWord32 (undefined :: Word32)
  , testElt' configWord64 (undefined :: Word64)
  , testElt' configFloat  (undefined :: Float)
  , testElt' configDouble (undefined :: Double)
  ]
  where
    backend         = get configBackend opt
    test_sum  xs z  = run1 backend (A.foldStream (A.zipWith (+)) (use z)) xs ~?= foldSRef (zipWithRef (+)) z xs
    testElt :: forall a. (Elt a, IsNum a, Similar a, Arbitrary a, Ord a)
            => (Config :-> Bool)
            -> a
            -> Maybe Test
    testElt ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testGroup (show (typeOf (undefined :: a)))
          [ testDim dim0
          , testDim dim1
          , testDim dim2
          ]
      where
        testDim :: forall sh. (Shape sh, Eq sh, Arbitrary sh, Arbitrary (Array sh a)) => sh -> Test
        testDim sh = testGroup ("DIM" P.++ show (dim sh))
          [ testProperty "sum" (  test_sum   :: [Array sh a] -> Array sh a -> Property)
          ]
    testElt' :: forall a. (Elt a, IsNum a, Similar a, Arbitrary a, Ord a)
            => (Config :-> Bool)
            -> a
            -> Maybe Test
    testElt' ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testGroup (show (typeOf (undefined :: a)))
          [ testDim' dim1
          , testDim' dim2
          ]
      where
        testDim' :: forall sh. (Shape sh, Eq sh, Arbitrary sh, Arbitrary [Array (sh :. Int) a]) => (sh :. Int) -> Test
        testDim' sh = testGroup ("DIM" P.++ show (dim sh))
          [ 
          ]


-- Reference Implementation
-- ------------------------

mapSRef :: (Shape sh, Shape sh', Elt a, Elt b) => (Array sh a -> Array sh' b) -> [Array sh a] -> [Array sh' b]
mapSRef = P.map

foldSRef :: (Shape sh, Elt a) => (Array sh a -> Array sh a -> Array sh a) -> Array sh a -> [Array sh a] -> Array sh a
foldSRef = P.foldl

fromStreamRef :: (Shape sh, Elt a) => [Array sh a] -> (Vector sh, Vector a)
fromStreamRef st = 
  let shapes = P.map arrayShape st 
      elems  = concatMap toList st
      sh     = (Z :. length shapes)
      sh'    = (Z :. length elems)
  in (fromList sh shapes, fromList sh' elems)
      


toStreamRef :: (Shape sh, Elt a) => (Array (sh :. Int) a) -> [Array sh a]
toStreamRef arr = 
  let (sh :. n) = arrayShape arr
      m        = Sugar.size sh
      elems     = toList arr
      starts    = P.replicate n m
  in [ fromList sh sub | sub <- splitPlaces starts elems ]
      

foldAllRef :: Elt e => (e -> e -> e) -> e -> Array sh e -> Array Z e
foldAllRef f z
  = A.fromList Z
  . return
  . foldl f z
  . A.toList

fold1Ref :: (Shape sh, Elt e) => (e -> e -> e) -> Array (sh :. Int) e -> Array sh e
fold1Ref f arr =
  let (sh :. n) = arrayShape arr
  in  fromList sh [ foldl1 f sub | sub <- splitEvery n (toList arr) ]

scanlRef :: Elt e => (e -> e -> e) -> e -> Vector e -> Vector e
scanlRef f z vec =
  let (Z :. n)  = arrayShape vec
  in  A.fromList (Z :. n+1) . P.scanl f z . A.toList $ vec

replRef = undefined