{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module QuickCheck.Arbitrary.Array where

import QuickCheck.Arbitrary.Shape

import Data.List
import Test.QuickCheck
import System.Random                                    ( Random )
import Data.Array.Accelerate.Array.Sugar                ( Array, Segments, Shape, Elt, Z(..), (:.)(..), (!), DIM0, DIM1, DIM2, DIM3, DIM4 )
import qualified Data.Array.Accelerate.Array.Sugar      as Sugar
import qualified Data.Set                               as Set



instance (Elt e, Arbitrary e) => Arbitrary (Array DIM0 e) where
  arbitrary  = arbitraryArray Z
  shrink arr = [ Sugar.fromList Z [x] | x <- shrink (arr ! Z) ]

instance (Elt e, Arbitrary e) => Arbitrary (Array DIM1 e) where
  arbitrary  = arbitraryArray =<< sized arbitraryShape
  shrink arr = [ Sugar.fromList (Z :. length sl) sl | sl <- shrink (Sugar.toList arr) ]

instance (Elt e, Arbitrary e) => Arbitrary (Array DIM2 e) where
  arbitrary  = arbitraryArray =<< sized arbitraryShape
  shrink arr =
    let (Z :. width :. height)   = Sugar.shape arr
    in
    [ Sugar.fromList (Z :. length slx :. length sly) [ arr ! (Z:.x:.y) | x <- slx, y <- sly ]
        | slx <- map nub $ shrink [0 .. width  - 1]
        , sly <- map nub $ shrink [0 .. height - 1]
    ]

instance (Elt e, Arbitrary e) => Arbitrary (Array DIM3 e) where
  arbitrary  = arbitraryArray =<< sized arbitraryShape
  shrink arr =
    let (Z :. width :. height :. depth)   = Sugar.shape arr
    in
    [ Sugar.fromList (Z :. length slx :. length sly :. length slz) [ arr ! (Z:.x:.y:.z) | x <- slx, y <- sly, z <- slz ]
        | slx <- map nub $ shrink [0 .. width  - 1]
        , sly <- map nub $ shrink [0 .. height - 1]
        , slz <- map nub $ shrink [0 .. depth  - 1]
    ]

instance (Elt e, Arbitrary e) => Arbitrary (Array DIM4 e) where
  arbitrary  = arbitraryArray =<< sized arbitraryShape
  shrink arr =
    let (Z :. width :. height :. depth :. time)   = Sugar.shape arr
    in
    [ Sugar.fromList (Z :. length slx :. length sly :. length slz :. length slw) [ arr ! (Z:.x:.y:.z:.w) | x <- slx, y <- sly, z <- slz, w <- slw ]
        | slx <- map nub $ shrink [0 .. width  - 1]
        , sly <- map nub $ shrink [0 .. height - 1]
        , slz <- map nub $ shrink [0 .. depth  - 1]
        , slw <- map nub $ shrink [0 .. time   - 1]
    ]

-- Generate an arbitrary array of the given shape using the default element
-- generator
--
arbitraryArray :: (Shape sh, Elt e, Arbitrary e) => sh -> Gen (Array sh e)
arbitraryArray sh = arbitraryArrayOf sh arbitrary

-- Generate an array of the given shape using the supplied element generator
-- function.
--
arbitraryArrayOf :: (Shape sh, Elt e) => sh -> Gen e -> Gen (Array sh e)
arbitraryArrayOf sh gen = Sugar.fromList sh `fmap` vectorOf (Sugar.size sh) gen

{--
 -- A version that does not use fromList. It does not gain us anything while
 -- being much more complex in implementation.
 --
arbitraryArrayOf :: (Shape sh, Elt e, Arbitrary e) => sh -> Gen e -> Gen (Array sh e)
arbitraryArrayOf sh (MkGen gen)
  = MkGen
  $ \g k -> let !n          = Sugar.size sh
                (adata, _)  = runArrayData $ do
                                arr <- newArrayData n
                                let go _  !i | i >= n = return ()
                                    go !r !i          =
                                      let (r1,r2) = split r
                                          v       = gen r1 k
                                      in
                                      unsafeWriteArrayData arr i (Sugar.fromElt v) >> go r2 (i+1)
                                --
                                go g 0
                                return (arr, undefined)
    in
    adata `seq` Sugar.Array (Sugar.fromElt sh) adata
--}

-- Generate an array where the outermost dimension satisfies the given segmented
-- array descriptor.
--
arbitrarySegmentedArray
    :: (Integral i, Shape sh, Elt e, Arbitrary sh, Arbitrary e)
    => Segments i
    -> Gen (Array (sh :. Int) e)
arbitrarySegmentedArray segs = do
  let sz        =  fromIntegral . sum $ Sugar.toList segs
  sh            <- sized $ \n -> arbitraryShape (n `div` 2)
  arbitraryArray (sh :. sz)

shrinkSegmentedArray
    :: (Integral i, Shape sh, Elt e, Arbitrary e)
    => Segments i
    -> Array (sh :. Int) e
    -> [Array (sh :. Int) e]
shrinkSegmentedArray segs arr =
  let n       = fromIntegral $ sum (Sugar.toList segs)
      sz :. _ = Sugar.shape arr
      req     = Sugar.size sz * n
  in
  [ Sugar.fromList (sz :. n) sl | sl <- shrink (Sugar.toList arr)
                                , length sl >= req
                                ]


-- Generate a segment descriptor. Both the array and individual segments might
-- be empty.
--
arbitrarySegments :: (Elt i, Integral i, Random i) => Gen (Segments i)
arbitrarySegments =
  sized $ \n -> do
    k <- choose (0,n)
    arbitraryArrayOf (Z:.k) (choose (0, fromIntegral n))

shrinkSegments :: (Elt i, Integral i, Arbitrary i) => Segments i -> [Segments i]
shrinkSegments arr =
  [ Sugar.fromList (Z :. length sl) sl | sl <- shrink (Sugar.toList arr)
                                       , all (>= 0) sl
                                       ]

-- Generate a possibly empty segment descriptor, where each segment is non-empty
--
arbitrarySegments1 :: (Elt i, Integral i, Random i) => Gen (Segments i)
arbitrarySegments1 =
  sized $ \n -> do
    k <- choose (0,n)
    arbitraryArrayOf (Z:.k) (choose (1, 1 `max` fromIntegral n))

shrinkSegments1 :: (Elt i, Integral i, Arbitrary i) => Segments i -> [Segments i]
shrinkSegments1 arr =
  [ Sugar.fromList (Z :. length sl) sl | sl <- shrink (Sugar.toList arr)
                                       , all (>= 1) sl
                                       ]


-- Generate an vector where every element in the array is unique. The maximum
-- size is based on the current 'sized' parameter.
--
arbitraryUniqueVectorOf :: (Elt e, Ord e) => Gen e -> Gen (Array DIM1 e)
arbitraryUniqueVectorOf gen =
  sized $ \n -> do
    set <- fmap Set.fromList (vectorOf n gen)
    k   <- choose (0, Set.size set)
    return $! Sugar.fromList (Z :. k) (Set.toList set)


-- Generate an arbitrary CSR matrix. The first parameter is the segment
-- descriptor, the second a sparse vector of (index,value) pairs, and the third
-- the matrix width (number of columns).
--
-- The matrix size is based on the current `sized` parameter.
--
arbitraryCSRMatrix
    :: (Elt i, Integral i, Random i, Elt e, Arbitrary e)
    => Gen ( Array DIM1 i, Array DIM1 (i,e), Int )
arbitraryCSRMatrix =
  sized $ \cols -> do
    segd        <- arbitrarySegments
    let nnz     =  fromIntegral . sum $ Sugar.toList segd
    smat        <- arbitraryArrayOf (Z :. nnz) $ do
                     val <- arbitrary
                     ind <- choose (0, fromIntegral cols - 1)
                     return (ind, val)
    return (segd, smat, cols)

