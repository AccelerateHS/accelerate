{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Test.Prelude.Permute (

  test_permute

) where

import Prelude                                                  as P
import Data.Label
import Data.Maybe
import Data.Typeable
import Control.Monad
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Config
import Test.Base
import QuickCheck.Arbitrary.Array

import Data.Array.Accelerate                                    as A
import Data.Array.Accelerate.IO.Data.Array.IArray               as A
import Data.Array.Accelerate.Array.Sugar                        ( rank )
import Data.Array.Accelerate.Examples.Internal                  as A

import Data.Array.ST                                            ( runSTArray )
import Data.Array.Unboxed                                       ( IArray, UArray )
import qualified Data.Array.Unboxed                             as IArray
import qualified Data.Array.MArray                              as M


--
-- Forward permutation ---------------------------------------------------------
--

test_permute :: Backend -> Config -> Test
test_permute backend opt = testGroup "permute" $ catMaybes
  [ testIntegralElt configInt8   (undefined :: Int8)
  , testIntegralElt configInt16  (undefined :: Int16)
  , testIntegralElt configInt32  (undefined :: Int32)
  , testIntegralElt configInt64  (undefined :: Int64)
  , testIntegralElt configWord8  (undefined :: Word8)
  , testIntegralElt configWord16 (undefined :: Word16)
  , testIntegralElt configWord32 (undefined :: Word32)
  , testIntegralElt configWord64 (undefined :: Word64)
  , testFloatingElt configFloat  (undefined :: Float)
  , testFloatingElt configDouble (undefined :: Double)
  ]
  where
    testIntegralElt :: forall e. (P.Integral e, A.Integral e, A.FromIntegral e Int, Arbitrary e, Similar e, IArray UArray e) => (Config :-> Bool) -> e -> Maybe Test
    testIntegralElt ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testGroup (show (typeOf (undefined :: e)))
          [
            test_fill (undefined :: e)
          , testProperty "scatter"   (test_scatter :: e -> Property)
          -- , testProperty "scatterIf" (test_scatterIf :: e -> Property)
          , testProperty "histogram" (test_histogram A.fromIntegral P.fromIntegral :: Vector e -> Property)
          ]

    testFloatingElt :: forall e. (P.RealFrac e, A.RealFrac e, Arbitrary e, Similar e, IArray UArray e) => (Config :-> Bool) -> e -> Maybe Test
    testFloatingElt ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testGroup (show (typeOf (undefined :: e)))
          [
            test_fill (undefined :: e)
          , testProperty "scatter"   (test_scatter :: e -> Property)
          -- , testProperty "scatterIf" (test_scatterIf :: e -> Property)
          , testProperty "histogram" (test_histogram A.floor P.floor :: Vector e -> Property)
          ]

    -- Test is permutation works by just copying elements directly from one
    -- array to the other. Does not attempt to use elements from the defaults
    -- array. Additionally, works for any dimension. (c.f. Issue #93)
    --
    test_fill :: forall e. (P.Num e, A.Num e, Arbitrary e, Similar e) => e -> Test
    test_fill _ = testGroup "fill"
      [ -- testDim dim0         -- Accelerate issue #87
        testDim dim1
      , testDim dim2
      ]
      where
        testDim :: forall sh. (Shape sh, P.Eq sh, Arbitrary (Array sh e)) => sh -> Test
        testDim sh = testProperty ("DIM" P.++ show (rank sh)) (push_fill :: Array sh e -> Property)
          where
            push_fill :: Array sh e -> Property
            push_fill xs = run1 backend go xs ~?= xs
              where
                go arr = permute const (A.fill (A.shape arr) (constant 0)) id arr

    -- Test if the combining operation for forward permutation works, by
    -- building a histogram. Often tricky for parallel backends.
    --
    test_histogram :: (P.Num e, A.Num e, Similar e, IArray UArray e) => (Exp e -> Exp Int) -> (e -> Int) -> Vector e -> Property
    test_histogram f g xs =
      forAll arbitrary $ \(Positive n) ->
        runN backend (histogramAcc f) (scalar n) xs ~?= histogramRef n g xs

    histogramAcc :: A.Num e => (Exp e -> Exp Int) -> Acc (Scalar Int) -> Acc (Vector e) -> Acc (Vector e)
    histogramAcc f n xs =
      let zeros = A.fill (index1 $ the n) 0
          ones  = A.fill (shape xs)       1
      in
      permute (+) zeros (\ix -> index1 $ f (xs A.! ix) `mod` the n) ones

    histogramRef :: forall e. (Elt e, P.Num e, IArray UArray e) => Int -> (e -> Int) -> Vector e -> Vector e
    histogramRef n f xs =
      let arr :: IArray.UArray Int e
          arr =  IArray.accumArray (+) 0 (0, n-1) [ (f e `mod` n, 1) | e <- toList xs ]
      in
      fromIArray arr

    -- Test for scattering functions
    --
    test_scatter :: forall e. (Elt e, Similar e, Arbitrary e) => e -> Property
    test_scatter _ =
      forAll (sized $ \n -> choose (0,n))               $ \k -> let m = 2*k in
      forAll (arbitraryArray (Z:.m+1))                  $ \defaultV ->
      forAll (arbitraryUniqueVectorOf (choose (0, m)))  $ \mapV -> let n = arraySize (arrayShape mapV) in
      forAll (arbitraryArray (Z:.n))                    $ \(inputV :: Vector e) ->
        toList (runN backend A.scatter mapV defaultV inputV)
        ~?=
        IArray.elems (scatterRef (toIArray Nothing mapV) (toIArray Nothing defaultV) (toIArray Nothing inputV))

    -- test_scatterIf :: forall e. (Elt e, Similar e, Arbitrary e) => e -> Property
    -- test_scatterIf _ =
    --   forAll (sized $ \n -> choose (0,n))               $ \k -> let m = 2*k in
    --   forAll (arbitraryArray (Z:.m+1))                  $ \defaultV ->
    --   forAll (arbitraryUniqueVectorOf (choose (0, m)))  $ \mapV -> let n = arraySize (arrayShape mapV) in
    --   forAll (arbitraryArray (Z:.n))                    $ \(maskV :: Vector Int) ->
    --   forAll (arbitraryArray (Z:.n))                    $ \(inputV :: Vector e) ->
    --     toList (runN backend (\p v d x -> A.scatterIf p v A.even d x) mapV maskV defaultV inputV)
    --     ~?=
    --     IArray.elems (scatterIfRef (toIArray Nothing mapV) (toIArray Nothing maskV) P.even (toIArray Nothing defaultV) (toIArray Nothing inputV))



-- Reference Implementation
-- ------------------------

scatterRef
    :: IArray.UArray Int Int
    -> IArray.Array Int e
    -> IArray.Array Int e
    -> IArray.Array Int e
scatterRef mapV defaultV inputV
  = runSTArray
  $ do mu <- M.thaw defaultV
       forM_ (IArray.assocs mapV) $ \(inIx, outIx) -> M.writeArray mu outIx (inputV IArray.! inIx)
       return mu

-- scatterIfRef
--     :: IArray.UArray Int Int
--     -> IArray.Array Int e
--     -> (e -> Bool)
--     -> IArray.Array Int t
--     -> IArray.Array Int t
--     -> IArray.Array Int t
-- scatterIfRef mapV maskV f defaultV inputV
--   = runSTArray
--   $ do mu <- M.thaw defaultV
--        forM_ (IArray.assocs mapV) $ \(inIx, outIx) ->
--          when (f (maskV IArray.! inIx)) $
--            M.writeArray mu outIx (inputV IArray.! inIx)
--        return mu

