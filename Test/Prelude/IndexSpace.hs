{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Test.Prelude.IndexSpace (

  test_permute,
  test_backpermute

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
import Data.Array.Accelerate.Array.Sugar                        ( newArray, dim )
import Data.Array.Accelerate.Examples.Internal                  as A

import Data.Array.ST                                            ( runSTArray )
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
    testIntegralElt :: forall e. (Elt e, Integral e, IsIntegral e, Arbitrary e, Similar e) => (Config :-> Bool) -> e -> Maybe Test
    testIntegralElt ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testGroup (show (typeOf (undefined :: e)))
          [
            test_fill (undefined :: e)
          , testProperty "scatter"   (test_scatter :: e -> Property)
          , testProperty "scatterIf" (test_scatterIf :: e -> Property)
          , testProperty "histogram" (test_histogram A.fromIntegral P.fromIntegral :: Vector e -> Property)
          ]

    testFloatingElt :: forall e. (Elt e, RealFrac e, IsFloating e, Arbitrary e, Similar e) => (Config :-> Bool) -> e -> Maybe Test
    testFloatingElt ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testGroup (show (typeOf (undefined :: e)))
          [
            test_fill (undefined :: e)
          , testProperty "scatter"   (test_scatter :: e -> Property)
          , testProperty "scatterIf" (test_scatterIf :: e -> Property)
          , testProperty "histogram" (test_histogram A.floor P.floor :: Vector e -> Property)
          ]

    -- Test is permutation works by just copying elements directly from one
    -- array to the other. Does not attempt to use elements from the defaults
    -- array. Additionally, works for any dimension. (c.f. Issue #93)
    --
    test_fill :: forall e. (Elt e, IsNum e, Arbitrary e, Similar e) => e -> Test
    test_fill _ = testGroup "fill"
      [ -- testDim dim0         -- Accelerate issue #87
        testDim dim1
      , testDim dim2
      ]
      where
        testDim :: forall sh. (Shape sh, Eq sh, Arbitrary sh, Arbitrary (Array sh e)) => sh -> Test
        testDim sh = testProperty ("DIM" P.++ show (dim sh)) (push_fill :: Array sh e -> Property)

        push_fill xs =
          let xs'   = use xs
              zeros = A.fill (A.shape xs') (constant 0)
          in
          run backend (permute const zeros id xs') ~?= xs

    -- Test if the combining operation for forward permutation works, by
    -- building a histogram. Often tricky for parallel backends.
    --
    test_histogram f g xs =
      forAll (sized return) $
        \n -> run backend (histogramAcc n f xs) ~?= histogramRef n g xs

    histogramAcc n f xs =
      let n'        = unit (constant n)
          xs'       = use xs
          zeros     = A.generate (constant (Z :. n)) (const 0)
          ones      = A.generate (shape xs')         (const 1)
      in
      permute (+) zeros (\ix -> index1 $ f (xs' A.! ix) `mod` the n') ones

    histogramRef n f xs =
      let arr :: IArray.UArray Int Int32
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
        toList (run backend $ A.scatter (use mapV) (use defaultV) (use inputV))
        ~?=
        IArray.elems (scatterRef (toIArray mapV) (toIArray defaultV) (toIArray inputV))

    test_scatterIf :: forall e. (Elt e, Similar e, Arbitrary e) => e -> Property
    test_scatterIf _ =
      forAll (sized $ \n -> choose (0,n))               $ \k -> let m = 2*k in
      forAll (arbitraryArray (Z:.m+1))                  $ \defaultV ->
      forAll (arbitraryUniqueVectorOf (choose (0, m)))  $ \mapV -> let n = arraySize (arrayShape mapV) in
      forAll (arbitraryArray (Z:.n))                    $ \(maskV :: Vector Int) ->
      forAll (arbitraryArray (Z:.n))                    $ \(inputV :: Vector e) ->
        toList (run backend $ A.scatterIf (use mapV) (use maskV) A.even (use defaultV) (use inputV))
        ~?=
        IArray.elems (scatterIfRef (toIArray mapV) (toIArray maskV) P.even (toIArray defaultV) (toIArray inputV))


--
-- Backward permutation --------------------------------------------------------
--

test_backpermute :: Backend -> Config -> Test
test_backpermute backend opt = testGroup "backpermute" $ catMaybes
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
    testElt :: forall e. (Elt e, Similar e, Arbitrary e) => (Config :-> Bool) -> e -> Maybe Test
    testElt ok _
      | P.not (get ok opt)  = Nothing
      | otherwise           = Just $ testGroup (show (typeOf (undefined::e)))
          [ testProperty "reverse"      (test_reverse   :: Array DIM1 e -> Property)
          , testProperty "transpose"    (test_transpose :: Array DIM2 e -> Property)
          , testProperty "init"         (test_init      :: Array DIM1 e -> Property)
          , testProperty "tail"         (test_tail      :: Array DIM1 e -> Property)
          , testProperty "take"         (test_take      :: Array DIM1 e -> Property)
          , testProperty "drop"         (test_drop      :: Array DIM1 e -> Property)
          , testProperty "slit"         (test_slit      :: Array DIM1 e -> Property)
          , testProperty "gather"       (test_gather    :: Array DIM1 e -> Property)
          , testProperty "gatherIf"     (test_gatherIf  :: Array DIM1 e -> Property)
          ]

    test_reverse xs   = run backend (reverseAcc xs)   ~?= reverseRef xs
    test_transpose xs = run backend (transposeAcc xs) ~?= transposeRef xs

    -- Reverse a vector
    --
    reverseAcc xs = A.reverse (use xs)
    reverseRef xs = fromList (arrayShape xs) (P.reverse $ toList xs)

    -- Transpose a 2D matrix
    --
    transposeAcc xs = A.transpose (use xs)
    transposeRef xs =
      let swap (Z:.x:.y)    = Z :. y :. x
      in  newArray (swap (arrayShape xs)) (\ix -> indexArray xs (swap ix))

    -- Extracting sub-vectors
    --
    test_init xs =
      P.not (isEmptyArray xs)
        ==> toList (run backend (A.init (A.use xs))) ~?= P.init (toList xs)

    test_tail xs =
      P.not (isEmptyArray xs)
        ==> toList (run backend (A.tail (A.use xs))) ~?= P.tail (toList xs)

    test_drop xs =
      let n = arraySize (arrayShape xs)
      in  forAll (choose (0, 0 `P.max` (n-1)))  $ \i ->
            toList (run backend (A.drop (constant i) (use xs))) ~?= P.drop i (toList xs)

    test_take xs =
      let n = arraySize (arrayShape xs)
      in  forAll (choose (0, 0 `P.max` (n-1)))  $ \i ->
            toList (run backend (A.take (constant i) (use xs))) ~?= P.take i (toList xs)

    test_slit xs =
      let n = arraySize (arrayShape xs)
      in  forAll (choose (0, 0 `P.max` (n-1)))   $ \i ->
          forAll (choose (0, 0 `P.max` (n-1-i))) $ \j ->
            toList (run backend (A.slit (constant i) (constant j) (use xs))) ~?= P.take j (P.drop i (toList xs))

    -- Gathering
    --
    test_gather xs =
      let n     = arraySize (arrayShape xs)
          n'    = 0 `P.max` (n-1)
      in
      forAll arbitrary                              $ \sh' ->
      forAll (arbitraryArrayOf sh' (choose (0,n'))) $ \mapv ->
        toList (run backend (A.gather (use mapv) (use xs)))
        ~?=
        [ xs `indexArray` (Z:.i) | i <- toList mapv ]

    test_gatherIf xs =
      let n             = arraySize (arrayShape xs)
          n'            = 0 `P.max` (n-1)
      in
      forAll arbitrary                              $ \sh' ->
      forAll (arbitraryArrayOf sh' (choose (0,n'))) $ \mapv ->
      forAll (arbitraryArray sh')                   $ \(maskv :: Vector Int) ->
      forAll (arbitraryArray sh')                   $ \defaultv ->
        toList (run backend $ A.gatherIf (use mapv) (use maskv) A.even (use defaultv) (use xs))
        ~?=
        gatherIfRef P.even mapv maskv defaultv xs


-- Reference Implementation
-- ------------------------

gatherIfRef :: (e -> Bool) -> Vector Int -> Vector e -> Vector t -> Vector t -> [t]
gatherIfRef g mapv maskv defaultv inputv
  = let n           = arraySize (arrayShape defaultv)
        select ix
          | g (maskv `indexArray` ix) = inputv   `indexArray` (Z :. mapv `indexArray` ix)
          | otherwise                 = defaultv `indexArray` ix
    in
    [ select ix | i <- [0 .. n-1], let ix = Z :. i ]


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


scatterIfRef
    :: IArray.UArray Int Int
    -> IArray.Array Int e
    -> (e -> Bool)
    -> IArray.Array Int t
    -> IArray.Array Int t
    -> IArray.Array Int t
scatterIfRef mapV maskV f defaultV inputV
  = runSTArray
  $ do mu <- M.thaw defaultV
       forM_ (IArray.assocs mapV) $ \(inIx, outIx) ->
         when (f (maskV IArray.! inIx)) $
           M.writeArray mu outIx (inputV IArray.! inIx)
       return mu

