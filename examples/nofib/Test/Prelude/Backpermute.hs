{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Test.Prelude.Backpermute (

  test_backpermute

) where

import Prelude                                                  as P
import Data.Label
import Data.Maybe
import Data.Typeable
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Config
import Test.Base
import QuickCheck.Arbitrary.Array

import Data.Array.Accelerate                                    as A
import Data.Array.Accelerate.Array.Sugar                        as A ( Array(..) )
import Data.Array.Accelerate.Examples.Internal                  as A


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
          -- , testProperty "gatherIf"     (test_gatherIf  :: Array DIM1 e -> Property)
          ]
      where
        test_reverse :: Vector e -> Property
        test_reverse xs   = run1 backend A.reverse xs   ~?= reverseRef xs

        test_transpose :: Array DIM2 e -> Property
        test_transpose xs = run1 backend A.transpose xs ~?= transposeRef xs

        -- Reverse a vector
        --
        reverseRef xs = fromList (arrayShape xs) (P.reverse $ toList xs)

        -- Transpose a 2D matrix
        --
        transposeRef xs =
          let swap (Z:.x:.y)    = Z :. y :. x
          in  fromFunction (swap (arrayShape xs)) (\ix -> indexArray xs (swap ix))

        -- Extracting sub-vectors
        --
        test_init :: Vector e -> Property
        test_init xs =
          P.not (isEmptyArray xs)
            ==> toList (run1 backend A.init xs) ~?= P.init (toList xs)

        test_tail :: Vector e -> Property
        test_tail xs =
          P.not (isEmptyArray xs)
            ==> toList (run1 backend A.tail xs) ~?= P.tail (toList xs)

        test_drop :: Vector e -> Property
        test_drop xs =
          let n = arraySize (arrayShape xs)
          in  forAll (choose (0, 0 `P.max` (n-1)))  $ \i ->
                toList (runN backend (\i' -> A.drop (the i')) (scalar i) xs)
                ~?=
                P.drop i (toList xs)

        test_take :: Vector e -> Property
        test_take xs@(Array _ adata) =
          let Z :. n = arrayShape xs
          in  forAll (choose (0, 0 `P.max` (n-1)))  $ \i ->
                runN backend (\i' -> A.take (the i')) (scalar i) xs
                ~?=
                Array ((),i) adata

        test_slit :: Vector e -> Property
        test_slit xs =
          let n = arraySize (arrayShape xs)
          in  forAll (choose (0, 0 `P.max` (n-1)))   $ \i ->
              forAll (choose (0, 0 `P.max` (n-1-i))) $ \j ->
                toList (runN backend (\i' j' -> A.slit (the i') (the j')) (scalar i) (scalar j) xs)
                ~?=
                P.take j (P.drop i (toList xs))

        -- Gathering
        --
        test_gather :: Vector e -> Property
        test_gather xs =
          let Z :. n  = arrayShape xs
          in  n P.> 0 ==>
                forAll arbitrary                               $ \(sh' :: DIM1) ->
                forAll (arbitraryArrayOf sh' (choose (0,n-1))) $ \mapv          ->
                  toList (runN backend A.gather mapv xs)
                  ~?=
                  [ xs `indexArray` (Z:.i) | i <- toList mapv ]

        -- test_gatherIf :: Vector e -> Property
        -- test_gatherIf xs =
        --   let n             = arraySize (arrayShape xs)
        --       n'            = 0 `P.max` (n-1)
        --   in
        --   forAll arbitrary                              $ \sh' ->
        --   forAll (arbitraryArrayOf sh' (choose (0,n'))) $ \mapv ->
        --   forAll (arbitraryArray sh')                   $ \(maskv :: Vector Int) ->
        --   forAll (arbitraryArray sh')                   $ \defaultv ->
        --     toList (runN backend (\p m d x -> A.gatherIf p m A.even d x) mapv maskv defaultv xs)
        --     ~?=
        --     gatherIfRef P.even mapv maskv defaultv xs


-- Reference Implementation
-- ------------------------

-- gatherIfRef :: (e -> Bool) -> Vector Int -> Vector e -> Vector t -> Vector t -> [t]
-- gatherIfRef g mapv maskv defaultv inputv
--   = let n           = arraySize (arrayShape defaultv)
--         select ix
--           | g (maskv `indexArray` ix) = inputv   `indexArray` (Z :. mapv `indexArray` ix)
--           | otherwise                 = defaultv `indexArray` ix
--     in
--     [ select ix | i <- [0 .. n-1], let ix = Z :. i ]

