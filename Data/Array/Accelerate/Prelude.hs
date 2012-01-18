{-# LANGUAGE TypeOperators, ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.Prelude
-- Copyright   : [2010..2011] Manuel M T Chakravarty, Ben Lever
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Standard functions that are not part of the core set (directly represented in the AST), but are
-- instead implemented in terms of the core set.
--

module Data.Array.Accelerate.Prelude (

  -- ** Map-like
  zip, zip3, zip4,
  unzip, unzip3, unzip4,
  
  -- ** Reductions
  foldAll, fold1All,

  -- ** Scans
  prescanl, postscanl, prescanr, postscanr,

  -- ** Segmented scans
  scanlSeg, scanlSeg', scanl1Seg, prescanlSeg, postscanlSeg,
  scanrSeg, scanrSeg', scanr1Seg, prescanrSeg, postscanrSeg,
  
  -- ** Reshaping of arrays
  flatten,

  -- ** Enumeration and filling
  fill, enumFromN, enumFromStepN,

  -- ** Gather and scatter
  gather, gatherIf, scatter, scatterIf,

  -- ** Subvector extraction
  init, tail, take, drop, slit

) where

-- avoid clashes with Prelude functions
import Prelude   hiding (replicate, zip, zip3, unzip, unzip3, map, zipWith,
                         scanl, scanl1, scanr, scanr1,
                         init, tail, take, drop, filter, max, min, not,
                         fst, snd, curry, uncurry,
                         fromIntegral)
import qualified Prelude

-- friends
import Data.Array.Accelerate.Array.Sugar hiding ((!), ignore, shape, size, index)
import Data.Array.Accelerate.Language
import Data.Array.Accelerate.Type


-- Map-like composites
-- -------------------

-- |Combine the elements of two arrays pairwise.  The shape of the result is
-- the intersection of the two argument shapes.
--
zip :: (Shape sh, Elt a, Elt b)
    => Acc (Array sh a)
    -> Acc (Array sh b)
    -> Acc (Array sh (a, b))
zip = zipWith (curry lift)

-- |Take three arrays and and return an array of triples, analogous to zip.
--
zip3 :: forall sh. forall a. forall b. forall c. (Shape sh, Elt a, Elt b, Elt c)
     => Acc (Array sh a)
     -> Acc (Array sh b)
     -> Acc (Array sh c)
     -> Acc (Array sh (a, b, c))
zip3 as bs cs
  = zipWith (\a bc -> let (b, c) = unlift bc :: (Exp b, Exp c) in lift (a, b, c)) as
  $ zip bs cs

-- |Take three arrays and and return an array of quadruples, analogous to zip.
--
zip4 :: forall sh. forall a. forall b. forall c. forall d. (Shape sh, Elt a, Elt b, Elt c, Elt d)
     => Acc (Array sh a)
     -> Acc (Array sh b)
     -> Acc (Array sh c)
     -> Acc (Array sh d)
     -> Acc (Array sh (a, b, c, d))
zip4 as bs cs ds
  = zipWith (\a bcd -> let (b, c, d) = unlift bcd :: (Exp b, Exp c, Exp d) in lift (a, b, c, d)) as
  $ zip3 bs cs ds

-- |The converse of 'zip', but the shape of the two results is identical to the
-- shape of the argument.
--
unzip :: (Shape sh, Elt a, Elt b)
      => Acc (Array sh (a, b))
      -> (Acc (Array sh a), Acc (Array sh b))
unzip arr = (map fst arr, map snd arr)

-- |Take an array of triples and return three arrays, analogous to unzip.
--
unzip3 :: forall sh. forall a. forall b. forall c. (Shape sh, Elt a, Elt b, Elt c)
       => Acc (Array sh (a, b, c))
       -> (Acc (Array sh a), Acc (Array sh b), Acc (Array sh c))
unzip3 abcs = (as, bs, cs)
  where
    (bs, cs)  = unzip bcs
    (as, bcs) = unzip
              $ map (\abc -> let (a, b, c) = unlift abc :: (Exp a, Exp b, Exp c) in lift (a, lift (b, c))) abcs

-- |Take an array of quadruples and return four arrays, analogous to unzip.
--
unzip4 :: forall sh. forall a. forall b. forall c. forall d. (Shape sh, Elt a, Elt b, Elt c, Elt d)
       => Acc (Array sh (a, b, c, d))
       -> (Acc (Array sh a), Acc (Array sh b), Acc (Array sh c), Acc (Array sh d))
unzip4 abcds = (as, bs, cs, ds)
  where
    (as, bs)   = unzip abs
    (cs, ds)   = unzip cds
    (abs, cds) = unzip
               $ map (\abcd -> let (a, b, c, d) = unlift abcd :: (Exp a, Exp b, Exp c, Exp d)
                               in lift (lift (a, b), lift (c, d)))
                     abcds


-- Reductions
-- ----------

-- |Reduction of an array of arbitrary rank to a single scalar value.  The first argument needs to be
-- an /associative/ function to enable an efficient parallel implementation.
--
foldAll :: (Shape sh, Elt a)
        => (Exp a -> Exp a -> Exp a)
        -> Exp a
        -> Acc (Array sh a)
        -> Acc (Scalar a)
foldAll f e arr = fold f e (reshape (index1 $ size arr) arr)

-- |Variant of 'foldAll' that requires the reduced array to be non-empty and doesn't need an default
-- value.
--
fold1All :: (Shape sh, Elt a)
         => (Exp a -> Exp a -> Exp a)
         -> Acc (Array sh a)
         -> Acc (Scalar a)
fold1All f arr = fold1 f (reshape (index1 $ size arr) arr)


-- Composite scans
-- ---------------

-- |Left-to-right prescan (aka exclusive scan).  As for 'scan', the first argument must be an
-- /associative/ function.  Denotationally, we have
--
-- > prescanl f e = Prelude.fst . scanl' f e
--
prescanl :: Elt a
         => (Exp a -> Exp a -> Exp a)
         -> Exp a
         -> Acc (Vector a)
         -> Acc (Vector a)
prescanl f e = Prelude.fst . scanl' f e

-- |Left-to-right postscan, a variant of 'scanl1' with an initial value.  Denotationally, we have
--
-- > postscanl f e = map (e `f`) . scanl1 f
--
postscanl :: Elt a
          => (Exp a -> Exp a -> Exp a)
          -> Exp a
          -> Acc (Vector a)
          -> Acc (Vector a)
postscanl f e = map (e `f`) . scanl1 f

-- |Right-to-left prescan (aka exclusive scan).  As for 'scan', the first argument must be an
-- /associative/ function.  Denotationally, we have
--
-- > prescanr f e = Prelude.fst . scanr' f e
--
prescanr :: Elt a
         => (Exp a -> Exp a -> Exp a)
         -> Exp a
         -> Acc (Vector a)
         -> Acc (Vector a)
prescanr f e = Prelude.fst . scanr' f e

-- |Right-to-left postscan, a variant of 'scanr1' with an initial value.  Denotationally, we have
--
-- > postscanr f e = map (e `f`) . scanr1 f
--
postscanr :: Elt a
          => (Exp a -> Exp a -> Exp a)
          -> Exp a
          -> Acc (Vector a)
          -> Acc (Vector a)
postscanr f e = map (`f` e) . scanr1 f


-- Segmented scans
-- ---------------

-- |Segmented version of 'scanl'.
--
scanlSeg :: forall a i. (Elt a, Elt i, IsIntegral i)
         => (Exp a -> Exp a -> Exp a)
         -> Exp a
         -> Acc (Vector a)
         -> Acc (Segments i)
         -> Acc (Vector a)
scanlSeg f e arr seg = scans
  where
    -- Segmented scan implemented by performing segmented exclusive-scan (scan1)
    -- on a vector formed by injecting the identity element at the start of each
    -- segment.
    scans      = scanl1Seg f idInjArr seg'
    idInjArr   = zipWith (\h x -> h ==* 1 ? (fst x, snd x)) headFlags $ zip idsArr arrShifted

    headFlags  = permute (+) zerosArr' (\ix -> index1' $ segOffsets' ! ix)
               $ generate (shape seg) (const (1 :: Exp i))

    arrShifted = backpermute nSh (\ix -> index1' $ shiftCoords ! ix) arr

    idsArr     = generate nSh (const e)

    -- As the identity elements are injected in to the vector for each segment, the
    -- remaining elements must be shifted forwarded (to the left). shiftCoords specifies
    -- how each element is backpermuted to its shifted position.
    shiftCoords = permute (+) zerosArr' (ilift1 $ \i -> i + (offsetArr ! index1' i) + 1) coords
    coords      = Prelude.fst $ scanl' (+) 0 onesArr

    offsetArr   = scanl1 max $ permute (+) zerosArr (\ix -> index1' $ segOffsets ! ix) segIxs
    segIxs      = Prelude.fst $ scanl' (+) 0 $ generate (index1' $ size seg) (const 1)

    segOffsets' = Prelude.fst $ scanl' (+) 0 seg'
    segOffsets  = Prelude.fst $ scanl' (+) 0 seg

    --
    nSh       = index1' $ size arr + size seg
    seg'      = map (+ 1) seg
    onesArr   = generate (shape arr) (const 1)
    zerosArr  = generate (shape arr) (const 0)
    zerosArr' = generate nSh (const 0)

-- |Segmented version of 'scanl''.
--
-- The first element of the resulting tuple is a vector of scanned values. The
-- second element is a vector of segment scan totals and has the same size as
-- the segment vector.
--
scanlSeg' :: forall a i. (Elt a, Elt i, IsIntegral i)
          => (Exp a -> Exp a -> Exp a)
          -> Exp a
          -> Acc (Vector a)
          -> Acc (Segments i)
          -> (Acc (Vector a), Acc (Vector a))
scanlSeg' f e arr seg = (scans, sums)
  where
    -- Segmented scan' implemented by performing segmented exclusive-scan on vector
    -- fromed by inserting identity element in at the start of each segment, shifting
    -- elements right, with the final element in the segment being removed.
    scans      = scanl1Seg f idInjArr seg
    idInjArr   = zipWith (\h x -> h ==* 1 ? (fst x, snd x)) headFlags $ zip idsArr arrShifted

    headFlags  = permute (+) zerosArr (\ix -> index1' $ segOffsets ! ix)
               $ generate (shape seg) (const (1 :: Exp i))
    segOffsets = Prelude.fst $ scanl' (+) 0 seg

    arrShifted = backpermute (shape arr) (ilift1 $ \i -> i ==* 0 ? (i, i - 1)) arr

    idsArr     = generate (shape arr) (const e)
    zerosArr   = generate (shape arr) (const 0)

    -- Sum of each segment is computed by performing a segmented postscan on
    -- the original vector and taking the tail elements.
    sums       = map (`f` e)
               $ backpermute (shape seg) (\ix -> index1' $ sumOffsets ! ix)
               $ scanl1Seg f arr seg
    sumOffsets = map (subtract 1) $ scanl1 (+) seg

-- |Segmented version of 'scanl1'.
--
scanl1Seg :: (Elt a, Elt i, IsIntegral i)
          => (Exp a -> Exp a -> Exp a)
          -> Acc (Vector a)
          -> Acc (Segments i)
          -> Acc (Vector a)
scanl1Seg f arr seg = map snd $ scanl1 (mkSegApply f) $ zip (mkHeadFlags seg) arr

-- |Segmented version of 'prescanl'.
--
prescanlSeg :: (Elt a, Elt i, IsIntegral i)
            => (Exp a -> Exp a -> Exp a)
            -> Exp a
            -> Acc (Vector a)
            -> Acc (Segments i)
            -> Acc (Vector a)
prescanlSeg f e arr seg = Prelude.fst $ scanlSeg' f e arr seg

-- |Segmented version of 'postscanl'.
--
postscanlSeg :: (Elt a, Elt i, IsIntegral i)
             => (Exp a -> Exp a -> Exp a)
             -> Exp a
             -> Acc (Vector a)
             -> Acc (Segments i)
             -> Acc (Vector a)
postscanlSeg f e arr seg = map (e `f`) $ scanl1Seg f arr seg

-- |Segmented version of 'scanr'.
--
scanrSeg :: forall a i. (Elt a, Elt i, IsIntegral i)
         => (Exp a -> Exp a -> Exp a)
         -> Exp a
         -> Acc (Vector a)
         -> Acc (Segments i)
         -> Acc (Vector a)
scanrSeg f e arr seg = scans
  where
    -- Using technique described for scanlSeg.
    scans      = scanr1Seg f idInjArr seg'
    idInjArr   = zipWith (\h x -> h ==* 1 ? (fst x, snd x)) tailFlags $ zip idsArr arrShifted

    tailFlags  = permute (+) zerosArr' (\ix -> index1' $ (segOffsets' ! ix) - 1)
               $ generate (shape seg) (const (1 :: Exp i))

    arrShifted = backpermute nSh (\ix -> index1' $ shiftCoords ! ix) arr

    idsArr     = generate nSh (const e)

    --
    shiftCoords = permute (+) zerosArr' (ilift1 $ \i -> i + (offsetArr ! index1' i)) coords
    coords      = Prelude.fst $ scanl' (+) 0 onesArr

    offsetArr   = scanl1 max $ permute (+) zerosArr (\ix -> index1' $ segOffsets ! ix) segIxs
    segIxs      = Prelude.fst $ scanl' (+) 0 $ generate (shape seg) (const 1)

    segOffsets' = scanl1 (+) seg'
    segOffsets  = Prelude.fst $ scanl' (+) 0 seg

    --
    nSh       = index1' $ size arr + size seg
    seg'      = map (+ 1) seg
    onesArr   = generate (shape arr) (const 1)
    zerosArr  = generate (shape arr) (const 0)
    zerosArr' = generate nSh (const 0)

-- |Segmented version of 'scanrSeg''.
--
scanrSeg' :: forall a i. (Elt a, Elt i, IsIntegral i)
          => (Exp a -> Exp a -> Exp a)
          -> Exp a
          -> Acc (Vector a)
          -> Acc (Segments i)
          -> (Acc (Vector a), Acc (Vector a))
scanrSeg' f e arr seg = (scans, sums)
  where
    -- Using technique described for scanlSeg'.
    scans      = scanr1Seg f idInjArr seg
    idInjArr   = zipWith (\t x -> t ==* 1 ? (fst x, snd x)) tailFlags $ zip idsArr arrShifted

    tailFlags  = permute (+) zerosArr (\ix -> index1' $ (segOffsets ! ix) - 1)
               $ generate (shape seg) (const (1 :: Exp i))
    segOffsets = scanl1 (+) seg

    arrShifted = backpermute (shape arr) (ilift1 $ \i -> i ==* (size arr - 1) ? (i, i + 1)) arr

    idsArr     = generate (shape arr) (const e)
    zerosArr   = generate (shape arr) (const 0)

    --
    sums       = map (`f` e) $  backpermute (shape seg) (\ix -> index1' $ sumOffsets ! ix)
               $ scanr1Seg f arr seg
    sumOffsets = Prelude.fst $ scanl' (+) 0 seg

-- |Segmented version of 'scanr1'.
--
scanr1Seg :: (Elt a, Elt i, IsIntegral i)
          => (Exp a -> Exp a -> Exp a)
          -> Acc (Vector a)
          -> Acc (Segments i)
          -> Acc (Vector a)
scanr1Seg f arr seg = map snd $ scanr1 (mkSegApply f) $ zip (mkTailFlags seg) arr

-- |Segmented version of 'prescanr'.
--
prescanrSeg :: (Elt a, Elt i, IsIntegral i)
            => (Exp a -> Exp a -> Exp a)
            -> Exp a
            -> Acc (Vector a)
            -> Acc (Segments i)
            -> Acc (Vector a)
prescanrSeg f e arr seg = Prelude.fst $ scanrSeg' f e arr seg

-- |Segmented version of 'postscanr'.
--
postscanrSeg :: (Elt a, Elt i, IsIntegral i)
             => (Exp a -> Exp a -> Exp a)
             -> Exp a
             -> Acc (Vector a)
             -> Acc (Segments i)
             -> Acc (Vector a)
postscanrSeg f e arr seg = map (`f` e) $ scanr1Seg f arr seg


-- Segmented scan helpers
-- ----------------------

-- |Compute head flags vector from segment vector for left-scans.
--
mkHeadFlags :: (Elt i, IsIntegral i) => Acc (Segments i) -> Acc (Segments i)
mkHeadFlags seg = permute (\_ _ -> 1) zerosArr (\ix -> index1' (segOffsets ! ix)) segOffsets
  where
    (segOffsets, len) = scanl' (+) 0 seg
    zerosArr          = generate (index1' $ the len) (const 0)

-- |Compute tail flags vector from segment vector for right-scans.
--
mkTailFlags :: (Elt i, IsIntegral i) => Acc (Segments i) -> Acc (Segments i)
mkTailFlags seg
  = permute (\_ _ -> 1) zerosArr (ilift1 $ \i -> (fromIntegral $ segOffsets ! index1' i) - 1) segOffsets
  where
    segOffsets = scanl1 (+) seg
    len        = segOffsets ! index1' (size seg - 1)
    zerosArr   = generate (index1' len) (const 0)

-- |Construct a segmented version of apply from a non-segmented version. The segmented apply
-- operates on a head-flag value tuple.
--
mkSegApply :: (Elt e, Elt i, IsIntegral i)
           => (Exp e -> Exp e -> Exp e)
           -> (Exp (i, e) -> Exp (i, e) -> Exp (i, e))
mkSegApply op = apply
  where
    apply a b = lift (fromIntegral $ boolToInt (aF ==* 1 ||* bF ==* 1), bF ==* 1 ? (bV, aV `op` bV))
      where
        aF = fst a
        aV = snd a
        bF = fst b
        bV = snd b

-- As 'index1', but parameterised in the first argument over integral types
--
index1' :: (Elt i, IsIntegral i) => Exp i -> Exp (Z :. Int)
index1' = index1 . fromIntegral


-- Reshaping of arrays
-- -------------------

-- | Flattens a given array of arbitrary dimension.
--
flatten :: (Shape ix, Elt a) => Acc (Array ix a) -> Acc (Array DIM1 a)
flatten a = reshape (index1 $ size a) a

-- Enumeration and filling
-- -----------------------

-- | Create an array where all elements are the same value.
--
fill :: (Shape sh, Elt e) => Exp sh -> Exp e -> Acc (Array sh e)
fill sh c = generate sh (const c)

-- | Create an array of the given shape containing the values x, x+1, etc (in
--   row-major order).
--
enumFromN :: (Shape sh, Elt e, IsNum e) => Exp sh -> Exp e -> Acc (Array sh e)
enumFromN sh x = enumFromStepN sh x 1

-- | Create an array of the given shape containing the values x, x+y, x+y+y, etc
--   (in row-major order).
--
enumFromStepN :: (Shape sh, Elt e, IsNum e)
              => Exp sh
              -> Exp e    -- ^x
              -> Exp e    -- ^y
              -> Acc (Array sh e)
enumFromStepN sh x y = reshape sh
                     $ generate (index1 $ shapeSize sh)
                                ((\i -> ((fromIntegral i) * y) + x) . unindex1)


-- Gather operations
-- -----------------

-- | Copy elements from source array to destination array according to a map. This
--   is a backpermute operation where a 'map' vector encodes the ouput to input
--   index mapping. For example:
--
--    input  = [1, 9, 6, 4, 4, 2, 0, 1, 2]
--    map    = [1, 3, 7, 2, 5, 3]
--
--    output = [9, 4, 1, 6, 2, 4]
--
gather :: (Elt e)
       => Acc (Vector Int)      -- ^map
       -> Acc (Vector e)        -- ^input
       -> Acc (Vector e)        -- ^output
gather mapV inputV = backpermute (shape mapV) bpF inputV
  where
    bpF ix = lift (Z :. (mapV ! ix))


-- | Conditionally copy elements from source array to destination array according
--   to a map. This is a backpermute opereation where a 'map' vector encdes the
--   output to input index mapping. In addition, there is a 'mask' vector, and an
--   associated predication function, that specifies whether an element will be
--   copied. If not copied, the output array assumes the default vector's value.
--   For example:
--
--    default = [6, 6, 6, 6, 6, 6]
--    map     = [1, 3, 7, 2, 5, 3]
--    mask    = [3, 4, 9, 2, 7, 5]
--    pred    = (> 4)
--    input   = [1, 9, 6, 4, 4, 2, 0, 1, 2]
--
--    output  = [6, 6, 1, 6, 2, 4]
--
gatherIf :: (Elt e, Elt e')
         => Acc (Vector Int)    -- ^map
         -> Acc (Vector e)      -- ^mask
         -> (Exp e -> Exp Bool) -- ^predicate
         -> Acc (Vector e')     -- ^default
         -> Acc (Vector e')     -- ^input
         -> Acc (Vector e')     -- ^output
gatherIf mapV maskV pred defaultV inputV = zipWith zwF predV gatheredV
  where
    zwF p g   = p ? (unlift g)
    gatheredV = zip (gather mapV inputV) defaultV
    predV     = map pred maskV


-- Scatter operations
-- ------------------

-- | Copy elements from source array to destination array according to a map. This
--   is a forward-permute operation where a 'map' vector encodes an input to output
--   index mapping. Output elements for indicies that are not mapped asume the default
--   vector's value.  For example:
--
--    default = [0, 0, 0, 0, 0, 0, 0, 0, 0]
--    map     = [1, 3, 7, 2, 5, 8]
--    input   = [1, 9, 6, 4, 4, 2, 5]
--
--    output  = [0, 1, 4, 9, 0, 4, 0, 6, 2]
--
--   Note if the same index appears in the map more than once, the result is
--   undefined. The map vector cannot be larger than the input vector.
--
scatter :: (Elt e)
        => Acc (Vector Int)      -- ^map
        -> Acc (Vector e)        -- ^default
        -> Acc (Vector e)        -- ^input
        -> Acc (Vector e)        -- ^output
scatter mapV defaultV inputV = permute (const) defaultV pF inputV
  where
    pF ix = lift (Z :. (mapV ! ix))


-- | Conditionally copy elements from source array to destination array according
--   to a map. This is a forward-permute operation where a 'map' vector encodes an
--   input to output index mapping. In addition, there is a 'mask' vector, and an
--   associated predicate function, that specifies whether an elements will be
--   copied. If not copied, the ouput array assumes the default vector's value. For
--   example:
--
--    default = [0, 0, 0, 0, 0, 0, 0, 0, 0]
--    map     = [1, 3, 7, 2, 5, 8]
--    mask    = [3, 4, 9, 2, 7, 5]
--    pred    = (> 4)
--    input   = [1, 9, 6, 4, 4, 2]
--
--    output  = [0, 0, 0, 0, 0, 4, 0, 6, 2]
--
--   Note if the same index appears in the map more than once, the result is
--   undefined. The map and input vector must be of the same length.
--
scatterIf :: (Elt e, Elt e')
          => Acc (Vector Int)      -- ^map
          -> Acc (Vector e)        -- ^mask
          -> (Exp e -> Exp Bool)   -- ^predicate
          -> Acc (Vector e')       -- ^default
          -> Acc (Vector e')       -- ^input
          -> Acc (Vector e')       -- ^output
scatterIf mapV maskV pred defaultV inputV = permute const defaultV pF inputV
  where
    pF ix = (pred (maskV ! ix)) ? (lift (Z :. (mapV ! ix)), ignore)



-- Extracting subvectors
-- ---------------------


-- | Yield the first 'n' elements of the input vector. The vector must contain
--   no more than 'n' elements.
--
take :: Elt e => Exp Int -> Acc (Vector e) -> Acc (Vector e)
take n = backpermute (index1 n) id

-- | Yield all but the first 'n' elements of the input vector. The vector must
--   contain no more than 'n' elements.
--
drop :: Elt e => Exp Int -> Acc (Vector e) -> Acc (Vector e)
drop n arr = backpermute (ilift1 (\x -> x - n) $ shape arr) (ilift1 (+ n)) arr


-- | Yield all but the last element of the input vector. The vector may not
--   be empty.
--
init :: Elt e => Acc (Vector e) -> Acc (Vector e)
init arr = take ((unindex1 $ shape arr) - 1) arr


-- | Yield all but the first element of the input vector. The vector may not
--   be empty.
tail :: Elt e => Acc (Vector e) -> Acc (Vector e)
tail = drop 1


-- | Yield a slit (slice) from the vector. The vector must contain at least
--   i + n elements.
--
slit :: Elt e
      => Exp Int
      -> Exp Int
      -> Acc (Vector e)
      -> Acc (Vector e)
slit i n = backpermute (index1 n) (ilift1 (+ i))

