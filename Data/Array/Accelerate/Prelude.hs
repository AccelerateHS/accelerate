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
  zip, unzip,
  
  -- ** Reductions
  foldAll, fold1All,
  
  -- ** Scans
  prescanl, postscanl, prescanr, postscanr, 

  -- ** Segmented scans
  scanlSeg, scanlSeg', scanl1Seg, prescanlSeg, postscanlSeg, 
  scanrSeg, scanrSeg', scanr1Seg, prescanrSeg, postscanrSeg
  
) where

-- avoid clashes with Prelude functions
import Prelude   hiding (replicate, zip, unzip, map, scanl, scanl1, scanr, scanr1, zipWith,
                         filter, max, min, not, fst, snd, curry, uncurry)
import qualified Prelude

-- friends  
import Data.Array.Accelerate.Array.Sugar hiding ((!), ignore, shape, size, index)
import Data.Array.Accelerate.Language


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

-- |The converse of 'zip', but the shape of the two results is identical to the
-- shape of the argument.
-- 
unzip :: (Shape sh, Elt a, Elt b)
      => Acc (Array sh (a, b))
      -> (Acc (Array sh a), Acc (Array sh b))
unzip arr = (map fst arr, map snd arr)


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
scanlSeg :: Elt a
         => (Exp a -> Exp a -> Exp a)
         -> Exp a
         -> Acc (Vector a)
         -> Acc Segments
         -> Acc (Vector a)
scanlSeg f e arr seg = scans
  where
    -- Segmented scan implemented by performing segmented exclusive-scan (scan1)
    -- on a vector formed by injecting the identity element at the start of each
    -- segment.
    scans      = scanl1Seg f idInjArr seg'
    idInjArr   = zipWith (\h x -> h ==* 1 ? (fst x, snd x)) headFlags $ zip idsArr arrShifted

    headFlags  = permute (+) zerosArr' (\ix -> index1 $ segOffsets' ! ix)
               $ generate (shape seg) (const 1)

    arrShifted = backpermute nSh (\ix -> index1 $ shiftCoords ! ix) arr

    idsArr     = generate nSh (const e)

    -- As the identity elements are injected in to the vector for each segment, the
    -- remaining elements must be shifted forwarded (to the left). shiftCoords specifies
    -- how each element is backpermuted to its shifted position.
    shiftCoords = permute (+) zerosArr' (ilift1 $ \i -> i + (offsetArr ! index1 i) + 1) coords
    coords      = Prelude.fst $ scanl' (+) 0 onesArr

    offsetArr   = scanl1 max $ permute (+) zerosArr (\ix -> index1 $ segOffsets ! ix) segIxs
    segIxs      = Prelude.fst $ scanl' (+) 0 $ generate (index1 $ size seg) (const 1)

    segOffsets' = Prelude.fst $ scanl' (+) 0 seg'
    segOffsets  = Prelude.fst $ scanl' (+) 0 seg

    --
    nSh       = index1 $ size arr + size seg
    seg'      = map (+ 1) seg
    onesArr   = generate (shape arr) (const 1)
    zerosArr  = generate (shape arr) (const 0)
    zerosArr' = generate nSh (const 0)

-- |Segmented version of 'scanl\''.
--
-- The first element of the resulting tuple is a vector of scanned values. The
-- second element is a vector of segment scan totals and has the same size as
-- the segment vector.
--
scanlSeg' :: Elt a
          => (Exp a -> Exp a -> Exp a)
          -> Exp a
          -> Acc (Vector a)
          -> Acc Segments
          -> (Acc (Vector a), Acc (Vector a))
scanlSeg' f e arr seg = (scans, sums)
  where
    -- Segmented scan' implemented by performing segmented exclusive-scan on vector
    -- fromed by inserting identity element in at the start of each segment, shifting
    -- elements right, with the final element in the segment being removed.
    scans      = scanl1Seg f idInjArr seg
    idInjArr   = zipWith (\h x -> h ==* 1 ? (fst x, snd x)) headFlags $ zip idsArr arrShifted

    headFlags  = permute (+) zerosArr (\ix -> index1 $ segOffsets ! ix)
               $ generate (shape seg) (const (1 :: Exp Int))
    segOffsets = Prelude.fst $ scanl' (+) 0 seg

    arrShifted = backpermute (shape arr) (ilift1 $ \i -> i ==* 0 ? (i, i - 1)) arr

    idsArr     = generate (shape arr) (const e)
    zerosArr   = generate (shape arr) (const 0)

    -- Sum of each segment is computed by performing a segmented postscan on
    -- the original vector and taking the tail elements.
    sums       = map (`f` e)
               $ backpermute (shape seg) (\ix -> index1 $ sumOffsets ! ix)
               $ scanl1Seg f arr seg
    sumOffsets = map (subtract 1) $ scanl1 (+) seg

-- |Segmented version of 'scanl1'.
--
scanl1Seg :: Elt a
          => (Exp a -> Exp a -> Exp a)
          -> Acc (Vector a)
          -> Acc Segments
          -> Acc (Vector a)
scanl1Seg f arr seg = map snd $ scanl1 (mkSegApply f) $ zip (mkHeadFlags seg) arr

-- |Segmented version of 'prescanl'.
--
prescanlSeg :: Elt a
            => (Exp a -> Exp a -> Exp a)
            -> Exp a
            -> Acc (Vector a)
            -> Acc Segments
            -> Acc (Vector a)
prescanlSeg f e arr seg = Prelude.fst $ scanlSeg' f e arr seg

-- |Segmented version of 'postscanl'.
--
postscanlSeg :: Elt a
             => (Exp a -> Exp a -> Exp a)
             -> Exp a
             -> Acc (Vector a)
             -> Acc Segments
             -> Acc (Vector a)
postscanlSeg f e arr seg = map (e `f`) $ scanl1Seg f arr seg

-- |Segmented version of 'scanr'.
--
scanrSeg :: Elt a
         => (Exp a -> Exp a -> Exp a)
         -> Exp a
         -> Acc (Vector a)
         -> Acc Segments
         -> Acc (Vector a)
scanrSeg f e arr seg = scans
  where
    -- Using technique described for scanlSeg.
    scans      = scanr1Seg f idInjArr seg'
    idInjArr   = zipWith (\h x -> h ==* 1 ? (fst x, snd x)) tailFlags $ zip idsArr arrShifted

    tailFlags  = permute (+) zerosArr' (\ix -> index1 $ (segOffsets' ! ix) - 1)
               $ generate (shape seg) (const 1)

    arrShifted = backpermute nSh (\ix -> index1 $ shiftCoords ! ix) arr

    idsArr     = generate nSh (const e)

    --
    shiftCoords = permute (+) zerosArr' (ilift1 $ \i -> i + (offsetArr ! index1 i)) coords
    coords      = Prelude.fst $ scanl' (+) 0 onesArr

    offsetArr   = scanl1 max $ permute (+) zerosArr (\ix -> index1 $ segOffsets ! ix) segIxs
    segIxs      = Prelude.fst $ scanl' (+) 0 $ generate (shape seg) (const 1)

    segOffsets' = scanl1 (+) seg'
    segOffsets  = Prelude.fst $ scanl' (+) 0 seg

    --
    nSh       = index1 $ size arr + size seg
    seg'      = map (+ 1) seg
    onesArr   = generate (shape arr) (const 1)
    zerosArr  = generate (shape arr) (const 0)
    zerosArr' = generate nSh (const 0)

-- |Segmented version of 'scanrSeg\''.
--
scanrSeg' :: Elt a
            => (Exp a -> Exp a -> Exp a)
            -> Exp a
            -> Acc (Vector a)
            -> Acc Segments
            -> (Acc (Vector a), Acc (Vector a))
scanrSeg' f e arr seg = (scans, sums)
  where
    -- Using technique described for scanlSeg'.
    scans      = scanr1Seg f idInjArr seg
    idInjArr   = zipWith (\t x -> t ==* 1 ? (fst x, snd x)) tailFlags $ zip idsArr arrShifted

    tailFlags  = permute (+) zerosArr (\ix -> index1 $ (segOffsets ! ix) - 1)
               $ generate (shape seg) (const (1 :: Exp Int))
    segOffsets = scanl1 (+) seg

    arrShifted = backpermute (shape arr) (ilift1 $ \i -> i ==* (size arr - 1) ? (i, i + 1)) arr

    idsArr     = generate (shape arr) (const e)
    zerosArr   = generate (shape arr) (const 0)

    --
    sums       = map (`f` e) $  backpermute (shape seg) (\ix -> index1 $ sumOffsets ! ix)
               $ scanr1Seg f arr seg
    sumOffsets = Prelude.fst $ scanl' (+) 0 seg

-- |Segmented version of 'scanr1'.
--
scanr1Seg :: Elt a
          => (Exp a -> Exp a -> Exp a)
          -> Acc (Vector a)
          -> Acc Segments
          -> Acc (Vector a)
scanr1Seg f arr seg = map snd $ scanr1 (mkSegApply f) $ zip (mkTailFlags seg) arr

-- |Segmented version of 'prescanr'.
--
prescanrSeg :: Elt a
            => (Exp a -> Exp a -> Exp a)
            -> Exp a
            -> Acc (Vector a)
            -> Acc Segments
            -> Acc (Vector a)
prescanrSeg f e arr seg = Prelude.fst $ scanrSeg' f e arr seg

-- |Segmented version of 'postscanr'.
--
postscanrSeg :: Elt a
             => (Exp a -> Exp a -> Exp a)
             -> Exp a
             -> Acc (Vector a)
             -> Acc Segments
             -> Acc (Vector a)
postscanrSeg f e arr seg = map (`f` e) $ scanr1Seg f arr seg


-- Segmented scan helpers
-- ----------------------

-- |Compute head flags vector from segment vector for left-scans.
--
mkHeadFlags :: Acc (Array DIM1 Int) -> Acc (Array DIM1 Int)
mkHeadFlags seg = permute (\_ _ -> 1) zerosArr (\ix -> index1 (segOffsets ! ix)) segOffsets
  where
    (segOffsets, len) = scanl' (+) 0 seg
    zerosArr          = generate (index1 $ the len) (const 0)

-- |Compute tail flags vector from segment vector for right-scans.
--
mkTailFlags :: Acc (Array DIM1 Int) -> Acc (Array DIM1 Int)
mkTailFlags seg
  = permute (\_ _ -> 1) zerosArr (ilift1 $ \i -> (segOffsets ! index1 i) - 1) segOffsets
  where
    segOffsets = scanl1 (+) seg
    len        = segOffsets ! index1 (size seg - 1)
    zerosArr   = generate (index1 len) (const 0)

-- |Construct a segmented version of apply from a non-segmented version. The segmented apply
-- operates on a head-flag value tuple.
--
mkSegApply :: (Elt e)
           => (Exp e -> Exp e -> Exp e)
           -> (Exp (Int, e) -> Exp (Int, e) -> Exp (Int, e))
mkSegApply op = apply
  where
    apply a b = lift (((aF ==* 1) ||* (bF ==* 1)) ? (1, 0), (bF ==* 1) ? (bV, aV `op` bV))
      where
        aF = fst a
        aV = snd a
        bF = fst b
        bV = snd b

