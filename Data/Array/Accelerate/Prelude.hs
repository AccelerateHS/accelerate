{-# LANGUAGE TypeOperators, ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.Prelude
-- Copyright   : [2010..2011] Manuel M T Chakravarty, Gabriele Keller, Ben Lever
--               [2009..2012] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
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

  -- * Zipping
  zipWith3, zipWith4,
  zip, zip3, zip4,

  -- * Unzipping
  unzip, unzip3, unzip4,

  -- * Reductions
  foldAll, fold1All,

  -- ** Specialised folds
  all, any, and, or, sum, product, minimum, maximum,

  -- * Scans
  prescanl, postscanl, prescanr, postscanr,

  -- ** Segmented scans
  scanlSeg, scanl'Seg, scanl1Seg, prescanlSeg, postscanlSeg,
  scanrSeg, scanr'Seg, scanr1Seg, prescanrSeg, postscanrSeg,

  -- * Shape manipulation
  flatten,

  -- * Enumeration and filling
  fill, enumFromN, enumFromStepN,

  -- * Working with predicates
  -- ** Filtering
  filter,

  -- ** Scatter / Gather
  scatter, scatterIf,
  gather,  gatherIf,

  -- * Permutations
  reverse, transpose,

  -- * Extracting sub-vectors
  init, tail, take, drop, slit

) where

-- avoid clashes with Prelude functions
--
import Data.Bits
import Data.Bool
import Prelude ((.), ($), (+), (-), (*), const, subtract, id)
import qualified Prelude as P

-- friends
import Data.Array.Accelerate.Array.Sugar hiding ((!), ignore, shape, size)
import Data.Array.Accelerate.Language
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Type


-- Map-like composites
-- -------------------

-- | Zip three arrays with the given function
--
zipWith3 :: (Shape sh, Elt a, Elt b, Elt c, Elt d)
         => (Exp a -> Exp b -> Exp c -> Exp d)
         -> Acc (Array sh a)
         -> Acc (Array sh b)
         -> Acc (Array sh c)
         -> Acc (Array sh d)
zipWith3 f as bs cs
  = map (\x -> let (a,b,c) = unlift x in f a b c)
  $ zip3 as bs cs

-- | Zip four arrays with the given function
--
zipWith4 :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e)
         => (Exp a -> Exp b -> Exp c -> Exp d -> Exp e)
         -> Acc (Array sh a)
         -> Acc (Array sh b)
         -> Acc (Array sh c)
         -> Acc (Array sh d)
         -> Acc (Array sh e)
zipWith4 f as bs cs ds
  = map (\x -> let (a,b,c,d) = unlift x in f a b c d)
  $ zip4 as bs cs ds

-- | Combine the elements of two arrays pairwise.  The shape of the result is
-- the intersection of the two argument shapes.
--
zip :: (Shape sh, Elt a, Elt b)
    => Acc (Array sh a)
    -> Acc (Array sh b)
    -> Acc (Array sh (a, b))
zip = zipWith (curry lift)

-- | Take three arrays and return an array of triples, analogous to zip.
--
zip3 :: forall sh. forall a. forall b. forall c. (Shape sh, Elt a, Elt b, Elt c)
     => Acc (Array sh a)
     -> Acc (Array sh b)
     -> Acc (Array sh c)
     -> Acc (Array sh (a, b, c))
zip3 as bs cs
  = zipWith (\a bc -> let (b, c) = unlift bc :: (Exp b, Exp c) in lift (a, b, c)) as
  $ zip bs cs

-- | Take four arrays and return an array of quadruples, analogous to zip.
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

-- | The converse of 'zip', but the shape of the two results is identical to the
-- shape of the argument.
--
unzip :: (Shape sh, Elt a, Elt b)
      => Acc (Array sh (a, b))
      -> (Acc (Array sh a), Acc (Array sh b))
unzip arr = (map fst arr, map snd arr)

-- | Take an array of triples and return three arrays, analogous to unzip.
--
unzip3 :: (Shape sh, Elt a, Elt b, Elt c)
       => Acc (Array sh (a, b, c))
       -> (Acc (Array sh a), Acc (Array sh b), Acc (Array sh c))
unzip3 xs = (map get1 xs, map get2 xs, map get3 xs)
  where
    get1 :: forall a b c. (Elt a, Elt b, Elt c) => Exp (a,b,c) -> Exp a
    get1 x = let (a, _ :: Exp b, _ :: Exp c) = unlift x in a

    get2 :: forall a b c. (Elt a, Elt b, Elt c) => Exp (a,b,c) -> Exp b
    get2 x = let (_ :: Exp a, b, _ :: Exp c) = unlift x in b

    get3 :: forall a b c. (Elt a, Elt b, Elt c) => Exp (a,b,c) -> Exp c
    get3 x = let (_ :: Exp a, _ :: Exp b, c) = unlift x in c


-- | Take an array of quadruples and return four arrays, analogous to unzip.
--
unzip4 :: (Shape sh, Elt a, Elt b, Elt c, Elt d)
       => Acc (Array sh (a, b, c, d))
       -> (Acc (Array sh a), Acc (Array sh b), Acc (Array sh c), Acc (Array sh d))
unzip4 xs = (map get1 xs, map get2 xs, map get3 xs, map get4 xs)
  where
    get1 :: forall a b c d. (Elt a, Elt b, Elt c, Elt d) => Exp (a,b,c,d) -> Exp a
    get1 x = let (a, _ :: Exp b, _ :: Exp c, _ :: Exp d) = unlift x in a

    get2 :: forall a b c d. (Elt a, Elt b, Elt c, Elt d) => Exp (a,b,c,d) -> Exp b
    get2 x = let (_ :: Exp a, b, _ :: Exp c, _ :: Exp d) = unlift x in b

    get3 :: forall a b c d. (Elt a, Elt b, Elt c, Elt d) => Exp (a,b,c,d) -> Exp c
    get3 x = let (_ :: Exp a, _ :: Exp b, c, _ :: Exp d) = unlift x in c

    get4 :: forall a b c d. (Elt a, Elt b, Elt c, Elt d) => Exp (a,b,c,d) -> Exp d
    get4 x = let (_ :: Exp a, _ :: Exp b, _ :: Exp c, d) = unlift x in d


-- Reductions
-- ----------

-- | Reduction of an array of arbitrary rank to a single scalar value.
--
foldAll :: (Shape sh, Elt a)
        => (Exp a -> Exp a -> Exp a)
        -> Exp a
        -> Acc (Array sh a)
        -> Acc (Scalar a)
foldAll f e arr = fold f e (flatten arr)

-- | Variant of 'foldAll' that requires the reduced array to be non-empty and
-- doesn't need an default value.
--
fold1All :: (Shape sh, Elt a)
         => (Exp a -> Exp a -> Exp a)
         -> Acc (Array sh a)
         -> Acc (Scalar a)
fold1All f arr = fold1 f (flatten arr)


-- Specialised reductions
-- ----------------------
--
-- Leave the results of these as scalar arrays to make it clear that these are
-- array computations, and thus can not be nested.

-- | Check if all elements satisfy a predicate
--
all :: (Shape sh, Elt e)
    => (Exp e -> Exp Bool)
    -> Acc (Array sh e)
    -> Acc (Scalar Bool)
all f = and . map f

-- | Check if any element satisfies the predicate
--
any :: (Shape sh, Elt e)
    => (Exp e -> Exp Bool)
    -> Acc (Array sh e)
    -> Acc (Scalar Bool)
any f = or . map f

-- | Check if all elements are 'True'
--
and :: Shape sh
    => Acc (Array sh Bool)
    -> Acc (Scalar Bool)
and = foldAll (&&*) (constant True)

-- | Check if any element is 'True'
--
or :: Shape sh
   => Acc (Array sh Bool)
   -> Acc (Scalar Bool)
or = foldAll (||*) (constant False)

-- | Compute the sum of elements
--
sum :: (Shape sh, Elt e, IsNum e)
    => Acc (Array sh e)
    -> Acc (Scalar e)
sum = foldAll (+) 0

-- | Compute the product of the elements
--
product :: (Shape sh, Elt e, IsNum e)
        => Acc (Array sh e)
        -> Acc (Scalar e)
product = foldAll (*) 1

-- | Yield the minimum element of an array. The array must not be empty.
--
minimum :: (Shape sh, Elt e, IsScalar e)
        => Acc (Array sh e)
        -> Acc (Scalar e)
minimum = fold1All min

-- | Yield the maximum element of an array. The array must not be empty.
--
maximum :: (Shape sh, Elt e, IsScalar e)
        => Acc (Array sh e)
        -> Acc (Scalar e)
maximum = fold1All max


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
prescanl f e = P.fst . scanl' f e

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
prescanr f e = P.fst . scanr' f e

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

-- |Segmented version of 'scanl'
--
scanlSeg :: (Elt a, Elt i, IsIntegral i)
         => (Exp a -> Exp a -> Exp a)
         -> Exp a
         -> Acc (Vector a)
         -> Acc (Segments i)
         -> Acc (Vector a)
scanlSeg f z vec seg = scanl1Seg f vec' seg'
  where
    -- Segmented exclusive scan is implemented by first injecting the seed
    -- element at the head of each segment, and then performing a segmented
    -- inclusive scan.
    --
    -- This is done by creating a creating a vector entirely of the seed
    -- element, and overlaying the input data in all places other than at the
    -- start of a segment.
    --
    seg'        = map (+1) seg
    vec'        = permute const
                          (fill (index1 $ size vec + size seg) z)
                          (\ix -> index1' $ unindex1' ix + inc ! ix)
                          vec

    -- Each element in the segments must be shifted to the right one additional
    -- place for each successive segment, to make room for the seed element.
    -- Here, we make use of the fact that the vector returned by 'mkHeadFlags'
    -- contains non-unit entries, which indicate zero length segments.
    --
    flags       = mkHeadFlags seg
    inc         = scanl1 (+) flags


-- |Segmented version of 'scanl''
--
-- The first element of the resulting tuple is a vector of scanned values. The
-- second element is a vector of segment scan totals and has the same size as
-- the segment vector.
--
scanl'Seg :: forall a i. (Elt a, Elt i, IsIntegral i)
          => (Exp a -> Exp a -> Exp a)
          -> Exp a
          -> Acc (Vector a)
          -> Acc (Segments i)
          -> Acc (Vector a, Vector a)
scanl'Seg f z vec seg = result
  where
    -- Returned the result combined, so that the sub-calculations are shared
    -- should the user require both results.
    --
    result      = lift (body, sums)

    -- Segmented scan' is implemented by deconstructing a segmented exclusive
    -- scan, to separate the final value and scan body.
    --
    -- TLM: Segmented scans, and this version in particular, expend a lot of
    --      effort scanning flag arrays. On inspection it appears that several
    --      of these operations are duplicated, but this will not be picked up
    --      by sharing _observation_. Perhaps a global CSE-style pass would be
    --      beneficial.
    --
    vec'        = scanlSeg f z vec seg

    -- Extract the final reduction value for each segment, which is at the last
    -- index of each segment.
    --
    seg'        = map (+1) seg
    tails       = zipWith (+) seg . P.fst $ scanl' (+) 0 seg'
    sums        = backpermute (shape seg) (\ix -> index1' $ tails ! ix) vec'

    -- Slice out the body of each segment.
    --
    -- Build a head-flags representation based on the original segment
    -- descriptor. This contains the target length of each of the body segments,
    -- which is one fewer element than the actual bodies stored in vec'. Thus,
    -- the flags align with the last element of each body section, and when
    -- scanned, this element will be incremented over.
    --
    offset      = scanl1 (+) seg
    inc         = scanl1 (+)
                $ permute (+) (fill (index1 $ size vec + 1) 0)
                              (\ix -> index1' $ offset ! ix)
                              (fill (shape seg) (1 :: Exp i))

    body        = backpermute (shape vec)
                              (\ix -> index1' $ unindex1' ix + inc ! ix)
                              vec'


-- |Segmented version of 'scanl1'.
--
scanl1Seg :: (Elt a, Elt i, IsIntegral i)
          => (Exp a -> Exp a -> Exp a)
          -> Acc (Vector a)
          -> Acc (Segments i)
          -> Acc (Vector a)
scanl1Seg f vec seg
  = P.snd
  . unzip
  . scanl1 (segmented f)
  $ zip (mkHeadFlags seg) vec

-- |Segmented version of 'prescanl'.
--
prescanlSeg :: (Elt a, Elt i, IsIntegral i)
            => (Exp a -> Exp a -> Exp a)
            -> Exp a
            -> Acc (Vector a)
            -> Acc (Segments i)
            -> Acc (Vector a)
prescanlSeg f e vec seg
  = P.fst
  . unatup2
  $ scanl'Seg f e vec seg

-- |Segmented version of 'postscanl'.
--
postscanlSeg :: (Elt a, Elt i, IsIntegral i)
             => (Exp a -> Exp a -> Exp a)
             -> Exp a
             -> Acc (Vector a)
             -> Acc (Segments i)
             -> Acc (Vector a)
postscanlSeg f e vec seg
  = map (f e)
  $ scanl1Seg f vec seg

-- |Segmented version of 'scanr'.
--
scanrSeg :: (Elt a, Elt i, IsIntegral i)
         => (Exp a -> Exp a -> Exp a)
         -> Exp a
         -> Acc (Vector a)
         -> Acc (Segments i)
         -> Acc (Vector a)
scanrSeg f z vec seg = scanr1Seg f vec' seg'
  where
    -- Using technique described for 'scanlSeg', where we intersperse the array
    -- with the seed element at the start of each segment, and then perform an
    -- inclusive segmented scan.
    --
    inc         = scanl1 (+) (mkHeadFlags seg)
    seg'        = map (+1) seg
    vec'        = permute const
                          (fill (index1 $ size vec + size seg) z)
                          (\ix -> index1' $ unindex1' ix + inc ! ix - 1)
                          vec


-- | Segmented version of 'scanr''.
--
scanr'Seg :: forall a i. (Elt a, Elt i, IsIntegral i)
          => (Exp a -> Exp a -> Exp a)
          -> Exp a
          -> Acc (Vector a)
          -> Acc (Segments i)
          -> Acc (Vector a, Vector a)
scanr'Seg f z vec seg = result
  where
    -- Using technique described for scanl'Seg
    --
    result      = lift (body, sums)
    vec'        = scanrSeg f z vec seg

    -- reduction values
    seg'        = map (+1) seg
    heads       = P.fst $ scanl' (+) 0 seg'
    sums        = backpermute (shape seg) (\ix -> index1' $ heads ! ix) vec'

    -- body segments
    inc         = scanl1 (+) $ mkHeadFlags seg
    body        = backpermute (shape vec)
                              (\ix -> index1' $ unindex1' ix + inc ! ix)
                              vec'


-- |Segmented version of 'scanr1'.
--
scanr1Seg :: (Elt a, Elt i, IsIntegral i)
          => (Exp a -> Exp a -> Exp a)
          -> Acc (Vector a)
          -> Acc (Segments i)
          -> Acc (Vector a)
scanr1Seg f vec seg
  = P.snd
  . unzip
  . scanr1 (segmented f)
  $ zip (mkTailFlags seg) vec

-- |Segmented version of 'prescanr'.
--
prescanrSeg :: (Elt a, Elt i, IsIntegral i)
            => (Exp a -> Exp a -> Exp a)
            -> Exp a
            -> Acc (Vector a)
            -> Acc (Segments i)
            -> Acc (Vector a)
prescanrSeg f e vec seg
  = P.fst
  . unatup2
  $ scanr'Seg f e vec seg

-- |Segmented version of 'postscanr'.
--
postscanrSeg :: (Elt a, Elt i, IsIntegral i)
             => (Exp a -> Exp a -> Exp a)
             -> Exp a
             -> Acc (Vector a)
             -> Acc (Segments i)
             -> Acc (Vector a)
postscanrSeg f e vec seg
  = map (f e)
  $ scanr1Seg f vec seg


-- Segmented scan helpers
-- ----------------------

-- |Compute head flags vector from segment vector for left-scans.
--
-- The vector will be full of zeros in the body of a segment, and non-zero
-- otherwise. The "flag" value, if greater than one, indicates that several
-- empty segments are represented by this single flag entry. This is additional
-- data is used by exclusive segmented scan.
--
mkHeadFlags :: (Elt i, IsIntegral i) => Acc (Segments i) -> Acc (Segments i)
mkHeadFlags seg
  = init
  $ permute (+) zeros (\ix -> index1' (offset ! ix)) ones
  where
    (offset, len)       = scanl' (+) 0 seg
    zeros               = fill (index1' $ the len + 1) 0
    ones                = fill (index1  $ size offset) 1

-- |Compute tail flags vector from segment vector for right-scans. That is, the
-- flag is placed at the last place in each segment.
--
mkTailFlags :: (Elt i, IsIntegral i) => Acc (Segments i) -> Acc (Segments i)
mkTailFlags seg
  = init
  $ permute (+) zeros (\ix -> index1' (the len - 1 - offset ! ix)) ones
  where
    (offset, len)       = scanr' (+) 0 seg
    zeros               = fill (index1' $ the len + 1) 0
    ones                = fill (index1  $ size offset) 1

-- |Construct a segmented version of a function from a non-segmented version.
-- The segmented apply operates on a head-flag value tuple, and follows the
-- procedure of Sengupta et. al.
--
segmented :: (Elt e, Elt i, IsIntegral i)
          => (Exp e -> Exp e -> Exp e)
          -> Exp (i, e) -> Exp (i, e) -> Exp (i, e)
segmented f a b =
  let (aF, aV) = unlift a
      (bF, bV) = unlift b
  in
  lift (aF .|. bF, bF /=* 0 ? (bV, f aV bV))

-- |Index construction and destruction generalised to integral types.
--
-- We generalise the segment descriptor to integral types because some
-- architectures, such as GPUs, have poor performance for 64-bit types. So,
-- there is a tension between performance and requiring 64-bit indices for some
-- applications, and we would not like to restrict ourselves to either one.
--
-- As we don't yet support non-Int dimensions in shapes, we will need to convert
-- back to concrete Int. However, don't put these generalised forms into the
-- base library, because it results in too many ambiguity errors.
--
index1' ::  (Elt i, IsIntegral i) => Exp i -> Exp DIM1
index1' i = lift (Z :. fromIntegral i)

unindex1' :: (Elt i, IsIntegral i) => Exp DIM1 -> Exp i
unindex1' ix = let Z :. i = unlift ix in fromIntegral i


-- Reshaping of arrays
-- -------------------

-- | Flattens a given array of arbitrary dimension.
--
flatten :: (Shape ix, Elt a) => Acc (Array ix a) -> Acc (Vector a)
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

-- | Create an array of the given shape containing the values @x@, @x+y@,
-- @x+y+y@ etc. (in row-major order).
--
enumFromStepN :: (Shape sh, Elt e, IsNum e)
              => Exp sh
              -> Exp e    -- ^ x: start
              -> Exp e    -- ^ y: step
              -> Acc (Array sh e)
enumFromStepN sh x y
  = reshape sh
  $ generate (index1 $ shapeSize sh)
             (\ix -> (fromIntegral (unindex1 ix :: Exp Int) * y) + x)


-- Filtering
-- ---------

-- | Drop elements that do not satisfy the predicate
--
filter :: Elt a
       => (Exp a -> Exp Bool)
       -> Acc (Vector a)
       -> Acc (Vector a)
filter p arr
  = let flags            = map (boolToInt . p) arr
        (targetIdx, len) = scanl' (+) 0 flags
        arr'             = backpermute (index1 $ the len) id arr
    in
    permute const arr' (\ix -> flags!ix ==* 0 ? (ignore, index1 $ targetIdx!ix)) arr
    -- FIXME: This is abusing 'permute' in that the first two arguments are
    --        only justified because we know the permutation function will
    --        write to each location in the target exactly once.
    --        Instead, we should have a primitive that directly encodes the
    --        compaction pattern of the permutation function.

{-# RULES
  "ACC filter/filter" forall f g arr.
    filter f (filter g arr) = filter (\x -> g x &&* f x) arr
 #-}


-- Gather operations
-- -----------------

-- | Copy elements from source array to destination array according to a map. This
--   is a backpermute operation where a 'map' vector encodes the ouput to input
--   index mapping.
--
--   For example:
--
--  > input  = [1, 9, 6, 4, 4, 2, 0, 1, 2]
--  > map    = [1, 3, 7, 2, 5, 3]
--  >
--  > output = [9, 4, 1, 6, 2, 4]
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
--
--   For example:
--
--  > default = [6, 6, 6, 6, 6, 6]
--  > map     = [1, 3, 7, 2, 5, 3]
--  > mask    = [3, 4, 9, 2, 7, 5]
--  > pred    = (> 4)
--  > input   = [1, 9, 6, 4, 4, 2, 0, 1, 2]
--  >
--  > output  = [6, 6, 1, 6, 2, 4]
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
--   index mapping. Output elements for indices that are not mapped assume the
--   default vector's value.
--
--   For example:
--
--  > default = [0, 0, 0, 0, 0, 0, 0, 0, 0]
--  > map     = [1, 3, 7, 2, 5, 8]
--  > input   = [1, 9, 6, 4, 4, 2, 5]
--  >
--  > output  = [0, 1, 4, 9, 0, 4, 0, 6, 2]
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
--   copied. If not copied, the output array assumes the default vector's value.
--
--   For example:
--
--  > default = [0, 0, 0, 0, 0, 0, 0, 0, 0]
--  > map     = [1, 3, 7, 2, 5, 8]
--  > mask    = [3, 4, 9, 2, 7, 5]
--  > pred    = (> 4)
--  > input   = [1, 9, 6, 4, 4, 2]
--  >
--  > output  = [0, 0, 0, 0, 0, 4, 0, 6, 2]
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


-- Permutations
-- ------------

-- | Reverse the elements of a vector.
--
reverse :: Elt e => Acc (Vector e) -> Acc (Vector e)
reverse xs =
  let len       = unindex1 (shape xs)
      pf i      = len - i - 1
  in  backpermute (shape xs) (ilift1 pf) xs

-- | Transpose the rows and columns of a matrix.
--
transpose :: Elt e => Acc (Array DIM2 e) -> Acc (Array DIM2 e)
transpose mat =
  let swap = lift1 $ \(Z:.x:.y) -> Z:.y:.x :: Z:.Exp Int:.Exp Int
  in  backpermute (swap $ shape mat) swap mat


-- Extracting sub-vectors
-- ----------------------

-- | Yield the first @n@ elements of the input vector. The vector must contain
-- no more than @n@ elements.
--
take :: Elt e => Exp Int -> Acc (Vector e) -> Acc (Vector e)
take n = backpermute (index1 n) id

-- | Yield all but the first @n@ elements of the input vector. The vector must
--   contain no more than @n@ elements.
--
drop :: Elt e => Exp Int -> Acc (Vector e) -> Acc (Vector e)
drop n arr =
  let n' = unit n
  in  backpermute (ilift1 (subtract n) (shape arr)) (ilift1 (+ the n')) arr


-- | Yield all but the last element of the input vector. The vector must not be
--   empty.
--
init :: Elt e => Acc (Vector e) -> Acc (Vector e)
init arr = take ((unindex1 $ shape arr) - 1) arr


-- | Yield all but the first element of the input vector. The vector must not be
--   empty.
--
tail :: Elt e => Acc (Vector e) -> Acc (Vector e)
tail arr = backpermute (ilift1 (subtract 1) (shape arr)) (ilift1 (+1)) arr


-- | Yield a slit (slice) from the vector. The vector must contain at least
--   @i + n@ elements. Denotationally, we have:
--
-- > slit i n = take n . drop i
--
slit :: Elt e => Exp Int -> Exp Int -> Acc (Vector e) -> Acc (Vector e)
slit i n =
  let i' = unit i
  in  backpermute (index1 n) (ilift1 (+ the i'))

