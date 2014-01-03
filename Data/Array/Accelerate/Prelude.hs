{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
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
-- Standard functions that are not part of the core set (directly represented in
-- the AST), but are instead implemented in terms of the core set.
--

module Data.Array.Accelerate.Prelude (

  -- * Zipping
  zipWith3, zipWith4, zipWith5, zipWith6, zipWith7, zipWith8, zipWith9,
  zip, zip3, zip4, zip5, zip6, zip7, zip8, zip9,

  -- * Unzipping
  unzip, unzip3, unzip4, unzip5, unzip6, unzip7, unzip8, unzip9,

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

  -- * Concatenation
  (++),

  -- * Working with predicates
  -- ** Filtering
  filter,

  -- ** Scatter / Gather
  scatter, scatterIf,
  gather,  gatherIf,

  -- * Permutations
  reverse, transpose,

  --  Extracting sub-vectors
  init, tail, take, drop, slit,

  -- * Array-level flow control
  (?|),

  -- * Expression-level flow control
  (?), caseof,

  -- * Scalar iteration
  iterate,

  -- * Scalar reduction
  sfoldl, -- sfoldr,

  -- * Lifting and unlifting
  Lift(..), Unlift(..),
  lift1, lift2, ilift1, ilift2,

  -- ** Tuple construction and destruction
  fst, snd, curry, uncurry,

  -- ** Index construction and destruction
  index0, index1, unindex1, index2, unindex2,

  -- * Array operations with a scalar result
  the, null,

) where

-- avoid clashes with Prelude functions
--
import Data.Bits
import Data.Bool
import Prelude ((.), ($), (+), (-), (*), const, subtract, id, min, max, Float,
  Double, Char)
import qualified Prelude as P

-- friends
import Data.Array.Accelerate.Array.Sugar hiding ((!), ignore, shape, size, intersect)
import Data.Array.Accelerate.Language
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Type


-- Map-like composites
-- -------------------

-- | Zip three arrays with the given function, analogous to 'zipWith'.
--
zipWith3 :: (Shape sh, Elt a, Elt b, Elt c, Elt d)
         => (Exp a -> Exp b -> Exp c -> Exp d)
         -> Acc (Array sh a)
         -> Acc (Array sh b)
         -> Acc (Array sh c)
         -> Acc (Array sh d)
zipWith3 f as bs cs
  = generate (shape as `intersect` shape bs `intersect` shape cs)
             (\ix -> f (as ! ix) (bs ! ix) (cs ! ix))

-- | Zip four arrays with the given function, analogous to 'zipWith'.
--
zipWith4 :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e)
         => (Exp a -> Exp b -> Exp c -> Exp d -> Exp e)
         -> Acc (Array sh a)
         -> Acc (Array sh b)
         -> Acc (Array sh c)
         -> Acc (Array sh d)
         -> Acc (Array sh e)
zipWith4 f as bs cs ds
  = generate (shape as `intersect` shape bs `intersect`
              shape cs `intersect` shape ds)
             (\ix -> f (as ! ix) (bs ! ix) (cs ! ix) (ds ! ix))

-- | Zip five arrays with the given function, analogous to 'zipWith'.
--
zipWith5 :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f)
         => (Exp a -> Exp b -> Exp c -> Exp d -> Exp e -> Exp f)
         -> Acc (Array sh a)
         -> Acc (Array sh b)
         -> Acc (Array sh c)
         -> Acc (Array sh d)
         -> Acc (Array sh e)
         -> Acc (Array sh f)
zipWith5 f as bs cs ds es
  = generate (shape as `intersect` shape bs `intersect` shape cs
                       `intersect` shape ds `intersect` shape es)
             (\ix -> f (as ! ix) (bs ! ix) (cs ! ix) (ds ! ix) (es ! ix))

-- | Zip six arrays with the given function, analogous to 'zipWith'.
--
zipWith6 :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g)
         => (Exp a -> Exp b -> Exp c -> Exp d -> Exp e -> Exp f -> Exp g)
         -> Acc (Array sh a)
         -> Acc (Array sh b)
         -> Acc (Array sh c)
         -> Acc (Array sh d)
         -> Acc (Array sh e)
         -> Acc (Array sh f)
         -> Acc (Array sh g)
zipWith6 f as bs cs ds es fs
  = generate (shape as `intersect` shape bs `intersect` shape cs
                       `intersect` shape ds `intersect` shape es
                       `intersect` shape fs)
             (\ix -> f (as ! ix) (bs ! ix) (cs ! ix) (ds ! ix) (es ! ix) (fs ! ix))

-- | Zip seven arrays with the given function, analogous to 'zipWith'.
--
zipWith7 :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h)
         => (Exp a -> Exp b -> Exp c -> Exp d -> Exp e -> Exp f -> Exp g -> Exp h)
         -> Acc (Array sh a)
         -> Acc (Array sh b)
         -> Acc (Array sh c)
         -> Acc (Array sh d)
         -> Acc (Array sh e)
         -> Acc (Array sh f)
         -> Acc (Array sh g)
         -> Acc (Array sh h)
zipWith7 f as bs cs ds es fs gs
  = generate (shape as `intersect` shape bs `intersect` shape cs
                       `intersect` shape ds `intersect` shape es
                       `intersect` shape fs `intersect` shape gs)
             (\ix -> f (as ! ix) (bs ! ix) (cs ! ix) (ds ! ix) (es ! ix) (fs ! ix) (gs ! ix))

-- | Zip eight arrays with the given function, analogous to 'zipWith'.
--
zipWith8 :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i)
         => (Exp a -> Exp b -> Exp c -> Exp d -> Exp e -> Exp f -> Exp g -> Exp h -> Exp i)
         -> Acc (Array sh a)
         -> Acc (Array sh b)
         -> Acc (Array sh c)
         -> Acc (Array sh d)
         -> Acc (Array sh e)
         -> Acc (Array sh f)
         -> Acc (Array sh g)
         -> Acc (Array sh h)
         -> Acc (Array sh i)
zipWith8 f as bs cs ds es fs gs hs
  = generate (shape as `intersect` shape bs `intersect` shape cs
                       `intersect` shape ds `intersect` shape es
                       `intersect` shape fs `intersect` shape gs
                       `intersect` shape hs)
             (\ix -> f (as ! ix) (bs ! ix) (cs ! ix) (ds ! ix) (es ! ix) (fs ! ix) (gs ! ix) (hs ! ix))

-- | Zip nine arrays with the given function, analogous to 'zipWith'.
--
zipWith9 :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j)
         => (Exp a -> Exp b -> Exp c -> Exp d -> Exp e -> Exp f -> Exp g -> Exp h -> Exp i -> Exp j)
         -> Acc (Array sh a)
         -> Acc (Array sh b)
         -> Acc (Array sh c)
         -> Acc (Array sh d)
         -> Acc (Array sh e)
         -> Acc (Array sh f)
         -> Acc (Array sh g)
         -> Acc (Array sh h)
         -> Acc (Array sh i)
         -> Acc (Array sh j)
zipWith9 f as bs cs ds es fs gs hs is
  = generate (shape as `intersect` shape bs `intersect` shape cs
                       `intersect` shape ds `intersect` shape es
                       `intersect` shape fs `intersect` shape gs
                       `intersect` shape hs `intersect` shape is)
             (\ix -> f (as ! ix) (bs ! ix) (cs ! ix) (ds ! ix) (es ! ix) (fs ! ix) (gs ! ix) (hs ! ix) (is ! ix))


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
zip3 :: (Shape sh, Elt a, Elt b, Elt c)
     => Acc (Array sh a)
     -> Acc (Array sh b)
     -> Acc (Array sh c)
     -> Acc (Array sh (a, b, c))
zip3 = zipWith3 (\a b c -> lift (a,b,c))

-- | Take four arrays and return an array of quadruples, analogous to zip.
--
zip4 :: (Shape sh, Elt a, Elt b, Elt c, Elt d)
     => Acc (Array sh a)
     -> Acc (Array sh b)
     -> Acc (Array sh c)
     -> Acc (Array sh d)
     -> Acc (Array sh (a, b, c, d))
zip4 = zipWith4 (\a b c d -> lift (a,b,c,d))

-- | Take five arrays and return an array of five-tuples, analogous to zip.
--
zip5 :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e)
     => Acc (Array sh a)
     -> Acc (Array sh b)
     -> Acc (Array sh c)
     -> Acc (Array sh d)
     -> Acc (Array sh e)
     -> Acc (Array sh (a, b, c, d, e))
zip5 = zipWith5 (\a b c d e -> lift (a,b,c,d,e))

-- | Take six arrays and return an array of six-tuples, analogous to zip.
--
zip6 :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f)
     => Acc (Array sh a)
     -> Acc (Array sh b)
     -> Acc (Array sh c)
     -> Acc (Array sh d)
     -> Acc (Array sh e)
     -> Acc (Array sh f)
     -> Acc (Array sh (a, b, c, d, e, f))
zip6 = zipWith6 (\a b c d e f -> lift (a,b,c,d,e,f))

-- | Take seven arrays and return an array of seven-tuples, analogous to zip.
--
zip7 :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g)
     => Acc (Array sh a)
     -> Acc (Array sh b)
     -> Acc (Array sh c)
     -> Acc (Array sh d)
     -> Acc (Array sh e)
     -> Acc (Array sh f)
     -> Acc (Array sh g)
     -> Acc (Array sh (a, b, c, d, e, f, g))
zip7 = zipWith7 (\a b c d e f g -> lift (a,b,c,d,e,f,g))

-- | Take seven arrays and return an array of seven-tuples, analogous to zip.
--
zip8 :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h)
     => Acc (Array sh a)
     -> Acc (Array sh b)
     -> Acc (Array sh c)
     -> Acc (Array sh d)
     -> Acc (Array sh e)
     -> Acc (Array sh f)
     -> Acc (Array sh g)
     -> Acc (Array sh h)
     -> Acc (Array sh (a, b, c, d, e, f, g, h))
zip8 = zipWith8 (\a b c d e f g h -> lift (a,b,c,d,e,f,g,h))

-- | Take seven arrays and return an array of seven-tuples, analogous to zip.
--
zip9 :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i)
     => Acc (Array sh a)
     -> Acc (Array sh b)
     -> Acc (Array sh c)
     -> Acc (Array sh d)
     -> Acc (Array sh e)
     -> Acc (Array sh f)
     -> Acc (Array sh g)
     -> Acc (Array sh h)
     -> Acc (Array sh i)
     -> Acc (Array sh (a, b, c, d, e, f, g, h, i))
zip9 = zipWith9 (\a b c d e f g h i -> lift (a,b,c,d,e,f,g,h,i))


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
    get1 x = let (a,_,_) = untup3 x in a
    get2 x = let (_,b,_) = untup3 x in b
    get3 x = let (_,_,c) = untup3 x in c


-- | Take an array of quadruples and return four arrays, analogous to unzip.
--
unzip4 :: (Shape sh, Elt a, Elt b, Elt c, Elt d)
       => Acc (Array sh (a, b, c, d))
       -> (Acc (Array sh a), Acc (Array sh b), Acc (Array sh c), Acc (Array sh d))
unzip4 xs = (map get1 xs, map get2 xs, map get3 xs, map get4 xs)
  where
    get1 x = let (a,_,_,_) = untup4 x in a
    get2 x = let (_,b,_,_) = untup4 x in b
    get3 x = let (_,_,c,_) = untup4 x in c
    get4 x = let (_,_,_,d) = untup4 x in d

-- | Take an array of 5-tuples and return five arrays, analogous to unzip.
--
unzip5 :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e)
       => Acc (Array sh (a, b, c, d, e))
       -> (Acc (Array sh a), Acc (Array sh b), Acc (Array sh c), Acc (Array sh d), Acc (Array sh e))
unzip5 xs = (map get1 xs, map get2 xs, map get3 xs, map get4 xs, map get5 xs)
  where
    get1 x = let (a,_,_,_,_) = untup5 x in a
    get2 x = let (_,b,_,_,_) = untup5 x in b
    get3 x = let (_,_,c,_,_) = untup5 x in c
    get4 x = let (_,_,_,d,_) = untup5 x in d
    get5 x = let (_,_,_,_,e) = untup5 x in e

-- | Take an array of 6-tuples and return six arrays, analogous to unzip.
--
unzip6 :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f)
       => Acc (Array sh (a, b, c, d, e, f))
       -> ( Acc (Array sh a), Acc (Array sh b), Acc (Array sh c)
          , Acc (Array sh d), Acc (Array sh e), Acc (Array sh f))
unzip6 xs = (map get1 xs, map get2 xs, map get3 xs, map get4 xs, map get5 xs, map get6 xs)
  where
    get1 x = let (a,_,_,_,_,_) = untup6 x in a
    get2 x = let (_,b,_,_,_,_) = untup6 x in b
    get3 x = let (_,_,c,_,_,_) = untup6 x in c
    get4 x = let (_,_,_,d,_,_) = untup6 x in d
    get5 x = let (_,_,_,_,e,_) = untup6 x in e
    get6 x = let (_,_,_,_,_,f) = untup6 x in f

-- | Take an array of 7-tuples and return seven arrays, analogous to unzip.
--
unzip7 :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g)
       => Acc (Array sh (a, b, c, d, e, f, g))
       -> ( Acc (Array sh a), Acc (Array sh b), Acc (Array sh c)
          , Acc (Array sh d), Acc (Array sh e), Acc (Array sh f)
          , Acc (Array sh g))
unzip7 xs = ( map get1 xs, map get2 xs, map get3 xs
            , map get4 xs, map get5 xs, map get6 xs
            , map get7 xs )
  where
    get1 x = let (a,_,_,_,_,_,_) = untup7 x in a
    get2 x = let (_,b,_,_,_,_,_) = untup7 x in b
    get3 x = let (_,_,c,_,_,_,_) = untup7 x in c
    get4 x = let (_,_,_,d,_,_,_) = untup7 x in d
    get5 x = let (_,_,_,_,e,_,_) = untup7 x in e
    get6 x = let (_,_,_,_,_,f,_) = untup7 x in f
    get7 x = let (_,_,_,_,_,_,g) = untup7 x in g

-- | Take an array of 8-tuples and return eight arrays, analogous to unzip.
--
unzip8 :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h)
       => Acc (Array sh (a, b, c, d, e, f, g, h))
       -> ( Acc (Array sh a), Acc (Array sh b), Acc (Array sh c)
          , Acc (Array sh d), Acc (Array sh e), Acc (Array sh f)
          , Acc (Array sh g), Acc (Array sh h) )
unzip8 xs = ( map get1 xs, map get2 xs, map get3 xs
            , map get4 xs, map get5 xs, map get6 xs
            , map get7 xs, map get8 xs )
  where
    get1 x = let (a,_,_,_,_,_,_,_) = untup8 x in a
    get2 x = let (_,b,_,_,_,_,_,_) = untup8 x in b
    get3 x = let (_,_,c,_,_,_,_,_) = untup8 x in c
    get4 x = let (_,_,_,d,_,_,_,_) = untup8 x in d
    get5 x = let (_,_,_,_,e,_,_,_) = untup8 x in e
    get6 x = let (_,_,_,_,_,f,_,_) = untup8 x in f
    get7 x = let (_,_,_,_,_,_,g,_) = untup8 x in g
    get8 x = let (_,_,_,_,_,_,_,h) = untup8 x in h

-- | Take an array of 8-tuples and return eight arrays, analogous to unzip.
--
unzip9 :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i)
       => Acc (Array sh (a, b, c, d, e, f, g, h, i))
       -> ( Acc (Array sh a), Acc (Array sh b), Acc (Array sh c)
          , Acc (Array sh d), Acc (Array sh e), Acc (Array sh f)
          , Acc (Array sh g), Acc (Array sh h), Acc (Array sh i))
unzip9 xs = ( map get1 xs, map get2 xs, map get3 xs
            , map get4 xs, map get5 xs, map get6 xs
            , map get7 xs, map get8 xs, map get9 xs )
  where
    get1 x = let (a,_,_,_,_,_,_,_,_) = untup9 x in a
    get2 x = let (_,b,_,_,_,_,_,_,_) = untup9 x in b
    get3 x = let (_,_,c,_,_,_,_,_,_) = untup9 x in c
    get4 x = let (_,_,_,d,_,_,_,_,_) = untup9 x in d
    get5 x = let (_,_,_,_,e,_,_,_,_) = untup9 x in e
    get6 x = let (_,_,_,_,_,f,_,_,_) = untup9 x in f
    get7 x = let (_,_,_,_,_,_,g,_,_) = untup9 x in g
    get8 x = let (_,_,_,_,_,_,_,h,_) = untup9 x in h
    get9 x = let (_,_,_,_,_,_,_,_,i) = untup9 x in i


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

-- Concatenation
-- -------------

-- | Concatenate outermost component of two arrays. The extent of the lower
--   dimensional component is the intersection of the two arrays.
--
infixr 5 ++
(++) :: forall sh e. (Slice sh, Shape sh, Elt e)
     => Acc (Array (sh :. Int) e)
     -> Acc (Array (sh :. Int) e)
     -> Acc (Array (sh :. Int) e)
(++) xs ys
  = let sh1 :. n        = unlift (shape xs)     :: Exp sh :. Exp Int
        sh2 :. m        = unlift (shape ys)     :: Exp sh :. Exp Int
    in
    generate (lift (intersect sh1 sh2 :. n + m))
             (\ix -> let sh :. i = unlift ix    :: Exp sh :. Exp Int
                     in  i <* n ? ( xs ! ix, ys ! lift (sh :. i-n)) )


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
--   is a backpermute operation where a 'map' vector encodes the output to input
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
--   to a map. This is a backpermute operation where a 'map' vector encodes the
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
take n =
  let n' = the (unit n)
  in  backpermute (index1 n') id

-- | Yield all but the first @n@ elements of the input vector. The vector must
--   contain no fewer than @n@ elements.
--
drop :: Elt e => Exp Int -> Acc (Vector e) -> Acc (Vector e)
drop n arr =
  let n' = the (unit n)
  in  backpermute (ilift1 (subtract n') (shape arr)) (ilift1 (+ n')) arr


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
  let i' = the (unit i)
      n' = the (unit n)
  in  backpermute (index1 n') (ilift1 (+ i'))


-- Flow control
-- ------------

-- | Infix version of 'acond'. If the predicate evaluates to 'True', the first
-- component of the tuple is returned, else the second.
--
infix 0 ?|
(?|) :: (Arrays a) => Exp Bool -> (Acc a, Acc a) -> Acc a
c ?| (t, e) = acond c t e

-- | An infix version of 'cond'. If the predicate evaluates to 'True', the first
-- component of the tuple is returned, else the second.
--
infix 0 ?
(?) :: Elt t => Exp Bool -> (Exp t, Exp t) -> Exp t
c ? (t, e) = cond c t e

-- | A case-like control structure
--
caseof :: (Elt a, Elt b)
       => Exp a                         -- ^ case subject
       -> [(Exp a -> Exp Bool, Exp b)]  -- ^ list of cases to attempt
       -> Exp b                         -- ^ default value
       -> Exp b
caseof _ []        e = e
caseof x ((p,b):l) e = cond (p x) b (caseof x l e)


-- Scalar iteration
-- ----------------

-- | Repeatedly apply a function a fixed number of times
--
iterate :: forall a. Elt a
        => Exp Int
        -> (Exp a -> Exp a)
        -> Exp a
        -> Exp a
iterate n f z
  = let step :: (Exp Int, Exp a) -> (Exp Int, Exp a)
        step (i, acc)   = ( i+1, f acc )
    in
    snd $ while (\v -> fst v <* n) (lift1 step) (lift (constant 0, z))


-- Scalar bulk operations
-- ----------------------

-- | Reduce along an innermost slice of an array /sequentially/, by applying a
-- binary operator to a starting value and the array from left to right.
--
sfoldl :: forall sh a b. (Shape sh, Slice sh, Elt a, Elt b)
       => (Exp a -> Exp b -> Exp a)
       -> Exp a
       -> Exp sh
       -> Acc (Array (sh :. Int) b)
       -> Exp a
sfoldl f z ix xs
  = let step :: (Exp Int, Exp a) -> (Exp Int, Exp a)
        step (i, acc)   = ( i+1, acc `f` (xs ! lift (ix :. i)) )
        (_ :. n)        = unlift (shape xs)     :: Exp sh :. Exp Int
    in
    snd $ while (\v -> fst v <* n) (lift1 step) (lift (constant 0, z))


-- Lifting surface expressions
-- ---------------------------

-- | The class of types @e@ which can be lifted into @c@.
class Lift c e where
  -- | An associated-type (i.e. a type-level function) that strips all
  --   instances of surface type constructors @c@ from the input type @e@.
  --
  --   For example, the tuple types @(Exp Int, Int)@ and @(Int, Exp
  --   Int)@ have the same \"Plain\" representation.  That is, the
  --   following type equality holds:
  --
  --    @Plain (Exp Int, Int) ~ (Int,Int) ~ Plain (Int, Exp Int)@
  type Plain e

  -- | Lift the given value into a surface type 'c' --- either 'Exp' for scalar
  -- expressions or 'Acc' for array computations. The value may already contain
  -- subexpressions in 'c'.
  --
  lift :: e -> c (Plain e)

-- | A limited subset of types which can be lifted, can also be unlifted.
class Lift c e => Unlift c e where

  -- | Unlift the outermost constructor through the surface type. This is only
  -- possible if the constructor is fully determined by its type - i.e., it is a
  -- singleton.
  --
  unlift :: c (Plain e) -> e

-- instances for indices

instance Lift Exp () where
  type Plain () = ()
  lift _ = Exp $ Tuple NilTup

instance Unlift Exp () where
  unlift _ = ()

instance Lift Exp Z where
  type Plain Z = Z
  lift _ = Exp $ IndexNil

instance Unlift Exp Z where
  unlift _ = Z

instance (Slice (Plain ix), Lift Exp ix) => Lift Exp (ix :. Int) where
  type Plain (ix :. Int) = Plain ix :. Int
  lift (ix:.i) = Exp $ IndexCons (lift ix) (Exp $ Const i)

instance (Slice (Plain ix), Lift Exp ix) => Lift Exp (ix :. All) where
  type Plain (ix :. All) = Plain ix :. All
  lift (ix:.i) = Exp $ IndexCons (lift ix) (Exp $ Const i)

instance (Elt e, Slice (Plain ix), Lift Exp ix) => Lift Exp (ix :. Exp e) where
  type Plain (ix :. Exp e) = Plain ix :. e
  lift (ix:.i) = Exp $ IndexCons (lift ix) i

instance (Elt e, Slice (Plain ix), Unlift Exp ix) => Unlift Exp (ix :. Exp e) where
  unlift e = unlift (Exp $ IndexTail e) :. Exp (IndexHead e)

instance (Elt e, Slice ix) => Unlift Exp (Exp ix :. Exp e) where
  unlift e = (Exp $ IndexTail e) :. Exp (IndexHead e)

instance Shape sh => Lift Exp (Any sh) where
 type Plain (Any sh) = Any sh
 lift Any = Exp $ IndexAny

-- instances for numeric types

instance Lift Exp Int where
  type Plain Int = Int
  lift = Exp . Const

instance Lift Exp Int8 where
  type Plain Int8 = Int8
  lift = Exp . Const

instance Lift Exp Int16 where
  type Plain Int16 = Int16
  lift = Exp . Const

instance Lift Exp Int32 where
  type Plain Int32 = Int32
  lift = Exp . Const

instance Lift Exp Int64 where
  type Plain Int64 = Int64
  lift = Exp . Const

instance Lift Exp Word where
  type Plain Word = Word
  lift = Exp . Const

instance Lift Exp Word8 where
  type Plain Word8 = Word8
  lift = Exp . Const

instance Lift Exp Word16 where
  type Plain Word16 = Word16
  lift = Exp . Const

instance Lift Exp Word32 where
  type Plain Word32 = Word32
  lift = Exp . Const

instance Lift Exp Word64 where
  type Plain Word64 = Word64
  lift = Exp . Const

instance Lift Exp CShort where
  type Plain CShort = CShort
  lift = Exp . Const

instance Lift Exp CUShort where
  type Plain CUShort = CUShort
  lift = Exp . Const

instance Lift Exp CInt where
  type Plain CInt = CInt
  lift = Exp . Const

instance Lift Exp CUInt where
  type Plain CUInt = CUInt
  lift = Exp . Const

instance Lift Exp CLong where
  type Plain CLong = CLong
  lift = Exp . Const

instance Lift Exp CULong where
  type Plain CULong = CULong
  lift = Exp . Const

instance Lift Exp CLLong where
  type Plain CLLong = CLLong
  lift = Exp . Const

instance Lift Exp CULLong where
  type Plain CULLong = CULLong
  lift = Exp . Const

instance Lift Exp Float where
  type Plain Float = Float
  lift = Exp . Const

instance Lift Exp Double where
  type Plain Double = Double
  lift = Exp . Const

instance Lift Exp CFloat where
  type Plain CFloat = CFloat
  lift = Exp . Const

instance Lift Exp CDouble where
  type Plain CDouble = CDouble
  lift = Exp . Const

instance Lift Exp Bool where
  type Plain Bool = Bool
  lift = Exp . Const

instance Lift Exp Char where
  type Plain Char = Char
  lift = Exp . Const

instance Lift Exp CChar where
  type Plain CChar = CChar
  lift = Exp . Const

instance Lift Exp CSChar where
  type Plain CSChar = CSChar
  lift = Exp . Const

instance Lift Exp CUChar where
  type Plain CUChar = CUChar
  lift = Exp . Const

-- Instances for tuples

instance (Lift Exp a, Lift Exp b, Elt (Plain a), Elt (Plain b)) => Lift Exp (a, b) where
  type Plain (a, b) = (Plain a, Plain b)
  lift (x, y) = tup2 (lift x, lift y)

instance (Elt a, Elt b) => Unlift Exp (Exp a, Exp b) where
  unlift = untup2

instance (Lift Exp a, Lift Exp b, Lift Exp c,
          Elt (Plain a), Elt (Plain b), Elt (Plain c))
  => Lift Exp (a, b, c) where
  type Plain (a, b, c) = (Plain a, Plain b, Plain c)
  lift (x, y, z) = tup3 (lift x, lift y, lift z)

instance (Elt a, Elt b, Elt c) => Unlift Exp (Exp a, Exp b, Exp c) where
  unlift = untup3

instance (Lift Exp a, Lift Exp b, Lift Exp c, Lift Exp d,
          Elt (Plain a), Elt (Plain b), Elt (Plain c), Elt (Plain d))
  => Lift Exp (a, b, c, d) where
  type Plain (a, b, c, d) = (Plain a, Plain b, Plain c, Plain d)
  lift (x, y, z, u) = tup4 (lift x, lift y, lift z, lift u)

instance (Elt a, Elt b, Elt c, Elt d) => Unlift Exp (Exp a, Exp b, Exp c, Exp d) where
  unlift = untup4

instance (Lift Exp a, Lift Exp b, Lift Exp c, Lift Exp d, Lift Exp e,
          Elt (Plain a), Elt (Plain b), Elt (Plain c), Elt (Plain d), Elt (Plain e))
  => Lift Exp (a, b, c, d, e) where
  type Plain (a, b, c, d, e) = (Plain a, Plain b, Plain c, Plain d, Plain e)
  lift (x, y, z, u, v) = tup5 (lift x, lift y, lift z, lift u, lift v)

instance (Elt a, Elt b, Elt c, Elt d, Elt e)
  => Unlift Exp (Exp a, Exp b, Exp c, Exp d, Exp e) where
  unlift = untup5

instance (Lift Exp a, Lift Exp b, Lift Exp c, Lift Exp d, Lift Exp e, Lift Exp f,
          Elt (Plain a), Elt (Plain b), Elt (Plain c), Elt (Plain d), Elt (Plain e), Elt (Plain f))
  => Lift Exp (a, b, c, d, e, f) where
  type Plain (a, b, c, d, e, f) = (Plain a, Plain b, Plain c, Plain d, Plain e, Plain f)
  lift (x, y, z, u, v, w) = tup6 (lift x, lift y, lift z, lift u, lift v, lift w)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f)
  => Unlift Exp (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f) where
  unlift = untup6

instance (Lift Exp a, Lift Exp b, Lift Exp c, Lift Exp d, Lift Exp e, Lift Exp f, Lift Exp g,
          Elt (Plain a), Elt (Plain b), Elt (Plain c), Elt (Plain d), Elt (Plain e), Elt (Plain f),
          Elt (Plain g))
  => Lift Exp (a, b, c, d, e, f, g) where
  type Plain (a, b, c, d, e, f, g) = (Plain a, Plain b, Plain c, Plain d, Plain e, Plain f, Plain g)
  lift (x, y, z, u, v, w, r) = tup7 (lift x, lift y, lift z, lift u, lift v, lift w, lift r)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g)
  => Unlift Exp (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g) where
  unlift = untup7

instance (Lift Exp a, Lift Exp b, Lift Exp c, Lift Exp d, Lift Exp e, Lift Exp f, Lift Exp g, Lift Exp h,
          Elt (Plain a), Elt (Plain b), Elt (Plain c), Elt (Plain d), Elt (Plain e), Elt (Plain f),
          Elt (Plain g), Elt (Plain h))
  => Lift Exp (a, b, c, d, e, f, g, h) where
  type Plain (a, b, c, d, e, f, g, h)
    = (Plain a, Plain b, Plain c, Plain d, Plain e, Plain f, Plain g, Plain h)
  lift (x, y, z, u, v, w, r, s)
    = tup8 (lift x, lift y, lift z, lift u, lift v, lift w, lift r, lift s)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h)
  => Unlift Exp (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h) where
  unlift = untup8

instance (Lift Exp a, Lift Exp b, Lift Exp c, Lift Exp d, Lift Exp e,
          Lift Exp f, Lift Exp g, Lift Exp h, Lift Exp i,
          Elt (Plain a), Elt (Plain b), Elt (Plain c), Elt (Plain d), Elt (Plain e),
          Elt (Plain f), Elt (Plain g), Elt (Plain h), Elt (Plain i))
  => Lift Exp (a, b, c, d, e, f, g, h, i) where
  type Plain (a, b, c, d, e, f, g, h, i)
    = (Plain a, Plain b, Plain c, Plain d, Plain e, Plain f, Plain g, Plain h, Plain i)
  lift (x, y, z, u, v, w, r, s, t)
    = tup9 (lift x, lift y, lift z, lift u, lift v, lift w, lift r, lift s, lift t)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i)
  => Unlift Exp (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i) where
  unlift = untup9

-- Instance for scalar Accelerate expressions

instance Lift Exp (Exp e) where
  type Plain (Exp e) = e
  lift = id


-- Instance for Accelerate array computations

instance Lift Acc (Acc a) where
  type Plain (Acc a) = a
  lift = id

-- Instances for Arrays class

--instance Lift Acc () where
--  type Plain () = ()
--  lift _ = Acc (Atuple NilAtup)

instance (Shape sh, Elt e) => Lift Acc (Array sh e) where
  type Plain (Array sh e) = Array sh e
  lift = Acc . Use

instance (Lift Acc a, Lift Acc b, Arrays (Plain a), Arrays (Plain b)) => Lift Acc (a, b) where
  type Plain (a, b) = (Plain a, Plain b)
  lift (x, y) = atup2 (lift x, lift y)

instance (Arrays a, Arrays b) => Unlift Acc (Acc a, Acc b) where
  unlift = unatup2

instance (Lift Acc a, Lift Acc b, Lift Acc c,
          Arrays (Plain a), Arrays (Plain b), Arrays (Plain c))
  => Lift Acc (a, b, c) where
  type Plain (a, b, c) = (Plain a, Plain b, Plain c)
  lift (x, y, z) = atup3 (lift x, lift y, lift z)

instance (Arrays a, Arrays b, Arrays c) => Unlift Acc (Acc a, Acc b, Acc c) where
  unlift = unatup3

instance (Lift Acc a, Lift Acc b, Lift Acc c, Lift Acc d,
          Arrays (Plain a), Arrays (Plain b), Arrays (Plain c), Arrays (Plain d))
  => Lift Acc (a, b, c, d) where
  type Plain (a, b, c, d) = (Plain a, Plain b, Plain c, Plain d)
  lift (x, y, z, u) = atup4 (lift x, lift y, lift z, lift u)

instance (Arrays a, Arrays b, Arrays c, Arrays d) => Unlift Acc (Acc a, Acc b, Acc c, Acc d) where
  unlift = unatup4

instance (Lift Acc a, Lift Acc b, Lift Acc c, Lift Acc d, Lift Acc e,
          Arrays (Plain a), Arrays (Plain b), Arrays (Plain c), Arrays (Plain d), Arrays (Plain e))
  => Lift Acc (a, b, c, d, e) where
  type Plain (a, b, c, d, e) = (Plain a, Plain b, Plain c, Plain d, Plain e)
  lift (x, y, z, u, v) = atup5 (lift x, lift y, lift z, lift u, lift v)

instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e)
  => Unlift Acc (Acc a, Acc b, Acc c, Acc d, Acc e) where
  unlift = unatup5

instance (Lift Acc a, Lift Acc b, Lift Acc c, Lift Acc d, Lift Acc e, Lift Acc f,
          Arrays (Plain a), Arrays (Plain b), Arrays (Plain c), Arrays (Plain d), Arrays (Plain e), Arrays (Plain f))
  => Lift Acc (a, b, c, d, e, f) where
  type Plain (a, b, c, d, e, f) = (Plain a, Plain b, Plain c, Plain d, Plain e, Plain f)
  lift (x, y, z, u, v, w) = atup6 (lift x, lift y, lift z, lift u, lift v, lift w)

instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f)
  => Unlift Acc (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f) where
  unlift = unatup6

instance (Lift Acc a, Lift Acc b, Lift Acc c, Lift Acc d, Lift Acc e, Lift Acc f, Lift Acc g,
          Arrays (Plain a), Arrays (Plain b), Arrays (Plain c), Arrays (Plain d), Arrays (Plain e), Arrays (Plain f),
          Arrays (Plain g))
  => Lift Acc (a, b, c, d, e, f, g) where
  type Plain (a, b, c, d, e, f, g) = (Plain a, Plain b, Plain c, Plain d, Plain e, Plain f, Plain g)
  lift (x, y, z, u, v, w, r) = atup7 (lift x, lift y, lift z, lift u, lift v, lift w, lift r)

instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g)
  => Unlift Acc (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g) where
  unlift = unatup7

instance (Lift Acc a, Lift Acc b, Lift Acc c, Lift Acc d, Lift Acc e, Lift Acc f, Lift Acc g, Lift Acc h,
          Arrays (Plain a), Arrays (Plain b), Arrays (Plain c), Arrays (Plain d), Arrays (Plain e), Arrays (Plain f),
          Arrays (Plain g), Arrays (Plain h))
  => Lift Acc (a, b, c, d, e, f, g, h) where
  type Plain (a, b, c, d, e, f, g, h)
    = (Plain a, Plain b, Plain c, Plain d, Plain e, Plain f, Plain g, Plain h)
  lift (x, y, z, u, v, w, r, s)
    = atup8 (lift x, lift y, lift z, lift u, lift v, lift w, lift r, lift s)

instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h)
  => Unlift Acc (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h) where
  unlift = unatup8

instance (Lift Acc a, Lift Acc b, Lift Acc c, Lift Acc d, Lift Acc e,
          Lift Acc f, Lift Acc g, Lift Acc h, Lift Acc i,
          Arrays (Plain a), Arrays (Plain b), Arrays (Plain c), Arrays (Plain d), Arrays (Plain e),
          Arrays (Plain f), Arrays (Plain g), Arrays (Plain h), Arrays (Plain i))
  => Lift Acc (a, b, c, d, e, f, g, h, i) where
  type Plain (a, b, c, d, e, f, g, h, i)
    = (Plain a, Plain b, Plain c, Plain d, Plain e, Plain f, Plain g, Plain h, Plain i)
  lift (x, y, z, u, v, w, r, s, t)
    = atup9 (lift x, lift y, lift z, lift u, lift v, lift w, lift r, lift s, lift t)

instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i)
  => Unlift Acc (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h, Acc i) where
  unlift = unatup9



-- |Lift a unary function into 'Exp'.
--
lift1 :: (Unlift Exp e1, Lift Exp e2)
      => (e1 -> e2)
      -> Exp (Plain e1)
      -> Exp (Plain e2)
lift1 f = lift . f . unlift

-- |Lift a binary function into 'Exp'.
--
lift2 :: (Unlift Exp e1, Unlift Exp e2, Lift Exp e3)
      => (e1 -> e2 -> e3)
      -> Exp (Plain e1)
      -> Exp (Plain e2)
      -> Exp (Plain e3)
lift2 f x y = lift $ f (unlift x) (unlift y)

-- |Lift a unary function to a computation over rank-1 indices.
--
ilift1 :: (Exp Int -> Exp Int) -> Exp DIM1 -> Exp DIM1
ilift1 f = lift1 (\(Z:.i) -> Z :. f i)

-- |Lift a binary function to a computation over rank-1 indices.
--
ilift2 :: (Exp Int -> Exp Int -> Exp Int) -> Exp DIM1 -> Exp DIM1 -> Exp DIM1
ilift2 f = lift2 (\(Z:.i) (Z:.j) -> Z :. f i j)


-- Tuples
-- ------

-- |Extract the first component of a pair.
--
fst :: forall f a b. Unlift f (f a, f b) => f (Plain (f a), Plain (f b)) -> f a
fst e = let (x, _:: f b) = unlift e in x

-- |Extract the second component of a pair.
--
snd :: forall f a b. Unlift f (f a, f b) => f (Plain (f a), Plain (f b)) -> f b
snd e = let (_::f a, y) = unlift e in y

-- |Converts an uncurried function to a curried function.
--
curry :: Lift f (f a, f b) => (f (Plain (f a), Plain (f b)) -> f c) -> f a -> f b -> f c
curry f x y = f (lift (x, y))

-- |Converts a curried function to a function on pairs.
--
uncurry :: Unlift f (f a, f b) => (f a -> f b -> f c) -> f (Plain (f a), Plain (f b)) -> f c
uncurry f t = let (x, y) = unlift t in f x y


-- Shapes and indices
-- ------------------

-- |The one index for a rank-0 array.
--
index0 :: Exp Z
index0 = lift Z

-- |Turn an 'Int' expression into a rank-1 indexing expression.
--
index1 :: Elt i => Exp i -> Exp (Z :. i)
index1 i = lift (Z :. i)

-- |Turn a rank-1 indexing expression into an 'Int' expression.
--
unindex1 :: Elt i => Exp (Z :. i) -> Exp i
unindex1 ix = let Z :. i = unlift ix in i

-- | Creates a rank-2 index from two Exp Int`s
--
index2 :: (Elt i, Slice (Z :. i))
       => Exp i
       -> Exp i
       -> Exp (Z :. i :. i)
index2 i j = lift (Z :. i :. j)

-- | Destructs a rank-2 index to an Exp tuple of two Int`s.
--
unindex2 :: forall i. (Elt i, Slice (Z :. i))
         => Exp (Z :. i :. i)
         -> Exp (i, i)
unindex2 ix
  = let Z :. i :. j = unlift ix :: Z :. Exp i :. Exp i
    in  lift (i, j)

-- Array operations with a scalar result
-- -------------------------------------

-- |Extraction of the element in a singleton array
--
the :: Elt e => Acc (Scalar e) -> Exp e
the = (!index0)

-- |Test whether an array is empty
--
null :: (Shape ix, Elt e) => Acc (Array ix e) -> Exp Bool
null arr = size arr ==* 0

