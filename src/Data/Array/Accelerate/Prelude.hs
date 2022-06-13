{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}   -- pattern synonyms
-- |
-- Module      : Data.Array.Accelerate.Prelude
-- Copyright   : [2009..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Standard functions that are not part of the core set (directly represented in
-- the AST), but are instead implemented in terms of the core set.
--

module Data.Array.Accelerate.Prelude (

  -- * Element-wise operations
  indexed,
  imap,

  -- * Zipping
  zipWith3, zipWith4, zipWith5, zipWith6, zipWith7, zipWith8, zipWith9,
  izipWith, izipWith3, izipWith4, izipWith5, izipWith6, izipWith7, izipWith8, izipWith9,
  zip, zip3, zip4, zip5, zip6, zip7, zip8, zip9,

  -- * Unzipping
  unzip, unzip3, unzip4, unzip5, unzip6, unzip7, unzip8, unzip9,

  -- * Reductions
  foldAll, fold1All,
  foldSeg, fold1Seg,

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
  (++), concatOn,

  -- * Working with predicates
  -- ** Filtering
  filter, compact,

  -- ** Scatter / Gather
  scatter, scatterIf,
  gather,  gatherIf,

  -- * Permutations
  reverse, transpose,
  reverseOn, transposeOn,

  -- * Extracting sub-vectors
  init, tail, take, drop, slit,
  initOn, tailOn, takeOn, dropOn, slitOn,

  -- * Controlling execution
  compute,

  -- * Flow control
  IfThenElse(..),

  -- ** Array-level
  (?|),

  -- ** Expression-level
  (?), match,

  -- * Scalar iteration
  iterate,

  -- * Scalar reduction
  sfoldl, -- sfoldr,

  -- * Lifting and unlifting
  Lift(..), Unlift(..),
  lift1, lift2, lift3, ilift1, ilift2, ilift3,

  -- ** Tuple construction and destruction
  fst, afst, snd, asnd, curry, uncurry,

  -- ** Index construction and destruction
  index0, index1, unindex1, index2, unindex2, index3, unindex3,

  -- * Array operations with a scalar result
  the, null, length,

  -- * Irregular data-parallelism
  expand,

  -- * Sequence operations
  -- fromSeq, fromSeqElems, fromSeqShapes, toSeqInner, toSeqOuter2, toSeqOuter3, generateSeq,

) where

import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Language
import Data.Array.Accelerate.Lift
import Data.Array.Accelerate.Pattern
import Data.Array.Accelerate.Pattern.Maybe
import Data.Array.Accelerate.Pattern.Shape
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Array                            ( Arrays, Array, Scalar, Vector, Segments,  fromList )
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Sugar.Shape                            ( Shape, Slice, DIM1, DIM2, empty )
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.Classes.Eq
import Data.Array.Accelerate.Classes.FromIntegral
import Data.Array.Accelerate.Classes.Integral
import Data.Array.Accelerate.Classes.Num
import Data.Array.Accelerate.Classes.Ord

import Data.Array.Accelerate.Data.Bits

import Lens.Micro                                                   ( Lens', (&), (^.), (.~), (+~), (-~), lens, over )
import Prelude                                                      ( (.), ($), Maybe(..), const, id, flip )


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Data.Array.Accelerate
-- >>> import Data.Array.Accelerate.Interpreter
-- >>> :{
--   let runExp :: Elt e => Exp e -> e
--       runExp e = indexArray (run (unit e)) Z
-- :}

-- Element-wise operations
-- -----------------------

-- | Pair each element with its index
--
-- >>> let xs = fromList (Z:.5) [0..] :: Vector Float
-- >>> run $ indexed (use xs)
-- Vector (Z :. 5) [(Z :. 0,0.0),(Z :. 1,1.0),(Z :. 2,2.0),(Z :. 3,3.0),(Z :. 4,4.0)]
--
-- >>> let mat = fromList (Z:.3:.4) [0..] :: Matrix Float
-- >>> run $ indexed (use mat)
-- Matrix (Z :. 3 :. 4)
--   [ (Z :. 0 :. 0,0.0), (Z :. 0 :. 1,1.0),  (Z :. 0 :. 2,2.0),  (Z :. 0 :. 3,3.0),
--     (Z :. 1 :. 0,4.0), (Z :. 1 :. 1,5.0),  (Z :. 1 :. 2,6.0),  (Z :. 1 :. 3,7.0),
--     (Z :. 2 :. 0,8.0), (Z :. 2 :. 1,9.0), (Z :. 2 :. 2,10.0), (Z :. 2 :. 3,11.0)]
--
indexed :: (Shape sh, Elt a) => Acc (Array sh a) -> Acc (Array sh (sh, a))
indexed = imap T2

-- | Apply a function to every element of an array and its index
--
imap :: (Shape sh, Elt a, Elt b)
     => (Exp sh -> Exp a -> Exp b)
     -> Acc (Array sh a)
     -> Acc (Array sh b)
imap f xs = zipWith f (generate (shape xs) id) xs

-- | Used to define the zipWith functions on more than two arrays
--
zipWithInduction
    :: (Shape sh, Elt a, Elt b)
    => ((Exp (a,b) -> rest) -> Acc (Array sh (a,b)) -> result) -- The zipWith function operating on one fewer array
    -> (Exp a -> Exp b -> rest)
    -> Acc (Array sh a)
    -> Acc (Array sh b)
    -> result
zipWithInduction prev f as bs = prev (\(T2 a b) -> f a b) (zip as bs)


-- | Zip three arrays with the given function, analogous to 'zipWith'.
--
zipWith3
    :: (Shape sh, Elt a, Elt b, Elt c, Elt d)
    => (Exp a -> Exp b -> Exp c -> Exp d)
    -> Acc (Array sh a)
    -> Acc (Array sh b)
    -> Acc (Array sh c)
    -> Acc (Array sh d)
zipWith3 = zipWithInduction zipWith

-- | Zip four arrays with the given function, analogous to 'zipWith'.
--
zipWith4
    :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e)
    => (Exp a -> Exp b -> Exp c -> Exp d -> Exp e)
    -> Acc (Array sh a)
    -> Acc (Array sh b)
    -> Acc (Array sh c)
    -> Acc (Array sh d)
    -> Acc (Array sh e)
zipWith4 = zipWithInduction zipWith3

-- | Zip five arrays with the given function, analogous to 'zipWith'.
--
zipWith5
    :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f)
    => (Exp a -> Exp b -> Exp c -> Exp d -> Exp e -> Exp f)
    -> Acc (Array sh a)
    -> Acc (Array sh b)
    -> Acc (Array sh c)
    -> Acc (Array sh d)
    -> Acc (Array sh e)
    -> Acc (Array sh f)
zipWith5 = zipWithInduction zipWith4

-- | Zip six arrays with the given function, analogous to 'zipWith'.
--
zipWith6
    :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g)
    => (Exp a -> Exp b -> Exp c -> Exp d -> Exp e -> Exp f -> Exp g)
    -> Acc (Array sh a)
    -> Acc (Array sh b)
    -> Acc (Array sh c)
    -> Acc (Array sh d)
    -> Acc (Array sh e)
    -> Acc (Array sh f)
    -> Acc (Array sh g)
zipWith6 = zipWithInduction zipWith5

-- | Zip seven arrays with the given function, analogous to 'zipWith'.
--
zipWith7
    :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h)
    => (Exp a -> Exp b -> Exp c -> Exp d -> Exp e -> Exp f -> Exp g -> Exp h)
    -> Acc (Array sh a)
    -> Acc (Array sh b)
    -> Acc (Array sh c)
    -> Acc (Array sh d)
    -> Acc (Array sh e)
    -> Acc (Array sh f)
    -> Acc (Array sh g)
    -> Acc (Array sh h)
zipWith7 = zipWithInduction zipWith6

-- | Zip eight arrays with the given function, analogous to 'zipWith'.
--
zipWith8
    :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i)
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
zipWith8 = zipWithInduction zipWith7

-- | Zip nine arrays with the given function, analogous to 'zipWith'.
--
zipWith9
    :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j)
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
zipWith9 = zipWithInduction zipWith8


-- | Used to define the izipWith functions on two or more arrays
--
izipWithInduction
    :: (Shape sh, Elt a, Elt b)
    => ((Exp sh -> Exp (a,b) -> rest) -> Acc (Array sh (a,b)) -> result) -- The zipWith function operating on one fewer array
    -> (Exp sh -> Exp a -> Exp b -> rest)
    -> Acc (Array sh a)
    -> Acc (Array sh b)
    -> result
izipWithInduction prev f as bs = prev (\ix (T2 a b) -> f ix a b) (zip as bs)


-- | Zip two arrays with a function that also takes the element index
--
izipWith
    :: (Shape sh, Elt a, Elt b, Elt c)
    => (Exp sh -> Exp a -> Exp b -> Exp c)
    -> Acc (Array sh a)
    -> Acc (Array sh b)
    -> Acc (Array sh c)
izipWith = izipWithInduction imap

-- | Zip three arrays with a function that also takes the element index,
-- analogous to 'izipWith'.
--
izipWith3
    :: (Shape sh, Elt a, Elt b, Elt c, Elt d)
    => (Exp sh -> Exp a -> Exp b -> Exp c -> Exp d)
    -> Acc (Array sh a)
    -> Acc (Array sh b)
    -> Acc (Array sh c)
    -> Acc (Array sh d)
izipWith3 = izipWithInduction izipWith

-- | Zip four arrays with the given function that also takes the element index,
-- analogous to 'zipWith'.
--
izipWith4
    :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e)
    => (Exp sh -> Exp a -> Exp b -> Exp c -> Exp d -> Exp e)
    -> Acc (Array sh a)
    -> Acc (Array sh b)
    -> Acc (Array sh c)
    -> Acc (Array sh d)
    -> Acc (Array sh e)
izipWith4 = izipWithInduction izipWith3

-- | Zip five arrays with the given function that also takes the element index,
-- analogous to 'zipWith'.
--
izipWith5
    :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f)
    => (Exp sh -> Exp a -> Exp b -> Exp c -> Exp d -> Exp e -> Exp f)
    -> Acc (Array sh a)
    -> Acc (Array sh b)
    -> Acc (Array sh c)
    -> Acc (Array sh d)
    -> Acc (Array sh e)
    -> Acc (Array sh f)
izipWith5 = izipWithInduction izipWith4

-- | Zip six arrays with the given function that also takes the element index,
-- analogous to 'zipWith'.
--
izipWith6
    :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g)
    => (Exp sh -> Exp a -> Exp b -> Exp c -> Exp d -> Exp e -> Exp f -> Exp g)
    -> Acc (Array sh a)
    -> Acc (Array sh b)
    -> Acc (Array sh c)
    -> Acc (Array sh d)
    -> Acc (Array sh e)
    -> Acc (Array sh f)
    -> Acc (Array sh g)
izipWith6 = izipWithInduction izipWith5

-- | Zip seven arrays with the given function that also takes the element
-- index, analogous to 'zipWith'.
--
izipWith7
    :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h)
    => (Exp sh -> Exp a -> Exp b -> Exp c -> Exp d -> Exp e -> Exp f -> Exp g -> Exp h)
    -> Acc (Array sh a)
    -> Acc (Array sh b)
    -> Acc (Array sh c)
    -> Acc (Array sh d)
    -> Acc (Array sh e)
    -> Acc (Array sh f)
    -> Acc (Array sh g)
    -> Acc (Array sh h)
izipWith7 = izipWithInduction izipWith6

-- | Zip eight arrays with the given function that also takes the element
-- index, analogous to 'zipWith'.
--
izipWith8
    :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i)
    => (Exp sh -> Exp a -> Exp b -> Exp c -> Exp d -> Exp e -> Exp f -> Exp g -> Exp h -> Exp i)
    -> Acc (Array sh a)
    -> Acc (Array sh b)
    -> Acc (Array sh c)
    -> Acc (Array sh d)
    -> Acc (Array sh e)
    -> Acc (Array sh f)
    -> Acc (Array sh g)
    -> Acc (Array sh h)
    -> Acc (Array sh i)
izipWith8 = izipWithInduction izipWith7

-- | Zip nine arrays with the given function that also takes the element index,
-- analogous to 'zipWith'.
--
izipWith9
    :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j)
    => (Exp sh -> Exp a -> Exp b -> Exp c -> Exp d -> Exp e -> Exp f -> Exp g -> Exp h -> Exp i -> Exp j)
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
izipWith9 = izipWithInduction izipWith8


-- | Combine the elements of two arrays pairwise. The shape of the result is the
-- intersection of the two argument shapes.
--
-- >>> let m1 = fromList (Z:.5:.10) [0..] :: Matrix Int
-- >>> let m2 = fromList (Z:.10:.5) [0..] :: Matrix Float
-- >>> run $ zip (use m1) (use m2)
-- Matrix (Z :. 5 :. 5)
--   [   (0,0.0),   (1,1.0),   (2,2.0),   (3,3.0),   (4,4.0),
--      (10,5.0),  (11,6.0),  (12,7.0),  (13,8.0),  (14,9.0),
--     (20,10.0), (21,11.0), (22,12.0), (23,13.0), (24,14.0),
--     (30,15.0), (31,16.0), (32,17.0), (33,18.0), (34,19.0),
--     (40,20.0), (41,21.0), (42,22.0), (43,23.0), (44,24.0)]
--
zip :: (Shape sh, Elt a, Elt b)
    => Acc (Array sh a)
    -> Acc (Array sh b)
    -> Acc (Array sh (a, b))
zip = zipWith T2

-- | Take three arrays and return an array of triples, analogous to zip.
--
zip3 :: (Shape sh, Elt a, Elt b, Elt c)
     => Acc (Array sh a)
     -> Acc (Array sh b)
     -> Acc (Array sh c)
     -> Acc (Array sh (a, b, c))
zip3 = zipWith3 T3

-- | Take four arrays and return an array of quadruples, analogous to zip.
--
zip4 :: (Shape sh, Elt a, Elt b, Elt c, Elt d)
     => Acc (Array sh a)
     -> Acc (Array sh b)
     -> Acc (Array sh c)
     -> Acc (Array sh d)
     -> Acc (Array sh (a, b, c, d))
zip4 = zipWith4 T4

-- | Take five arrays and return an array of five-tuples, analogous to zip.
--
zip5 :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e)
     => Acc (Array sh a)
     -> Acc (Array sh b)
     -> Acc (Array sh c)
     -> Acc (Array sh d)
     -> Acc (Array sh e)
     -> Acc (Array sh (a, b, c, d, e))
zip5 = zipWith5 T5

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
zip6 = zipWith6 T6

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
zip7 = zipWith7 T7

-- | Take eight arrays and return an array of eight-tuples, analogous to zip.
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
zip8 = zipWith8 T8

-- | Take nine arrays and return an array of nine-tuples, analogous to zip.
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
zip9 = zipWith9 T9


-- | The converse of 'zip', but the shape of the two results is identical to the
-- shape of the argument.
--
-- If the argument array is manifest in memory, 'unzip' is a no-op.
--
unzip :: (Shape sh, Elt a, Elt b)
      => Acc (Array sh (a, b))
      -> (Acc (Array sh a), Acc (Array sh b))
unzip arr = (map fst arr, map snd arr)

-- | Take an array of triples and return three arrays, analogous to 'unzip'.
--
unzip3 :: (Shape sh, Elt a, Elt b, Elt c)
       => Acc (Array sh (a, b, c))
       -> (Acc (Array sh a), Acc (Array sh b), Acc (Array sh c))
unzip3 xs = (map get1 xs, map get2 xs, map get3 xs)
  where
    get1 (T3 a _ _) = a
    get2 (T3 _ b _) = b
    get3 (T3 _ _ c) = c


-- | Take an array of quadruples and return four arrays, analogous to 'unzip'.
--
unzip4 :: (Shape sh, Elt a, Elt b, Elt c, Elt d)
       => Acc (Array sh (a, b, c, d))
       -> (Acc (Array sh a), Acc (Array sh b), Acc (Array sh c), Acc (Array sh d))
unzip4 xs = (map get1 xs, map get2 xs, map get3 xs, map get4 xs)
  where
    get1 (T4 a _ _ _) = a
    get2 (T4 _ b _ _) = b
    get3 (T4 _ _ c _) = c
    get4 (T4 _ _ _ d) = d

-- | Take an array of 5-tuples and return five arrays, analogous to 'unzip'.
--
unzip5 :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e)
       => Acc (Array sh (a, b, c, d, e))
       -> (Acc (Array sh a), Acc (Array sh b), Acc (Array sh c), Acc (Array sh d), Acc (Array sh e))
unzip5 xs = (map get1 xs, map get2 xs, map get3 xs, map get4 xs, map get5 xs)
  where
    get1 (T5 a _ _ _ _) = a
    get2 (T5 _ b _ _ _) = b
    get3 (T5 _ _ c _ _) = c
    get4 (T5 _ _ _ d _) = d
    get5 (T5 _ _ _ _ e) = e

-- | Take an array of 6-tuples and return six arrays, analogous to 'unzip'.
--
unzip6 :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f)
       => Acc (Array sh (a, b, c, d, e, f))
       -> ( Acc (Array sh a), Acc (Array sh b), Acc (Array sh c)
          , Acc (Array sh d), Acc (Array sh e), Acc (Array sh f))
unzip6 xs = (map get1 xs, map get2 xs, map get3 xs, map get4 xs, map get5 xs, map get6 xs)
  where
    get1 (T6 a _ _ _ _ _) = a
    get2 (T6 _ b _ _ _ _) = b
    get3 (T6 _ _ c _ _ _) = c
    get4 (T6 _ _ _ d _ _) = d
    get5 (T6 _ _ _ _ e _) = e
    get6 (T6 _ _ _ _ _ f) = f

-- | Take an array of 7-tuples and return seven arrays, analogous to 'unzip'.
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
    get1 (T7 a _ _ _ _ _ _) = a
    get2 (T7 _ b _ _ _ _ _) = b
    get3 (T7 _ _ c _ _ _ _) = c
    get4 (T7 _ _ _ d _ _ _) = d
    get5 (T7 _ _ _ _ e _ _) = e
    get6 (T7 _ _ _ _ _ f _) = f
    get7 (T7 _ _ _ _ _ _ g) = g

-- | Take an array of 8-tuples and return eight arrays, analogous to 'unzip'.
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
    get1 (T8 a _ _ _ _ _ _ _) = a
    get2 (T8 _ b _ _ _ _ _ _) = b
    get3 (T8 _ _ c _ _ _ _ _) = c
    get4 (T8 _ _ _ d _ _ _ _) = d
    get5 (T8 _ _ _ _ e _ _ _) = e
    get6 (T8 _ _ _ _ _ f _ _) = f
    get7 (T8 _ _ _ _ _ _ g _) = g
    get8 (T8 _ _ _ _ _ _ _ h) = h

-- | Take an array of 9-tuples and return nine arrays, analogous to 'unzip'.
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
    get1 (T9 a _ _ _ _ _ _ _ _) = a
    get2 (T9 _ b _ _ _ _ _ _ _) = b
    get3 (T9 _ _ c _ _ _ _ _ _) = c
    get4 (T9 _ _ _ d _ _ _ _ _) = d
    get5 (T9 _ _ _ _ e _ _ _ _) = e
    get6 (T9 _ _ _ _ _ f _ _ _) = f
    get7 (T9 _ _ _ _ _ _ g _ _) = g
    get8 (T9 _ _ _ _ _ _ _ h _) = h
    get9 (T9 _ _ _ _ _ _ _ _ i) = i


-- Reductions
-- ----------

-- | Reduction of an array of arbitrary rank to a single scalar value. The first
-- argument needs to be an /associative/ function to enable efficient parallel
-- implementation. The initial element does not need to be an identity element.
--
-- >>> let vec = fromList (Z:.10) [0..] :: Vector Float
-- >>> run $ foldAll (+) 42 (use vec)
-- Scalar Z [87.0]
--
-- >>> let mat = fromList (Z:.5:.10) [0..] :: Matrix Float
-- >>> run $ foldAll (+) 0 (use mat)
-- Scalar Z [1225.0]
--
foldAll
    :: (Shape sh, Elt a)
    => (Exp a -> Exp a -> Exp a)
    -> Exp a
    -> Acc (Array sh a)
    -> Acc (Scalar a)
foldAll f e arr = fold f e (flatten arr)

-- | Variant of 'foldAll' that requires the reduced array to be non-empty and
-- does not need a default value. The first argument must be an /associative/
-- function.
--
fold1All
    :: (Shape sh, Elt a)
    => (Exp a -> Exp a -> Exp a)
    -> Acc (Array sh a)
    -> Acc (Scalar a)
fold1All f arr = fold1 f (flatten arr)


-- | Segmented reduction along the innermost dimension of an array. The segment
-- descriptor specifies the lengths of the logical sub-arrays, each of which is
-- reduced independently. The innermost dimension must contain at least as many
-- elements as required by the segment descriptor (sum thereof).
--
-- >>> let seg = fromList (Z:.4) [1,4,0,3] :: Segments Int
-- >>> seg
-- Vector (Z :. 4) [1,4,0,3]
--
-- >>> let mat = fromList (Z:.5:.10) [0..] :: Matrix Int
-- >>> mat
-- Matrix (Z :. 5 :. 10)
--   [  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
--     10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
--     20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
--     30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
--     40, 41, 42, 43, 44, 45, 46, 47, 48, 49]
--
-- >>> run $ foldSeg (+) 0 (use mat) (use seg)
-- Matrix (Z :. 5 :. 4)
--   [  0,  10, 0,  18,
--     10,  50, 0,  48,
--     20,  90, 0,  78,
--     30, 130, 0, 108,
--     40, 170, 0, 138]
--
foldSeg
    :: forall sh e i. (Shape sh, Elt e, Num i, IsSingleIntegral (EltR i))
    => (Exp e -> Exp e -> Exp e)
    -> Exp e
    -> Acc (Array (sh:.Int) e)
    -> Acc (Segments i)
    -> Acc (Array (sh:.Int) e)
foldSeg f z arr seg = foldSeg' f z arr (scanl plus zero seg)
  where
    (plus, zero) =
      case singleIntegralType @(EltR i) of
        TypeInt8{}    -> ((+), 0)
        TypeInt16{}   -> ((+), 0)
        TypeInt32{}   -> ((+), 0)
        TypeInt64{}   -> ((+), 0)
        TypeInt128{}  -> ((+), 0)
        TypeWord8{}   -> ((+), 0)
        TypeWord16{}  -> ((+), 0)
        TypeWord32{}  -> ((+), 0)
        TypeWord64{}  -> ((+), 0)
        TypeWord128{} -> ((+), 0)


-- | Variant of 'foldSeg' that requires /all/ segments of the reduced array
-- to be non-empty, and does not need a default value. The segment
-- descriptor species the length of each of the logical sub-arrays.
--
fold1Seg
    :: forall sh e i. (Shape sh, Elt e, Num i, IsSingleIntegral (EltR i))
    => (Exp e -> Exp e -> Exp e)
    -> Acc (Array (sh:.Int) e)
    -> Acc (Segments i)
    -> Acc (Array (sh:.Int) e)
fold1Seg f arr seg = fold1Seg' f arr (scanl plus zero seg)
  where
    plus :: Exp i -> Exp i -> Exp i
    zero :: Exp i
    (plus, zero) =
      case singleIntegralType @(EltR i) of
        TypeInt8{}    -> ((+), 0)
        TypeInt16{}   -> ((+), 0)
        TypeInt32{}   -> ((+), 0)
        TypeInt64{}   -> ((+), 0)
        TypeInt128{}  -> ((+), 0)
        TypeWord8{}   -> ((+), 0)
        TypeWord16{}  -> ((+), 0)
        TypeWord32{}  -> ((+), 0)
        TypeWord64{}  -> ((+), 0)
        TypeWord128{} -> ((+), 0)


-- Specialised reductions
-- ----------------------
--
-- Leave the results of these as scalar arrays to make it clear that these are
-- array computations, and thus can not be nested.

-- | Check if all elements along the innermost dimension satisfy a predicate.
--
-- >>> let mat = fromList (Z :. 4 :. 10) [1,2,3,4,5,6,7,8,9,10,1,1,1,1,1,2,2,2,2,2,2,4,6,8,10,12,14,16,18,20,1,3,5,7,9,11,13,15,17,19] :: Matrix Int
-- >>> mat
-- Matrix (Z :. 4 :. 10)
--   [ 1, 2, 3, 4,  5,  6,  7,  8,  9, 10,
--     1, 1, 1, 1,  1,  2,  2,  2,  2,  2,
--     2, 4, 6, 8, 10, 12, 14, 16, 18, 20,
--     1, 3, 5, 7,  9, 11, 13, 15, 17, 19]
--
-- >>> run $ all even (use mat)
-- Vector (Z :. 4) [False,False,True,False]
--
all :: (Shape sh, Elt e)
    => (Exp e -> Exp Bool)
    -> Acc (Array (sh:.Int) e)
    -> Acc (Array sh Bool)
all f = and . map f

-- | Check if any element along the innermost dimension satisfies the predicate.
--
-- >>> let mat = fromList (Z :. 4 :. 10) [1,2,3,4,5,6,7,8,9,10,1,1,1,1,1,2,2,2,2,2,2,4,6,8,10,12,14,16,18,20,1,3,5,7,9,11,13,15,17,19] :: Matrix Int
-- >>> mat
-- Matrix (Z :. 4 :. 10)
--   [ 1, 2, 3, 4,  5,  6,  7,  8,  9, 10,
--     1, 1, 1, 1,  1,  2,  2,  2,  2,  2,
--     2, 4, 6, 8, 10, 12, 14, 16, 18, 20,
--     1, 3, 5, 7,  9, 11, 13, 15, 17, 19]
--
-- >>> run $ any even (use mat)
-- Vector (Z :. 4) [True,True,True,False]
--
any :: (Shape sh, Elt e)
    => (Exp e -> Exp Bool)
    -> Acc (Array (sh:.Int) e)
    -> Acc (Array sh Bool)
any f = or . map f

-- | Check if all elements along the innermost dimension are 'True'.
--
and :: Shape sh
    => Acc (Array (sh:.Int) Bool)
    -> Acc (Array sh Bool)
and = fold (&&) True_

-- | Check if any element along the innermost dimension is 'True'.
--
or :: Shape sh
   => Acc (Array (sh:.Int) Bool)
   -> Acc (Array sh Bool)
or = fold (||) False_

-- | Compute the sum of elements along the innermost dimension of the array. To
-- find the sum of the entire array, 'flatten' it first.
--
-- >>> let mat = fromList (Z:.2:.5) [0..] :: Matrix Int
-- >>> run $ sum (use mat)
-- Vector (Z :. 2) [10,35]
--
sum :: (Shape sh, Num e)
    => Acc (Array (sh:.Int) e)
    -> Acc (Array sh e)
sum = fold (+) 0

-- | Compute the product of the elements along the innermost dimension of the
-- array. To find the product of the entire array, 'flatten' it first.
--
-- >>> let mat = fromList (Z:.2:.5) [0..] :: Matrix Int
-- >>> run $ product (use mat)
-- Vector (Z :. 2) [0,15120]
--
product
    :: (Shape sh, Num e)
    => Acc (Array (sh:.Int) e)
    -> Acc (Array sh e)
product = fold (*) 1

-- | Yield the minimum element along the innermost dimension of the array. To
-- find find the minimum element of the entire array, 'flatten' it first.
--
-- The array must not be empty. See also 'fold1'.
--
-- >>> let mat = fromList (Z :. 3 :. 4) [1,4,3,8, 0,2,8,4, 7,9,8,8] :: Matrix Int
-- >>> mat
-- Matrix (Z :. 3 :. 4)
--   [ 1, 4, 3, 8,
--     0, 2, 8, 4,
--     7, 9, 8, 8]
--
-- >>> run $ minimum (use mat)
-- Vector (Z :. 3) [1,0,7]
--
minimum
    :: (Shape sh, Ord e)
    => Acc (Array (sh:.Int) e)
    -> Acc (Array sh e)
minimum = fold1 min

-- | Yield the maximum element along the innermost dimension of the array. To
-- find the maximum element of the entire array, 'flatten' it first.
--
-- The array must not be empty. See also 'fold1'.
--
-- >>> let mat = fromList (Z :. 3 :. 4) [1,4,3,8, 0,2,8,4, 7,9,8,8] :: Matrix Int
-- >>> mat
-- Matrix (Z :. 3 :. 4)
--   [ 1, 4, 3, 8,
--     0, 2, 8, 4,
--     7, 9, 8, 8]
--
-- >>> run $ maximum (use mat)
-- Vector (Z :. 3) [8,8,9]
--
maximum
    :: (Shape sh, Ord e)
    => Acc (Array (sh:.Int) e)
    -> Acc (Array sh e)
maximum = fold1 max


-- Composite scans
-- ---------------

-- | Left-to-right pre-scan (aka exclusive scan). As for 'scan', the first
-- argument must be an /associative/ function. Denotationally, we have:
--
-- > prescanl f e = afst . scanl' f e
--
-- >>> let vec = fromList (Z:.10) [1..10] :: Vector Int
-- >>> run $ prescanl (+) 0 (use vec)
-- Vector (Z :. 10) [0,1,3,6,10,15,21,28,36,45]
--
prescanl
    :: (Shape sh, Elt a)
    => (Exp a -> Exp a -> Exp a)
    -> Exp a
    -> Acc (Array (sh:.Int) a)
    -> Acc (Array (sh:.Int) a)
prescanl f e = afst . scanl' f e

-- | Left-to-right post-scan, a variant of 'scanl1' with an initial value. As
-- with 'scanl1', the array must not be empty. Denotationally, we have:
--
-- > postscanl f e = map (e `f`) . scanl1 f
--
-- >>> let vec = fromList (Z:.10) [1..10] :: Vector Int
-- >>> run $ postscanl (+) 42 (use vec)
-- Vector (Z :. 10) [43,45,48,52,57,63,70,78,87,97]
--
postscanl
    :: (Shape sh, Elt a)
    => (Exp a -> Exp a -> Exp a)
    -> Exp a
    -> Acc (Array (sh:.Int) a)
    -> Acc (Array (sh:.Int) a)
postscanl f e = map (e `f`) . scanl1 f

-- | Right-to-left pre-scan (aka exclusive scan). As for 'scan', the first
-- argument must be an /associative/ function. Denotationally, we have:
--
-- > prescanr f e = afst . scanr' f e
--
prescanr
    :: (Shape sh, Elt a)
    => (Exp a -> Exp a -> Exp a)
    -> Exp a
    -> Acc (Array (sh:.Int) a)
    -> Acc (Array (sh:.Int) a)
prescanr f e = afst . scanr' f e

-- | Right-to-left postscan, a variant of 'scanr1' with an initial value.
-- Denotationally, we have:
--
-- > postscanr f e = map (e `f`) . scanr1 f
--
postscanr
    :: (Shape sh, Elt a)
    => (Exp a -> Exp a -> Exp a)
    -> Exp a
    -> Acc (Array (sh:.Int) a)
    -> Acc (Array (sh:.Int) a)
postscanr f e = map (`f` e) . scanr1 f


-- Segmented scans
-- ---------------

-- | Segmented version of 'scanl' along the innermost dimension of an array. The
-- innermost dimension must have at least as many elements as the sum of the
-- segment descriptor.
--
-- >>> let seg = fromList (Z:.4) [1,4,0,3] :: Segments Int
-- >>> seg
-- Vector (Z :. 4) [1,4,0,3]
--
-- >>> let mat = fromList (Z:.5:.10) [0..] :: Matrix Int
-- >>> mat
-- Matrix (Z :. 5 :. 10)
--   [  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
--     10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
--     20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
--     30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
--     40, 41, 42, 43, 44, 45, 46, 47, 48, 49]
--
-- >>> run $ scanlSeg (+) 0 (use mat) (use seg)
-- Matrix (Z :. 5 :. 12)
--   [ 0,  0, 0,  1,  3,   6,  10, 0, 0,  5, 11,  18,
--     0, 10, 0, 11, 23,  36,  50, 0, 0, 15, 31,  48,
--     0, 20, 0, 21, 43,  66,  90, 0, 0, 25, 51,  78,
--     0, 30, 0, 31, 63,  96, 130, 0, 0, 35, 71, 108,
--     0, 40, 0, 41, 83, 126, 170, 0, 0, 45, 91, 138]
--
scanlSeg
    :: forall sh e i. (Shape sh, Slice sh, Elt e, Integral i, Bits i, FromIntegral i Int)
    => (Exp e -> Exp e -> Exp e)
    -> Exp e
    -> Acc (Array (sh:.Int) e)
    -> Acc (Segments i)
    -> Acc (Array (sh:.Int) e)
scanlSeg f z arr seg =
  if null arr || null flags
    then fill (sh :. sz + length seg) z
    else scanl1Seg f arr' seg'
  where
    -- Segmented exclusive scan is implemented by first injecting the seed
    -- element at the head of each segment, and then performing a segmented
    -- inclusive scan.
    --
    -- This is done by creating a vector entirely of the seed element, and
    -- overlaying the input data in all places other than at the start of
    -- a segment.
    --
    sh :. sz  = shape arr
    seg'      = map (+1) seg
    arr'      = permute const
                        (fill (sh :. sz + length seg) z)
                        (\(sx :. i) -> Just_ (sx :. i + fromIntegral (inc ! I1 i)))
                        (take (length flags) arr)

    -- Each element in the segments must be shifted to the right one additional
    -- place for each successive segment, to make room for the seed element.
    -- Here, we make use of the fact that the vector returned by 'mkHeadFlags'
    -- contains non-unit entries, which indicate zero length segments.
    --
    flags     = mkHeadFlags seg
    inc       = scanl1 (+) flags


-- | Segmented version of 'scanl'' along the innermost dimension of an array. The
-- innermost dimension must have at least as many elements as the sum of the
-- segment descriptor.
--
-- The first element of the resulting tuple is a vector of scanned values. The
-- second element is a vector of segment scan totals and has the same size as
-- the segment vector.
--
-- >>> let seg = fromList (Z:.4) [1,4,0,3] :: Segments Int
-- >>> seg
-- Vector (Z :. 4) [1,4,0,3]
--
-- >>> let mat = fromList (Z:.5:.10) [0..] :: Matrix Int
-- >>> mat
-- Matrix (Z :. 5 :. 10)
--   [  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
--     10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
--     20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
--     30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
--     40, 41, 42, 43, 44, 45, 46, 47, 48, 49]
--
-- >>> let (res,sums) = run $ scanl'Seg (+) 0 (use mat) (use seg)
-- >>> res
-- Matrix (Z :. 5 :. 8)
--   [ 0, 0,  1,  3,   6, 0,  5, 11,
--     0, 0, 11, 23,  36, 0, 15, 31,
--     0, 0, 21, 43,  66, 0, 25, 51,
--     0, 0, 31, 63,  96, 0, 35, 71,
--     0, 0, 41, 83, 126, 0, 45, 91]
-- >>> sums
-- Matrix (Z :. 5 :. 4)
--   [  0,  10, 0,  18,
--     10,  50, 0,  48,
--     20,  90, 0,  78,
--     30, 130, 0, 108,
--     40, 170, 0, 138]
--
scanl'Seg
    :: forall sh e i. (Shape sh, Slice sh, Elt e, Integral i, Bits i, FromIntegral i Int)
    => (Exp e -> Exp e -> Exp e)
    -> Exp e
    -> Acc (Array (sh:.Int) e)
    -> Acc (Segments i)
    -> Acc (Array (sh:.Int) e, Array (sh:.Int) e)
scanl'Seg f z arr seg =
  if null arr
    then T2 arr  (fill (indexTail (shape arr) :. length seg) z)
    else T2 body sums
  where
    -- Segmented scan' is implemented by deconstructing a segmented exclusive
    -- scan, to separate the final value and scan body.
    --
    -- TLM: Segmented scans, and this version in particular, expend a lot of
    --      effort scanning flag arrays. On inspection it appears that several
    --      of these operations are duplicated, but this will not be picked up
    --      by sharing _observation_. Perhaps a global CSE-style pass would be
    --      beneficial.
    --
    arr'        = scanlSeg f z arr seg

    -- Extract the final reduction value for each segment, which is at the last
    -- index of each segment.
    --
    seg'        = map (+1) seg
    tails       = zipWith (+) seg $ prescanl (+) 0 seg'
    sums        = backpermute
                    (indexTail (shape arr') :. length seg)
                    (\(sz :. i) -> sz :. fromIntegral (tails ! I1 i))
                    arr'

    -- Slice out the body of each segment.
    --
    -- Build a head-flags representation based on the original segment
    -- descriptor. This contains the target length of each of the body segments,
    -- which is one fewer element than the actual bodies stored in arr'. Thus,
    -- the flags align with the last element of each body section, and when
    -- scanned, this element will be incremented over.
    --
    offset      = scanl1 (+) seg
    inc         = scanl1 (+)
                $ permute (+) (fill (I1 $ size arr + 1) 0)
                              (\ix -> Just_ (index1' (offset ! ix)))
                              (fill (shape seg) (1 :: Exp i))

    len         = offset ! I1 (length offset - 1)
    body        = backpermute
                    (indexTail (shape arr) :. fromIntegral len)
                    (\(sz :. i) -> sz :. i + fromIntegral (inc ! I1 i))
                    arr'


-- | Segmented version of 'scanl1' along the innermost dimension.
--
-- As with 'scanl1', the total number of elements considered, in this case given
-- by the 'sum' of segment descriptor, must not be zero. The input vector must
-- contain at least this many elements.
--
-- Zero length segments are allowed, and the behaviour is as if those entries
-- were not present in the segment descriptor; that is:
--
-- > scanl1Seg f xs [n,0,0] == scanl1Seg f xs [n]   where n /= 0
--
-- >>> let seg = fromList (Z:.4) [1,4,0,3] :: Segments Int
-- >>> seg
-- Vector (Z :. 4) [1,4,0,3]
--
-- >>> let mat = fromList (Z:.5:.10) [0..] :: Matrix Int
-- >>> mat
-- Matrix (Z :. 5 :. 10)
--   [  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
--     10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
--     20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
--     30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
--     40, 41, 42, 43, 44, 45, 46, 47, 48, 49]
--
-- >>> run $ scanl1Seg (+) (use mat) (use seg)
-- Matrix (Z :. 5 :. 8)
--   [  0,  1,  3,   6,  10,  5, 11,  18,
--     10, 11, 23,  36,  50, 15, 31,  48,
--     20, 21, 43,  66,  90, 25, 51,  78,
--     30, 31, 63,  96, 130, 35, 71, 108,
--     40, 41, 83, 126, 170, 45, 91, 138]
--
scanl1Seg
    :: forall sh i e. (Shape sh, Slice sh, Elt e, Integral i, Bits i, FromIntegral i Int)
    => (Exp e -> Exp e -> Exp e)
    -> Acc (Array (sh:.Int) e)
    -> Acc (Segments i)
    -> Acc (Array (sh:.Int) e)
scanl1Seg f arr seg
  = map snd
  . scanl1 (segmentedL f)
  $ zip (replicate @(sh :. All) (indexTail (shape arr) :. All) (mkHeadFlags seg)) arr

-- |Segmented version of 'prescanl'.
--
prescanlSeg
    :: (Shape sh, Slice sh, Elt e, Integral i, Bits i, FromIntegral i Int)
    => (Exp e -> Exp e -> Exp e)
    -> Exp e
    -> Acc (Array (sh:.Int) e)
    -> Acc (Segments i)
    -> Acc (Array (sh:.Int) e)
prescanlSeg f e vec seg
  = afst
  $ scanl'Seg f e vec seg

-- |Segmented version of 'postscanl'.
--
postscanlSeg
    :: (Shape sh, Slice sh, Elt e, Integral i, Bits i, FromIntegral i Int)
    => (Exp e -> Exp e -> Exp e)
    -> Exp e
    -> Acc (Array (sh:.Int) e)
    -> Acc (Segments i)
    -> Acc (Array (sh:.Int) e)
postscanlSeg f e vec seg
  = map (f e)
  $ scanl1Seg f vec seg

-- | Segmented version of 'scanr' along the innermost dimension of an array. The
-- innermost dimension must have at least as many elements as the sum of the
-- segment descriptor.
--
-- >>> let seg = fromList (Z:.4) [1,4,0,3] :: Segments Int
-- >>> seg
-- Vector (Z :. 4) [1,4,0,3]
--
-- >>> let mat = fromList (Z:.5:.10) [0..] :: Matrix Int
-- >>> mat
-- Matrix (Z :. 5 :. 10)
--   [  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
--     10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
--     20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
--     30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
--     40, 41, 42, 43, 44, 45, 46, 47, 48, 49]
--
-- >>> run $ scanrSeg (+) 0 (use mat) (use seg)
-- Matrix (Z :. 5 :. 12)
--   [  2, 0,  18,  15, 11,  6, 0, 0,  24, 17,  9, 0,
--     12, 0,  58,  45, 31, 16, 0, 0,  54, 37, 19, 0,
--     22, 0,  98,  75, 51, 26, 0, 0,  84, 57, 29, 0,
--     32, 0, 138, 105, 71, 36, 0, 0, 114, 77, 39, 0,
--     42, 0, 178, 135, 91, 46, 0, 0, 144, 97, 49, 0]
--
scanrSeg
    :: forall sh e i. (Shape sh, Slice sh, Elt e, Integral i, Bits i, FromIntegral i Int)
    => (Exp e -> Exp e -> Exp e)
    -> Exp e
    -> Acc (Array (sh:.Int) e)
    -> Acc (Segments i)
    -> Acc (Array (sh:.Int) e)
scanrSeg f z arr seg =
  if null arr || null flags
    then fill (sh :. sz + length seg) z
    else scanr1Seg f arr' seg'
  where
    sh :. sz    = shape arr

    -- Using technique described for 'scanlSeg', where we intersperse the array
    -- with the seed element at the start of each segment, and then perform an
    -- inclusive segmented scan.
    --
    flags       = mkHeadFlags seg
    inc         = scanl1 (+) flags

    seg'        = map (+1) seg
    arr'        = permute const
                          (fill (sh :. sz + length seg) z)
                          (\(sx :. i) -> Just_ (sx :. i + fromIntegral (inc !! i) - 1))
                          (drop (sz - length flags) arr)


-- | Segmented version of 'scanr''.
--
-- >>> let seg = fromList (Z:.4) [1,4,0,3] :: Segments Int
-- >>> seg
-- Vector (Z :. 4) [1,4,0,3]
--
-- >>> let mat = fromList (Z:.5:.10) [0..] :: Matrix Int
-- >>> mat
-- Matrix (Z :. 5 :. 10)
--   [  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
--     10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
--     20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
--     30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
--     40, 41, 42, 43, 44, 45, 46, 47, 48, 49]
--
-- >>> let (res,sums) = run $ scanr'Seg (+) 0 (use mat) (use seg)
-- >>> res
-- Matrix (Z :. 5 :. 8)
--   [ 0,  15, 11,  6, 0, 17,  9, 0,
--     0,  45, 31, 16, 0, 37, 19, 0,
--     0,  75, 51, 26, 0, 57, 29, 0,
--     0, 105, 71, 36, 0, 77, 39, 0,
--     0, 135, 91, 46, 0, 97, 49, 0]
-- >>> sums
-- Matrix (Z :. 5 :. 4)
--   [  2,  18, 0,  24,
--     12,  58, 0,  54,
--     22,  98, 0,  84,
--     32, 138, 0, 114,
--     42, 178, 0, 144]
--
scanr'Seg
    :: forall sh e i. (Shape sh, Slice sh, Elt e, Integral i, Bits i, FromIntegral i Int)
    => (Exp e -> Exp e -> Exp e)
    -> Exp e
    -> Acc (Array (sh:.Int) e)
    -> Acc (Segments i)
    -> Acc (Array (sh:.Int) e, Array (sh:.Int) e)
scanr'Seg f z arr seg =
  if null arr
    then T2 arr  (fill (indexTail (shape arr) :. length seg) z)
    else T2 body sums
  where
    -- Using technique described for scanl'Seg
    --
    arr'        = scanrSeg f z arr seg

    -- reduction values
    seg'        = map (+1) seg
    heads       = prescanl (+) 0 seg'
    sums        = backpermute
                    (indexTail (shape arr') :. length seg)
                    (\(sz :.i) -> sz :. fromIntegral (heads ! I1 i))
                    arr'

    -- body segments
    flags       = mkHeadFlags seg
    inc         = scanl1 (+) flags
    body        = backpermute
                    (indexTail (shape arr) :. indexHead (shape flags))
                    (\(sz :. i) -> sz :. i + fromIntegral (inc ! I1 i))
                    arr'


-- | Segmented version of 'scanr1'.
--
-- >>> let seg = fromList (Z:.4) [1,4,0,3] :: Segments Int
-- >>> seg
-- Vector (Z :. 4) [1,4,0,3]
--
-- >>> let mat = fromList (Z:.5:.10) [0..] :: Matrix Int
-- >>> mat
-- Matrix (Z :. 5 :. 10)
--   [  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
--     10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
--     20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
--     30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
--     40, 41, 42, 43, 44, 45, 46, 47, 48, 49]
--
-- >>> run $ scanr1Seg (+) (use mat) (use seg)
-- Matrix (Z :. 5 :. 8)
--   [  0,  10,   9,  7,  4,  18, 13,  7,
--     10,  50,  39, 27, 14,  48, 33, 17,
--     20,  90,  69, 47, 24,  78, 53, 27,
--     30, 130,  99, 67, 34, 108, 73, 37,
--     40, 170, 129, 87, 44, 138, 93, 47]
--
scanr1Seg
    :: forall sh i e. (Shape sh, Slice sh, Elt e, Integral i, Bits i, FromIntegral i Int)
    => (Exp e -> Exp e -> Exp e)
    -> Acc (Array (sh:.Int) e)
    -> Acc (Segments i)
    -> Acc (Array (sh:.Int) e)
scanr1Seg f arr seg
  = map snd
  . scanr1 (segmentedR f)
  $ zip (replicate @(sh :. All) (indexTail (shape arr) :. All) (mkTailFlags seg)) arr


-- |Segmented version of 'prescanr'.
--
prescanrSeg
    :: (Shape sh, Slice sh, Elt e, Integral i, Bits i, FromIntegral i Int)
    => (Exp e -> Exp e -> Exp e)
    -> Exp e
    -> Acc (Array (sh:.Int) e)
    -> Acc (Segments i)
    -> Acc (Array (sh:.Int) e)
prescanrSeg f e vec seg
  = afst
  $ scanr'Seg f e vec seg

-- |Segmented version of 'postscanr'.
--
postscanrSeg
    :: (Shape sh, Slice sh, Elt e, Integral i, Bits i, FromIntegral i Int)
    => (Exp e -> Exp e -> Exp e)
    -> Exp e
    -> Acc (Array (sh:.Int) e)
    -> Acc (Segments i)
    -> Acc (Array (sh:.Int) e)
postscanrSeg f e vec seg
  = map (f e)
  $ scanr1Seg f vec seg


-- Segmented scan helpers
-- ----------------------

-- | Compute head flags vector from segment vector for left-scans.
--
-- The vector will be full of zeros in the body of a segment, and non-zero
-- otherwise. The "flag" value, if greater than one, indicates that several
-- empty segments are represented by this single flag entry. This is additional
-- data is used by exclusive segmented scan.
--
mkHeadFlags
    :: (Integral i, FromIntegral i Int)
    => Acc (Segments i)
    -> Acc (Segments i)
mkHeadFlags seg
  = init
  $ permute (+) zeros (\ix -> Just_ (index1' (offset ! ix))) ones
  where
    T2 offset len = scanl' (+) 0 seg
    zeros         = fill (index1' $ the len + 1) 0
    ones          = fill (index1  $ size offset) 1

-- | Compute tail flags vector from segment vector for right-scans. That
-- is, the flag is placed at the last place in each segment.
--
mkTailFlags
    :: (Integral i, FromIntegral i Int)
    => Acc (Segments i)
    -> Acc (Segments i)
mkTailFlags seg
  = init
  $ permute (+) zeros (\ix -> Just_ (index1' (the len - 1 - offset ! ix))) ones
  where
    T2 offset len = scanr' (+) 0 seg
    zeros         = fill (index1' $ the len + 1) 0
    ones          = fill (index1  $ size offset) 1

-- | Construct a segmented version of a function from a non-segmented
-- version. The segmented apply operates on a head-flag value tuple, and
-- follows the procedure of Sengupta et. al.
--
segmentedL
    :: (Elt e, Num i, Bits i)
    => (Exp e -> Exp e -> Exp e)
    -> (Exp (i, e) -> Exp (i, e) -> Exp (i, e))
segmentedL f (T2 aF aV) (T2 bF bV) =
  T2 (aF .|. bF)
     (bF /= 0 ? (bV, f aV bV))

segmentedR
    :: (Elt e, Num i, Bits i)
    => (Exp e -> Exp e -> Exp e)
    -> (Exp (i, e) -> Exp (i, e) -> Exp (i, e))
segmentedR f y x = segmentedL (flip f) x y

-- | Index construction and destruction generalised to integral types.
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
index1' ::  (Integral i, FromIntegral i Int) => Exp i -> Exp DIM1
index1' i = Z :. fromIntegral i


-- Reshaping of arrays
-- -------------------

-- | Flatten the given array of arbitrary dimension into a one-dimensional
-- vector. As with 'reshape', this operation performs no work.
--
flatten :: forall sh e. (Shape sh, Elt e) => Acc (Array sh e) -> Acc (Vector e)
flatten a
  | Just Refl <- matchShapeType @sh @DIM1
  = a
flatten a
  = reshape (I1 (size a)) a


-- Enumeration and filling
-- -----------------------

-- | Create an array where all elements are the same value.
--
-- >>> run $ fill (constant (Z:.10)) 0 :: Vector Float
-- Vector (Z :. 10) [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]
--
fill :: (Shape sh, Elt e) => Exp sh -> Exp e -> Acc (Array sh e)
fill sh c = generate sh (const c)

-- | Create an array of the given shape containing the values @x@, @x+1@, etc.
-- (in row-major order).
--
-- >>> run $ enumFromN (constant (Z:.5:.10)) 0 :: Matrix Int
-- Matrix (Z :. 5 :. 10)
--   [  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
--     10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
--     20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
--     30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
--     40, 41, 42, 43, 44, 45, 46, 47, 48, 49]
--
enumFromN
    :: (Shape sh, Num e, FromIntegral Int e)
    => Exp sh
    -> Exp e
    -> Acc (Array sh e)
enumFromN sh x = enumFromStepN sh x 1

-- | Create an array of the given shape containing the values @x@, @x+y@,
-- @x+y+y@ etc. (in row-major order).
--
-- >>> run $ enumFromStepN (constant (Z:.5:.10)) 0 0.5 :: Matrix Float
-- Matrix (Z :. 5 :. 10)
--   [  0.0,  0.5,  1.0,  1.5,  2.0,  2.5,  3.0,  3.5,  4.0,  4.5,
--      5.0,  5.5,  6.0,  6.5,  7.0,  7.5,  8.0,  8.5,  9.0,  9.5,
--     10.0, 10.5, 11.0, 11.5, 12.0, 12.5, 13.0, 13.5, 14.0, 14.5,
--     15.0, 15.5, 16.0, 16.5, 17.0, 17.5, 18.0, 18.5, 19.0, 19.5,
--     20.0, 20.5, 21.0, 21.5, 22.0, 22.5, 23.0, 23.5, 24.0, 24.5]
--
enumFromStepN
    :: (Shape sh, Num e, FromIntegral Int e)
    => Exp sh
    -> Exp e              -- ^ x: start
    -> Exp e              -- ^ y: step
    -> Acc (Array sh e)
enumFromStepN sh x y
  = reshape sh
  $ generate (I1 (shapeSize sh))
             (\ix -> (fromIntegral (unindex1 ix :: Exp Int) * y) + x)

-- Concatenation
-- -------------

-- | Concatenate innermost component of two arrays. The extent of the lower
--   dimensional component is the intersection of the two arrays.
--
-- >>> let m1 = fromList (Z:.5:.10) [0..] :: Matrix Int
-- >>> m1
-- Matrix (Z :. 5 :. 10)
--   [  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
--     10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
--     20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
--     30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
--     40, 41, 42, 43, 44, 45, 46, 47, 48, 49]
--
-- >>> let m2 = fromList (Z:.10:.3) [0..] :: Matrix Int
-- >>> m2
-- Matrix (Z :. 10 :. 3)
--   [  0,  1,  2,
--      3,  4,  5,
--      6,  7,  8,
--      9, 10, 11,
--     12, 13, 14,
--     15, 16, 17,
--     18, 19, 20,
--     21, 22, 23,
--     24, 25, 26,
--     27, 28, 29]
--
-- >>> run $ use m1 ++ use m2
-- Matrix (Z :. 5 :. 13)
--   [  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  0,  1,  2,
--     10, 11, 12, 13, 14, 15, 16, 17, 18, 19,  3,  4,  5,
--     20, 21, 22, 23, 24, 25, 26, 27, 28, 29,  6,  7,  8,
--     30, 31, 32, 33, 34, 35, 36, 37, 38, 39,  9, 10, 11,
--     40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 12, 13, 14]
--
infixr 5 ++
(++) :: (Shape sh, Elt e)
     => Acc (Array (sh :. Int) e)
     -> Acc (Array (sh :. Int) e)
     -> Acc (Array (sh :. Int) e)
(++) = concatOn _1


-- | Generalised version of '(++)' where the argument 'Lens'' specifies which
-- dimension to concatenate along.
--
-- Appropriate lenses are available from <https://hackage.haskell.org/package/lens-accelerate lens-accelerate>.
--
-- >>> let m1 = fromList (Z:.5:.10) [0..] :: Matrix Int
-- >>> m1
-- Matrix (Z :. 5 :. 10)
--   [  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
--     10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
--     20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
--     30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
--     40, 41, 42, 43, 44, 45, 46, 47, 48, 49]
--
-- >>> let m2 = fromList (Z:.10:.5) [0..] :: Matrix Int
-- >>> m2
-- Matrix (Z :. 10 :. 5)
--   [  0,  1,  2,  3,  4,
--      5,  6,  7,  8,  9,
--     10, 11, 12, 13, 14,
--     15, 16, 17, 18, 19,
--     20, 21, 22, 23, 24,
--     25, 26, 27, 28, 29,
--     30, 31, 32, 33, 34,
--     35, 36, 37, 38, 39,
--     40, 41, 42, 43, 44,
--     45, 46, 47, 48, 49]
--
-- >>> run $ concatOn _1 (use m1) (use m2)
-- Matrix (Z :. 5 :. 15)
--   [  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  0,  1,  2,  3,  4,
--     10, 11, 12, 13, 14, 15, 16, 17, 18, 19,  5,  6,  7,  8,  9,
--     20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 10, 11, 12, 13, 14,
--     30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 15, 16, 17, 18, 19,
--     40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 20, 21, 22, 23, 24]
--
-- >>> run $ concatOn _2 (use m1) (use m2)
-- Matrix (Z :. 15 :. 5)
--   [  0,  1,  2,  3,  4,
--     10, 11, 12, 13, 14,
--     20, 21, 22, 23, 24,
--     30, 31, 32, 33, 34,
--     40, 41, 42, 43, 44,
--      0,  1,  2,  3,  4,
--      5,  6,  7,  8,  9,
--     10, 11, 12, 13, 14,
--     15, 16, 17, 18, 19,
--     20, 21, 22, 23, 24,
--     25, 26, 27, 28, 29,
--     30, 31, 32, 33, 34,
--     35, 36, 37, 38, 39,
--     40, 41, 42, 43, 44,
--     45, 46, 47, 48, 49]
--
concatOn
    :: (Shape sh, Elt e)
    => Lens' (Exp sh) (Exp Int)
    -> Acc (Array sh e)
    -> Acc (Array sh e)
    -> Acc (Array sh e)
concatOn dim xs ys =
  let
      shx   = shape xs
      shy   = shape ys
      m     = shx ^. dim
      n     = shy ^. dim
      shx'  = shx & dim .~ m+n
      shy'  = shy & dim .~ m+n
  in
  generate (shx' `intersect` shy')
           (\ix -> let i = ix ^. dim in
                   if  i < m then xs ! ix
                             else ys ! (ix & dim -~ m))

-- TLM: If we have something like (concat . split) then the source array will
--      have two use sites, but is actually safe (and better) to inline.


-- Filtering
-- ---------

-- | Drop elements that do not satisfy the predicate. Returns the elements which
-- pass the predicate, together with a segment descriptor indicating how many
-- elements along each outer dimension were valid.
--
-- >>> let vec = fromList (Z :. 10) [1..10] :: Vector Int
-- >>> vec
-- Vector (Z :. 10) [1,2,3,4,5,6,7,8,9,10]
--
-- >>> run $ filter even (use vec)
-- (Vector (Z :. 5) [2,4,6,8,10],Scalar Z [5])
--
-- >>> let mat = fromList (Z :. 4 :. 10) [1,2,3,4,5,6,7,8,9,10,1,1,1,1,1,2,2,2,2,2,2,4,6,8,10,12,14,16,18,20,1,3,5,7,9,11,13,15,17,19] :: Matrix Int
-- >>> mat
-- Matrix (Z :. 4 :. 10)
--   [ 1, 2, 3, 4,  5,  6,  7,  8,  9, 10,
--     1, 1, 1, 1,  1,  2,  2,  2,  2,  2,
--     2, 4, 6, 8, 10, 12, 14, 16, 18, 20,
--     1, 3, 5, 7,  9, 11, 13, 15, 17, 19]
--
-- >>> run $ filter odd (use mat)
-- (Vector (Z :. 20) [1,3,5,7,9,1,1,1,1,1,1,3,5,7,9,11,13,15,17,19],Vector (Z :. 4) [5,5,0,10])
--
filter :: (Shape sh, Elt e)
       => (Exp e -> Exp Bool)
       -> Acc (Array (sh:.Int) e)
       -> Acc (Vector e, Array sh Int)
filter p arr = compact (map p arr) arr
{-# NOINLINE filter #-}
{-# RULES
  "ACC filter/filter" forall f g arr.
    filter f (afst (filter g arr)) = filter (\x -> g x && f x) arr
 #-}


-- | As 'filter', but with separate arrays for the data elements and the
-- flags indicating which elements of that array should be kept.
--
compact :: forall sh e. (Shape sh, Elt e)
        => Acc (Array (sh:.Int) Bool)
        -> Acc (Array (sh:.Int) e)
        -> Acc (Vector e, Array sh Int)
compact keep arr
  -- Optimise 1-dimensional arrays, where we can avoid additional computations
  -- for the offset indices.
  | Just Refl <- matchShapeType @sh @Z
  = let
        T2 target len   = scanl' (+) 0 (map boolToInt keep)
        prj ix          = if keep!ix
                             then Just_ (I1 (target!ix))
                             else Nothing_
        dummy           = fill (I1 (the len)) undef
        result          = permute const dummy prj arr
    in
    if null arr
       then T2 emptyArray (fill Z 0)
       else
    if the len == unindex1 (shape arr)
       then T2 arr    len
       else T2 result len

compact keep arr
  = let
        sz              = indexTail (shape arr)
        T2 target len   = scanl' (+) 0 (map boolToInt keep)
        T2 offset valid = scanl' (+) 0 (flatten len)
        prj ix          = if keep!ix
                             then Just_ (I1 (offset !! (toIndex sz (indexTail ix)) + target!ix))
                             else Nothing_
        dummy           = fill (I1 (the valid)) undef
        result          = permute const dummy prj arr
    in
    if null arr
      then T2 emptyArray (fill sz 0)
      else T2 result len


-- Gather operations
-- -----------------

-- | Gather elements from a source array by reading values at the given indices.
--
-- >>> let input = fromList (Z:.9) [1,9,6,4,4,2,0,1,2] :: Vector Int
-- >>> let from  = fromList (Z:.6) [1,3,7,2,5,3] :: Vector Int
-- >>> run $ gather (use from) (use input)
-- Vector (Z :. 6) [9,4,1,6,2,4]
--
gather
    :: (Shape sh, Elt e)
    => Acc (Array sh Int)         -- ^ index of source at each index to gather
    -> Acc (Vector e)             -- ^ source values
    -> Acc (Array sh e)
gather indices input = map (input !!) indices
  -- TLM NOTES:
  --  * (!!) has potential for later optimisation
  --  * We needn't fix the source array to Vector, but this matches the
  --    intuition that 'Int' ~ 'DIM1'.


-- | Conditionally copy elements from source array to destination array
-- according to an index mapping.
--
-- In addition, the 'mask' vector and associated predication function specifies
-- whether the element is copied or a default value is used instead.
--
-- >>> let defaults = fromList (Z :. 6) [6,6,6,6,6,6] :: Vector Float
-- >>> let from     = fromList (Z :. 6) [1,3,7,2,5,3] :: Vector Int
-- >>> let mask     = fromList (Z :. 6) [3,4,9,2,7,5] :: Vector Int
-- >>> let input    = fromList (Z :. 9) [1,9,6,4,4,2,0,1,2] :: Vector Float
-- >>> run $ gatherIf (use from) (use mask) (> 4) (use defaults) (use input)
-- Vector (Z :. 6) [6.0,6.0,1.0,6.0,2.0,4.0]
--
gatherIf
    :: (Elt a, Elt b)
    => Acc (Vector Int)           -- ^ source indices to gather from
    -> Acc (Vector a)             -- ^ mask vector
    -> (Exp a -> Exp Bool)        -- ^ predicate function
    -> Acc (Vector b)             -- ^ default values
    -> Acc (Vector b)             -- ^ source values
    -> Acc (Vector b)
gatherIf from maskV pred defaults input = zipWith zf pf gatheredV
  where
    zf p g      = p ? (unlift g)
    gatheredV   = zip (gather from input) defaults
    pf          = map pred maskV


-- Scatter operations
-- ------------------

-- | Overwrite elements of the destination by scattering the values of the
-- source array according to the given index mapping.
--
-- Note that if the destination index appears more than once in the mapping the
-- result is undefined.
--
-- >>> let to    = fromList (Z :. 6) [1,3,7,2,5,8] :: Vector Int
-- >>> let input = fromList (Z :. 7) [1,9,6,4,4,2,5] :: Vector Int
-- >>> run $ scatter (use to) (fill (constant (Z:.10)) 0) (use input)
-- Vector (Z :. 10) [0,1,4,9,0,4,0,6,2,0]
--
scatter
    :: Elt e
    => Acc (Vector Int)           -- ^ destination indices to scatter into
    -> Acc (Vector e)             -- ^ default values
    -> Acc (Vector e)             -- ^ source values
    -> Acc (Vector e)
scatter to defaults input = permute const defaults pf input'
  where
    pf ix   = Just_ (I1 (to ! ix))
    input'  = backpermute (shape to `intersect` shape input) id input


-- | Conditionally overwrite elements of the destination by scattering values of
-- the source array according to a given index mapping, whenever the masking
-- function resolves to 'True'.
--
-- Note that if the destination index appears more than once in the mapping the
-- result is undefined.
--
-- >>> let to    = fromList (Z :. 6) [1,3,7,2,5,8] :: Vector Int
-- >>> let mask  = fromList (Z :. 6) [3,4,9,2,7,5] :: Vector Int
-- >>> let input = fromList (Z :. 7) [1,9,6,4,4,2,5] :: Vector Int
-- >>> run $ scatterIf (use to) (use mask) (> 4) (fill (constant (Z:.10)) 0) (use input)
-- Vector (Z :. 10) [0,0,0,0,0,4,0,6,2,0]
--
scatterIf
    :: (Elt a, Elt b)
    => Acc (Vector Int)           -- ^ destination indices to scatter into
    -> Acc (Vector a)             -- ^ mask vector
    -> (Exp a -> Exp Bool)        -- ^ predicate function
    -> Acc (Vector b)             -- ^ default values
    -> Acc (Vector b)             -- ^ source values
    -> Acc (Vector b)
scatterIf to maskV pred defaults input = permute const defaults pf input'
  where
    input'  = backpermute (shape to `intersect` shape input) id input
    pf ix   = if pred (maskV ! ix)
                 then Just_ (I1 (to ! ix))
                 else Nothing_


-- Permutations
-- ------------

-- | Reverse the elements of a vector.
--
reverse :: Elt e => Acc (Vector e) -> Acc (Vector e)
reverse = reverseOn _1

-- | Transpose the rows and columns of a matrix.
--
transpose :: Elt e => Acc (Array DIM2 e) -> Acc (Array DIM2 e)
transpose = transposeOn _1 _2


-- | Generalised version of 'reverse' where the argument 'Lens'' specifies which
-- dimension to reverse.
--
-- Appropriate lenses are available from <https://hackage.haskell.org/package/lens-accelerate lens-accelerate>.
--
-- >>> let mat = fromList (Z:.5:.10) [0..] :: Matrix Int
-- >>> mat
-- Matrix (Z :. 5 :. 10)
--   [  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
--     10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
--     20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
--     30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
--     40, 41, 42, 43, 44, 45, 46, 47, 48, 49]
--
-- >>> run $ reverseOn _1 (use mat)
-- Matrix (Z :. 5 :. 10)
--   [  9,  8,  7,  6,  5,  4,  3,  2,  1,  0,
--     19, 18, 17, 16, 15, 14, 13, 12, 11, 10,
--     29, 28, 27, 26, 25, 24, 23, 22, 21, 20,
--     39, 38, 37, 36, 35, 34, 33, 32, 31, 30,
--     49, 48, 47, 46, 45, 44, 43, 42, 41, 40]
--
-- >>> run $ reverseOn _2 (use mat)
-- Matrix (Z :. 5 :. 10)
--   [ 40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
--     30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
--     20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
--     10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
--      0,  1,  2,  3,  4,  5,  6,  7,  8,  9]
--
-- @since 1.2.0.0
--
reverseOn
    :: (Shape sh, Elt e)
    => Lens' (Exp sh) (Exp Int)
    -> Acc (Array sh e)
    -> Acc (Array sh e)
reverseOn dim xs =
  let
      sh = shape xs
      n  = sh ^. dim
  in
  backpermute sh (over dim $ \i -> n - i - 1) xs

-- | Generalised version of 'transpose' where the argument 'Lens''s specify
-- which two dimensions to transpose.
--
-- Appropriate lenses are available from <https://hackage.haskell.org/package/lens-accelerate lens-accelerate>.
--
-- >>> let mat = fromList (Z:.5:.10) [0..] :: Matrix Int
-- >>> mat
-- Matrix (Z :. 5 :. 10)
--   [  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
--     10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
--     20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
--     30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
--     40, 41, 42, 43, 44, 45, 46, 47, 48, 49]
--
-- >>> run $ transposeOn _1 _2 (use mat)
-- Matrix (Z :. 10 :. 5)
--   [ 0, 10, 20, 30, 40,
--     1, 11, 21, 31, 41,
--     2, 12, 22, 32, 42,
--     3, 13, 23, 33, 43,
--     4, 14, 24, 34, 44,
--     5, 15, 25, 35, 45,
--     6, 16, 26, 36, 46,
--     7, 17, 27, 37, 47,
--     8, 18, 28, 38, 48,
--     9, 19, 29, 39, 49]
--
-- >>> let box = fromList (Z:.2:.3:.5) [0..] :: Array DIM3 Int
-- >>> box
-- Array (Z :. 2 :. 3 :. 5) [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29]
--
-- >>> run $ transposeOn _1 _2 (use box)
-- Array (Z :. 2 :. 5 :. 3) [0,5,10,1,6,11,2,7,12,3,8,13,4,9,14,15,20,25,16,21,26,17,22,27,18,23,28,19,24,29]
--
-- >>> run $ transposeOn _2 _3 (use box)
-- Array (Z :. 3 :. 2 :. 5) [0,1,2,3,4,15,16,17,18,19,5,6,7,8,9,20,21,22,23,24,10,11,12,13,14,25,26,27,28,29]
--
-- >>> run $ transposeOn _1 _3 (use box)
-- Array (Z :. 5 :. 3 :. 2) [0,15,5,20,10,25,1,16,6,21,11,26,2,17,7,22,12,27,3,18,8,23,13,28,4,19,9,24,14,29]
--
-- @since 1.2.0.0
--
transposeOn
    :: (Shape sh, Elt e)
    => Lens' (Exp sh) (Exp Int)
    -> Lens' (Exp sh) (Exp Int)
    -> Acc (Array sh e)
    -> Acc (Array sh e)
transposeOn dim1 dim2 xs =
  let
      swap ix = ix & dim2 .~ ix ^. dim1
                   & dim1 .~ ix ^. dim2
  in
  backpermute (swap (shape xs)) swap xs


-- Extracting sub-vectors
-- ----------------------

-- | Yield the first @n@ elements in the innermost dimension of the array (plus
-- all lower dimensional elements).
--
-- >>> let mat = fromList (Z:.5:.10) [0..] :: Matrix Int
-- >>> mat
-- Matrix (Z :. 5 :. 10)
--   [  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
--     10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
--     20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
--     30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
--     40, 41, 42, 43, 44, 45, 46, 47, 48, 49]
--
-- >>> run $ take 5 (use mat)
-- Matrix (Z :. 5 :. 5)
--   [  0,  1,  2,  3,  4,
--     10, 11, 12, 13, 14,
--     20, 21, 22, 23, 24,
--     30, 31, 32, 33, 34,
--     40, 41, 42, 43, 44]
--
take :: (Shape sh, Elt e)
     => Exp Int
     -> Acc (Array (sh :. Int) e)
     -> Acc (Array (sh :. Int) e)
take = takeOn _1


-- | Yield all but the first @n@ elements along the innermost dimension of the
-- array (plus all lower dimensional elements).
--
-- >>> let mat = fromList (Z:.5:.10) [0..] :: Matrix Int
-- >>> mat
-- Matrix (Z :. 5 :. 10)
--   [  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
--     10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
--     20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
--     30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
--     40, 41, 42, 43, 44, 45, 46, 47, 48, 49]
--
-- >>> run $ drop 7 (use mat)
-- Matrix (Z :. 5 :. 3)
--   [  7,  8,  9,
--     17, 18, 19,
--     27, 28, 29,
--     37, 38, 39,
--     47, 48, 49]
--
drop :: (Shape sh, Elt e)
     => Exp Int
     -> Acc (Array (sh :. Int) e)
     -> Acc (Array (sh :. Int) e)
drop = dropOn _1


-- | Yield all but the elements in the last index of the innermost dimension.
--
-- >>> let mat = fromList (Z:.5:.10) [0..] :: Matrix Int
-- >>> mat
-- Matrix (Z :. 5 :. 10)
--   [  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
--     10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
--     20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
--     30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
--     40, 41, 42, 43, 44, 45, 46, 47, 48, 49]
--
-- >>> run $ init (use mat)
-- Matrix (Z :. 5 :. 9)
--   [  0,  1,  2,  3,  4,  5,  6,  7,  8,
--     10, 11, 12, 13, 14, 15, 16, 17, 18,
--     20, 21, 22, 23, 24, 25, 26, 27, 28,
--     30, 31, 32, 33, 34, 35, 36, 37, 38,
--     40, 41, 42, 43, 44, 45, 46, 47, 48]
--
init :: (Shape sh, Elt e)
     => Acc (Array (sh :. Int) e)
     -> Acc (Array (sh :. Int) e)
init = initOn _1


-- | Yield all but the first element along the innermost dimension of an array.
-- The innermost dimension must not be empty.
--
-- >>> let mat = fromList (Z:.5:.10) [0..] :: Matrix Int
-- >>> mat
-- Matrix (Z :. 5 :. 10)
--   [  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
--     10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
--     20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
--     30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
--     40, 41, 42, 43, 44, 45, 46, 47, 48, 49]
--
-- >>> run $ tail (use mat)
-- Matrix (Z :. 5 :. 9)
--   [  1,  2,  3,  4,  5,  6,  7,  8,  9,
--     11, 12, 13, 14, 15, 16, 17, 18, 19,
--     21, 22, 23, 24, 25, 26, 27, 28, 29,
--     31, 32, 33, 34, 35, 36, 37, 38, 39,
--     41, 42, 43, 44, 45, 46, 47, 48, 49]
--
tail :: (Shape sh, Elt e)
     => Acc (Array (sh :. Int) e)
     -> Acc (Array (sh :. Int) e)
tail = tailOn _1


-- | Yield a slit (slice) of the innermost indices of an array. Denotationally,
-- we have:
--
-- > slit i n = take n . drop i
--
slit :: (Shape sh, Elt e)
     => Exp Int                     -- ^ starting index
     -> Exp Int                     -- ^ length
     -> Acc (Array (sh :. Int) e)
     -> Acc (Array (sh :. Int) e)
slit = slitOn _1


-- | Generalised version of 'init' where the argument 'Lens'' specifies which
-- dimension to operate over.
--
-- Appropriate lenses are available from <https://hackage.haskell.org/package/lens-accelerate lens-accelerate>.
--
-- @since 1.2.0.0
--
initOn
    :: (Shape sh, Elt e)
    => Lens' (Exp sh) (Exp Int)
    -> Acc (Array sh e)
    -> Acc (Array sh e)
initOn dim xs =
  let
      sh  = shape xs
      sh' = over dim (\i -> 0 `max` (i-1)) sh
  in
  backpermute sh' id xs


-- | Generalised version of 'tail' where the argument 'Lens'' specifies which
-- dimension to operate over.
--
-- Appropriate lenses are available from <https://hackage.haskell.org/package/lens-accelerate lens-accelerate>.
--
-- @since 1.2.0.0
--
tailOn
    :: (Shape sh, Elt e)
    => Lens' (Exp sh) (Exp Int)
    -> Acc (Array sh e)
    -> Acc (Array sh e)
tailOn dim xs =
  let
      sh  = shape xs
      sh' = over dim (\i -> 0 `max` (i-1)) sh
  in
  backpermute sh' (& dim +~ 1) xs


-- | Generalised version of 'take' where the argument 'Lens'' specifies which
-- dimension to operate over.
--
-- Appropriate lenses are available from <https://hackage.haskell.org/package/lens-accelerate lens-accelerate>.
--
-- @since 1.2.0.0
--
takeOn
    :: (Shape sh, Elt e)
    => Lens' (Exp sh) (Exp Int)
    -> Exp Int
    -> Acc (Array sh e)
    -> Acc (Array sh e)
takeOn dim n xs =
  let
      sh = shape xs
      m  = sh ^. dim
  in
  backpermute (sh & dim .~ min m n) id xs


-- | Generalised version of 'drop' where the argument 'Lens'' specifies which
-- dimension to operate over.
--
-- Appropriate lenses are available from <https://hackage.haskell.org/package/lens-accelerate lens-accelerate>.
--
-- @since 1.2.0.0
--
dropOn
    :: (Shape sh, Elt e)
    => Lens' (Exp sh) (Exp Int)
    -> Exp Int
    -> Acc (Array sh e)
    -> Acc (Array sh e)
dropOn dim n xs =
  let
      sh = shape xs
      m  = sh ^. dim
      n' = max 0 n
  in
  backpermute (sh & dim .~ max 0 (m - n')) (& dim +~ n') xs

-- Note: [embedding constants in take & drop]
--
-- Previously the 'take' and 'drop functions prevented the value of the
-- take/drop amount from being embedded directly in the generated code. This was
-- done by writing the value into a scalar array and reading that value out,
-- rather than using it directly. Although that is better from a code
-- cache/reuse standpoint, I've now removed this as it prevents the user from
-- specialising their code, and in a real program this extra indirection is
-- probably not necessary anyway.
--

-- | Generalised version of 'drop' where the argument 'Lens'' specifies which
-- dimension to operate over.
--
-- Appropriate lenses are available from <https://hackage.haskell.org/package/lens-accelerate lens-accelerate>.
--
-- @since 1.2.0.0
--
slitOn
    :: (Shape sh, Elt e)
    => Lens' (Exp sh) (Exp Int)
    -> Exp Int                      -- ^ starting index
    -> Exp Int                      -- ^ length
    -> Acc (Array sh e)
    -> Acc (Array sh e)
slitOn dim i n = takeOn dim n . dropOn dim i


-- Controlling execution
-- ---------------------

-- | Force an array expression to be evaluated, preventing it from fusing with
-- other operations. Forcing operations to be computed to memory, rather than
-- being fused into their consuming function, can sometimes improve performance.
-- For example, computing a matrix 'transpose' could provide better memory
-- locality for the subsequent operation. Preventing fusion to split large
-- operations into several simpler steps could also help by reducing register
-- pressure.
--
-- Preventing fusion also means that the individual operations are available to
-- be executed concurrently with other kernels. In particular, consider using
-- this if you have a series of operations that are compute bound rather than
-- memory bound.
--
-- Here is the synthetic example:
--
-- > loop :: Exp Int -> Exp Int
-- > loop ticks =
-- >   let clockRate = 900000   -- kHz
-- >   in  while (\i -> i < clockRate * ticks) (+1) 0
-- >
-- > test :: Acc (Vector Int)
-- > test =
-- >   zip3
-- >     (compute $ map loop (use $ fromList (Z:.1) [10]))
-- >     (compute $ map loop (use $ fromList (Z:.1) [10]))
-- >     (compute $ map loop (use $ fromList (Z:.1) [10]))
-- >
--
-- Without the use of 'compute', the operations are fused together and the three
-- long-running loops are executed sequentially in a single kernel. Instead, the
-- individual operations can now be executed concurrently, potentially reducing
-- overall runtime.
--
compute :: Arrays a => Acc a -> Acc a
compute = id >-> id


-- Flow control
-- ------------

-- | Infix version of 'acond'. If the predicate evaluates to 'True', the first
-- component of the tuple is returned, else the second.
--
-- Enabling the @RebindableSyntax@ extension will allow you to use the standard
-- if-then-else syntax instead.
--
infix 0 ?|
(?|) :: Arrays a => Exp Bool -> (Acc a, Acc a) -> Acc a
c ?| (t, e) = acond c t e

-- | An infix version of 'cond'. If the predicate evaluates to 'True', the first
-- component of the tuple is returned, else the second.
--
-- Enabling the @RebindableSyntax@ extension will allow you to use the standard
-- if-then-else syntax instead.
--
infix 0 ?
(?) :: Elt t => Exp Bool -> (Exp t, Exp t) -> Exp t
c ? (t, e) = cond c t e

-- | For use with @-XRebindableSyntax@, this class provides 'ifThenElse'
-- for host and embedded scalar and array types.
--
class IfThenElse bool a where
  ifThenElse :: bool -> a -> a -> a

instance IfThenElse Bool a where
  ifThenElse p t e =
    case p of
      True  -> t
      False -> e

instance Elt a => IfThenElse (Exp Bool) (Exp a) where
  ifThenElse = cond

instance Arrays a => IfThenElse (Exp Bool) (Acc a) where
  ifThenElse = acond


-- | The 'match' operation is the core operation which enables embedded
-- pattern matching. It is applied to an n-ary scalar function, and
-- generates the necessary case-statements in the embedded code for each
-- argument. For example, given the function:
--
-- > example1 :: Exp (Maybe Bool) -> Exp Int
-- > example1 Nothing_ = 0
-- > example1 (Just_ False_) = 1
-- > example1 (Just_ True_) = 2
--
-- In order to use this function it must be applied to the 'match'
-- operator:
--
-- > match example1
--
-- Using the infix-flip operator ('Data.Function.&'), we can also write
-- case statements inline. For example, instead of this:
--
-- > example2 x = case f x of
-- >   Nothing_ -> ...      -- error: embedded pattern synonym...
-- >   Just_ y  -> ...      -- ...used outside of 'match' context
--
-- This can be written instead as:
--
-- > example3 x = f x & match \case
-- >   Nothing_ -> ...
-- >   Just_ y  -> ...
--
-- And utilising the @LambdaCase@ and @BlockArguments@ syntactic extensions.
--
-- The Template Haskell splice 'Data.Array.Accelerate.mkPattern' (or
-- 'Data.Array.Accelerate.mkPatterns') can be used to generate the pattern
-- synonyms for a given Haskell'98 sum or product data type. For example:
--
-- > data Option a = None | Some a
-- >   deriving (Generic, Elt)
-- >
-- > mkPattern ''Option
--
-- Which can then be used such as:
--
-- > isNone :: Elt a => Exp (Option a) -> Exp Bool
-- > isNone = match \case
-- >   None_   -> True_
-- >   Some_{} -> False_
--
-- @since 1.3.0.0
--
match :: Matching f => f -> f
match f = mkFun (mkMatch f) id

data Args f where
  (:->)  :: Exp a -> Args b -> Args (Exp a -> b)
  Result :: Args (Exp a)

class Matching a where
  type ResultT a
  mkMatch :: a -> Args a -> Exp (ResultT a)
  mkFun   :: (Args f -> Exp (ResultT a))
          -> (Args a -> Args f)
          -> a

instance Elt a => Matching (Exp a) where
  type ResultT (Exp a) = a

  mkFun f k = f (k Result)
  mkMatch (Exp e) Result =
    case e of
      SmartExp (Match _ x) -> Exp x
      _                    -> Exp e

instance (Elt e, Matching r) => Matching (Exp e -> r) where
  type ResultT (Exp e -> r) = ResultT r

  mkFun f k x = mkFun f (\xs -> k (x :-> xs))
  mkMatch f (x@(Exp p) :-> xs) =
    case p of
      -- This first case is used when we have nested calls to 'match'
      SmartExp Match{} -> mkMatch (f x) xs

      -- If there is only a single alternative, we can elide the case
      -- statement at this point. This can occur when pattern matching on
      -- product types.
      _ -> case rhs of
             [(_,r)] -> Exp r
             _       -> Exp (SmartExp (Case p rhs))
    where
      rhs = [ (tag, unExp (mkMatch (f x') xs))
            | tag <- tagsR @e
            , let x' = Exp (SmartExp (Match tag p)) ]


-- Scalar iteration
-- ----------------

-- | Repeatedly apply a function a fixed number of times
--
iterate
    :: Elt a
    => Exp Int
    -> (Exp a -> Exp a)
    -> Exp a
    -> Exp a
iterate n f z
  = let step (T2 i acc) = T2 (i+1) (f acc)
     in snd $ while (\v -> fst v < n) step (T2 0 z)


-- Scalar bulk operations
-- ----------------------

-- | Reduce along an innermost slice of an array /sequentially/, by applying a
-- binary operator to a starting value and the array from left to right.
--
sfoldl :: (Shape sh, Elt a, Elt b)
       => (Exp a -> Exp b -> Exp a)
       -> Exp a
       -> Exp sh
       -> Acc (Array (sh :. Int) b)
       -> Exp a
sfoldl f z ix xs
  = let n               = indexHead (shape xs)
        step (T2 i acc) = T2 (i+1) (acc `f` (xs ! (ix :. i)))
     in snd $ while (\v -> fst v < n) step (T2 0 z)


-- Tuples
-- ------

-- |Extract the first component of a scalar pair.
--
fst :: (Elt a, Elt b) => Exp (a, b) -> Exp a
fst (T2 a _) = a

-- |Extract the first component of an array pair.
{-# NOINLINE[1] afst #-}
afst :: (Arrays a, Arrays b) => Acc (a, b) -> Acc a
afst (T2 a _) = a

-- |Extract the second component of a scalar pair.
--
snd :: (Elt a, Elt b) => Exp (a, b) -> Exp b
snd (T2 _ b) = b

-- | Extract the second component of an array pair
asnd :: (Arrays a, Arrays b) => Acc (a, b) -> Acc b
asnd (T2 _ b) = b

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

-- | The one index for a rank-0 array.
--
index0 :: Exp Z
index0 = Z

-- | Turn an 'Int' expression into a rank-1 indexing expression.
--
index1 :: Elt i => Exp i -> Exp (Z :. i)
index1 i = Z :. i

-- | Turn a rank-1 indexing expression into an 'Int' expression.
--
unindex1 :: Elt i => Exp (Z :. i) -> Exp i
unindex1 (Z :. i) = i

-- | Creates a rank-2 index from two Exp Int`s
--
index2
    :: Elt i
    => Exp i
    -> Exp i
    -> Exp (Z :. i :. i)
index2 i j = Z :. i :. j

-- | Destructs a rank-2 index to an Exp tuple of two Int`s.
--
unindex2
    :: Elt i
    => Exp (Z :. i :. i)
    -> Exp (i, i)
unindex2 (Z :. i :. j) = T2 i j

-- | Create a rank-3 index from three Exp Int`s
--
index3
    :: Elt i
    => Exp i
    -> Exp i
    -> Exp i
    -> Exp (Z :. i :. i :. i)
index3 k j i = Z :. k :. j :. i

-- | Destruct a rank-3 index into an Exp tuple of Int`s
unindex3
    :: Elt i
    => Exp (Z :. i :. i :. i)
    -> Exp (i, i, i)
unindex3 (Z :. k :. j :. i) = T3 k j i


-- Array operations with a scalar result
-- -------------------------------------

-- | Extract the element of a singleton array.
--
-- > the xs  ==  xs ! Z
--
the :: Elt e => Acc (Scalar e) -> Exp e
the = (! index0)

-- | Test whether an array is empty.
--
null :: (Shape sh, Elt e) => Acc (Array sh e) -> Exp Bool
null arr = size arr == 0

-- | Get the length of a vector.
--
length :: Elt e => Acc (Vector e) -> Exp Int
length = unindex1 . shape


-- Operations to facilitate irregular data parallelism
-- ---------------------------------------------------

-- | A recipe for generating flattened implementations of some kinds of
-- irregular nested parallelism. Given two functions that:
--
--   (1) for each source element, determine how many target
--       elements it expands into; and
--
--   (2) computes a particular target element based on a source element and
--       the target element index associated with the source
--
-- The following example implements the Sieve of Eratosthenes,
-- a contraction style algorithm which first computes all primes less than
-- /sqrt n/, then uses this intermediate result to sieve away all numbers
-- in the range /[sqrt n .. n]/. The 'expand' function is used to calculate
-- and flatten the sieves. For each prime /p/ and upper limit /c2/,
-- function /sz/ computes the number of contributions in the sieve. Then,
-- for each prime /p/ and sieve index /i/, the function /get/ computes the
-- sieve contribution. The final step produces all the new primes in the
-- interval /[c1 .. c2]/.
--
-- >>> :{
--   primes :: Exp Int -> Acc (Vector Int)
--   primes n = afst loop
--     where
--       c0    = unit 2
--       a0    = use $ fromList (Z:.0) []
--       limit = truncate (sqrt (fromIntegral (n+1) :: Exp Float))
--       loop  = awhile
--                 (\(T2 _   c) -> map (< n+1) c)
--                 (\(T2 old c) ->
--                   let c1 = the c
--                       c2 = c1 < limit ? ( c1*c1, n+1 )
--                       --
--                       sieves =
--                         let sz p    = (c2 - p) `quot` p
--                             get p i = (2+i)*p
--                         in
--                         map (subtract c1) (expand sz get old)
--                       --
--                       new =
--                         let m     = c2-c1
--                             put i = let s = sieves ! i
--                                      in s >= 0 && s < m ? (Just_ (I1 s), Nothing_)
--                         in
--                         afst
--                           $ filter (> 0)
--                           $ permute const (enumFromN (I1 m) c1) put
--                           $ fill (shape sieves) 0
--                    in
--                    T2 (old ++ new) (unit c2))
--                 (T2 a0 c0)
-- :}
--
-- >>> run $ primes 100
-- Vector (Z :. 25) [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
--
-- Inspired by the paper /Data-Parallel Flattening by Expansion/ by Martin
-- Elsman, Troels Henriksen, and Niels Gustav Westphal Serup, ARRAY'19.
--
-- @since 1.3.0.0
--
expand :: (Elt a, Elt b)
       => (Exp a -> Exp Int)
       -> (Exp a -> Exp Int -> Exp b)
       -> Acc (Vector a)
       -> Acc (Vector b)
expand f g xs =
  let
      szs           = map f xs
      T2 offset len = scanl' (+) 0 szs
      m             = the len
  in
  if length xs == 0 || m == 0
     then use $ fromList (Z:.0) []
     else
      let
          n          = m + 1
          put ix     = Just_ (I1 (offset ! ix))

          head_flags :: Acc (Vector Int)
          head_flags = permute const (fill (I1 n) 0) put (fill (shape szs) 1)

          idxs       = map (subtract 1)
                     $ map snd
                     $ scanl1 (segmentedL (+))
                     $ zip head_flags
                     $ fill (I1 m) 1

          iotas      = map snd
                     $ scanl1 (segmentedL const)
                     $ zip head_flags
                     $ permute const
                               (fill (I1 n) undef)
                               -- If any of the elements expand to zero new
                               -- elements then this would result in multiple
                               -- writes to the same index since the offsets are
                               -- also the same, which is undefined behaviour
                               (\ix -> if szs ! ix > 0
                                         then put ix
                                         else Nothing_)
                     $ enumFromN (shape xs) 0
      in
      zipWith g (gather iotas xs) idxs


{--
-- Sequence operations
-- -------------------

-- | Reduce a sequence by appending all the shapes and all the elements in two
-- separate vectors.
--
fromSeq :: (Shape sh, Elt a) => Seq [Array sh a] -> Seq (Vector sh, Vector a)
fromSeq = foldSeqFlatten f (lift (emptyArray, emptyArray))
  where
    f x sh1 a1 =
      let (sh0, a0) = unlift x
      in lift (sh0 ++ sh1, a0 ++ a1)


fromSeqElems :: (Shape sh, Elt a) => Seq [Array sh a] -> Seq (Vector a)
fromSeqElems = foldSeqFlatten f emptyArray
  where
    f a0 _ a1 = a0 ++ a1

fromSeqShapes :: (Shape sh, Elt a) => Seq [Array sh a] -> Seq (Vector sh)
fromSeqShapes = foldSeqFlatten f emptyArray
  where
    f sh0 sh1 _ = sh0 ++ sh1

-- | Sequence an array on the innermost dimension.
--
toSeqInner :: (Shape sh, Elt a) => Acc (Array (sh :. Int) a) -> Seq [Array sh a]
toSeqInner a = toSeq (Any :. Split) a

-- | Sequence a 2-dimensional array on the outermost dimension.
--
toSeqOuter2 :: Elt a => Acc (Array DIM2 a) -> Seq [Array DIM1 a]
toSeqOuter2 a = toSeq (Z :. Split :. All) a

-- | Sequence a 3-dimensional array on the outermost dimension.
toSeqOuter3 :: Elt a => Acc (Array DIM3 a) -> Seq [Array DIM2 a]
toSeqOuter3 a = toSeq (Z :. Split :. All :. All) a

-- | Generate a scalar sequence of a fixed given length, by applying
-- the given scalar function at each index.
generateSeq :: Elt a => Exp Int -> (Exp Int -> Exp a) -> Seq [Scalar a]
generateSeq n f = toSeq (Z :. Split) (generate (index1 n) (f . unindex1))
--}

-- Utilities
-- ---------

emptyArray :: (Shape sh, Elt e) => Acc (Array sh e)
emptyArray = fill (constant empty) undef


-- Lenses
-- ------
--
-- Imported from `lens-accelerate` (which provides more general Field instances)
--
_1 :: forall sh. Elt sh => Lens' (Exp (sh:.Int)) (Exp Int)
_1 = lens (\ix   -> let _  :. x = ix in x)
          (\ix x -> let sh :. _ = ix in sh :. x)

_2 :: forall sh. Elt sh => Lens' (Exp (sh:.Int:.Int)) (Exp Int)
_2 = lens (\ix   -> let _  :. y :. _ = ix in y)
          (\ix y -> let sh :. _ :. x = ix in sh :. y :. x)

_3 :: forall sh. Elt sh => Lens' (Exp (sh:.Int:.Int:.Int)) (Exp Int)
_3 = lens (\ix   -> let _  :. z :. _ :. _ = ix in z)
          (\ix z -> let sh :. _ :. y :. x = ix in sh :. z :. y :. x)

