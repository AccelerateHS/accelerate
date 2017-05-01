{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- |
-- Module      : Data.Array.Accelerate.Prelude
-- Copyright   : [2009..2014] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
--               [2010..2011] Ben Lever
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

  -- ** Specialised folds
  all, any, and, or, sum, product, minimum, maximum,

  -- * Scans
  prescanl, postscanl, prescanr, postscanr,

  -- ** Segmented scans
  scanlSeg, scanl'Seg, scanl1Seg, prescanlSeg, postscanlSeg,
  scanrSeg, scanr'Seg, scanr1Seg, prescanrSeg, postscanrSeg,

  -- * Shape manipulation
  flatten,

  -- * Enumeration, filling and other construction
  fill, enumFromN, enumFromStepN, emptyArray,

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

  -- * Extracting sub-vectors
  init, tail, take, drop, slit,

  -- * Controlling execution
  compute,

  -- * Flow control
  IfThenElse(..),

  -- ** Array-level
  (?|),

  -- ** Expression-level
  (?), caseof,

  -- * Scalar iteration
  iterate,

  -- * Scalar reduction
  sfoldl, -- sfoldr,

  -- * Lifting and unlifting
  Lift(..), Unlift(..),
  lift1, lift2, lift3, ilift1, ilift2, ilift3,

  -- ** Tuple construction and destruction
  fst, afst, snd, asnd, curry, uncurry, curry3, uncurry3,

  -- ** Index construction and destruction
  index0, index1, unindex1, index2, unindex2, index3, unindex3, indexSnoc,
  indexInit, indexLast,

  -- * Array operations with a scalar result
  the, null, length,

  -- * Sequence reductions
  foldSeqE, fromSeq, shapes, foldSeqFlatten,

  -- * Sequence generators
  toSeq, toSeqInner, toSeqOuter, produceScalar, fromShapes, fromOffsets,

  -- * Sequence transducers
  mapSeqE, zipWithSeqE, zipSeq, unzipSeq,

) where

-- avoid clashes with Prelude functions
--
import Data.Proxy
import Data.Typeable                                                ( gcast, eqT )
import GHC.Base                                                     ( Constraint )
import Prelude                                                      ( (.), ($), Maybe(..), const, id, fromInteger, flip, undefined, fail )
import qualified Prelude                                            as P

-- friends
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Array.Sugar                            hiding ( (!), ignore, shape, size, intersect, union, transpose, toSlice, toIndex )
import Data.Array.Accelerate.Classes
import Data.Array.Accelerate.Language
import Data.Array.Accelerate.Lift
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.Data.Bits


-- Element-wise operations
-- -----------------------

-- | Pair each element with its index
--
indexed :: (Shape sh, Elt a) => Acc (Array sh a) -> Acc (Array sh (sh, a))
indexed xs = zip (generate (shape xs) id) xs

-- | Apply a function to every element of an array and its index
--
imap :: (Shape sh, Elt a, Elt b)
     => (Exp sh -> Exp a -> Exp b)
     -> Acc (Array sh a)
     -> Acc (Array sh b)
imap f xs = zipWith f (generate (shape xs) id) xs


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


-- | Zip two arrays with a function that also takes the element index
--
izipWith :: (Shape sh, Elt a, Elt b, Elt c)
         => (Exp sh -> Exp a -> Exp b -> Exp c)
         -> Acc (Array sh a)
         -> Acc (Array sh b)
         -> Acc (Array sh c)
izipWith f as bs
  = generate (shape as `intersect` shape bs)
             (\ix -> f ix (as ! ix) (bs ! ix))

-- | Zip three arrays with a function that also takes the element index,
-- analogous to 'izipWith'.
--
izipWith3 :: (Shape sh, Elt a, Elt b, Elt c, Elt d)
          => (Exp sh -> Exp a -> Exp b -> Exp c -> Exp d)
          -> Acc (Array sh a)
          -> Acc (Array sh b)
          -> Acc (Array sh c)
          -> Acc (Array sh d)
izipWith3 f as bs cs
  = generate (shape as `intersect` shape bs `intersect` shape cs)
             (\ix -> f ix (as ! ix) (bs ! ix) (cs ! ix))

-- | Zip four arrays with the given function that also takes the element index,
-- analogous to 'zipWith'.
--
izipWith4 :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e)
          => (Exp sh -> Exp a -> Exp b -> Exp c -> Exp d -> Exp e)
          -> Acc (Array sh a)
          -> Acc (Array sh b)
          -> Acc (Array sh c)
          -> Acc (Array sh d)
          -> Acc (Array sh e)
izipWith4 f as bs cs ds
  = generate (shape as `intersect` shape bs `intersect`
              shape cs `intersect` shape ds)
             (\ix -> f ix (as ! ix) (bs ! ix) (cs ! ix) (ds ! ix))

-- | Zip five arrays with the given function that also takes the element index,
-- analogous to 'zipWith'.
--
izipWith5 :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f)
          => (Exp sh -> Exp a -> Exp b -> Exp c -> Exp d -> Exp e -> Exp f)
          -> Acc (Array sh a)
          -> Acc (Array sh b)
          -> Acc (Array sh c)
          -> Acc (Array sh d)
          -> Acc (Array sh e)
          -> Acc (Array sh f)
izipWith5 f as bs cs ds es
  = generate (shape as `intersect` shape bs `intersect` shape cs
                       `intersect` shape ds `intersect` shape es)
             (\ix -> f ix (as ! ix) (bs ! ix) (cs ! ix) (ds ! ix) (es ! ix))

-- | Zip six arrays with the given function that also takes the element index,
-- analogous to 'zipWith'.
--
izipWith6 :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g)
          => (Exp sh -> Exp a -> Exp b -> Exp c -> Exp d -> Exp e -> Exp f -> Exp g)
          -> Acc (Array sh a)
          -> Acc (Array sh b)
          -> Acc (Array sh c)
          -> Acc (Array sh d)
          -> Acc (Array sh e)
          -> Acc (Array sh f)
          -> Acc (Array sh g)
izipWith6 f as bs cs ds es fs
  = generate (shape as `intersect` shape bs `intersect` shape cs
                       `intersect` shape ds `intersect` shape es
                       `intersect` shape fs)
             (\ix -> f ix (as ! ix) (bs ! ix) (cs ! ix) (ds ! ix) (es ! ix) (fs ! ix))

-- | Zip seven arrays with the given function that also takes the element
-- index, analogous to 'zipWith'.
--
izipWith7 :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h)
          => (Exp sh -> Exp a -> Exp b -> Exp c -> Exp d -> Exp e -> Exp f -> Exp g -> Exp h)
          -> Acc (Array sh a)
          -> Acc (Array sh b)
          -> Acc (Array sh c)
          -> Acc (Array sh d)
          -> Acc (Array sh e)
          -> Acc (Array sh f)
          -> Acc (Array sh g)
          -> Acc (Array sh h)
izipWith7 f as bs cs ds es fs gs
  = generate (shape as `intersect` shape bs `intersect` shape cs
                       `intersect` shape ds `intersect` shape es
                       `intersect` shape fs `intersect` shape gs)
             (\ix -> f ix (as ! ix) (bs ! ix) (cs ! ix) (ds ! ix) (es ! ix) (fs ! ix) (gs ! ix))

-- | Zip eight arrays with the given function that also takes the element
-- index, analogous to 'zipWith'.
--
izipWith8 :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i)
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
izipWith8 f as bs cs ds es fs gs hs
  = generate (shape as `intersect` shape bs `intersect` shape cs
                       `intersect` shape ds `intersect` shape es
                       `intersect` shape fs `intersect` shape gs
                       `intersect` shape hs)
             (\ix -> f ix (as ! ix) (bs ! ix) (cs ! ix) (ds ! ix) (es ! ix) (fs ! ix) (gs ! ix) (hs ! ix))

-- | Zip nine arrays with the given function that also takes the element index,
-- analogous to 'zipWith'.
--
izipWith9 :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j)
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
izipWith9 f as bs cs ds es fs gs hs is
  = generate (shape as `intersect` shape bs `intersect` shape cs
                       `intersect` shape ds `intersect` shape es
                       `intersect` shape fs `intersect` shape gs
                       `intersect` shape hs `intersect` shape is)
             (\ix -> f ix (as ! ix) (bs ! ix) (cs ! ix) (ds ! ix) (es ! ix) (fs ! ix) (gs ! ix) (hs ! ix) (is ! ix))


-- | Combine the elements of two arrays pairwise. The shape of the result is the
-- intersection of the two argument shapes.
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
-- If the argument array is manifest in memory, 'unzip' is a NOP.
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

-- | Reduction of an array of arbitrary rank to a single scalar value. The first
-- argument needs to be an /associative/ function to enable efficient parallel
-- implementation. The initial element does not need to be an identity element.
--
-- >>> let vec = fromList (Z:.10) [0..]
-- >>> foldAll (+) 42 (use vec)
-- Scalar Z [87]
--
-- >>> let mat = fromList (Z:.5:.10) [0..]
-- >>> foldAll (+) 0 (use mat)
-- Scalar Z [1225]
--
foldAll :: (Shape sh, Elt a)
        => (Exp a -> Exp a -> Exp a)
        -> Exp a
        -> Acc (Array sh a)
        -> Acc (Scalar a)
foldAll f e arr = fold f e (flatten arr)

-- | Variant of 'foldAll' that requires the reduced array to be non-empty and
-- doesn't need an default value. The first argument must be an /associative/
-- function.
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
and = foldAll (&&) (constant True)

-- | Check if any element is 'True'
--
or :: Shape sh
   => Acc (Array sh Bool)
   -> Acc (Scalar Bool)
or = foldAll (||) (constant False)

-- | Compute the sum of elements
--
sum :: (Shape sh, Num e)
    => Acc (Array sh e)
    -> Acc (Scalar e)
sum = foldAll (+) 0

-- | Compute the product of the elements
--
product :: (Shape sh, Num e)
        => Acc (Array sh e)
        -> Acc (Scalar e)
product = foldAll (*) 1

-- | Yield the minimum element of an array. The array must not be empty.
--
minimum :: (Shape sh, Ord e)
        => Acc (Array sh e)
        -> Acc (Scalar e)
minimum = fold1All min

-- | Yield the maximum element of an array. The array must not be empty.
--
maximum :: (Shape sh, Ord e)
        => Acc (Array sh e)
        -> Acc (Scalar e)
maximum = fold1All max


-- Composite scans
-- ---------------

-- | Left-to-right prescan (aka exclusive scan).  As for 'scan', the first
-- argument must be an /associative/ function.  Denotationally, we have
--
-- > prescanl f e = Prelude.fst . scanl' f e
--
-- >>> let vec = fromList (Z:.10) [1..10]
-- >>> prescanl (+) 0 (use vec)
-- Vector (Z :. 10) [0,0,1,3,6,10,15,21,28,36]
--
prescanl :: (Shape sh, Elt a)
         => (Exp a -> Exp a -> Exp a)
         -> Exp a
         -> Acc (Array (sh:.Int) a)
         -> Acc (Array (sh:.Int) a)
prescanl f e = P.fst . scanl' f e

-- | Left-to-right postscan, a variant of 'scanl1' with an initial value. As
-- with 'scanl1', the array must not be empty. Denotationally, we have:
--
-- > postscanl f e = map (e `f`) . scanl1 f
--
-- >>> let vec = fromList (Z:.10) [1..10]
-- >>> postscanl (+) 42 (use vec)
-- Vector (Z :. 10) [42,43,45,48,52,57,63,70,78,87]
--
postscanl :: (Shape sh, Elt a)
          => (Exp a -> Exp a -> Exp a)
          -> Exp a
          -> Acc (Array (sh:.Int) a)
          -> Acc (Array (sh:.Int) a)
postscanl f e = map (e `f`) . scanl1 f

-- |Right-to-left prescan (aka exclusive scan).  As for 'scan', the first argument must be an
-- /associative/ function.  Denotationally, we have
--
-- > prescanr f e = Prelude.fst . scanr' f e
--
prescanr :: (Shape sh, Elt a)
         => (Exp a -> Exp a -> Exp a)
         -> Exp a
         -> Acc (Array (sh:.Int) a)
         -> Acc (Array (sh:.Int) a)
prescanr f e = P.fst . scanr' f e

-- |Right-to-left postscan, a variant of 'scanr1' with an initial value.  Denotationally, we have
--
-- > postscanr f e = map (e `f`) . scanr1 f
--
postscanr :: (Shape sh, Elt a)
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
-- >>> let seg = fromList (Z:.4) [1,4,0,3]
-- >>> seg
-- Vector (Z :. 4) [1,4,0,3]
--
-- >>> let mat = fromList (Z:.5:.10) [0..]
-- >>> mat
-- Matrix (Z :. 5 :. 10)
--   [  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
--     10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
--     20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
--     30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
--     40, 41, 42, 43, 44, 45, 46, 47, 48, 49]
--
-- >>> scanlSeg (+) 0 (use mat) (use seg)
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
    then fill (lift (sh:.sz + length seg)) z
    else scanl1Seg f arr' seg'
  where
    sh :. sz    = unlift (shape arr) :: Exp sh :. Exp Int

    -- Segmented exclusive scan is implemented by first injecting the seed
    -- element at the head of each segment, and then performing a segmented
    -- inclusive scan.
    --
    -- This is done by creating a creating a vector entirely of the seed
    -- element, and overlaying the input data in all places other than at the
    -- start of a segment.
    --
    seg'        = map (+1) seg
    arr'        = permute const
                          (fill (lift (sh :. sz + length seg)) z)
                          (\ix -> let sx :. i = unlift ix :: Exp sh :. Exp Int
                                  in  lift (sx :. i + fromIntegral (inc ! index1 i)))
                          (take (length flags) arr)

    -- Each element in the segments must be shifted to the right one additional
    -- place for each successive segment, to make room for the seed element.
    -- Here, we make use of the fact that the vector returned by 'mkHeadFlags'
    -- contains non-unit entries, which indicate zero length segments.
    --
    flags       = mkHeadFlags seg
    inc         = scanl1 (+) flags


-- | Segmented version of 'scanl'' along the innermost dimension of an array. The
-- innermost dimension must have at least as many elements as the sum of the
-- segment descriptor.
--
-- The first element of the resulting tuple is a vector of scanned values. The
-- second element is a vector of segment scan totals and has the same size as
-- the segment vector.
--
-- >>> let seg = fromList (Z:.4) [1,4,0,3]
-- >>> seg
-- Vector (Z :. 4) [1,4,0,3]
--
-- >>> let mat = fromList (Z:.5:.10) [0..]
-- >>> mat
-- Matrix (Z :. 5 :. 10)
--   [  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
--     10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
--     20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
--     30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
--     40, 41, 42, 43, 44, 45, 46, 47, 48, 49]
--
-- >>> let (res,sums) = scanl'Seg (+) 0 (use mat) (use seg)
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
    then lift (arr,  fill (lift (indexTail (shape arr) :. length seg)) z)
    else lift (body, sums)
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
    tails       = zipWith (+) seg . P.fst $ scanl' (+) 0 seg'
    sums        = backpermute
                    (lift (indexTail (shape arr') :. length seg))
                    (\ix -> let sz:.i = unlift ix :: Exp sh :. Exp Int
                            in  lift (sz :. fromIntegral (tails ! index1 i)))
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
                $ permute (+) (fill (index1 $ size arr + 1) 0)
                              (\ix -> index1' $ offset ! ix)
                              (fill (shape seg) (1 :: Exp i))

    len         = offset ! index1 (length offset - 1)
    body        = backpermute
                    (lift (indexTail (shape arr) :. fromIntegral len))
                    (\ix -> let sz:.i = unlift ix :: Exp sh :. Exp Int
                            in  lift (sz :. i + fromIntegral (inc ! index1 i)))
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
-- >>> let seg = fromList (Z:.4) [1,4,0,3]
-- >>> seg
-- Vector (Z :. 4) [1,4,0,3]
--
-- >>> let mat = fromList (Z:.5:.10) [0..]
-- >>> mat
-- Matrix (Z :. 5 :. 10)
--   [  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
--     10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
--     20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
--     30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
--     40, 41, 42, 43, 44, 45, 46, 47, 48, 49]
--
-- >>> scanl1Seg (+) (use mat) (use seg)
-- Matrix (Z :. 5 :. 8)
--   [  0,  1,  3,   6,  10,  5, 11,  18,
--     10, 11, 23,  36,  50, 15, 31,  48,
--     20, 21, 43,  66,  90, 25, 51,  78,
--     30, 31, 63,  96, 130, 35, 71, 108,
--     40, 41, 83, 126, 170, 45, 91, 138]
--
scanl1Seg
    :: (Shape sh, Slice sh, Elt e, Integral i, Bits i, FromIntegral i Int)
    => (Exp e -> Exp e -> Exp e)
    -> Acc (Array (sh:.Int) e)
    -> Acc (Segments i)
    -> Acc (Array (sh:.Int) e)
scanl1Seg f arr seg
  = P.snd
  . unzip
  . scanl1 (segmented f)
  $ zip (replicate (lift (indexTail (shape arr) :. All)) (mkHeadFlags seg)) arr

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
-- >>> let seg = fromList (Z:.4) [1,4,0,3]
-- >>> seg
-- Vector (Z :. 4) [1,4,0,3]
--
-- >>> let mat = fromList (Z:.5:.10) [0..]
-- >>> mat
-- Matrix (Z :. 5 :. 10)
--   [  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
--     10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
--     20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
--     30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
--     40, 41, 42, 43, 44, 45, 46, 47, 48, 49]
--
-- >>> scanrSeg (+) 0 (use mat) (use seg)
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
    then fill (lift (sh :. sz + length seg)) z
    else scanr1Seg f arr' seg'
  where
    sh :. sz    = unlift (shape arr) :: Exp sh :. Exp Int

    -- Using technique described for 'scanlSeg', where we intersperse the array
    -- with the seed element at the start of each segment, and then perform an
    -- inclusive segmented scan.
    --
    flags       = mkHeadFlags seg
    inc         = scanl1 (+) flags

    seg'        = map (+1) seg
    arr'        = permute const
                          (fill (lift (sh :. sz + length seg)) z)
                          (\ix -> let sx :. i = unlift ix :: Exp sh :. Exp Int
                                  in  lift (sx :. i + fromIntegral (inc ! index1 i) - 1))
                          (drop (sz - length flags) arr)


-- | Segmented version of 'scanr''.
--
-- >>> let seg = fromList (Z:.4) [1,4,0,3]
-- >>> seg
-- Vector (Z :. 4) [1,4,0,3]
--
-- >>> let mat = fromList (Z:.5:.10) [0..]
-- >>> mat
-- Matrix (Z :. 5 :. 10)
--   [  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
--     10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
--     20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
--     30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
--     40, 41, 42, 43, 44, 45, 46, 47, 48, 49]
--
-- >>> let (res,sums) = scanr'Seg (+) 0 (use mat) (use seg)
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
    then lift (arr,  fill (lift (indexTail (shape arr) :. length seg)) z)
    else lift (body, sums)
  where
    -- Using technique described for scanl'Seg
    --
    arr'        = scanrSeg f z arr seg

    -- reduction values
    seg'        = map (+1) seg
    heads       = P.fst $ scanl' (+) 0 seg'
    sums        = backpermute
                    (lift (indexTail (shape arr') :. length seg))
                    (\ix -> let sz:.i = unlift ix :: Exp sh :. Exp Int
                            in  lift (sz :. fromIntegral (heads ! index1 i)))
                    arr'

    -- body segments
    flags       = mkHeadFlags seg
    inc         = scanl1 (+) flags
    body        = backpermute
                    (lift (indexTail (shape arr) :. indexHead (shape flags)))
                    (\ix -> let sz:.i = unlift ix :: Exp sh :. Exp Int
                            in  lift (sz :. i + fromIntegral (inc ! index1 i)))
                    arr'


-- | Segmented version of 'scanr1'.
--
-- >>> let seg = fromList (Z:.4) [1,4,0,3]
-- >>> seg
-- Vector (Z :. 4) [1,4,0,3]
--
-- >>> let mat = fromList (Z:.5:.10) [0..]
-- >>> mat
-- Matrix (Z :. 5 :. 10)
--   [  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
--     10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
--     20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
--     30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
--     40, 41, 42, 43, 44, 45, 46, 47, 48, 49]
--
-- >>> scanr1Seg (+) (use mat) (use seg)
-- Matrix (Z :. 5 :. 8)
--   [  0,  10,   9,  7,  4,  18, 13,  7,
--     10,  50,  39, 27, 14,  48, 33, 17,
--     20,  90,  69, 47, 24,  78, 53, 27,
--     30, 130,  99, 67, 34, 108, 73, 37,
--     40, 170, 129, 87, 44, 138, 93, 47]
--
scanr1Seg
    :: (Shape sh, Slice sh, Elt e, Integral i, Bits i, FromIntegral i Int)
    => (Exp e -> Exp e -> Exp e)
    -> Acc (Array (sh:.Int) e)
    -> Acc (Segments i)
    -> Acc (Array (sh:.Int) e)
scanr1Seg f arr seg
  = P.snd
  . unzip
  . scanr1 (flip (segmented f))
  $ zip (replicate (lift (indexTail (shape arr) :. All)) (mkTailFlags seg)) arr


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

-- |Compute head flags vector from segment vector for left-scans.
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
  $ permute (+) zeros (\ix -> index1' (offset ! ix)) ones
  where
    (offset, len)       = scanl' (+) 0 seg
    zeros               = fill (index1' $ the len + 1) 0
    ones                = fill (index1  $ size offset) 1

-- |Compute tail flags vector from segment vector for right-scans. That is, the
-- flag is placed at the last place in each segment.
--
mkTailFlags
    :: (Integral i, FromIntegral i Int)
    => Acc (Segments i)
    -> Acc (Segments i)
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
segmented
    :: (Elt e, Num i, Bits i)
    => (Exp e -> Exp e -> Exp e)
    -> Exp (i, e)
    -> Exp (i, e)
    -> Exp (i, e)
segmented f a b =
  let (aF, aV) = unlift a
      (bF, bV) = unlift b
  in
  lift (aF .|. bF, bF /= 0 ? (bV, f aV bV))

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
index1' ::  (Integral i, FromIntegral i Int) => Exp i -> Exp DIM1
index1' i = lift (Z :. fromIntegral i)


-- Reshaping of arrays
-- -------------------

-- | Flatten the given array of arbitrary dimension into a one-dimensional
-- vector. As with 'reshape', this operation performs no work.
--
flatten :: forall sh e. (Shape sh, Elt e) => Acc (Array sh e) -> Acc (Vector e)
flatten a
  | Just Refl <- matchShapeType (undefined::sh) (undefined::DIM1)
  = a
flatten a
  = reshape (index1 $ size a) a


-- Enumeration and filling
-- -----------------------

-- | Create an array where all elements are the same value.
--
fill :: (Shape sh, Elt e) => Exp sh -> Exp e -> Acc (Array sh e)
fill sh c = generate sh (const c)

-- | Create an array of the given shape containing the values @x@, @x+1@, etc.
-- (in row-major order).
--
-- >>> enumFromN (constant (Z:.5:.10)) 0 :: Array DIM2 Int
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
-- >>> enumFromStepN (constant (Z:.5:.10)) 0 0.5 :: Array DIM2 Float
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
  $ generate (index1 $ shapeSize sh)
             (\ix -> (fromIntegral (unindex1 ix :: Exp Int) * y) + x)

-- Concatenation
-- -------------

-- | Concatenate innermost component of two arrays. The extent of the lower
--   dimensional component is the intersection of the two arrays.
--
-- >>> let m1 = fromList (Z:.5:.10) [0..]
-- >>> m1
-- Matrix (Z :. 5 :. 10)
--   [  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
--     10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
--     20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
--     30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
--     40, 41, 42, 43, 44, 45, 46, 47, 48, 49]
--
-- >>> let m2 = fromList (Z:.10:.3) [0..]
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
-- >>> use m1 ++ use m2
-- Matrix (Z :. 5 :. 13)
--   [  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  0,  1,  2,
--     10, 11, 12, 13, 14, 15, 16, 17, 18, 19,  3,  4,  5,
--     20, 21, 22, 23, 24, 25, 26, 27, 28, 29,  6,  7,  8,
--     30, 31, 32, 33, 34, 35, 36, 37, 38, 39,  9, 10, 11,
--     40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 12, 13, 14]
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
                     in  i < n ? ( xs ! ix, ys ! lift (sh :. i-n)) )

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
-- >>> filter even (use vec)
-- (Vector (Z :. 5) [2,4,6,8,10], Scalar Z [5])
--
-- >>> let mat = fromList (Z :. 4 :. 10) [1,2,3,4,5,6,7,8,9,10,1,1,1,1,1,2,2,2,2,2,2,4,6,8,10,12,14,16,18,20,1,3,5,7,9,11,13,15,17,19] :: Array DIM2 Int
-- >>> mat
-- Matrix (Z :. 4 :. 10)
--   [ 1, 2, 3, 4,  5,  6,  7,  8,  9, 10,
--     1, 1, 1, 1,  1,  2,  2,  2,  2,  2,
--     2, 4, 6, 8, 10, 12, 14, 16, 18, 20,
--     1, 3, 5, 7,  9, 11, 13, 15, 17, 19]
--
-- >>> filter odd (use mat)
-- (Vector (Z :. 20) [1,3,5,7,9,1,1,1,1,1,1,3,5,7,9,11,13,15,17,19], Vector (Z :. 4) [5,5,0,10])
--
filter :: forall sh e. (Shape sh, Slice sh, Elt e)
       => (Exp e -> Exp Bool)
       -> Acc (Array (sh:.Int) e)
       -> Acc (Vector e, Array sh Int)
filter p arr
  -- Optimise 1-dimensional arrays, where we can avoid additional computations
  -- for the offset indices.
  | Just Refl <- matchShapeType (undefined::sh) (undefined::Z)
  = let
        keep            = map p arr
        (target, len)   = scanl' (+) 0 (map boolToInt keep)
        prj ix          = keep!ix ? ( index1 (target!ix), ignore )
        dummy           = backpermute (index1 (the len)) id arr
        result          = permute const dummy prj arr
    in
    if null arr
      then lift (emptyArray, fill (constant Z) 0)
      else lift (result, len)

filter p arr
  = let
        sz              = indexTail (shape arr)
        keep            = map p arr
        (target, len)   = scanl' (+) 0 (map boolToInt keep)
        (offset, valid) = scanl' (+) 0 (flatten len)
        prj ix          = if keep!ix
                            then index1 $ offset!index1 (toIndex sz (indexTail ix)) + target!ix
                            else ignore
        dummy           = backpermute (index1 (the valid)) id (flatten arr)
        result          = permute const dummy prj arr
    in
    if null arr
      then lift (emptyArray, fill sz 0)
      else lift (result, len)

-- FIXME: [Permute in the filter operation]
--
-- This is abusing 'permute' in that the first two arguments, the combination
-- function and array of default values, are only justified because we know the
-- permutation function will write to each location in the target exactly once.
--
-- Instead, we should have a primitive that directly encodes the compaction
-- pattern of the permutation function. This may be more efficient to compute,
-- and avoids the computation of the defaults array, which is ultimately wasted
-- work.
--

{-# NOINLINE filter #-}
{-# RULES
  "ACC filter/filter" forall f g arr.
    filter f (afst (filter g arr)) = filter (\x -> g x && f x) arr
 #-}


-- Gather operations
-- -----------------

-- | Gather elements from a source array by reading values at the given indices.
--
-- >>> let input = fromList (Z:.9) [1,9,6,4,4,2,0,1,2]
-- >>> let from  = fromList (Z:.6) [1,3,7,2,5,3]
-- >>> gather (use from) (use input)
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
-- >>> let defaults = fromList (Z :. 6) [6,6,6,6,6,6]
-- >>> let from     = fromList (Z :. 6) [1,3,7,2,5,3]
-- >>> let mask     = fromList (Z :. 6) [3,4,9,2,7,5]
-- >>> let input    = fromList (Z :. 9) [1,9,6,4,4,2,0,1,2]
-- >>> gatherIf (use from) (use mask) (> 4) (use defaults) (use input)
-- Vector (Z :. 6) [6,6,1,6,2,4]
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
-- >>> let to    = fromList (Z :. 6) [1,3,7,2,5,8]
-- >>> let input = fromList (Z :. 7) [1,9,6,4,4,2,5]
-- >>> scatter (use to) (fill (constant (Z:.10)) 0) (use input)
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
    pf ix       = index1 (to ! ix)
    input'      = backpermute (shape to `intersect` shape input) id input


-- | Conditionally overwrite elements of the destination by scattering values of
-- the source array according to a given index mapping, whenever the masking
-- function resolves to 'True'.
--
-- Note that if the destination index appears more than once in the mapping the
-- result is undefined.
--
-- >>> let to    = fromList (Z :. 6) [1,3,7,2,5,8]
-- >>> let mask  = fromList (Z :. 6) [3,4,9,2,7,5]
-- >>> let input = fromList (Z :. 7) [1,9,6,4,4,2,5]
-- >>> scatterIf (use to) (use mask) (> 4) (fill (constant (Z:.10)) 0) (use input)
-- Vector (Z :. 10) [0,0,0,0,0,4,0,6,2,0]
--
scatterIf
    :: (Elt e, Elt e')
    => Acc (Vector Int)           -- ^ destination indices to scatter into
    -> Acc (Vector e)             -- ^ mask vector
    -> (Exp e -> Exp Bool)        -- ^ predicate function
    -> Acc (Vector e')            -- ^ default values
    -> Acc (Vector e')            -- ^ source values
    -> Acc (Vector e')
scatterIf to maskV pred defaults input = permute const defaults pf input'
  where
    pf ix       = pred (maskV ! ix) ? ( index1 (to ! ix), ignore )
    input'      = backpermute (shape to `intersect` shape input) id input


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
transpose :: (Shape sh, Elt e) => Acc (Array sh e) -> Acc (Array sh e)
transpose mat = backpermute (indexTrans $ shape mat) indexTrans mat


-- Extracting sub-vectors
-- ----------------------

-- | Yield the first @n@ elements in the innermost dimension of the array (plus
-- all lower dimensional elements).
--
-- >>> let mat = fromList (Z:.5:.10) [0..]
-- >>> mat
-- Matrix (Z :. 5 :. 10)
--   [  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
--     10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
--     20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
--     30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
--     40, 41, 42, 43, 44, 45, 46, 47, 48, 49]
--
-- >>> take 5 (use mat)
-- Matrix (Z :. 5 :. 5)
--   [  0,  1,  2,  3,  4,
--     10, 11, 12, 13, 14,
--     20, 21, 22, 23, 24,
--     30, 31, 32, 33, 34,
--     40, 41, 42, 43, 44]
--
take :: forall sh e. (Slice sh, Shape sh, Elt e)
     => Exp Int
     -> Acc (Array (sh :. Int) e)
     -> Acc (Array (sh :. Int) e)
take n acc =
  let n'        = the (unit (n `min` sz))
      sh :. sz  = unlift (shape acc)            :: Exp sh :. Exp Int
  in
  backpermute (lift (sh :. n')) id acc


-- | Yield all but the first @n@ elements along the innermost dimension of the
-- array (plus all lower dimensional elements).
--
-- >>> let mat = fromList (Z:.5:.10) [0..]
-- >>> mat
-- Matrix (Z :. 5 :. 10)
--   [  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
--     10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
--     20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
--     30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
--     40, 41, 42, 43, 44, 45, 46, 47, 48, 49]
--
-- >>> drop 7 (use mat)
-- Matrix (Z :. 5 :. 3)
--   [  7,  8,  9,
--     17, 18, 19,
--     27, 28, 29,
--     37, 38, 39,
--     47, 48, 49]
--
drop :: forall sh e. (Slice sh, Shape sh, Elt e)
     => Exp Int
     -> Acc (Array (sh :. Int) e)
     -> Acc (Array (sh :. Int) e)
drop n acc =
  let n'        = the (unit n)
      sh :. sz  = unlift (shape acc)            :: Exp sh :. Exp Int
      index ix  = let j :. i = unlift ix        :: Exp sh :. Exp Int
                  in  lift (j :. i + n')
  in
  backpermute (lift (sh :. 0 `max` (sz - n'))) index acc


-- | Yield all but the elements in the last index of the innermost dimension.
--
-- >>> let mat = fromList (Z:.5:.10) [0..]
-- >>> mat
-- Matrix (Z :. 5 :. 10)
--   [  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
--     10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
--     20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
--     30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
--     40, 41, 42, 43, 44, 45, 46, 47, 48, 49]
--
-- >>> init (use mat)
-- Matrix (Z :. 5 :. 9)
--   [  0,  1,  2,  3,  4,  5,  6,  7,  8,
--     10, 11, 12, 13, 14, 15, 16, 17, 18,
--     20, 21, 22, 23, 24, 25, 26, 27, 28,
--     30, 31, 32, 33, 34, 35, 36, 37, 38,
--     40, 41, 42, 43, 44, 45, 46, 47, 48]
--
init :: forall sh e. (Slice sh, Shape sh, Elt e)
     => Acc (Array (sh :. Int) e)
     -> Acc (Array (sh :. Int) e)
init acc =
  let sh :. sz  = unlift (shape acc)            :: Exp sh :. Exp Int
  in  backpermute (lift (sh :. sz `min` (sz - 1))) id acc


-- | Yield all but the first element along the innermost dimension of an array.
-- The innermost dimension must not be empty.
--
-- >>> let mat = fromList (Z:.5:.10) [0..]
-- >>> mat
-- Matrix (Z :. 5 :. 10)
--   [  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
--     10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
--     20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
--     30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
--     40, 41, 42, 43, 44, 45, 46, 47, 48, 49]
--
-- >>> tail (use mat)
-- Matrix (Z :. 5 :. 9)
--   [  1,  2,  3,  4,  5,  6,  7,  8,  9,
--     11, 12, 13, 14, 15, 16, 17, 18, 19,
--     21, 22, 23, 24, 25, 26, 27, 28, 29,
--     31, 32, 33, 34, 35, 36, 37, 38, 39,
--     41, 42, 43, 44, 45, 46, 47, 48, 49]
--
tail :: forall sh e. (Slice sh, Shape sh, Elt e)
     => Acc (Array (sh :. Int) e)
     -> Acc (Array (sh :. Int) e)
tail acc =
  let sh :. sz  = unlift (shape acc)            :: Exp sh :. Exp Int
      index ix  = let j :. i = unlift ix        :: Exp sh :. Exp Int
                  in  lift (j :. i + 1)
  in
  backpermute (lift (sh :. 0 `max` (sz - 1))) index acc


-- | Yield a slit (slice) of the innermost indices of an array. Denotationally,
-- we have:
--
-- > slit i n = take n . drop i
--
slit :: forall sh e. (Slice sh, Shape sh, Elt e)
     => Exp Int
     -> Exp Int
     -> Acc (Array (sh :. Int) e)
     -> Acc (Array (sh :. Int) e)
slit m n acc =
  let m'        = the (unit m)
      n'        = the (unit n)
      sh :. sz  = unlift (shape acc)            :: Exp sh :. Exp Int
      index ix  = let j :. i = unlift ix        :: Exp sh :. Exp Int
                  in  lift (j :. i + m')
  in
  backpermute (lift (sh :. (n' `min` ((sz - m') `max` 0)))) index acc


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
-- See also: 'ifThenElse'.
--
infix 0 ?|
(?|) :: Arrays a => Exp Bool -> (Acc a, Acc a) -> Acc a
c ?| (t, e) = acond c t e

-- | An infix version of 'cond'. If the predicate evaluates to 'True', the first
-- component of the tuple is returned, else the second.
--
-- See also: 'ifThenElse'.
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


-- | For use with @-XRebindableSyntax@, this class provides 'ifThenElse' lifted
-- to both scalar and array types.
--
class IfThenElse t where
  type EltT t a :: Constraint
  ifThenElse :: EltT t a => Exp Bool -> t a -> t a -> t a

instance IfThenElse Exp where
  type EltT Exp t = Elt t
  ifThenElse = cond

instance IfThenElse Acc where
  type EltT Acc a = Arrays a
  ifThenElse = acond


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
    snd $ while (\v -> fst v < n) (lift1 step) (lift (constant 0, z))


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
    snd $ while (\v -> fst v < n) (lift1 step) (lift (constant 0, z))


-- Tuples
-- ------

-- |Extract the first component of a scalar pair.
--
fst :: forall a b. (Elt a, Elt b) => Exp (a, b) -> Exp a
fst e = let (x, _::Exp b) = unlift e in x

-- |Extract the first component of an array pair.
afst :: forall a b. (Arrays a, Arrays b) => Acc (a, b) -> Acc a
afst a = let (x, _::Acc b) = unlift a in x

-- |Extract the second component of a scalar pair.
--
snd :: forall a b. (Elt a, Elt b) => Exp (a, b) -> Exp b
snd e = let (_:: Exp a, y) = unlift e in y

-- | Extract the second component of an array pair
asnd :: forall a b. (Arrays a, Arrays b) => Acc (a, b) -> Acc b
asnd a = let (_::Acc a, y) = unlift a in y

-- |Converts an uncurried function to a curried function.
--
curry :: Lift f (f a, f b) => (f (Plain (f a), Plain (f b)) -> f c) -> f a -> f b -> f c
curry f x y = f (lift (x, y))

-- |Converts a curried function to a function on pairs.
--
uncurry :: Unlift f (f a, f b) => (f a -> f b -> f c) -> f (Plain (f a), Plain (f b)) -> f c
uncurry f t = let (x, y) = unlift t in f x y

-- |Same as curry but for triples
--
curry3 :: Lift f (f a, f b, f c) => (f (Plain (f a), Plain (f b), Plain (f c)) -> f d) -> f a -> f b -> f c -> f d
curry3 f x y z = f (lift (x, y, z))

-- |Same as for uncurry but for triples
--
uncurry3 :: Unlift f (f a, f b, f c) => (f a -> f b -> f c -> f d) -> f (Plain (f a), Plain (f b), Plain (f c)) -> f d
uncurry3 f t = let (x, y, z) = unlift t in f x y z


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

-- | Create a rank-3 index from three Exp Int`s
--
index3
    :: (Elt i, Slice (Z :. i), Slice (Z :. i :. i))
    => Exp i
    -> Exp i
    -> Exp i
    -> Exp (Z :. i :. i :. i)
index3 k j i = lift (Z :. k :. j :. i)

-- | Destruct a rank-3 index into an Exp tuple of Int`s
unindex3
    :: forall i. (Elt i, Slice (Z :. i), Slice (Z :. i :. i))
    => Exp (Z :. i :. i :. i)
    -> Exp (i, i, i)
unindex3 ix = let Z :. k :. j :. i = unlift ix  :: Z :. Exp i :. Exp i :. Exp i
              in  lift (k, j, i)

indexLast
  :: forall sh. Shape sh
  => Exp (sh:.Int)
  -> Exp Int
indexLast | AsSlice <- asSlice (Proxy :: Proxy sh)
          = indexHead . indexTrans

indexInit
  :: forall sh. Shape sh
  => Exp (sh:.Int)
  -> Exp sh
indexInit | AsSlice <- asSlice (Proxy :: Proxy sh)
          = indexTrans . indexTail . indexTrans

indexSnoc
  :: forall sh. Shape sh
  => Exp sh
  -> Exp Int
  -> Exp (sh:.Int)
indexSnoc sh | AsSlice <- asSlice (Proxy :: Proxy sh)
             = indexTrans . (Exp . IndexCons (indexTrans sh))



-- Array operations with a scalar result
-- -------------------------------------

-- | Extract the element of a singleton array.
--
-- > the xs  ==  xs ! Z
--
the :: Elt e => Acc (Scalar e) -> Exp e
the = (!index0)

-- | Test whether an array is empty.
--
null :: (Shape sh, Elt e) => Acc (Array sh e) -> Exp Bool
null arr = size arr == 0

-- | Get the length of a vector.
--
length :: Elt e => Acc (Vector e) -> Exp Int
length = unindex1 . shape


--
-- Sequence operations
-- --------------------------------------

-- | foldSeqE (+) a0 x seq. Fold a sequence x by combining each
-- element using the given binary operation (+). (+) must be
-- associative:
--
--   Forall a b c. (a + b) + c = a + (b + c),
--
foldSeqE :: Elt a
         => (Exp a -> Exp a -> Exp a)
         -> Exp a
         -> Seq [Scalar a]
         -> Seq (Scalar a)
foldSeqE f z = foldBatch (\s as -> lift (s, elements as)) (\(unatup2 -> (s,as)) -> fold f (the s) as) (unit z)

-- | Reduce a sequence by appending all the shapes and all the
-- elements in two seperate vectors.
--
fromSeq :: (Shape ix, Elt a) => Seq [Array ix a] -> Seq (Vector ix, Vector a)
fromSeq s = lift (shapes s, elements s)

shapes :: (Shape ix, Elt a) => Seq [Array ix a] -> Seq (Vector ix)
shapes = elements . mapSeq (unit . shape)

-- | Sequence an array on the innermost dimension.
--
toSeqInner :: (Shape sh, Elt a) => Acc (Array (sh :. Int) a) -> Seq [Array sh a]
toSeqInner = toSeq (lift (Any :. (0 :: Int)))

-- | Sequence an array on the outermost dimension.
--
toSeqOuter :: (Shape sh, Elt e) => Acc (Array (sh:.Int) e) -> Seq [Array sh e]
toSeqOuter = mapSeq transpose . toSeqInner . transpose

-- | Generate a scalar sequence of a fixed given length, by applying
-- the given scalar function at each index.
--
produceScalar :: Elt a => Exp Int -> (Exp Int -> Exp a) -> Seq [Scalar a]
produceScalar n f = produce n (unit . f . the)

-- | Produce a sequence from shape segments and a flattened vector of values.
--
fromShapes :: (Shape sh, Elt e) => Acc (Segments sh) -> Acc (Vector e) -> Seq [Array sh e]
fromShapes shs = fromSegs (zip offs shs) (length shs)
  where
    (offs,_) = scanl' (+) 0 (map shapeSize shs)

-- | Produce a sequence from shape segments and a flattened vector of values.
--
fromOffsets :: Elt e => Acc (Segments Int) -> Acc (Vector e) -> Seq [Vector e]
fromOffsets offs vs = fromSegs (zip offs shs) (length offs) vs
  where
    ts  = length vs
    shs = map index1 (zipWith (-) (offs ++ flatten (unit ts)) offs)

-- | Map over sequences specialised to scalar sequences.
--
mapSeqE :: (Elt a, Elt b) => (Exp a -> Exp b) -> Seq [Scalar a] -> Seq [Scalar b]
mapSeqE f = mapSeq (map f)

-- | ZipWith over sequences specialised to scalar sequences.
--
zipWithSeqE :: (Elt a, Elt b, Elt c)
            => (Exp a -> Exp b -> Exp c)
            -> Seq [Scalar a]
            -> Seq [Scalar b]
            -> Seq [Scalar c]
zipWithSeqE f = zipWithSeq (zipWith f)

-- Take two sequences and turn them into a sequence of tuples
--
zipSeq :: (Arrays a, Arrays b) => Seq [a] -> Seq [b] -> Seq [(a,b)]
zipSeq = zipWithSeq (curry lift)

-- Unzip for sequences
--
unzipSeq :: (Arrays a, Arrays b) => Seq [(a,b)] -> (Seq [a], Seq [b])
unzipSeq s = (mapSeq afst s, mapSeq asnd s)

-- | Perform an exclusive left-to-right scan over a scalar sequence x by
-- combining each element using the given binary operation (+). (+) must be
-- associative.
--
-- scanSeqE :: Elt e
--          => (Exp e -> Exp e -> Exp e)
--          -> Exp e
--          -> Seq [Scalar e]
--          -> Seq [Scalar e]
-- scanSeqE f z = mapSeq asnd .
--                mapBatch (const id)
--                         (\acc es -> let (es', acc') = scanl' f (the acc) (flatten (unregular es))
--                                     in lift (acc', regular es'))
--                         (\acc es -> let (es', acc') = scanl' f (the acc) (irregularValues es)
--                                     in lift (acc', sparsify (regular es')))
--                         (unit z)

-- | Convert the given array to a sequence by dividing the array up into slices.
-- This is similar to 'slice' in the way the array is indexed. The initial slice
-- index represents the starting point with the sequence yielding each
-- subsequent slice.
--
toSeq :: forall slix a. (Slice slix, Elt a)
      => Exp slix
      -> Acc (Array (FullShape slix) a)
      -> Seq [Array (SliceShape slix) a]
toSeq spec acc
  | Just Refl <- eqT :: Maybe (slix :~: DIM1)
  = produce (size acc - unindex1 spec) (\ix -> unit (acc !! (unindex1 spec + the ix)))
toSeq spec acc
  = let length = slicesLeft spec (shape acc)
    in produce length (\ix -> slice acc (toSlice spec (shape acc) (the ix)))

slicesLeft :: forall slix. Slice slix => Exp slix -> Exp (FullShape slix) -> Exp Int
slicesLeft spec sh = total - soFar
  where
    soFar = shapeSize (indexFull spec (constant sl1))

    total | ASlice spec' <- coSlice (sliceType (undefined :: slix -> slix)) :: ASlice (FullShape slix)
          = shapeSize (indexSlice (constant spec') sh)

    sl1 :: SliceShape slix
    sl1 = listToShape (P.replicate (rank (undefined :: SliceShape slix)) 1)

data ASlice sh where
  ASlice :: (Slice slix, FullShape slix ~ sh) => slix -> ASlice sh

coSlice :: forall slix. SliceR slix -> ASlice (FullShape slix)
coSlice SliceRnil = ASlice Z
coSlice (SliceRall sl) | ASlice sl' <- coSlice sl
                       = ASlice (sl':.(0 ::Int))
coSlice (SliceRfixed sl) | ASlice sl' <- coSlice sl
                         = ASlice (sl':.All)
coSlice SliceRany | AsSlice <- asSlice (const anyShape)
                  = ASlice (anyShape)
  where
    anyShape :: forall sh. (slix ~ Any sh, Shape sh) => sh
    anyShape = listToShape (P.replicate (rank (undefined :: sh)) 0)

-- | foldSeqFlatten f a0 x seq. f must be semi-associative, with
-- vecotor append (++) as the companion operator:
--
--   Forall b sh1 a1 sh2 a2.
--     f (f b sh1 a1) sh2 a2 = f b (sh1 ++ sh2) (a1 ++ a2).
--
-- It is common to ignore the shape vectors, yielding the usual
-- semi-associativity law:
--
--   f b a _ = b + a,
--
-- for some (+) satisfying:
--
--   Forall b a1 a2. (b + a1) + a2 = b + (a1 ++ a2).
--
foldSeqFlatten :: (Arrays a, Shape sh, Elt e)
               => (Acc a -> Acc (Vector sh) -> Acc (Vector e) -> Acc a)
               -> Acc a
               -> Seq [Array sh e]
               -> Seq a
foldSeqFlatten f = foldBatch (\a s -> lift (a, shapes s, elements s)) (uncurry3 f)


-- Utilities
-- ---------

emptyArray :: (Shape sh, Elt e) => Acc (Array sh e)
emptyArray = use (fromList empty [])

matchShapeType :: forall s t. (Shape s, Shape t) => s -> t -> Maybe (s :~: t)
matchShapeType _ _
  | Just Refl <- matchTupleType (eltType (undefined::s)) (eltType (undefined::t))
  = gcast Refl

matchShapeType _ _
  = Nothing
