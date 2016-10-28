{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
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
  fst, afst, snd, asnd, curry, uncurry,

  -- ** Index construction and destruction
  index0, index1, unindex1, index2, unindex2, index3, unindex3,

  -- * Array operations with a scalar result
  the, null, length,

  -- * Sequence operations
  fromSeq, fromSeqElems, fromSeqShapes, toSeqInner, toSeqOuter2, toSeqOuter3, generateSeq,

) where

-- avoid clashes with Prelude functions
--
import Data.Typeable                                                ( gcast )
import GHC.Base                                                     ( Constraint )
import Prelude                                                      ( (.), ($), Maybe(..), const, id, flip, undefined )
import qualified Prelude                                            as P

-- friends
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Array.Sugar                            hiding ( (!), ignore, shape, size, intersect )
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


-- | Zip two arrays with a function that also takes the elements' index
--
izipWith :: (Shape sh, Elt a, Elt b, Elt c)
         => (Exp sh -> Exp a -> Exp b -> Exp c)
         -> Acc (Array sh a)
         -> Acc (Array sh b)
         -> Acc (Array sh c)
izipWith f as bs
  = generate (shape as `intersect` shape bs)
             (\ix -> f ix (as ! ix) (bs ! ix))

-- | Zip three arrays with a function that also takes the elements index,
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

-- | Zip four arrays with the given function that also takes the elements index,
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

-- | Zip five arrays with the given function that also takes the elements index,
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

-- | Zip six arrays with the given function that also takes the elements index,
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

-- | Zip seven arrays with the given function that also takes the elements
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

-- | Zip eight arrays with the given function that also takes the elements
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

-- | Zip nine arrays with the given function that also takes the elements index,
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
scanlSeg :: (Elt a, Integral i, Bits i, FromIntegral i Int, FromIntegral Int i)
         => (Exp a -> Exp a -> Exp a)
         -> Exp a
         -> Acc (Vector a)
         -> Acc (Segments i)
         -> Acc (Vector a)
scanlSeg f z vec seg = null flags ?| ( fill (shape seg) z , scanl1Seg f vec' seg' )
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
scanl'Seg :: forall a i. (Elt a, Integral i, Bits i, FromIntegral i Int, FromIntegral Int i)
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
    result      = lift ( null vec  ?| (emptyArray, body)
                       , null vec' ?| (emptyArray, sums)
                       )

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


-- | Segmented version of 'scanl1'.
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
scanl1Seg :: (Elt a, Integral i, Bits i, FromIntegral i Int)
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
prescanlSeg :: (Elt a, Integral i, Bits i, FromIntegral i Int, FromIntegral Int i)
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
postscanlSeg :: (Elt a, Integral i, Bits i, FromIntegral i Int)
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
scanrSeg :: (Elt a, Integral i, Bits i, FromIntegral i Int, FromIntegral Int i)
         => (Exp a -> Exp a -> Exp a)
         -> Exp a
         -> Acc (Vector a)
         -> Acc (Segments i)
         -> Acc (Vector a)
scanrSeg f z vec seg = null flags ?| ( fill (shape seg) z, scanr1Seg f vec' seg' )
  where
    -- Using technique described for 'scanlSeg', where we intersperse the array
    -- with the seed element at the start of each segment, and then perform an
    -- inclusive segmented scan.
    --
    flags       = mkHeadFlags seg
    inc         = scanl1 (+) flags

    seg'        = map (+1) seg
    vec'        = permute const
                          (fill (index1 $ size vec + size seg) z)
                          (\ix -> index1' $ unindex1' ix + inc ! ix - 1)
                          vec


-- | Segmented version of 'scanr''.
--
scanr'Seg :: forall a i. (Elt a, Integral i, Bits i, FromIntegral i Int, FromIntegral Int i)
          => (Exp a -> Exp a -> Exp a)
          -> Exp a
          -> Acc (Vector a)
          -> Acc (Segments i)
          -> Acc (Vector a, Vector a)
scanr'Seg f z vec seg = result
  where
    -- Using technique described for scanl'Seg
    --
    result      = lift ( null vec  ?| (emptyArray, body)
                       , null vec' ?| (emptyArray, sums)
                       )

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
scanr1Seg :: (Elt a, Integral i, Bits i, FromIntegral i Int)
          => (Exp a -> Exp a -> Exp a)
          -> Acc (Vector a)
          -> Acc (Segments i)
          -> Acc (Vector a)
scanr1Seg f vec seg
  = P.snd
  . unzip
  . scanr1 (flip (segmented f))
  $ zip (mkTailFlags seg) vec


-- |Segmented version of 'prescanr'.
--
prescanrSeg :: (Elt a, Integral i, Bits i, FromIntegral i Int, FromIntegral Int i)
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
postscanrSeg :: (Elt a, Integral i, Bits i, FromIntegral i Int)
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
index1' ::  (Integral i, FromIntegral i Int) => Exp i -> Exp DIM1
index1' i = lift (Z :. fromIntegral i)

unindex1' :: FromIntegral Int i => Exp DIM1 -> Exp i
unindex1' ix = let Z :. i = unlift ix in fromIntegral i


-- Reshaping of arrays
-- -------------------

-- | Flattens a given array of arbitrary dimension.
--
flatten :: forall sh e. (Shape sh, Elt e) => Acc (Array sh e) -> Acc (Vector e)
flatten a
  | Just REFL <- matchTupleType (eltType (undefined::sh)) (eltType (undefined::DIM1))
  , Just a'   <- gcast a
  = a'
flatten a
  = reshape (index1 $ size a) a


-- Enumeration and filling
-- -----------------------

-- | Create an array where all elements are the same value.
--
fill :: (Shape sh, Elt e) => Exp sh -> Exp e -> Acc (Array sh e)
fill sh c = generate sh (const c)

-- | Create an array of the given shape containing the values x, x+1, etc (in
--   row-major order).
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

-- TLM: If we have something like (concat . split) then the source array will
--      have two use sites, but is actually safe (and better) to inline.


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

{-# NOINLINE filter #-}
{-# RULES
  "ACC filter/filter" forall f g arr.
    filter f (filter g arr) = filter (\x -> g x &&* f x) arr
 #-}


-- Gather operations
-- -----------------

-- | Gather elements from a source array at the given indices. For example:
--
--  > input  = [1, 9, 6, 4, 4, 2, 0, 1, 2]
--  > from   = [1, 3, 7, 2, 5, 3]
--  >
--  > output = [9, 4, 1, 6, 2, 4]
--
gather :: (Shape sh, Elt e)
       => Acc (Array sh Int)      -- ^ index of source at each index to gather
       -> Acc (Vector e)          -- ^ source vector
       -> Acc (Array sh e)        -- ^ output
gather indices input = map (input !!) indices
  -- TLM NOTES:
  --  * (!!) has potential for later optimisation
  --  * We needn't fix the source array to Vector, but this matches the
  --    intuition that 'Int' ~ 'DIM1'.


-- | Conditionally copy elements from source array to destination array according
--   to an index mapping. This is a backpermute operation where a 'from' vector
--   encodes the output to input index mapping. In addition, there is a 'mask'
--   vector, and an associated predication function, that specifies whether an
--   element will be copied. If not copied, the output array assumes the default
--   vector's value.
--
--   For example:
--
--  > default = [6, 6, 6, 6, 6, 6]
--  > from    = [1, 3, 7, 2, 5, 3]
--  > mask    = [3, 4, 9, 2, 7, 5]
--  > pred    = (>* 4)
--  > input   = [1, 9, 6, 4, 4, 2, 0, 1, 2]
--  >
--  > output  = [6, 6, 1, 6, 2, 4]
--
gatherIf :: (Elt a, Elt b)
         => Acc (Vector Int)      -- ^index mapping
         -> Acc (Vector a)        -- ^mask
         -> (Exp a -> Exp Bool)   -- ^predicate
         -> Acc (Vector b)        -- ^default
         -> Acc (Vector b)        -- ^input
         -> Acc (Vector b)        -- ^output
gatherIf from maskV pred defaults input = zipWith zf pf gatheredV
  where
    zf p g      = p ? (unlift g)
    gatheredV   = zip (gather from input) defaults
    pf          = map pred maskV


-- Scatter operations
-- ------------------

-- | Copy elements from source array to destination array according to an index
--   mapping. This is a forward-permute operation where a 'to' vector encodes an
--   input to output index mapping. Output elements for indices that are not
--   mapped assume the default vector's value.
--
--   For example:
--
--  > default = [0, 0, 0, 0, 0, 0, 0, 0, 0]
--  > to      = [1, 3, 7, 2, 5, 8]
--  > input   = [1, 9, 6, 4, 4, 2, 5]
--  >
--  > output  = [0, 1, 4, 9, 0, 4, 0, 6, 2]
--
--   Note if the same index appears in the index mapping more than once, the
--   result is undefined. It does not makes sense for the 'to' vector to be
--   larger than the 'input' vector.
--
scatter :: Elt e
        => Acc (Vector Int)       -- ^index mapping
        -> Acc (Vector e)         -- ^default
        -> Acc (Vector e)         -- ^input
        -> Acc (Vector e)         -- ^output
scatter to defaults input = permute const defaults pf input'
  where
    pf ix       = index1 (to ! ix)
    input'      = backpermute (shape to `intersect` shape input) id input


-- | Conditionally copy elements from source array to destination array according
--   to an index mapping. This is a forward-permute operation where a 'to'
--   vector encodes an input to output index mapping. In addition, there is a
--   'mask' vector, and an associated predicate function. The mapping will only
--   occur if the predicate function applied to the mask at that position
--   resolves to 'True'. If not copied, the output array assumes the default
--   vector's value.
--
--   For example:
--
--  > default = [0, 0, 0, 0, 0, 0, 0, 0, 0]
--  > to      = [1, 3, 7, 2, 5, 8]
--  > mask    = [3, 4, 9, 2, 7, 5]
--  > pred    = (>* 4)
--  > input   = [1, 9, 6, 4, 4, 2, 5]
--  >
--  > output  = [0, 0, 0, 0, 0, 4, 0, 6, 2]
--
--   Note if the same index appears in the mapping more than once, the result is
--   undefined. The 'to' and 'mask' vectors must be the same length. It does not
--   make sense for these to be larger than the 'input' vector.
--
scatterIf :: (Elt e, Elt e')
          => Acc (Vector Int)      -- ^index mapping
          -> Acc (Vector e)        -- ^mask
          -> (Exp e -> Exp Bool)   -- ^predicate
          -> Acc (Vector e')       -- ^default
          -> Acc (Vector e')       -- ^input
          -> Acc (Vector e')       -- ^output
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
transpose :: Elt e => Acc (Array DIM2 e) -> Acc (Array DIM2 e)
transpose mat =
  let swap = lift1 $ \(Z:.x:.y) -> Z:.y:.x :: Z:.Exp Int:.Exp Int
  in  backpermute (swap $ shape mat) swap mat


-- Extracting sub-vectors
-- ----------------------

-- | Yield the first @n@ elements in the outermost dimension of the array (plus
-- all lower dimensional elements).
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


-- | Yield all but the first @n@ elements along the outermost dimension of the
-- array (plus all lower dimensional elements).
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


-- | Yield all but the elements in the last index of the outermost dimension.
--
init :: forall sh e. (Slice sh, Shape sh, Elt e)
     => Acc (Array (sh :. Int) e)
     -> Acc (Array (sh :. Int) e)
init acc =
  let sh :. sz  = unlift (shape acc)            :: Exp sh :. Exp Int
  in  backpermute (lift (sh :. sz `min` (sz - 1))) id acc


-- | Yield all but the first element of the input vector. The vector must not be
--   empty.
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


-- | Yield a slit (slice) of the outermost indices of an array. Denotationally,
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
-- other operations.
--
-- In the case of GPU execution, this also means that the operation is available
-- to be executed concurrently with other kernels. In particular, consider using
-- this if you have a series of operations that are compute bound rather than
-- memory bound.
--
-- Here is the synthetic example:
--
-- > loop :: Exp Int -> Exp Int
-- > loop ticks =
-- >   let clockRate = 900000   -- kHz
-- >   in  A.while (\i -> i <* clockRate * ticks) (+1) 0
-- >
-- > test :: Acc (Vector Int)
-- > test =
-- >   A.zip3
-- >     (compute $ A.map loop (use $ A.fromList (Z:.1) [10]))
-- >     (compute $ A.map loop (use $ A.fromList (Z:.1) [10]))
-- >     (compute $ A.map loop (use $ A.fromList (Z:.1) [10]))
-- >
--
-- Without the use of 'compute', the operations are fused together and the three
-- long-running loops are executed sequentially in a single kernel. Instead, the
-- individual kernels can now be executed concurrently, reducing overall runtime
-- (on hardware that supports concurrent kernel execution).
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

-- |Get the length of a vector
--
length :: Elt e => Acc (Vector e) -> Exp Int
length = unindex1 . shape

-- Sequence operations
-- --------------------------------------

emptyArray :: (Shape sh, Elt e) => Acc (Array sh e)
emptyArray = use (fromList empty [])

-- | Reduce a sequence by appending all the shapes and all the elements in two
-- separate vectors.
--
fromSeq :: (Shape ix, Elt a) => Seq [Array ix a] -> Seq (Vector ix, Vector a)
fromSeq = foldSeqFlatten f (lift (emptyArray, emptyArray))
  where
    f x sh1 a1 =
      let (sh0, a0) = unlift x
      in lift (sh0 ++ sh1, a0 ++ a1)


fromSeqElems :: (Shape ix, Elt a) => Seq [Array ix a] -> Seq (Vector a)
fromSeqElems = foldSeqFlatten f emptyArray
  where
    f a0 _ a1 = a0 ++ a1

fromSeqShapes :: (Shape ix, Elt a) => Seq [Array ix a] -> Seq (Vector ix)
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

