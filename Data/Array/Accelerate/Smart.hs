{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Smart
-- Copyright   : [2008..2014] Manuel M T Chakravarty, Gabriele Keller
--               [2008..2009] Sean Lee
--               [2009..2014] Trevor L. McDonell
--               [2013..2014] Robert Clifton-Everest
--               [2014..2014] Frederik M. Madsen
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This modules defines the AST of the user-visible embedded language using more
-- convenient higher-order abstract syntax (instead of de Bruijn indices).
-- Moreover, it defines smart constructors to construct programs.
--

module Data.Array.Accelerate.Smart (

  -- * HOAS AST
  Acc(..), PreAcc(..), Exp(..), PreExp(..), Boundary(..), Stencil(..), Level,
  PreSeq(..), Seq(..),

  -- * Smart constructors for literals
  constant,

  -- * Smart constructors and destructors for tuples
  tup2, tup3, tup4, tup5, tup6, tup7, tup8, tup9,
  untup2, untup3, untup4, untup5, untup6, untup7, untup8, untup9,

  atup2, atup3, atup4, atup5, atup6, atup7, atup8, atup9,
  unatup2, unatup3, unatup4, unatup5, unatup6, unatup7, unatup8, unatup9,

  stup2, stup3, stup4, stup5, stup6, stup7, stup8, stup9,

  -- * Smart constructors for constants
  mkMinBound, mkMaxBound, mkPi,
  mkSin, mkCos, mkTan,
  mkAsin, mkAcos, mkAtan,
  mkAsinh, mkAcosh, mkAtanh,
  mkExpFloating, mkSqrt, mkLog,
  mkFPow, mkLogBase,
  mkTruncate, mkRound, mkFloor, mkCeiling,
  mkAtan2,

  -- * Smart constructors for primitive functions
  mkAdd, mkSub, mkMul, mkNeg, mkAbs, mkSig, mkQuot, mkRem, mkIDiv, mkMod,
  mkBAnd, mkBOr, mkBXor, mkBNot, mkBShiftL, mkBShiftR, mkBRotateL, mkBRotateR,
  mkFDiv, mkRecip, mkLt, mkGt, mkLtEq, mkGtEq, mkEq, mkNEq, mkMax, mkMin,
  mkLAnd, mkLOr, mkLNot,

  -- * Smart constructors for type coercion functions
  mkOrd, mkChr, mkBoolToInt, mkFromIntegral,

  -- * Auxiliary functions
  ($$), ($$$), ($$$$), ($$$$$),

  -- Debugging
  showPreAccOp, showPreExpOp, showPreSeqOp

) where

-- standard library
import Prelude                                  hiding ( exp )
import Data.List
import Data.Typeable

-- friends
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.AST                hiding (
  PreOpenAcc(..), OpenAcc(..), Acc, Stencil(..), PreOpenExp(..), OpenExp, PreExp, Exp, Seq, PreOpenSeq(..), Producer(..), Consumer(..),
  showPreAccOp, showPreExpOp )
import qualified Data.Array.Accelerate.AST      as AST

-- Array computations
-- ------------------

-- The level of lambda-bound variables. The root has level 0; then it increases with each bound
-- variable — i.e., it is the same as the size of the environment at the defining occurrence.
--
type Level = Int

-- | Array-valued collective computations without a recursive knot
--
data PreAcc acc seq exp as where
    -- Needed for conversion to de Bruijn form
  Atag          :: Arrays as
                => Level                        -- environment size at defining occurrence
                -> PreAcc acc seq exp as

  Pipe          :: (Arrays as, Arrays bs, Arrays cs)
                => (Acc as -> acc bs)
                -> (Acc bs -> acc cs)
                -> acc as
                -> PreAcc acc seq exp cs

  Aforeign      :: (Arrays arrs, Arrays a, Foreign f)
                => f arrs a
                -> (Acc arrs -> Acc a)
                -> acc arrs
                -> PreAcc acc seq exp a

  Acond         :: Arrays as
                => exp Bool
                -> acc as
                -> acc as
                -> PreAcc acc seq exp as

  Awhile        :: Arrays arrs
                => (Acc arrs -> acc (Scalar Bool))
                -> (Acc arrs -> acc arrs)
                -> acc arrs
                -> PreAcc acc seq exp arrs

  Atuple        :: (Arrays arrs, IsAtuple arrs)
                => Atuple acc (TupleRepr arrs)
                -> PreAcc acc seq exp arrs

  Aprj          :: (Arrays arrs, IsAtuple arrs, Arrays a)
                => TupleIdx (TupleRepr arrs) a
                ->        acc     arrs
                -> PreAcc acc seq exp a

  Use           :: Arrays arrs
                => arrs
                -> PreAcc acc seq exp arrs

  Unit          :: Elt e
                => exp e
                -> PreAcc acc seq exp (Scalar e)

  Generate      :: (Shape sh, Elt e)
                => exp sh
                -> (Exp sh -> exp e)
                -> PreAcc acc seq exp (Array sh e)

  Reshape       :: (Shape sh, Shape sh', Elt e)
                => exp sh
                -> acc (Array sh' e)
                -> PreAcc acc seq exp (Array sh e)

  Replicate     :: (Slice slix, Elt e,
                    Typeable (SliceShape slix), Typeable (FullShape slix))
                    -- the Typeable constraints shouldn't be necessary as they are implied by
                    -- 'SliceIx slix' — unfortunately, the (old) type checker doesn't grok that
                => exp slix
                -> acc            (Array (SliceShape slix) e)
                -> PreAcc acc seq exp (Array (FullShape  slix) e)

  Slice         :: (Slice slix, Elt e,
                    Typeable (SliceShape slix), Typeable (FullShape slix))
                    -- the Typeable constraints shouldn't be necessary as they are implied by
                    -- 'SliceIx slix' — unfortunately, the (old) type checker doesn't grok that
                => acc            (Array (FullShape  slix) e)
                -> exp slix
                -> PreAcc acc seq exp (Array (SliceShape slix) e)

  Map           :: (Shape sh, Elt e, Elt e')
                => (Exp e -> exp e')
                -> acc (Array sh e)
                -> PreAcc acc seq exp (Array sh e')

  ZipWith       :: (Shape sh, Elt e1, Elt e2, Elt e3)
                => (Exp e1 -> Exp e2 -> exp e3)
                -> acc (Array sh e1)
                -> acc (Array sh e2)
                -> PreAcc acc seq exp (Array sh e3)

  Fold          :: (Shape sh, Elt e)
                => (Exp e -> Exp e -> exp e)
                -> exp e
                -> acc (Array (sh:.Int) e)
                -> PreAcc acc seq exp (Array sh e)

  Fold1         :: (Shape sh, Elt e)
                => (Exp e -> Exp e -> exp e)
                -> acc (Array (sh:.Int) e)
                -> PreAcc acc seq exp (Array sh e)

  FoldSeg       :: (Shape sh, Elt e, Elt i, IsIntegral i)
                => (Exp e -> Exp e -> exp e)
                -> exp e
                -> acc (Array (sh:.Int) e)
                -> acc (Segments i)
                -> PreAcc acc seq exp (Array (sh:.Int) e)

  Fold1Seg      :: (Shape sh, Elt e, Elt i, IsIntegral i)
                => (Exp e -> Exp e -> exp e)
                -> acc (Array (sh:.Int) e)
                -> acc (Segments i)
                -> PreAcc acc seq exp (Array (sh:.Int) e)

  Scanl         :: Elt e
                => (Exp e -> Exp e -> exp e)
                -> exp e
                -> acc (Vector e)
                -> PreAcc acc seq exp (Vector e)

  Scanl'        :: Elt e
                => (Exp e -> Exp e -> exp e)
                -> exp e
                -> acc (Vector e)
                -> PreAcc acc seq exp (Vector e, Scalar e)

  Scanl1        :: Elt e
                => (Exp e -> Exp e -> exp e)
                -> acc (Vector e)
                -> PreAcc acc seq exp (Vector e)

  Scanr         :: Elt e
                => (Exp e -> Exp e -> exp e)
                -> exp e
                -> acc (Vector e)
                -> PreAcc acc seq exp (Vector e)

  Scanr'        :: Elt e
                => (Exp e -> Exp e -> exp e)
                -> exp e
                -> acc (Vector e)
                -> PreAcc acc seq exp (Vector e, Scalar e)

  Scanr1        :: Elt e
                => (Exp e -> Exp e -> exp e)
                -> acc (Vector e)
                -> PreAcc acc seq exp (Vector e)

  Permute       :: (Shape sh, Shape sh', Elt e)
                => (Exp e -> Exp e -> exp e)
                -> acc (Array sh' e)
                -> (Exp sh -> exp sh')
                -> acc (Array sh e)
                -> PreAcc acc seq exp (Array sh' e)

  Backpermute   :: (Shape sh, Shape sh', Elt e)
                => exp sh'
                -> (Exp sh' -> exp sh)
                -> acc (Array sh e)
                -> PreAcc acc seq exp (Array sh' e)

  Stencil       :: (Shape sh, Elt a, Elt b, Stencil sh a stencil)
                => (stencil -> exp b)
                -> Boundary a
                -> acc (Array sh a)
                -> PreAcc acc seq exp (Array sh b)

  Stencil2      :: (Shape sh, Elt a, Elt b, Elt c,
                   Stencil sh a stencil1, Stencil sh b stencil2)
                => (stencil1 -> stencil2 -> exp c)
                -> Boundary a
                -> acc (Array sh a)
                -> Boundary b
                -> acc (Array sh b)
                -> PreAcc acc seq exp (Array sh c)

  Collect       :: Arrays arrs
                => seq arrs
                -> PreAcc acc seq exp arrs

data PreSeq acc seq exp arrs where
  -- Convert the given array to a sequence.
  -- Example:
  -- slix = Z :. All :. Int :. All :. All :. Int
  --              ^      ^      ^      ^      ^
  --              |       \    /      /       |
  --              |        \__/______/_______ Iteration space.
  --              |          /      /
  --           Element______/______/
  --            shape.
  --
  ToSeq :: (Slice slix, Elt e,
               Typeable (FullShape slix), Typeable (SliceShape slix))
        => exp slix
        -> acc (Array (FullShape slix) e)
        -> PreSeq acc seq exp [Array (SliceShape slix) e]

  -- Use main memory vector as a sequence.
  UseLazy :: (Slice slix, Elt e,
               Typeable (FullShape slix), Typeable (SliceShape slix))
          => exp slix
          -> Array (FullShape slix) e
          -> PreSeq acc seq exp [Array (SliceShape slix) e]

  -- TODO: Add a function for using a list directly.

  -- Apply the given the given function to all elements of the given sequence.
  MapSeq :: (Arrays a, Arrays b)
         => (Acc a -> acc b)
         -> seq [a]
         -> PreSeq acc seq exp [b]

  -- Apply a given binary function pairwise to all elements of the given sequences.
  -- The length of the result is the length of the shorter of the two argument
  -- arrays.
  ZipWithSeq :: (Arrays a, Arrays b, Arrays c)
             => (Acc a -> Acc b -> acc c)
             -> seq [a]
             -> seq [b]
             -> PreSeq acc seq exp [c]

  -- ScanSeq (+) a0 x. Scan a sequence x by combining each element
  -- using the given binary operation (+). (+) must be associative:
  --
  --   Forall a b c. (a + b) + c = a + (b + c),
  --
  -- and a0 must be the identity element for (+):
  --
  --   Forall a. a0 + a = a = a + a0.
  --
  ScanSeq :: Arrays a
          => (Acc a -> Acc a -> acc a)
          -> acc a
          -> seq [a]
          -> PreSeq acc seq exp [a]

  -- ScanSeqAct (+) (*) a0 b0 x. Scan a sequence x by the given
  -- binary operation (+). (+) must be semi-associative, where (*) is
  -- the companion operator:
  --
  --   Forall a b1 b2. (a + b1) + b2 = a + (b1 * b2).
  --
  -- and b0 must be the identity element for (*).
  --
  --   Forall b. b0 * b = b = b * b0.
  --
  -- Note on the name: Act is short for "semigroup action".
  --
  ScanSeqAct :: (Arrays a, Arrays b)
             => (Acc a -> Acc b -> acc a)
             -> (Acc b -> Acc b -> acc b)
             -> acc a
             -> acc b
             -> seq [b]
             -> PreSeq acc seq exp [a]

  -- Convert the given sequence to an array.
  FromSeq :: (Shape sh, Elt e)
          => seq [Array sh e]
          -> PreSeq acc seq exp (Array (Z:.Int) sh, Array (Z:.Int) e)

  -- FoldSeq (+) a0 x. Fold a sequence x by combining each element
  -- using the given binary operation (+). (+) must be associative:
  --
  --   Forall a b c. (a + b) + c = a + (b + c),
  --
  -- and a0 must be the identity element for (+):
  --
  --   Forall a. a0 + a = a = a + a0.
  --
  FoldSeq :: Arrays a
          => (Acc a -> Acc a -> acc a)
          -> acc a
          -> seq [a]
          -> PreSeq acc seq exp a

  -- FoldSeqAct (+) (*) a0 b0 x. Fold a sequence x by the given
  -- binary operation (+). (+) must be semi-associative, where (*) is
  -- the companion operator:
  --
  --   Forall a b1 b2. (a + b1) + b2 = a + (b1 * b2).
  --
  -- and b0 must be the identity element for (*).
  --
  --   Forall b. b0 * b = b = b * b0.
  --
  -- Note on the name: Act is short for "semigroup action".
  --
  FoldSeqAct :: (Arrays a, Arrays b)
             => (Acc a -> Acc b -> acc a)
             -> (Acc b -> Acc b -> acc b)
             -> acc a
             -> acc b
             -> seq [b]
             -> PreSeq acc seq exp a

  -- FoldSeqFlatten f a0 x. A specialized version of FoldSeqAct
  -- where reduction with the companion operator corresponds to
  -- flattening. f must be semi-associative, with vecotor append (++)
  -- as the companion operator:
  --
  --   Forall b s1 a2 sh2 a2.
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
  FoldSeqFlatten :: (Arrays a, Shape sh, Elt e)
                 => (Acc a -> Acc (Vector sh) -> Acc (Vector e) -> acc a)
                 -> acc a
                 -> seq [Array sh e]
                 -> PreSeq acc seq exp a

  -- Tuple up the results of a sequence computation. Note that the Arrays
  -- constraint requires that the elements of the tuple are Arrays, not
  -- streams ([]).
  Stuple :: (Arrays arrs, IsAtuple arrs)
         => Atuple (seq) (TupleRepr arrs)
         -> PreSeq acc seq exp arrs

-- |Array-valued collective computations
--
newtype Acc a = Acc (PreAcc Acc Seq Exp a)

-- |Array-valued sequence computations
--
newtype Seq a = Seq (PreSeq Acc Seq Exp a)

deriving instance Typeable Acc
deriving instance Typeable Seq


-- Embedded expressions of the surface language
-- --------------------------------------------

-- HOAS expressions mirror the constructors of `AST.OpenExp', but with the `Tag' constructor instead
-- of variables in the form of de Bruijn indices. Moreover, HOAS expression use n-tuples and the
-- type class 'Elt' to constrain element types, whereas `AST.OpenExp' uses nested pairs and the GADT
-- 'TupleType'.
--

-- | Scalar expressions to parametrise collective array operations, themselves parameterised over
-- the type of collective array operations.
--
data PreExp acc seq exp t where
    -- Needed for conversion to de Bruijn form
  Tag           :: Elt t
                => Level                        -- environment size at defining occurrence
                -> PreExp acc seq exp t

  -- All the same constructors as 'AST.Exp'
  Const         :: Elt t
                => t
                -> PreExp acc seq exp t

  Tuple         :: (Elt t, IsTuple t)
                => Tuple exp (TupleRepr t)
                -> PreExp acc seq exp t

  Prj           :: (Elt t, IsTuple t, Elt e)
                => TupleIdx (TupleRepr t) e
                -> exp t
                -> PreExp acc seq exp e

  IndexNil      :: PreExp acc seq exp Z

  IndexCons     :: (Slice sl, Elt a)
                => exp sl
                -> exp a
                -> PreExp acc seq exp (sl:.a)

  IndexHead     :: (Slice sl, Elt a)
                => exp (sl:.a)
                -> PreExp acc seq exp a

  IndexTail     :: (Slice sl, Elt a)
                => exp (sl:.a)
                -> PreExp acc seq exp sl

  IndexAny      :: Shape sh
                => PreExp acc seq exp (Any sh)

  ToIndex       :: Shape sh
                => exp sh
                -> exp sh
                -> PreExp acc seq exp Int

  FromIndex     :: Shape sh
                => exp sh
                -> exp Int
                -> PreExp acc seq exp sh

  Cond          :: Elt t
                => exp Bool
                -> exp t
                -> exp t
                -> PreExp acc seq exp t

  While         :: Elt t
                => (Exp t -> exp Bool)
                -> (Exp t -> exp t)
                -> exp t
                -> PreExp acc seq exp t

  PrimConst     :: Elt t
                => PrimConst t
                -> PreExp acc seq exp t

  PrimApp       :: (Elt a, Elt r)
                => PrimFun (a -> r)
                -> exp a
                -> PreExp acc seq exp r

  Index         :: (Shape sh, Elt t)
                => acc (Array sh t)
                -> exp sh
                -> PreExp acc seq exp t

  LinearIndex   :: (Shape sh, Elt t)
                => acc (Array sh t)
                -> exp Int
                -> PreExp acc seq exp t

  Shape         :: (Shape sh, Elt e)
                => acc (Array sh e)
                -> PreExp acc seq exp sh

  ShapeSize     :: Shape sh
                => exp sh
                -> PreExp acc seq exp Int

  Intersect     :: Shape sh
                => exp sh
                -> exp sh
                -> PreExp acc seq exp sh

  Union         :: Shape sh
                => exp sh
                -> exp sh
                -> PreExp acc seq exp sh

  Foreign       :: (Elt x, Elt y, Foreign f)
                => f x y
                -> (Exp x -> Exp y) -- RCE: Using Exp instead of exp to aid in sharing recovery.
                -> exp x
                -> PreExp acc seq exp y

-- | Scalar expressions for plain array computations.
--
newtype Exp t = Exp (PreExp Acc Seq Exp t)

deriving instance Typeable Exp


-- Smart constructors and destructors for array tuples
-- ---------------------------------------------------

atup2 :: (Arrays a, Arrays b) => (Acc a, Acc b) -> Acc (a, b)
atup2 (x1, x2) = Acc $ Atuple (NilAtup `SnocAtup` x1 `SnocAtup` x2)

atup3 :: (Arrays a, Arrays b, Arrays c) => (Acc a, Acc b, Acc c) -> Acc (a, b, c)
atup3 (x1, x2, x3) = Acc $ Atuple (NilAtup `SnocAtup` x1 `SnocAtup` x2 `SnocAtup` x3)

atup4 :: (Arrays a, Arrays b, Arrays c, Arrays d)
     => (Acc a, Acc b, Acc c, Acc d) -> Acc (a, b, c, d)
atup4 (x1, x2, x3, x4)
  = Acc $ Atuple (NilAtup `SnocAtup` x1 `SnocAtup` x2 `SnocAtup` x3 `SnocAtup` x4)

atup5 :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e)
     => (Acc a, Acc b, Acc c, Acc d, Acc e) -> Acc (a, b, c, d, e)
atup5 (x1, x2, x3, x4, x5)
  = Acc $ Atuple $
      NilAtup `SnocAtup` x1 `SnocAtup` x2 `SnocAtup` x3 `SnocAtup` x4 `SnocAtup` x5

atup6 :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f)
     => (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f) -> Acc (a, b, c, d, e, f)
atup6 (x1, x2, x3, x4, x5, x6)
  = Acc $ Atuple $
      NilAtup `SnocAtup` x1 `SnocAtup` x2 `SnocAtup` x3
              `SnocAtup` x4 `SnocAtup` x5 `SnocAtup` x6

atup7 :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g)
     => (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g)
     -> Acc (a, b, c, d, e, f, g)
atup7 (x1, x2, x3, x4, x5, x6, x7)
  = Acc $ Atuple $
      NilAtup `SnocAtup` x1 `SnocAtup` x2 `SnocAtup` x3
              `SnocAtup` x4 `SnocAtup` x5 `SnocAtup` x6 `SnocAtup` x7

atup8 :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h)
     => (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h)
     -> Acc (a, b, c, d, e, f, g, h)
atup8 (x1, x2, x3, x4, x5, x6, x7, x8)
  = Acc $ Atuple $
      NilAtup `SnocAtup` x1 `SnocAtup` x2 `SnocAtup` x3 `SnocAtup` x4
              `SnocAtup` x5 `SnocAtup` x6 `SnocAtup` x7 `SnocAtup` x8

atup9 :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i)
     => (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h, Acc i)
     -> Acc (a, b, c, d, e, f, g, h, i)
atup9 (x1, x2, x3, x4, x5, x6, x7, x8, x9)
  = Acc $ Atuple $
      NilAtup `SnocAtup` x1 `SnocAtup` x2 `SnocAtup` x3 `SnocAtup` x4
              `SnocAtup` x5 `SnocAtup` x6 `SnocAtup` x7 `SnocAtup` x8 `SnocAtup` x9

unatup2 :: (Arrays a, Arrays b) => Acc (a, b) -> (Acc a, Acc b)
unatup2 e = (Acc $ SuccTupIdx ZeroTupIdx `Aprj` e, Acc $ ZeroTupIdx `Aprj` e)

unatup3 :: (Arrays a, Arrays b, Arrays c) => Acc (a, b, c) -> (Acc a, Acc b, Acc c)
unatup3 e =
  ( Acc $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Aprj` e
  , Acc $ SuccTupIdx ZeroTupIdx `Aprj` e
  , Acc $ ZeroTupIdx `Aprj` e )

unatup4
    :: (Arrays a, Arrays b, Arrays c, Arrays d)
    => Acc (a, b, c, d) -> (Acc a, Acc b, Acc c, Acc d)
unatup4 e =
  ( Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Aprj` e
  , Acc $ SuccTupIdx ZeroTupIdx `Aprj` e
  , Acc $ ZeroTupIdx `Aprj` e )

unatup5
    :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e)
    => Acc (a, b, c, d, e) -> (Acc a, Acc b, Acc c, Acc d, Acc e)
unatup5 e =
  ( Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Aprj` e
  , Acc $ SuccTupIdx ZeroTupIdx `Aprj` e
  , Acc $ ZeroTupIdx `Aprj` e )

unatup6
    :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f)
    => Acc (a, b, c, d, e, f) -> (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f)
unatup6 e =
  ( Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Aprj` e
  , Acc $ SuccTupIdx ZeroTupIdx `Aprj` e
  , Acc $ ZeroTupIdx `Aprj` e )

unatup7
    :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g)
    => Acc (a, b, c, d, e, f, g) -> (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g)
unatup7 e =
  ( Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Aprj` e
  , Acc $ SuccTupIdx ZeroTupIdx `Aprj` e
  , Acc $ ZeroTupIdx `Aprj` e )

unatup8
    :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h)
    => Acc (a, b, c, d, e, f, g, h) -> (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h)
unatup8 e =
  ( Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Aprj` e
  , Acc $ SuccTupIdx ZeroTupIdx `Aprj` e
  , Acc $ ZeroTupIdx `Aprj` e )

unatup9
    :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i)
    => Acc (a, b, c, d, e, f, g, h, i) -> (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h, Acc i)
unatup9 e =
  ( Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Aprj` e
  , Acc $ SuccTupIdx ZeroTupIdx `Aprj` e
  , Acc $ ZeroTupIdx `Aprj` e )


-- Smart constructors for stencil reification
-- ------------------------------------------

-- Stencil reification
--
-- In the AST representation, we turn the stencil type from nested tuples of Accelerate expressions
-- into an Accelerate expression whose type is a tuple nested in the same manner.  This enables us
-- to represent the stencil function as a unary function (which also only needs one de Bruijn
-- index). The various positions in the stencil are accessed via tuple indices (i.e., projections).

class (Elt (StencilRepr sh stencil), AST.Stencil sh a (StencilRepr sh stencil))
  => Stencil sh a stencil where
  type StencilRepr sh stencil :: *
  stencilPrj :: sh{-dummy-} -> a{-dummy-} -> Exp (StencilRepr sh stencil) -> stencil

-- DIM1
instance Elt e => Stencil DIM1 e (Exp e, Exp e, Exp e) where
  type StencilRepr DIM1 (Exp e, Exp e, Exp e)
    = (e, e, e)
  stencilPrj _ _ s = (Exp $ Prj tix2 s,
                      Exp $ Prj tix1 s,
                      Exp $ Prj tix0 s)
instance Elt e => Stencil DIM1 e (Exp e, Exp e, Exp e, Exp e, Exp e) where
  type StencilRepr DIM1 (Exp e, Exp e, Exp e, Exp e, Exp e)
    = (e, e, e, e, e)
  stencilPrj _ _ s = (Exp $ Prj tix4 s,
                      Exp $ Prj tix3 s,
                      Exp $ Prj tix2 s,
                      Exp $ Prj tix1 s,
                      Exp $ Prj tix0 s)
instance Elt e => Stencil DIM1 e (Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e) where
  type StencilRepr DIM1 (Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e)
    = (e, e, e, e, e, e, e)
  stencilPrj _ _ s = (Exp $ Prj tix6 s,
                      Exp $ Prj tix5 s,
                      Exp $ Prj tix4 s,
                      Exp $ Prj tix3 s,
                      Exp $ Prj tix2 s,
                      Exp $ Prj tix1 s,
                      Exp $ Prj tix0 s)
instance Elt e => Stencil DIM1 e (Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e)
  where
  type StencilRepr DIM1 (Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e)
    = (e, e, e, e, e, e, e, e, e)
  stencilPrj _ _ s = (Exp $ Prj tix8 s,
                      Exp $ Prj tix7 s,
                      Exp $ Prj tix6 s,
                      Exp $ Prj tix5 s,
                      Exp $ Prj tix4 s,
                      Exp $ Prj tix3 s,
                      Exp $ Prj tix2 s,
                      Exp $ Prj tix1 s,
                      Exp $ Prj tix0 s)

-- DIM(n+1)
instance (Stencil (sh:.Int) a row2,
          Stencil (sh:.Int) a row1,
          Stencil (sh:.Int) a row0) => Stencil (sh:.Int:.Int) a (row2, row1, row0) where
  type StencilRepr (sh:.Int:.Int) (row2, row1, row0)
    = (StencilRepr (sh:.Int) row2, StencilRepr (sh:.Int) row1, StencilRepr (sh:.Int) row0)
  stencilPrj _ a s = (stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix2 s),
                      stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix1 s),
                      stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix0 s))
instance (Stencil (sh:.Int) a row1,
          Stencil (sh:.Int) a row2,
          Stencil (sh:.Int) a row3,
          Stencil (sh:.Int) a row4,
          Stencil (sh:.Int) a row5) => Stencil (sh:.Int:.Int) a (row1, row2, row3, row4, row5) where
  type StencilRepr (sh:.Int:.Int) (row1, row2, row3, row4, row5)
    = (StencilRepr (sh:.Int) row1, StencilRepr (sh:.Int) row2, StencilRepr (sh:.Int) row3,
       StencilRepr (sh:.Int) row4, StencilRepr (sh:.Int) row5)
  stencilPrj _ a s = (stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix4 s),
                      stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix3 s),
                      stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix2 s),
                      stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix1 s),
                      stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix0 s))
instance (Stencil (sh:.Int) a row1,
          Stencil (sh:.Int) a row2,
          Stencil (sh:.Int) a row3,
          Stencil (sh:.Int) a row4,
          Stencil (sh:.Int) a row5,
          Stencil (sh:.Int) a row6,
          Stencil (sh:.Int) a row7)
  => Stencil (sh:.Int:.Int) a (row1, row2, row3, row4, row5, row6, row7) where
  type StencilRepr (sh:.Int:.Int) (row1, row2, row3, row4, row5, row6, row7)
    = (StencilRepr (sh:.Int) row1, StencilRepr (sh:.Int) row2, StencilRepr (sh:.Int) row3,
       StencilRepr (sh:.Int) row4, StencilRepr (sh:.Int) row5, StencilRepr (sh:.Int) row6,
       StencilRepr (sh:.Int) row7)
  stencilPrj _ a s = (stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix6 s),
                      stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix5 s),
                      stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix4 s),
                      stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix3 s),
                      stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix2 s),
                      stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix1 s),
                      stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix0 s))
instance (Stencil (sh:.Int) a row1,
          Stencil (sh:.Int) a row2,
          Stencil (sh:.Int) a row3,
          Stencil (sh:.Int) a row4,
          Stencil (sh:.Int) a row5,
          Stencil (sh:.Int) a row6,
          Stencil (sh:.Int) a row7,
          Stencil (sh:.Int) a row8,
          Stencil (sh:.Int) a row9)
  => Stencil (sh:.Int:.Int) a (row1, row2, row3, row4, row5, row6, row7, row8, row9) where
  type StencilRepr (sh:.Int:.Int) (row1, row2, row3, row4, row5, row6, row7, row8, row9)
    = (StencilRepr (sh:.Int) row1, StencilRepr (sh:.Int) row2, StencilRepr (sh:.Int) row3,
       StencilRepr (sh:.Int) row4, StencilRepr (sh:.Int) row5, StencilRepr (sh:.Int) row6,
       StencilRepr (sh:.Int) row7, StencilRepr (sh:.Int) row8, StencilRepr (sh:.Int) row9)
  stencilPrj _ a s = (stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix8 s),
                      stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix7 s),
                      stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix6 s),
                      stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix5 s),
                      stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix4 s),
                      stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix3 s),
                      stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix2 s),
                      stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix1 s),
                      stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix0 s))

-- Auxiliary tuple index constants
--
tix0 :: Elt s => TupleIdx (t, s) s
tix0 = ZeroTupIdx
tix1 :: Elt s => TupleIdx ((t, s), s1) s
tix1 = SuccTupIdx tix0
tix2 :: Elt s => TupleIdx (((t, s), s1), s2) s
tix2 = SuccTupIdx tix1
tix3 :: Elt s => TupleIdx ((((t, s), s1), s2), s3) s
tix3 = SuccTupIdx tix2
tix4 :: Elt s => TupleIdx (((((t, s), s1), s2), s3), s4) s
tix4 = SuccTupIdx tix3
tix5 :: Elt s => TupleIdx ((((((t, s), s1), s2), s3), s4), s5) s
tix5 = SuccTupIdx tix4
tix6 :: Elt s => TupleIdx (((((((t, s), s1), s2), s3), s4), s5), s6) s
tix6 = SuccTupIdx tix5
tix7 :: Elt s => TupleIdx ((((((((t, s), s1), s2), s3), s4), s5), s6), s7) s
tix7 = SuccTupIdx tix6
tix8 :: Elt s => TupleIdx (((((((((t, s), s1), s2), s3), s4), s5), s6), s7), s8) s
tix8 = SuccTupIdx tix7

-- Smart constructors for array tuples in sequence computations
-- ---------------------------------------------------

stup2 :: (Arrays a, Arrays b) => (Seq a, Seq b) -> Seq (a, b)
stup2 (x1, x2) = Seq $ Stuple (NilAtup `SnocAtup` x1 `SnocAtup` x2)

stup3 :: (Arrays a, Arrays b, Arrays c) => (Seq a, Seq b, Seq c) -> Seq (a, b, c)
stup3 (x1, x2, x3) = Seq $ Stuple (NilAtup `SnocAtup` x1 `SnocAtup` x2 `SnocAtup` x3)

stup4 :: (Arrays a, Arrays b, Arrays c, Arrays d)
     => (Seq a, Seq b, Seq c, Seq d) -> Seq (a, b, c, d)
stup4 (x1, x2, x3, x4)
  = Seq $ Stuple (NilAtup `SnocAtup` x1 `SnocAtup` x2 `SnocAtup` x3 `SnocAtup` x4)

stup5 :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e)
     => (Seq a, Seq b, Seq c, Seq d, Seq e) -> Seq (a, b, c, d, e)
stup5 (x1, x2, x3, x4, x5)
  = Seq $ Stuple $
      NilAtup `SnocAtup` x1 `SnocAtup` x2 `SnocAtup` x3 `SnocAtup` x4 `SnocAtup` x5

stup6 :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f)
     => (Seq a, Seq b, Seq c, Seq d, Seq e, Seq f) -> Seq (a, b, c, d, e, f)
stup6 (x1, x2, x3, x4, x5, x6)
  = Seq $ Stuple $
      NilAtup `SnocAtup` x1 `SnocAtup` x2 `SnocAtup` x3
              `SnocAtup` x4 `SnocAtup` x5 `SnocAtup` x6

stup7 :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g)
     => (Seq a, Seq b, Seq c, Seq d, Seq e, Seq f, Seq g)
     -> Seq (a, b, c, d, e, f, g)
stup7 (x1, x2, x3, x4, x5, x6, x7)
  = Seq $ Stuple $
      NilAtup `SnocAtup` x1 `SnocAtup` x2 `SnocAtup` x3
              `SnocAtup` x4 `SnocAtup` x5 `SnocAtup` x6 `SnocAtup` x7

stup8 :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h)
     => (Seq a, Seq b, Seq c, Seq d, Seq e, Seq f, Seq g, Seq h)
     -> Seq (a, b, c, d, e, f, g, h)
stup8 (x1, x2, x3, x4, x5, x6, x7, x8)
  = Seq $ Stuple $
      NilAtup `SnocAtup` x1 `SnocAtup` x2 `SnocAtup` x3 `SnocAtup` x4
              `SnocAtup` x5 `SnocAtup` x6 `SnocAtup` x7 `SnocAtup` x8

stup9 :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i)
     => (Seq a, Seq b, Seq c, Seq d, Seq e, Seq f, Seq g, Seq h, Seq i)
     -> Seq (a, b, c, d, e, f, g, h, i)
stup9 (x1, x2, x3, x4, x5, x6, x7, x8, x9)
  = Seq $ Stuple $
      NilAtup `SnocAtup` x1 `SnocAtup` x2 `SnocAtup` x3 `SnocAtup` x4
              `SnocAtup` x5 `SnocAtup` x6 `SnocAtup` x7 `SnocAtup` x8 `SnocAtup` x9

-- Smart constructor for literals
--

-- | Scalar expression inlet: make a Haskell value available for processing in
-- an Accelerate scalar expression.
--
-- Note that this embeds the value directly into the expression. Depending on
-- the backend used to execute the computation, this might not always be
-- desirable. For example, a backend that does external code generation may
-- embed this constant directly into the generated code, which means new code
-- will need to be generated and compiled every time the value changes. In such
-- cases, consider instead lifting scalar values into (singleton) arrays so that
-- they can be passed as an input to the computation and thus the value can
-- change without the need to generate fresh code.
--
constant :: Elt t => t -> Exp t
constant = Exp . Const

-- Smart constructor and destructors for scalar tuples
--
tup2 :: (Elt a, Elt b) => (Exp a, Exp b) -> Exp (a, b)
tup2 (x1, x2) = Exp $ Tuple (NilTup `SnocTup` x1 `SnocTup` x2)

tup3 :: (Elt a, Elt b, Elt c) => (Exp a, Exp b, Exp c) -> Exp (a, b, c)
tup3 (x1, x2, x3) = Exp $ Tuple (NilTup `SnocTup` x1 `SnocTup` x2 `SnocTup` x3)

tup4 :: (Elt a, Elt b, Elt c, Elt d)
     => (Exp a, Exp b, Exp c, Exp d) -> Exp (a, b, c, d)
tup4 (x1, x2, x3, x4)
  = Exp $ Tuple (NilTup `SnocTup` x1 `SnocTup` x2 `SnocTup` x3 `SnocTup` x4)

tup5 :: (Elt a, Elt b, Elt c, Elt d, Elt e)
     => (Exp a, Exp b, Exp c, Exp d, Exp e) -> Exp (a, b, c, d, e)
tup5 (x1, x2, x3, x4, x5)
  = Exp $ Tuple $
      NilTup `SnocTup` x1 `SnocTup` x2 `SnocTup` x3 `SnocTup` x4 `SnocTup` x5

tup6 :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f)
     => (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f) -> Exp (a, b, c, d, e, f)
tup6 (x1, x2, x3, x4, x5, x6)
  = Exp $ Tuple $
      NilTup `SnocTup` x1 `SnocTup` x2 `SnocTup` x3 `SnocTup` x4 `SnocTup` x5 `SnocTup` x6

tup7 :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g)
     => (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g)
     -> Exp (a, b, c, d, e, f, g)
tup7 (x1, x2, x3, x4, x5, x6, x7)
  = Exp $ Tuple $
      NilTup `SnocTup` x1 `SnocTup` x2 `SnocTup` x3
             `SnocTup` x4 `SnocTup` x5 `SnocTup` x6 `SnocTup` x7

tup8 :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h)
     => (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h)
     -> Exp (a, b, c, d, e, f, g, h)
tup8 (x1, x2, x3, x4, x5, x6, x7, x8)
  = Exp $ Tuple $
      NilTup `SnocTup` x1 `SnocTup` x2 `SnocTup` x3 `SnocTup` x4
             `SnocTup` x5 `SnocTup` x6 `SnocTup` x7 `SnocTup` x8

tup9 :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i)
     => (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i)
     -> Exp (a, b, c, d, e, f, g, h, i)
tup9 (x1, x2, x3, x4, x5, x6, x7, x8, x9)
  = Exp $ Tuple $
      NilTup `SnocTup` x1 `SnocTup` x2 `SnocTup` x3 `SnocTup` x4
             `SnocTup` x5 `SnocTup` x6 `SnocTup` x7 `SnocTup` x8 `SnocTup` x9

untup2 :: (Elt a, Elt b) => Exp (a, b) -> (Exp a, Exp b)
untup2 e = (Exp $ SuccTupIdx ZeroTupIdx `Prj` e, Exp $ ZeroTupIdx `Prj` e)

untup3 :: (Elt a, Elt b, Elt c) => Exp (a, b, c) -> (Exp a, Exp b, Exp c)
untup3 e = (Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` e,
            Exp $ SuccTupIdx ZeroTupIdx `Prj` e,
            Exp $ ZeroTupIdx `Prj` e)

untup4 :: (Elt a, Elt b, Elt c, Elt d)
       => Exp (a, b, c, d) -> (Exp a, Exp b, Exp c, Exp d)
untup4 e = (Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` e,
            Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` e,
            Exp $ SuccTupIdx ZeroTupIdx `Prj` e,
            Exp $ ZeroTupIdx `Prj` e)

untup5 :: (Elt a, Elt b, Elt c, Elt d, Elt e)
       => Exp (a, b, c, d, e) -> (Exp a, Exp b, Exp c, Exp d, Exp e)
untup5 e = (Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))) `Prj` e,
            Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` e,
            Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` e,
            Exp $ SuccTupIdx ZeroTupIdx `Prj` e,
            Exp $ ZeroTupIdx `Prj` e)

untup6 :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f)
       => Exp (a, b, c, d, e, f) -> (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f)
untup6 e = (Exp $
              SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))) `Prj` e,
            Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))) `Prj` e,
            Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` e,
            Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` e,
            Exp $ SuccTupIdx ZeroTupIdx `Prj` e,
            Exp $ ZeroTupIdx `Prj` e)

untup7 :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g)
       => Exp (a, b, c, d, e, f, g) -> (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g)
untup7 e = (Exp $
              SuccTupIdx
                (SuccTupIdx
                  (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))) `Prj` e,
            Exp $
              SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))) `Prj` e,
            Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))) `Prj` e,
            Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` e,
            Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` e,
            Exp $ SuccTupIdx ZeroTupIdx `Prj` e,
            Exp $ ZeroTupIdx `Prj` e)

untup8 :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h)
       => Exp (a, b, c, d, e, f, g, h) -> (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h)
untup8 e = (Exp $
              SuccTupIdx
                (SuccTupIdx
                  (SuccTupIdx
                    (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))) `Prj` e,
            Exp $
              SuccTupIdx
                (SuccTupIdx
                  (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))) `Prj` e,
            Exp $
              SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))) `Prj` e,
            Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))) `Prj` e,
            Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` e,
            Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` e,
            Exp $ SuccTupIdx ZeroTupIdx `Prj` e,
            Exp $ ZeroTupIdx `Prj` e)

untup9 :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i)
       => Exp (a, b, c, d, e, f, g, h, i) -> (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i)
untup9 e = (Exp $
              SuccTupIdx
                (SuccTupIdx
                  (SuccTupIdx
                    (SuccTupIdx
                      (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))))) `Prj` e,
            Exp $
              SuccTupIdx
                (SuccTupIdx
                  (SuccTupIdx
                    (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))) `Prj` e,
            Exp $
              SuccTupIdx
                (SuccTupIdx
                  (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))) `Prj` e,
            Exp $
              SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))) `Prj` e,
            Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))) `Prj` e,
            Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` e,
            Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` e,
            Exp $ SuccTupIdx ZeroTupIdx `Prj` e,
            Exp $ ZeroTupIdx `Prj` e)

-- Smart constructor for constants
--

mkMinBound :: (Elt t, IsBounded t) => Exp t
mkMinBound = Exp $ PrimConst (PrimMinBound boundedType)

mkMaxBound :: (Elt t, IsBounded t) => Exp t
mkMaxBound = Exp $ PrimConst (PrimMaxBound boundedType)

mkPi :: (Elt r, IsFloating r) => Exp r
mkPi = Exp $ PrimConst (PrimPi floatingType)


-- Smart constructors for primitive applications
--

-- Operators from Floating

mkSin :: (Elt t, IsFloating t) => Exp t -> Exp t
mkSin x = Exp $ PrimSin floatingType `PrimApp` x

mkCos :: (Elt t, IsFloating t) => Exp t -> Exp t
mkCos x = Exp $ PrimCos floatingType `PrimApp` x

mkTan :: (Elt t, IsFloating t) => Exp t -> Exp t
mkTan x = Exp $ PrimTan floatingType `PrimApp` x

mkAsin :: (Elt t, IsFloating t) => Exp t -> Exp t
mkAsin x = Exp $ PrimAsin floatingType `PrimApp` x

mkAcos :: (Elt t, IsFloating t) => Exp t -> Exp t
mkAcos x = Exp $ PrimAcos floatingType `PrimApp` x

mkAtan :: (Elt t, IsFloating t) => Exp t -> Exp t
mkAtan x = Exp $ PrimAtan floatingType `PrimApp` x

mkAsinh :: (Elt t, IsFloating t) => Exp t -> Exp t
mkAsinh x = Exp $ PrimAsinh floatingType `PrimApp` x

mkAcosh :: (Elt t, IsFloating t) => Exp t -> Exp t
mkAcosh x = Exp $ PrimAcosh floatingType `PrimApp` x

mkAtanh :: (Elt t, IsFloating t) => Exp t -> Exp t
mkAtanh x = Exp $ PrimAtanh floatingType `PrimApp` x

mkExpFloating :: (Elt t, IsFloating t) => Exp t -> Exp t
mkExpFloating x = Exp $ PrimExpFloating floatingType `PrimApp` x

mkSqrt :: (Elt t, IsFloating t) => Exp t -> Exp t
mkSqrt x = Exp $ PrimSqrt floatingType `PrimApp` x

mkLog :: (Elt t, IsFloating t) => Exp t -> Exp t
mkLog x = Exp $ PrimLog floatingType `PrimApp` x

mkFPow :: (Elt t, IsFloating t) => Exp t -> Exp t -> Exp t
mkFPow x y = Exp $ PrimFPow floatingType `PrimApp` tup2 (x, y)

mkLogBase :: (Elt t, IsFloating t) => Exp t -> Exp t -> Exp t
mkLogBase x y = Exp $ PrimLogBase floatingType `PrimApp` tup2 (x, y)

-- Operators from Num

mkAdd :: (Elt t, IsNum t) => Exp t -> Exp t -> Exp t
mkAdd x y = Exp $ PrimAdd numType `PrimApp` tup2 (x, y)

mkSub :: (Elt t, IsNum t) => Exp t -> Exp t -> Exp t
mkSub x y = Exp $ PrimSub numType `PrimApp` tup2 (x, y)

mkMul :: (Elt t, IsNum t) => Exp t -> Exp t -> Exp t
mkMul x y = Exp $ PrimMul numType `PrimApp` tup2 (x, y)

mkNeg :: (Elt t, IsNum t) => Exp t -> Exp t
mkNeg x = Exp $ PrimNeg numType `PrimApp` x

mkAbs :: (Elt t, IsNum t) => Exp t -> Exp t
mkAbs x = Exp $ PrimAbs numType `PrimApp` x

mkSig :: (Elt t, IsNum t) => Exp t -> Exp t
mkSig x = Exp $ PrimSig numType `PrimApp` x

-- Operators from Integral & Bits

mkQuot :: (Elt t, IsIntegral t) => Exp t -> Exp t -> Exp t
mkQuot x y = Exp $ PrimQuot integralType `PrimApp` tup2 (x, y)

mkRem :: (Elt t, IsIntegral t) => Exp t -> Exp t -> Exp t
mkRem x y = Exp $ PrimRem integralType `PrimApp` tup2 (x, y)

mkIDiv :: (Elt t, IsIntegral t) => Exp t -> Exp t -> Exp t
mkIDiv x y = Exp $ PrimIDiv integralType `PrimApp` tup2 (x, y)

mkMod :: (Elt t, IsIntegral t) => Exp t -> Exp t -> Exp t
mkMod x y = Exp $ PrimMod integralType `PrimApp` tup2 (x, y)

mkBAnd :: (Elt t, IsIntegral t) => Exp t -> Exp t -> Exp t
mkBAnd x y = Exp $ PrimBAnd integralType `PrimApp` tup2 (x, y)

mkBOr :: (Elt t, IsIntegral t) => Exp t -> Exp t -> Exp t
mkBOr x y = Exp $ PrimBOr integralType `PrimApp` tup2 (x, y)

mkBXor :: (Elt t, IsIntegral t) => Exp t -> Exp t -> Exp t
mkBXor x y = Exp $ PrimBXor integralType `PrimApp` tup2 (x, y)

mkBNot :: (Elt t, IsIntegral t) => Exp t -> Exp t
mkBNot x = Exp $ PrimBNot integralType `PrimApp` x

mkBShiftL :: (Elt t, IsIntegral t) => Exp t -> Exp Int -> Exp t
mkBShiftL x i = Exp $ PrimBShiftL integralType `PrimApp` tup2 (x, i)

mkBShiftR :: (Elt t, IsIntegral t) => Exp t -> Exp Int -> Exp t
mkBShiftR x i = Exp $ PrimBShiftR integralType `PrimApp` tup2 (x, i)

mkBRotateL :: (Elt t, IsIntegral t) => Exp t -> Exp Int -> Exp t
mkBRotateL x i = Exp $ PrimBRotateL integralType `PrimApp` tup2 (x, i)

mkBRotateR :: (Elt t, IsIntegral t) => Exp t -> Exp Int -> Exp t
mkBRotateR x i = Exp $ PrimBRotateR integralType `PrimApp` tup2 (x, i)

-- Operators from Fractional

mkFDiv :: (Elt t, IsFloating t) => Exp t -> Exp t -> Exp t
mkFDiv x y = Exp $ PrimFDiv floatingType `PrimApp` tup2 (x, y)

mkRecip :: (Elt t, IsFloating t) => Exp t -> Exp t
mkRecip x = Exp $ PrimRecip floatingType `PrimApp` x

-- Operators from RealFrac

mkTruncate :: (Elt a, Elt b, IsFloating a, IsIntegral b) => Exp a -> Exp b
mkTruncate x = Exp $ PrimTruncate floatingType integralType `PrimApp` x

mkRound :: (Elt a, Elt b, IsFloating a, IsIntegral b) => Exp a -> Exp b
mkRound x = Exp $ PrimRound floatingType integralType `PrimApp` x

mkFloor :: (Elt a, Elt b, IsFloating a, IsIntegral b) => Exp a -> Exp b
mkFloor x = Exp $ PrimFloor floatingType integralType `PrimApp` x

mkCeiling :: (Elt a, Elt b, IsFloating a, IsIntegral b) => Exp a -> Exp b
mkCeiling x = Exp $ PrimCeiling floatingType integralType `PrimApp` x

-- Operators from RealFloat

mkAtan2 :: (Elt t, IsFloating t) => Exp t -> Exp t -> Exp t
mkAtan2 x y = Exp $ PrimAtan2 floatingType `PrimApp` tup2 (x, y)

-- FIXME: add missing operations from Floating, RealFrac & RealFloat

-- Relational and equality operators

mkLt :: (Elt t, IsScalar t) => Exp t -> Exp t -> Exp Bool
mkLt x y = Exp $ PrimLt scalarType `PrimApp` tup2 (x, y)

mkGt :: (Elt t, IsScalar t) => Exp t -> Exp t -> Exp Bool
mkGt x y = Exp $ PrimGt scalarType `PrimApp` tup2 (x, y)

mkLtEq :: (Elt t, IsScalar t) => Exp t -> Exp t -> Exp Bool
mkLtEq x y = Exp $ PrimLtEq scalarType `PrimApp` tup2 (x, y)

mkGtEq :: (Elt t, IsScalar t) => Exp t -> Exp t -> Exp Bool
mkGtEq x y = Exp $ PrimGtEq scalarType `PrimApp` tup2 (x, y)

mkEq :: (Elt t, IsScalar t) => Exp t -> Exp t -> Exp Bool
mkEq x y = Exp $ PrimEq scalarType `PrimApp` tup2 (x, y)

mkNEq :: (Elt t, IsScalar t) => Exp t -> Exp t -> Exp Bool
mkNEq x y = Exp $ PrimNEq scalarType `PrimApp` tup2 (x, y)

mkMax :: (Elt t, IsScalar t) => Exp t -> Exp t -> Exp t
mkMax x y = Exp $ PrimMax scalarType `PrimApp` tup2 (x, y)

mkMin :: (Elt t, IsScalar t) => Exp t -> Exp t -> Exp t
mkMin x y = Exp $ PrimMin scalarType `PrimApp` tup2 (x, y)

-- Logical operators

mkLAnd :: Exp Bool -> Exp Bool -> Exp Bool
mkLAnd x y = Exp $ PrimLAnd `PrimApp` tup2 (x, y)

mkLOr :: Exp Bool -> Exp Bool -> Exp Bool
mkLOr x y = Exp $ PrimLOr `PrimApp` tup2 (x, y)

mkLNot :: Exp Bool -> Exp Bool
mkLNot x = Exp $ PrimLNot `PrimApp` x

-- Character conversions

mkOrd :: Exp Char -> Exp Int
mkOrd x = Exp $ PrimOrd `PrimApp` x

mkChr :: Exp Int -> Exp Char
mkChr x = Exp $ PrimChr `PrimApp` x

-- Numeric conversions

mkFromIntegral :: (Elt a, Elt b, IsIntegral a, IsNum b) => Exp a -> Exp b
mkFromIntegral x = Exp $ PrimFromIntegral integralType numType `PrimApp` x

-- Other conversions

mkBoolToInt :: Exp Bool -> Exp Int
mkBoolToInt b = Exp $ PrimBoolToInt `PrimApp` b


-- Auxiliary functions
-- --------------------

infixr 0 $$
($$) :: (b -> a) -> (c -> d -> b) -> c -> d -> a
(f $$ g) x y = f (g x y)

infixr 0 $$$
($$$) :: (b -> a) -> (c -> d -> e -> b) -> c -> d -> e -> a
(f $$$ g) x y z = f (g x y z)

infixr 0 $$$$
($$$$) :: (b -> a) -> (c -> d -> e -> f -> b) -> c -> d -> e -> f -> a
(f $$$$ g) x y z u = f (g x y z u)

infixr 0 $$$$$
($$$$$) :: (b -> a) -> (c -> d -> e -> f -> g -> b) -> c -> d -> e -> f -> g-> a
(f $$$$$ g) x y z u v = f (g x y z u v)


-- Debugging
-- ---------

showPreAccOp :: forall acc seq exp arrs. PreAcc acc seq exp arrs -> String
showPreAccOp (Atag i)           = "Atag " ++ show i
showPreAccOp (Use a)            = "Use "  ++ showArrays a
showPreAccOp Pipe{}             = "Pipe"
showPreAccOp Acond{}            = "Acond"
showPreAccOp Awhile{}           = "Awhile"
showPreAccOp Atuple{}           = "Atuple"
showPreAccOp Aprj{}             = "Aprj"
showPreAccOp Unit{}             = "Unit"
showPreAccOp Generate{}         = "Generate"
showPreAccOp Reshape{}          = "Reshape"
showPreAccOp Replicate{}        = "Replicate"
showPreAccOp Slice{}            = "Slice"
showPreAccOp Map{}              = "Map"
showPreAccOp ZipWith{}          = "ZipWith"
showPreAccOp Fold{}             = "Fold"
showPreAccOp Fold1{}            = "Fold1"
showPreAccOp FoldSeg{}          = "FoldSeg"
showPreAccOp Fold1Seg{}         = "Fold1Seg"
showPreAccOp Scanl{}            = "Scanl"
showPreAccOp Scanl'{}           = "Scanl'"
showPreAccOp Scanl1{}           = "Scanl1"
showPreAccOp Scanr{}            = "Scanr"
showPreAccOp Scanr'{}           = "Scanr'"
showPreAccOp Scanr1{}           = "Scanr1"
showPreAccOp Permute{}          = "Permute"
showPreAccOp Backpermute{}      = "Backpermute"
showPreAccOp Stencil{}          = "Stencil"
showPreAccOp Stencil2{}         = "Stencil2"
showPreAccOp Aforeign{}         = "Aforeign"
showPreAccOp Collect{}          = "Collect"

showPreSeqOp :: PreSeq acc seq exp arrs -> String
showPreSeqOp (ToSeq{})          = "ToSeq"
showPreSeqOp (UseLazy{})        = "UseLazy"
showPreSeqOp (MapSeq{})         = "MapSeq"
showPreSeqOp (ZipWithSeq{})     = "ZipWithSeq"
showPreSeqOp (ScanSeq{})        = "ScanSeq"
showPreSeqOp (ScanSeqAct{})     = "ScanSeqAct"
showPreSeqOp (FromSeq{})        = "FromSeq"
showPreSeqOp (FoldSeq{})        = "FoldSeq"
showPreSeqOp (FoldSeqAct{})     = "FoldSeqAct"
showPreSeqOp (FoldSeqFlatten{}) = "FoldSeqFlatten"
showPreSeqOp (Stuple{})         = "Stuple"

showArrays :: forall arrs. Arrays arrs => arrs -> String
showArrays = display . collect (arrays (undefined::arrs)) . fromArr
  where
    collect :: ArraysR a -> a -> [String]
    collect ArraysRunit         _        = []
    collect ArraysRarray        arr      = [showShortendArr arr]
    collect (ArraysRpair r1 r2) (a1, a2) = collect r1 a1 ++ collect r2 a2
    --
    display []  = []
    display [x] = x
    display xs  = "(" ++ intercalate ", " xs ++ ")"


showShortendArr :: Elt e => Array sh e -> String
showShortendArr arr
  = show (take cutoff l) ++ if length l > cutoff then ".." else ""
  where
    l      = toList arr
    cutoff = 5


showPreExpOp :: PreExp acc seq exp t -> String
showPreExpOp (Const c)          = "Const " ++ show c
showPreExpOp (Tag i)            = "Tag" ++ show i
showPreExpOp Tuple{}            = "Tuple"
showPreExpOp Prj{}              = "Prj"
showPreExpOp IndexNil           = "IndexNil"
showPreExpOp IndexCons{}        = "IndexCons"
showPreExpOp IndexHead{}        = "IndexHead"
showPreExpOp IndexTail{}        = "IndexTail"
showPreExpOp IndexAny           = "IndexAny"
showPreExpOp ToIndex{}          = "ToIndex"
showPreExpOp FromIndex{}        = "FromIndex"
showPreExpOp Cond{}             = "Cond"
showPreExpOp While{}            = "While"
showPreExpOp PrimConst{}        = "PrimConst"
showPreExpOp PrimApp{}          = "PrimApp"
showPreExpOp Index{}            = "Index"
showPreExpOp LinearIndex{}      = "LinearIndex"
showPreExpOp Shape{}            = "Shape"
showPreExpOp ShapeSize{}        = "ShapeSize"
showPreExpOp Intersect{}        = "Intersect"
showPreExpOp Union{}            = "Union"
showPreExpOp Foreign{}          = "Foreign"

