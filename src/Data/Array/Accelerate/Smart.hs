{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Smart
-- Copyright   : [2008..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This modules defines the AST of the user-visible embedded language using more
-- convenient higher-order abstract syntax (instead of de Bruijn indices).
-- Moreover, it defines smart constructors to construct programs.
--

module Data.Array.Accelerate.Smart (

  -- * HOAS AST
  -- ** Array computations
  Acc(..), SmartAcc(..), PreSmartAcc(..),
  Level, Direction(..), Message(..),

  -- ** Scalar expressions
  Exp(..), SmartExp(..), PreSmartExp(..),
  Stencil(..),
  Boundary(..), PreBoundary(..),
  PrimBool,
  PrimMaybe,

  -- ** Extracting type information
  HasArraysR(..),
  HasTypeR(..),

  -- ** Smart constructors for literals
  constant, undef,

  -- ** Smart destructors for shapes
  indexHead, indexTail,

  -- ** Vector operations
  mkPack, mkUnpack,
  extract, mkExtract,
  insert, mkInsert,
  shuffle,
  select,

  -- ** Smart constructors for primitive functions
  -- *** Operators from Num
  mkAdd, mkSub, mkMul, mkNeg, mkAbs, mkSig,

  -- *** Operators from Integral
  mkQuot, mkRem, mkQuotRem, mkIDiv, mkMod, mkDivMod,

  -- *** Operators from FiniteBits
  mkBAnd, mkBOr, mkBXor, mkBNot,
  mkBShiftL, mkBShiftR, mkBRotateL, mkBRotateR,
  mkPopCount, mkCountLeadingZeros, mkCountTrailingZeros,

  -- *** Operators from Fractional and Floating
  mkFDiv, mkRecip,
  mkSin, mkCos, mkTan,
  mkAsin, mkAcos, mkAtan,
  mkSinh, mkCosh, mkTanh,
  mkAsinh, mkAcosh, mkAtanh,
  mkExpFloating,
  mkSqrt, mkLog,
  mkFPow, mkLogBase,

  -- *** Operators from RealFrac and RealFloat
  mkTruncate, mkRound, mkFloor, mkCeiling,
  mkAtan2,
  mkIsNaN, mkIsInfinite,

  -- *** Relational and equality operators
  mkLt, mkGt, mkLtEq, mkGtEq, mkEq, mkNEq, mkMax, mkMin, mkLAnd, mkLOr, mkLNot,

  -- ** Smart constructors for type coercion functions
  mkFromIntegral, mkToFloating, mkToBool, mkFromBool, mkBitcast, mkCoerce, Coerce(..),

  -- ** Auxiliary functions
  ($$), ($$$), ($$$$), ($$$$$),
  ApplyAcc(..),
  unAcc, unAccFunction,
  mkExp, unExp, unExpFunction, unExpBinaryFunction, mkPrimUnary, mkPrimBinary,
  unPair, mkPairToTuple,

  -- ** Miscellaneous
  formatPreAccOp,
  formatPreExpOp,

) where


import Data.Array.Accelerate.AST                                    ( Direction(..), Message(..), PrimBool, PrimMaybe, PrimFun(..), BitOrMask, primFunType )
import Data.Array.Accelerate.AST.Idx
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Representation.Array
import Data.Array.Accelerate.Representation.Elt
import Data.Array.Accelerate.Representation.Shape
import Data.Array.Accelerate.Representation.Slice
import Data.Array.Accelerate.Representation.Stencil                 hiding ( StencilR, stencilR )
import Data.Array.Accelerate.Representation.Tag
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Sugar.Array                            ( Arrays )
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Sugar.Foreign
import Data.Array.Accelerate.Sugar.Shape                            ( (:.)(..) )
import Data.Array.Accelerate.Sugar.Vec
import Data.Array.Accelerate.Type
import qualified Data.Array.Accelerate.Representation.Stencil       as R
import qualified Data.Array.Accelerate.Sugar.Array                  as Sugar
import qualified Data.Array.Accelerate.Sugar.Shape                  as Sugar

import qualified Data.Primitive.Vec                                 as Prim

import Data.Kind
import Data.Text.Lazy.Builder
import Formatting

import GHC.Prim
import GHC.TypeLits
import GHC.TypeLits.Extra


-- Array computations
-- ------------------

-- | Accelerate is an /embedded language/ that distinguishes between vanilla
-- arrays (e.g. in Haskell memory on the CPU) and embedded arrays (e.g. in
-- device memory on a GPU), as well as the computations on both of these. Since
-- Accelerate is an embedded language, programs written in Accelerate are not
-- compiled by the Haskell compiler (GHC). Rather, each Accelerate backend is
-- a /runtime compiler/ which generates and executes parallel SIMD code of the
-- target language at application /runtime/.
--
-- The type constructor 'Acc' represents embedded collective array operations.
-- A term of type @Acc a@ is an Accelerate program which, once executed, will
-- produce a value of type 'a' (an 'Array' or a tuple of 'Arrays'). Collective
-- operations of type @Acc a@ comprise many /scalar expressions/, wrapped in
-- type constructor 'Exp', which will be executed in parallel. Although
-- collective operations comprise many scalar operations executed in parallel,
-- scalar operations /cannot/ initiate new collective operations: this
-- stratification between scalar operations in 'Exp' and array operations in
-- 'Acc' helps statically exclude /nested data parallelism/, which is difficult
-- to execute efficiently on constrained hardware such as GPUs.
--
-- [/A simple example/]
--
-- As a simple example, to compute a vector dot product we can write:
--
-- > dotp :: Num a => Vector a -> Vector a -> Acc (Scalar a)
-- > dotp xs ys =
-- >   let
-- >       xs' = use xs
-- >       ys' = use ys
-- >   in
-- >   fold (+) 0 (zipWith (*) xs' ys')
--
-- The function @dotp@ consumes two one-dimensional arrays ('Vector's) of
-- values, and produces a single ('Scalar') result as output. As the return type
-- is wrapped in the type 'Acc', we see that it is an embedded Accelerate
-- computation - it will be evaluated in the /object/ language of dynamically
-- generated parallel code, rather than the /meta/ language of vanilla Haskell.
--
-- As the arguments to @dotp@ are plain Haskell arrays, to make these available
-- to Accelerate computations they must be embedded with the
-- 'Data.Array.Accelerate.Language.use' function.
--
-- An Accelerate backend is used to evaluate the embedded computation and return
-- the result back to vanilla Haskell. Calling the 'run' function of a backend
-- will generate code for the target architecture, compile, and execute it. For
-- example, the following backends are available:
--
--  * <http://hackage.haskell.org/package/accelerate-llvm-native accelerate-llvm-native>: for execution on multicore CPUs
--  * <http://hackage.haskell.org/package/accelerate-llvm-ptx accelerate-llvm-ptx>: for execution on NVIDIA CUDA-capable GPUs
--
-- See also 'Exp', which encapsulates embedded /scalar/ computations.
--
-- [/Avoiding nested parallelism/]
--
-- As mentioned above, embedded scalar computations of type 'Exp' can not
-- initiate further collective operations.
--
-- Suppose we wanted to extend our above @dotp@ function to matrix-vector
-- multiplication. First, let's rewrite our @dotp@ function to take 'Acc' arrays
-- as input (which is typically what we want):
--
-- > dotp :: Num a => Acc (Vector a) -> Acc (Vector a) -> Acc (Scalar a)
-- > dotp xs ys = fold (+) 0 $ zipWith (*) xs ys
--
-- We might then be inclined to lift our dot-product program to the following
-- (incorrect) matrix-vector product, by applying @dotp@ to each row of the
-- input matrix:
--
-- > mvm_ndp :: Num a => Acc (Matrix a) -> Acc (Vector a) -> Acc (Vector a)
-- > mvm_ndp mat vec =
-- >   let I2 rows cols = shape mat
-- >   in  generate (I1 rows)
-- >                (\(I1 row) -> the $ dotp vec (slice mat (I2 row All_)))
--
-- Here, we use 'Data.Array.Accelerate.generate' to create a one-dimensional
-- vector by applying at each index a function to 'Data.Array.Accelerate.slice'
-- out the corresponding @row@ of the matrix to pass to the @dotp@ function.
-- However, since both 'Data.Array.Accelerate.generate' and
-- 'Data.Array.Accelerate.slice' are data-parallel operations, and moreover that
-- 'Data.Array.Accelerate.slice' /depends on/ the argument @row@ given to it by
-- the 'Data.Array.Accelerate.generate' function, this definition requires
-- nested data-parallelism, and is thus not permitted. The clue that this
-- definition is invalid is that in order to create a program which will be
-- accepted by the type checker, we must use the function
-- 'Data.Array.Accelerate.the' to retrieve the result of the @dotp@ operation,
-- effectively concealing that @dotp@ is a collective array computation in order
-- to match the type expected by 'Data.Array.Accelerate.generate', which is that
-- of scalar expressions. Additionally, since we have fooled the type-checker,
-- this problem will only be discovered at program runtime.
--
-- In order to avoid this problem, we can make use of the fact that operations
-- in Accelerate are /rank polymorphic/. The 'Data.Array.Accelerate.fold'
-- operation reduces along the innermost dimension of an array of arbitrary
-- rank, reducing the rank (dimensionality) of the array by one. Thus, we can
-- 'Data.Array.Accelerate.replicate' the input vector to as many @rows@ there
-- are in the input matrix, and perform the dot-product of the vector with every
-- row simultaneously:
--
-- > mvm :: Num a => Acc (Matrix a) -> Acc (Vector a) -> Acc (Vector a)
-- > mvm mat vec =
-- >   let I2 rows cols = shape mat
-- >       vec'         = replicate (I2 rows All_) vec
-- >   in
-- >   fold (+) 0 $ zipWith (*) mat vec'
--
-- Note that the intermediate, replicated array @vec'@ is never actually created
-- in memory; it will be fused directly into the operation which consumes it. We
-- discuss fusion next.
--
-- [/Fusion/]
--
-- Array computations of type 'Acc' will be subject to /array fusion/;
-- Accelerate will combine individual 'Acc' computations into a single
-- computation, which reduces the number of traversals over the input data and
-- thus improves performance. As such, it is often useful to have some intuition
-- on when fusion should occur.
--
-- The main idea is to first partition array operations into two categories:
--
--   1. Element-wise operations, such as 'Data.Array.Accelerate.map',
--      'Data.Array.Accelerate.generate', and
--      'Data.Array.Accelerate.backpermute'. Each element of these operations
--      can be computed independently of all others.
--
--   2. Collective operations such as 'Data.Array.Accelerate.fold',
--      'Data.Array.Accelerate.scanl', and 'Data.Array.Accelerate.stencil'. To
--      compute each output element of these operations requires reading
--      multiple elements from the input array(s).
--
-- Element-wise operations fuse together whenever the consumer operation uses
-- a single element of the input array. Element-wise operations can both fuse
-- their inputs into themselves, as well be fused into later operations. Both
-- these examples should fuse into a single loop:
--
-- <<images/fusion_example_1.png>>
--
-- <<images/fusion_example_2.png>>
--
-- If the consumer operation uses more than one element of the input array
-- (typically, via 'Data.Array.Accelerate.generate' indexing an array multiple
-- times), then the input array will be completely evaluated first; no fusion
-- occurs in this case, because fusing the first operation into the second
-- implies duplicating work.
--
-- On the other hand, collective operations can fuse their input arrays into
-- themselves, but on output always evaluate to an array; collective operations
-- will not be fused into a later step. For example:
--
-- <<images/fusion_example_3.png>>
--
-- Here the element-wise sequence ('Data.Array.Accelerate.use'
-- + 'Data.Array.Accelerate.generate' + 'Data.Array.Accelerate.zipWith') will
-- fuse into a single operation, which then fuses into the collective
-- 'Data.Array.Accelerate.fold' operation. At this point in the program the
-- 'Data.Array.Accelerate.fold' must now be evaluated. In the final step the
-- 'Data.Array.Accelerate.map' reads in the array produced by
-- 'Data.Array.Accelerate.fold'. As there is no fusion between the
-- 'Data.Array.Accelerate.fold' and 'Data.Array.Accelerate.map' steps, this
-- program consists of two "loops"; one for the 'Data.Array.Accelerate.use'
-- + 'Data.Array.Accelerate.generate' + 'Data.Array.Accelerate.zipWith'
-- + 'Data.Array.Accelerate.fold' step, and one for the final
-- 'Data.Array.Accelerate.map' step.
--
-- You can see how many operations will be executed in the fused program by
-- 'Show'-ing the 'Acc' program, or by using the debugging option @-ddump-dot@
-- to save the program as a graphviz DOT file.
--
-- As a special note, the operations 'Data.Array.Accelerate.unzip' and
-- 'Data.Array.Accelerate.reshape', when applied to a real array, are executed
-- in constant time, so in this situation these operations will not be fused.
--
-- [/Tips/]
--
--  * Since 'Acc' represents embedded computations that will only be executed
--    when evaluated by a backend, we can programatically generate these
--    computations using the meta language Haskell; for example, unrolling loops
--    or embedding input values into the generated code.
--
--  * It is usually best to keep all intermediate computations in 'Acc', and
--    only 'run' the computation at the very end to produce the final result.
--    This enables optimisations between intermediate results (e.g. array
--    fusion) and, if the target architecture has a separate memory space, as is
--    the case of GPUs, to prevent excessive data transfers.
--
newtype Acc a = Acc (SmartAcc (Sugar.ArraysR a))

newtype SmartAcc a = SmartAcc (PreSmartAcc SmartAcc SmartExp a)


-- The level of lambda-bound variables. The root has level 0; then it
-- increases with each bound variable — i.e., it is the same as the size of
-- the environment at the defining occurrence.
--
type Level = Int

-- | Array-valued collective computations without a recursive knot
--
data PreSmartAcc acc exp as where
    -- Needed for conversion to de Bruijn form
  Atag          :: ArraysR as
                -> Level                        -- environment size at defining occurrence
                -> PreSmartAcc acc exp as

  Pipe          :: ArraysR as
                -> ArraysR bs
                -> ArraysR cs
                -> (SmartAcc as -> acc bs)
                -> (SmartAcc bs -> acc cs)
                -> acc as
                -> PreSmartAcc acc exp cs

  Aforeign      :: Foreign asm
                => ArraysR bs
                -> asm (as -> bs)
                -> (SmartAcc as -> SmartAcc bs)
                -> acc as
                -> PreSmartAcc acc exp bs

  Acond         :: exp PrimBool
                -> acc as
                -> acc as
                -> PreSmartAcc acc exp as

  Awhile        :: ArraysR arrs
                -> (SmartAcc arrs -> acc (Scalar PrimBool))
                -> (SmartAcc arrs -> acc arrs)
                -> acc arrs
                -> PreSmartAcc acc exp arrs

  Anil          :: PreSmartAcc acc exp ()

  Apair         :: acc arrs1
                -> acc arrs2
                -> PreSmartAcc acc exp (arrs1, arrs2)

  Aprj          :: PairIdx (arrs1, arrs2) arrs
                -> acc (arrs1, arrs2)
                -> PreSmartAcc acc exp arrs

  Atrace        :: Message arrs1
                -> acc arrs1
                -> acc arrs2
                -> PreSmartAcc acc exp arrs2

  Use           :: ArrayR (Array sh e)
                -> Array sh e
                -> PreSmartAcc acc exp (Array sh e)

  Unit          :: TypeR e
                -> exp e
                -> PreSmartAcc acc exp (Scalar e)

  Generate      :: ArrayR (Array sh e)
                -> exp sh
                -> (SmartExp sh -> exp e)
                -> PreSmartAcc acc exp (Array sh e)

  Reshape       :: ShapeR sh
                -> exp sh
                -> acc (Array sh' e)
                -> PreSmartAcc acc exp (Array sh e)

  Replicate     :: SliceIndex slix sl co sh
                -> exp slix
                -> acc                 (Array sl e)
                -> PreSmartAcc acc exp (Array sh e)

  Slice         :: SliceIndex slix sl co sh
                -> acc                 (Array sh e)
                -> exp slix
                -> PreSmartAcc acc exp (Array sl e)

  Map           :: TypeR e
                -> TypeR e'
                -> (SmartExp e -> exp e')
                -> acc (Array sh e)
                -> PreSmartAcc acc exp (Array sh e')

  ZipWith       :: TypeR e1
                -> TypeR e2
                -> TypeR e3
                -> (SmartExp e1 -> SmartExp e2 -> exp e3)
                -> acc (Array sh e1)
                -> acc (Array sh e2)
                -> PreSmartAcc acc exp (Array sh e3)

  Fold          :: TypeR e
                -> (SmartExp e -> SmartExp e -> exp e)
                -> Maybe (exp e)
                -> acc (Array (sh, INT) e)
                -> PreSmartAcc acc exp (Array sh e)

  FoldSeg       :: SingleIntegralType i
                -> TypeR e
                -> (SmartExp e -> SmartExp e -> exp e)
                -> Maybe (exp e)
                -> acc (Array (sh, INT) e)
                -> acc (Segments i)
                -> PreSmartAcc acc exp (Array (sh, INT) e)

  Scan          :: Direction
                -> TypeR e
                -> (SmartExp e -> SmartExp e -> exp e)
                -> Maybe (exp e)
                -> acc (Array (sh, INT) e)
                -> PreSmartAcc acc exp (Array (sh, INT) e)

  Scan'         :: Direction
                -> TypeR e
                -> (SmartExp e -> SmartExp e -> exp e)
                -> exp e
                -> acc (Array (sh, INT) e)
                -> PreSmartAcc acc exp (Array (sh, INT) e, Array sh e)

  Permute       :: ArrayR (Array sh e)
                -> (SmartExp e -> SmartExp e -> exp e)
                -> acc (Array sh' e)
                -> (SmartExp sh -> exp (PrimMaybe sh'))
                -> acc (Array sh e)
                -> PreSmartAcc acc exp (Array sh' e)

  Backpermute   :: ShapeR sh'
                -> exp sh'
                -> (SmartExp sh' -> exp sh)
                -> acc (Array sh e)
                -> PreSmartAcc acc exp (Array sh' e)

  Stencil       :: R.StencilR sh a stencil
                -> TypeR b
                -> (SmartExp stencil -> exp b)
                -> PreBoundary acc exp (Array sh a)
                -> acc (Array sh a)
                -> PreSmartAcc acc exp (Array sh b)

  Stencil2      :: R.StencilR sh a stencil1
                -> R.StencilR sh b stencil2
                -> TypeR c
                -> (SmartExp stencil1 -> SmartExp stencil2 -> exp c)
                -> PreBoundary acc exp (Array sh a)
                -> acc (Array sh a)
                -> PreBoundary acc exp (Array sh b)
                -> acc (Array sh b)
                -> PreSmartAcc acc exp (Array sh c)


-- Embedded expressions of the surface language
-- --------------------------------------------

-- HOAS expressions mirror the constructors of 'AST.OpenExp', but with the 'Tag'
-- constructor instead of variables in the form of de Bruijn indices.
--

-- | The type 'Exp' represents embedded scalar expressions. The collective
-- operations of Accelerate 'Acc' consist of many scalar expressions executed in
-- data-parallel.
--
-- Note that scalar expressions can not initiate new collective operations:
-- doing so introduces /nested data parallelism/, which is difficult to execute
-- efficiently on constrained hardware such as GPUs, and is thus currently
-- unsupported.
--
newtype Exp t = Exp (SmartExp (EltR t))
newtype SmartExp t = SmartExp (PreSmartExp SmartAcc SmartExp t)

-- | Scalar expressions to parametrise collective array operations, themselves parameterised over
-- the type of collective array operations.
--
data PreSmartExp acc exp t where
  -- Needed for conversion to de Bruijn form
  Tag           :: TypeR t
                -> Level                        -- environment size at defining occurrence
                -> PreSmartExp acc exp t

  -- Needed for embedded pattern matching
  Match         :: TagR t
                -> exp t
                -> PreSmartExp acc exp t

  -- All the same constructors as 'AST.Exp', plus projection
  Const         :: ScalarType t
                -> t
                -> PreSmartExp acc exp t

  Nil           :: PreSmartExp acc exp ()

  Pair          :: exp t1
                -> exp t2
                -> PreSmartExp acc exp (t1, t2)

  Prj           :: PairIdx (t1, t2) t
                -> exp (t1, t2)
                -> PreSmartExp acc exp t

  Extract       :: ScalarType (Prim.Vec n a)
                -> SingleIntegralType i
                -> exp (Prim.Vec n a)
                -> exp i
                -> PreSmartExp acc exp a

  Insert        :: ScalarType (Prim.Vec n a)
                -> SingleIntegralType i
                -> exp (Prim.Vec n a)
                -> exp i
                -> exp a
                -> PreSmartExp acc exp (Prim.Vec n a)

  Shuffle       :: ScalarType (Prim.Vec m a)
                -> SingleIntegralType i
                -> exp (Prim.Vec n a)
                -> exp (Prim.Vec n a)
                -> exp (Prim.Vec m i)
                -> PreSmartExp acc exp (Prim.Vec m a)

  Select        :: exp (Prim.Vec n Bit)
                -> exp (Prim.Vec n a)
                -> exp (Prim.Vec n a)
                -> PreSmartExp acc exp (Prim.Vec n a)

  ToIndex       :: ShapeR sh
                -> exp sh
                -> exp sh
                -> PreSmartExp acc exp INT

  FromIndex     :: ShapeR sh
                -> exp sh
                -> exp INT
                -> PreSmartExp acc exp sh

  Case          :: exp a
                -> [(TagR a, exp b)]
                -> PreSmartExp acc exp b

  Cond          :: exp PrimBool
                -> exp t
                -> exp t
                -> PreSmartExp acc exp t

  While         :: TypeR t
                -> (SmartExp t -> exp PrimBool)
                -> (SmartExp t -> exp t)
                -> exp t
                -> PreSmartExp acc exp t

  PrimApp       :: PrimFun (a -> r)
                -> exp a
                -> PreSmartExp acc exp r

  Index         :: TypeR t
                -> acc (Array sh t)
                -> exp sh
                -> PreSmartExp acc exp t

  LinearIndex   :: TypeR t
                -> acc (Array sh t)
                -> exp INT
                -> PreSmartExp acc exp t

  Shape         :: ShapeR sh
                -> acc (Array sh e)
                -> PreSmartExp acc exp sh

  ShapeSize     :: ShapeR sh
                -> exp sh
                -> PreSmartExp acc exp INT

  Foreign       :: Foreign asm
                => TypeR y
                -> asm (x -> y)
                -> (SmartExp x -> SmartExp y) -- RCE: Using SmartExp instead of exp to aid in sharing recovery.
                -> exp x
                -> PreSmartExp acc exp y

  Undef         :: ScalarType t
                -> PreSmartExp acc exp t

  Coerce        :: ScalarType a
                -> ScalarType b
                -> exp a
                -> PreSmartExp acc exp b


-- Smart constructors for stencils
-- -------------------------------

-- | Boundary condition specification for stencil operations
--
data Boundary t where
  Boundary  :: PreBoundary SmartAcc SmartExp (Array (EltR sh) (EltR e))
            -> Boundary (Sugar.Array sh e)

data PreBoundary acc exp t where
  Clamp     :: PreBoundary acc exp t
  Mirror    :: PreBoundary acc exp t
  Wrap      :: PreBoundary acc exp t

  Constant  :: e
            -> PreBoundary acc exp (Array sh e)

  Function  :: (SmartExp sh -> exp e)
            -> PreBoundary acc exp (Array sh e)


-- Stencil reification
-- -------------------
--
-- In the AST representation, we turn the stencil type from nested tuples
-- of Accelerate expressions into an Accelerate expression whose type is
-- a tuple nested in the same manner. This enables us to represent the
-- stencil function as a unary function (which also only needs one de
-- Bruijn index). The various positions in the stencil are accessed via
-- tuple indices (i.e., projections).
--
class Stencil sh e stencil where
  type StencilR sh stencil :: Type

  stencilR   :: R.StencilR (EltR sh) (EltR e) (StencilR sh stencil)
  stencilPrj :: SmartExp (StencilR sh stencil) -> stencil

-- DIM1
instance Elt e => Stencil Sugar.DIM1 e (Exp e, Exp e, Exp e) where
  type StencilR Sugar.DIM1 (Exp e, Exp e, Exp e)
    = EltR (e, e, e)
  stencilR = StencilRunit3 @(EltR e) $ eltR @e
  stencilPrj s = (Exp $ prj2 s,
                  Exp $ prj1 s,
                  Exp $ prj0 s)

instance Elt e => Stencil Sugar.DIM1 e (Exp e, Exp e, Exp e, Exp e, Exp e) where
  type StencilR Sugar.DIM1 (Exp e, Exp e, Exp e, Exp e, Exp e)
    = EltR (e, e, e, e, e)
  stencilR = StencilRunit5 $ eltR @e
  stencilPrj s = (Exp $ prj4 s,
                  Exp $ prj3 s,
                  Exp $ prj2 s,
                  Exp $ prj1 s,
                  Exp $ prj0 s)

instance Elt e => Stencil Sugar.DIM1 e (Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e) where
  type StencilR Sugar.DIM1 (Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e)
    = EltR (e, e, e, e, e, e, e)
  stencilR = StencilRunit7 $ eltR @e
  stencilPrj s = (Exp $ prj6 s,
                  Exp $ prj5 s,
                  Exp $ prj4 s,
                  Exp $ prj3 s,
                  Exp $ prj2 s,
                  Exp $ prj1 s,
                  Exp $ prj0 s)

instance Elt e => Stencil Sugar.DIM1 e (Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e)
  where
  type StencilR Sugar.DIM1 (Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e)
    = EltR (e, e, e, e, e, e, e, e, e)
  stencilR = StencilRunit9 $ eltR @e
  stencilPrj s = (Exp $ prj8 s,
                  Exp $ prj7 s,
                  Exp $ prj6 s,
                  Exp $ prj5 s,
                  Exp $ prj4 s,
                  Exp $ prj3 s,
                  Exp $ prj2 s,
                  Exp $ prj1 s,
                  Exp $ prj0 s)

-- DIM(n+1)
instance (Stencil (sh:.Int) a row2,
          Stencil (sh:.Int) a row1,
          Stencil (sh:.Int) a row0) => Stencil (sh:.Int:.Int) a (row2, row1, row0) where
  type StencilR (sh:.Int:.Int) (row2, row1, row0)
    = Tup3 (StencilR (sh:.Int) row2) (StencilR (sh:.Int) row1) (StencilR (sh:.Int) row0)
  stencilR = StencilRtup3 (stencilR @(sh:.Int) @a @row2) (stencilR @(sh:.Int) @a @row1) (stencilR @(sh:.Int) @a @row0)
  stencilPrj s = (stencilPrj @(sh:.Int) @a $ prj2 s,
                  stencilPrj @(sh:.Int) @a $ prj1 s,
                  stencilPrj @(sh:.Int) @a $ prj0 s)

instance (Stencil (sh:.Int) a row4,
          Stencil (sh:.Int) a row3,
          Stencil (sh:.Int) a row2,
          Stencil (sh:.Int) a row1,
          Stencil (sh:.Int) a row0) => Stencil (sh:.Int:.Int) a (row4, row3, row2, row1, row0) where
  type StencilR (sh:.Int:.Int) (row4, row3, row2, row1, row0)
    = Tup5 (StencilR (sh:.Int) row4) (StencilR (sh:.Int) row3) (StencilR (sh:.Int) row2)
       (StencilR (sh:.Int) row1) (StencilR (sh:.Int) row0)
  stencilR = StencilRtup5 (stencilR @(sh:.Int) @a @row4) (stencilR @(sh:.Int) @a @row3)
                  (stencilR @(sh:.Int) @a @row2) (stencilR @(sh:.Int) @a @row1) (stencilR @(sh:.Int) @a @row0)
  stencilPrj s = (stencilPrj @(sh:.Int) @a $ prj4 s,
                  stencilPrj @(sh:.Int) @a $ prj3 s,
                  stencilPrj @(sh:.Int) @a $ prj2 s,
                  stencilPrj @(sh:.Int) @a $ prj1 s,
                  stencilPrj @(sh:.Int) @a $ prj0 s)

instance (Stencil (sh:.Int) a row6,
          Stencil (sh:.Int) a row5,
          Stencil (sh:.Int) a row4,
          Stencil (sh:.Int) a row3,
          Stencil (sh:.Int) a row2,
          Stencil (sh:.Int) a row1,
          Stencil (sh:.Int) a row0)
  => Stencil (sh:.Int:.Int) a (row6, row5, row4, row3, row2, row1, row0) where
  type StencilR (sh:.Int:.Int) (row6, row5, row4, row3, row2, row1, row0)
    = Tup7 (StencilR (sh:.Int) row6) (StencilR (sh:.Int) row5) (StencilR (sh:.Int) row4)
       (StencilR (sh:.Int) row3) (StencilR (sh:.Int) row2) (StencilR (sh:.Int) row1)
       (StencilR (sh:.Int) row0)
  stencilR = StencilRtup7 (stencilR @(sh:.Int) @a @row6)
                  (stencilR @(sh:.Int) @a @row5) (stencilR @(sh:.Int) @a @row4) (stencilR @(sh:.Int) @a @row3)
                  (stencilR @(sh:.Int) @a @row2) (stencilR @(sh:.Int) @a @row1) (stencilR @(sh:.Int) @a @row0)
  stencilPrj s = (stencilPrj @(sh:.Int) @a $ prj6 s,
                  stencilPrj @(sh:.Int) @a $ prj5 s,
                  stencilPrj @(sh:.Int) @a $ prj4 s,
                  stencilPrj @(sh:.Int) @a $ prj3 s,
                  stencilPrj @(sh:.Int) @a $ prj2 s,
                  stencilPrj @(sh:.Int) @a $ prj1 s,
                  stencilPrj @(sh:.Int) @a $ prj0 s)

instance (Stencil (sh:.Int) a row8,
          Stencil (sh:.Int) a row7,
          Stencil (sh:.Int) a row6,
          Stencil (sh:.Int) a row5,
          Stencil (sh:.Int) a row4,
          Stencil (sh:.Int) a row3,
          Stencil (sh:.Int) a row2,
          Stencil (sh:.Int) a row1,
          Stencil (sh:.Int) a row0)
  => Stencil (sh:.Int:.Int) a (row8, row7, row6, row5, row4, row3, row2, row1, row0) where
  type StencilR (sh:.Int:.Int) (row8, row7, row6, row5, row4, row3, row2, row1, row0)
    = Tup9 (StencilR (sh:.Int) row8) (StencilR (sh:.Int) row7) (StencilR (sh:.Int) row6)
       (StencilR (sh:.Int) row5) (StencilR (sh:.Int) row4) (StencilR (sh:.Int) row3)
       (StencilR (sh:.Int) row2) (StencilR (sh:.Int) row1) (StencilR (sh:.Int) row0)
  stencilR = StencilRtup9
                  (stencilR @(sh:.Int) @a @row8) (stencilR @(sh:.Int) @a @row7) (stencilR @(sh:.Int) @a @row6)
                  (stencilR @(sh:.Int) @a @row5) (stencilR @(sh:.Int) @a @row4) (stencilR @(sh:.Int) @a @row3)
                  (stencilR @(sh:.Int) @a @row2) (stencilR @(sh:.Int) @a @row1) (stencilR @(sh:.Int) @a @row0)
  stencilPrj s = (stencilPrj @(sh:.Int) @a $ prj8 s,
                  stencilPrj @(sh:.Int) @a $ prj7 s,
                  stencilPrj @(sh:.Int) @a $ prj6 s,
                  stencilPrj @(sh:.Int) @a $ prj5 s,
                  stencilPrj @(sh:.Int) @a $ prj4 s,
                  stencilPrj @(sh:.Int) @a $ prj3 s,
                  stencilPrj @(sh:.Int) @a $ prj2 s,
                  stencilPrj @(sh:.Int) @a $ prj1 s,
                  stencilPrj @(sh:.Int) @a $ prj0 s)

prjTail :: SmartExp (t, a) -> SmartExp t
prjTail = SmartExp . Prj PairIdxLeft

prj0 :: SmartExp (t, a) -> SmartExp a
prj0 = SmartExp . Prj PairIdxRight

prj1 :: SmartExp ((t, a), s0) -> SmartExp a
prj1 = prj0 . prjTail

prj2 :: SmartExp (((t, a), s1), s0) -> SmartExp a
prj2 = prj1 . prjTail

prj3 :: SmartExp ((((t, a), s2), s1), s0) -> SmartExp a
prj3 = prj2 . prjTail

prj4 :: SmartExp (((((t, a), s3), s2), s1), s0) -> SmartExp a
prj4 = prj3 . prjTail

prj5 :: SmartExp ((((((t, a), s4), s3), s2), s1), s0) -> SmartExp a
prj5 = prj4 . prjTail

prj6 :: SmartExp (((((((t, a), s5), s4), s3), s2), s1), s0) -> SmartExp a
prj6 = prj5 . prjTail

prj7 :: SmartExp ((((((((t, a), s6), s5), s4), s3), s2), s1), s0) -> SmartExp a
prj7 = prj6 . prjTail

prj8 :: SmartExp (((((((((t, a), s7), s6), s5), s4), s3), s2), s1), s0) -> SmartExp a
prj8 = prj7 . prjTail


-- Extracting type information
-- ---------------------------

class HasArraysR f where
  arraysR :: f a -> ArraysR a

instance HasArraysR SmartAcc where
  arraysR (SmartAcc e) = arraysR e

arrayR :: HasArraysR f => f (Array sh e) -> ArrayR (Array sh e)
arrayR acc = case arraysR acc of
  TupRsingle repr -> repr

instance HasArraysR acc => HasArraysR (PreSmartAcc acc exp) where
  arraysR = \case
    Atag repr _               -> repr
    Pipe _ _ repr  _ _ _      -> repr
    Aforeign repr _ _ _       -> repr
    Acond _ a _               -> arraysR a
    Awhile _ _ _ a            -> arraysR a
    Anil                      -> TupRunit
    Apair a1 a2               -> arraysR a1 `TupRpair` arraysR a2
    Aprj idx a | TupRpair t1 t2 <- arraysR a
                              -> case idx of
                                   PairIdxLeft  -> t1
                                   PairIdxRight -> t2
    Aprj _ _                  -> error "Ejector seat? You're joking!"
    Atrace _ _ a              -> arraysR a
    Use repr _                -> TupRsingle repr
    Unit aR _                 -> TupRsingle $ ArrayR ShapeRz $ aR
    Generate repr _ _         -> TupRsingle repr
    Reshape shr _ a           -> let ArrayR _ aR = arrayR a
                                 in  TupRsingle $ ArrayR shr aR
    Replicate si _ a          -> let ArrayR _ aR = arrayR a
                                 in  TupRsingle $ ArrayR (sliceDomainR si) aR
    Slice si a _              -> let ArrayR _ aR = arrayR a
                                 in  TupRsingle $ ArrayR (sliceShapeR si) aR
    Map _ aR _ a              -> let ArrayR shr _ = arrayR a
                                 in  TupRsingle $ ArrayR shr aR
    ZipWith _ _ aR _ a _      -> let ArrayR shr _ = arrayR a
                                 in  TupRsingle $ ArrayR shr aR
    Fold _ _ _ a              -> let ArrayR (ShapeRsnoc shr) aR = arrayR a
                                 in  TupRsingle (ArrayR shr aR)
    FoldSeg _ _ _ _ a _       -> arraysR a
    Scan _ _ _ _ a            -> arraysR a
    Scan' _ _ _ _ a           -> let repr@(ArrayR (ShapeRsnoc shr) aR) = arrayR a
                                 in  TupRsingle repr `TupRpair` TupRsingle (ArrayR shr aR)
    Permute _ _ a _ _         -> arraysR a
    Backpermute shr _ _ a     -> let ArrayR _ aR = arrayR a
                                 in  TupRsingle (ArrayR shr aR)
    Stencil s aR _ _ _        -> TupRsingle $ ArrayR (stencilShapeR s) aR
    Stencil2 s _ aR _ _ _ _ _ -> TupRsingle $ ArrayR (stencilShapeR s) aR


class HasTypeR f where
  typeR :: HasCallStack => f t -> TypeR t

instance HasTypeR SmartExp where
  typeR (SmartExp e) = typeR e

instance HasTypeR exp => HasTypeR (PreSmartExp acc exp) where
  typeR = \case
    Tag tR _                        -> tR
    Match _ e                       -> typeR e
    Const tR _                      -> TupRsingle tR
    Nil                             -> TupRunit
    Pair e1 e2                      -> typeR e1 `TupRpair` typeR e2
    Prj idx e
      | TupRpair t1 t2 <- typeR e   -> case idx of
                                         PairIdxLeft  -> t1
                                         PairIdxRight -> t2
    Prj _ _                         -> error "I never joke about my work"
    Extract vR _ _ _                -> TupRsingle (scalar vR)
      where
        scalar :: ScalarType (Prim.Vec n a) -> ScalarType a
        scalar (NumScalarType t) = NumScalarType (num t)
        scalar (BitScalarType t) = BitScalarType (bit t)

        bit :: BitType (Prim.Vec n a) -> BitType a
        bit TypeMask{} = TypeBit

        num :: NumType (Prim.Vec n a) -> NumType a
        num (IntegralNumType t) = IntegralNumType (integral t)
        num (FloatingNumType t) = FloatingNumType (floating t)

        integral :: IntegralType (Prim.Vec n a) -> IntegralType a
        integral (SingleIntegralType   t) = case t of
        integral (VectorIntegralType _ t) = SingleIntegralType t

        floating :: FloatingType (Prim.Vec n a) -> FloatingType a
        floating (SingleFloatingType   t) = case t of
        floating (VectorFloatingType _ t) = SingleFloatingType t
    --
    Insert t _ _ _ _                -> TupRsingle t
    Shuffle t _ _ _ _               -> TupRsingle t
    Select _ x _                    -> typeR x
    ToIndex _ _ _                   -> TupRsingle (scalarType @INT)
    FromIndex shr _ _               -> shapeType shr
    Case _ ((_,c):_)                -> typeR c
    Case{}                          -> internalError "encountered empty case"
    Cond _ e _                      -> typeR e
    While t _ _ _                   -> t
    PrimApp f _                     -> snd $ primFunType f
    Index tR _ _                    -> tR
    LinearIndex tR _ _              -> tR
    Shape shr _                     -> shapeType shr
    ShapeSize _ _                   -> TupRsingle (scalarType @INT)
    Foreign tR _ _ _                -> tR
    Undef tR                        -> TupRsingle tR
    Coerce _ tR _                   -> TupRsingle tR


-- Smart constructors
-- ------------------

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
constant :: forall e. (HasCallStack, Elt e) => e -> Exp e
constant = Exp . go (eltR @e) . fromElt
  where
    go :: HasCallStack => TypeR t -> t -> SmartExp t
    go TupRunit         ()       = SmartExp $ Nil
    go (TupRsingle tR)  c        = SmartExp $ Const tR c
    go (TupRpair t1 t2) (c1, c2) = SmartExp $ go t1 c1 `Pair` go t2 c2


-- | 'undef' can be used anywhere a constant is expected, and indicates that the
-- consumer of the value can receive an unspecified bit pattern.
--
-- This is useful because a store of an undefined value can be assumed to not
-- have any effect; we can assume that the value is overwritten with bits that
-- happen to match what was already there. However, a store /to/ an undefined
-- location could clobber arbitrary memory, therefore, its use in such a context
-- would introduce undefined /behaviour/.
--
-- There are (at least) two cases where you may want to use this:
--
--   1. The 'Data.Array.Accelerate.Language.permute' function requires an array
--      of default values, into which the new values are combined. However, if
--      you are sure the default values are not used, and will (eventually) be
--      completely overwritten, then 'Data.Array.Accelerate.Prelude.fill'ing an
--      array with this value will give you a new uninitialised array.
--
--   2. In the definition of sum data types. See for example
--      "Data.Array.Accelerate.Data.Maybe" and
--      "Data.Array.Accelerate.Data.Either".
--
-- @since 1.2.0.0
--
undef :: forall e. Elt e => Exp e
undef = Exp $ go $ eltR @e
  where
    go :: TypeR t -> SmartExp t
    go TupRunit         = SmartExp $ Nil
    go (TupRsingle t)   = SmartExp $ Undef t
    go (TupRpair t1 t2) = SmartExp $ go t1 `Pair` go t2


-- | Get the innermost dimension of a shape.
--
-- The innermost dimension (right-most component of the shape) is the index of
-- the array which varies most rapidly, and corresponds to elements of the array
-- which are adjacent in memory.
--
-- Another way to think of this is, for example when writing nested loops over
-- an array in C, this index corresponds to the index iterated over by the
-- innermost nested loop.
--
indexHead :: (Elt sh, Elt a) => Exp (sh :. a) -> Exp a
indexHead (Exp x) = mkExp $ Prj PairIdxRight x

-- | Get all but the innermost element of a shape
--
indexTail :: (Elt sh, Elt a) => Exp (sh :. a) -> Exp sh
indexTail (Exp x) = mkExp $ Prj PairIdxLeft x


mkUnpack :: forall n a. (SIMD n a, Elt a) => Exp (Vec n a) -> [Exp a]
mkUnpack v =
  let n = fromIntegral (natVal' (proxy# :: Proxy# n)) :: Word8
   in map (extract v . constant) [0 .. n-1]

mkPack :: forall n a. (SIMD n a, Elt a) => [Exp a] -> Exp (Vec n a)
mkPack xs =
  let go :: Word8 -> [Exp a] -> Exp (Vec n a) -> Exp (Vec n a)
      go _ []     vec = vec
      go i (v:vs) vec = go (i+1) vs (insert vec (constant i) v)
      --
      n = fromIntegral (natVal' (proxy# :: Proxy# n))
  in
  go 0 (take n xs) undef

-- | Extract a single scalar element from the given SIMD vector at the
-- specified index
--
-- @since 1.4.0.0
--
extract :: forall n a i. (Elt a, SIMD n a, IsSingleIntegral (EltR i))
        => Exp (Vec n a)
        -> Exp i
        -> Exp a
extract (Exp v) (Exp i) = Exp $ mkExtract (vecR @n @a) (eltR @a) singleIntegralType v i

mkExtract :: TypeR v -> TypeR e -> SingleIntegralType i -> SmartExp v -> SmartExp i -> SmartExp e
mkExtract _vR _eR iR _v i = go _vR _eR _v
  where
    go :: TypeR v -> TypeR e -> SmartExp v -> SmartExp e
    go TupRunit           TupRunit           _ = SmartExp Nil
    go (TupRsingle vR)    (TupRsingle eR)    v = scalar vR eR v
    go (TupRpair vR1 vR2) (TupRpair eR1 eR2) v =
      let (v1, v2) = unPair v
       in SmartExp $ go vR1 eR1 v1 `Pair` go vR2 eR2 v2
    go _ _ _ = error "impossible"

    scalar :: ScalarType v -> ScalarType e -> SmartExp v -> SmartExp e
    scalar (NumScalarType vR) (NumScalarType eR) = num vR eR
    scalar (BitScalarType vR) (BitScalarType eR) = bit vR eR
    scalar _ _ = error "impossible"

    bit :: BitType v -> BitType e -> SmartExp v -> SmartExp e
    bit TypeMask{} TypeBit v = SmartExp $ Extract scalarType iR v i
    bit _ _ _ = error "impossible"

    num :: NumType v -> NumType e -> SmartExp v -> SmartExp e
    num (IntegralNumType vR) (IntegralNumType eR) = integral vR eR
    num (FloatingNumType vR) (FloatingNumType eR) = floating vR eR
    num _ _ = error "impossible"

    integral :: IntegralType v -> IntegralType e -> SmartExp v -> SmartExp e
    integral (VectorIntegralType n vR) (SingleIntegralType tR) v
      | Just Refl <- matchSingleIntegralType vR tR
      = SmartExp $ Extract (NumScalarType (IntegralNumType (VectorIntegralType n vR))) iR v i
    integral _ _ _ = error "impossible"

    floating :: FloatingType v -> FloatingType e -> SmartExp v -> SmartExp e
    floating (VectorFloatingType n vR) (SingleFloatingType tR) v
      | Just Refl <- matchSingleFloatingType vR tR
      = SmartExp $ Extract (NumScalarType (FloatingNumType (VectorFloatingType n vR))) iR v i
    floating _ _ _ = error "impossible"


-- | Insert a scalar element into the given SIMD vector at the specified
-- index
--
-- @since 1.4.0.0
--
insert :: forall n a i. (Elt a, SIMD n a, IsSingleIntegral (EltR i))
       => Exp (Vec n a)
       -> Exp i
       -> Exp a
       -> Exp (Vec n a)
insert (Exp v) (Exp i) (Exp x) = Exp $ mkInsert (vecR @n @a) (eltR @a) singleIntegralType v i x

mkInsert :: TypeR v -> TypeR e -> SingleIntegralType i -> SmartExp v -> SmartExp i -> SmartExp e -> SmartExp v
mkInsert _vR _eR iR _v i _x = go _vR _eR _v _x
  where
    go :: TypeR v -> TypeR e -> SmartExp v -> SmartExp e -> SmartExp v
    go TupRunit           TupRunit           _ _ = SmartExp Nil
    go (TupRsingle vR)    (TupRsingle eR)    v e = scalar vR eR v e
    go (TupRpair vR1 vR2) (TupRpair eR1 eR2) v e =
      let (v1, v2) = unPair v
          (e1, e2) = unPair e
       in SmartExp $ go vR1 eR1 v1 e1 `Pair` go vR2 eR2 v2 e2
    go _ _ _ _ = error "impossible"

    scalar :: ScalarType v -> ScalarType e -> SmartExp v -> SmartExp e -> SmartExp v
    scalar (NumScalarType vR) (NumScalarType eR) = num vR eR
    scalar (BitScalarType vR) (BitScalarType eR) = bit vR eR
    scalar _ _ = error "impossible"

    bit :: BitType v -> BitType e -> SmartExp v -> SmartExp e -> SmartExp v
    bit TypeMask{} TypeBit v = SmartExp . Insert scalarType iR v i
    bit _ _ _ = error "impossible"

    num :: NumType v -> NumType e -> SmartExp v -> SmartExp e -> SmartExp v
    num (IntegralNumType vR) (IntegralNumType eR) = integral vR eR
    num (FloatingNumType vR) (FloatingNumType eR) = floating vR eR
    num _ _ = error "impossible"

    integral :: IntegralType v -> IntegralType e -> SmartExp v -> SmartExp e -> SmartExp v
    integral (VectorIntegralType n vR) (SingleIntegralType tR) v
      | Just Refl <- matchSingleIntegralType vR tR
      = SmartExp . Insert (NumScalarType (IntegralNumType (VectorIntegralType n vR))) iR v i
    integral _ _ _ = error "impossible"

    floating :: FloatingType v -> FloatingType e -> SmartExp v -> SmartExp e -> SmartExp v
    floating (VectorFloatingType n vR) (SingleFloatingType tR) v
      | Just Refl <- matchSingleFloatingType vR tR
      = SmartExp . Insert (NumScalarType (FloatingNumType (VectorFloatingType n vR))) iR v i
    floating _ _ _ = error "impossible"


-- | Construct a permutation of elements from two input vectors
--
-- The elements of the two input vectors are concatenated and numbered from
-- zero left-to-right. For each element in the result vector, the shuffle
-- mask selects the corresponding element from this concatenated vector to
-- copy to the result.
--
-- @since 1.4.0.0
--
shuffle :: forall m n a i. (SIMD n a, SIMD m a, SIMD m i, IsSingleIntegral (EltR i))
        => Exp (Vec n a)
        -> Exp (Vec n a)
        -> Exp (Vec m i)
        -> Exp (Vec m a)
shuffle (Exp xs) (Exp ys) (Exp i) = Exp $ go (vecR @n @a) (vecR @m @a) xs ys
  where
    go :: TypeR t -> TypeR r -> SmartExp t -> SmartExp t -> SmartExp r
    go TupRunit           TupRunit           _ _ = SmartExp Nil
    go (TupRsingle vR)    (TupRsingle rR)    x y = scalar vR rR x y
    go (TupRpair vR1 vR2) (TupRpair rR1 rR2) x y =
      let (x1, x2) = unPair x
          (y1, y2) = unPair y
       in SmartExp $ go vR1 rR1 x1 y1 `Pair` go vR2 rR2 x2 y2
    go _ _ _ _ = error "impossible"

    scalar :: ScalarType t -> ScalarType r -> SmartExp t -> SmartExp t -> SmartExp r
    scalar (NumScalarType vR) (NumScalarType rR) = num vR rR
    scalar (BitScalarType vR) (BitScalarType rR) = bit vR rR
    scalar _ _ = error "impossible"

    bit :: BitType t -> BitType r -> SmartExp t -> SmartExp t -> SmartExp r
    bit (TypeMask _) (TypeMask m) x y
      | TupRsingle (NumScalarType (IntegralNumType (VectorIntegralType m' iR))) <- vecR @m @i
      , Just Refl <- sameNat' m m'
      = SmartExp $ Shuffle (BitScalarType (TypeMask m)) iR x y i
    bit _ _ _ _ = error "impossible"

    num :: NumType t -> NumType r -> SmartExp t -> SmartExp t -> SmartExp r
    num (IntegralNumType vR) (IntegralNumType rR) = integral vR rR
    num (FloatingNumType vR) (FloatingNumType rR) = floating vR rR
    num _ _ = error "impossible"

    integral :: IntegralType t -> IntegralType r -> SmartExp t -> SmartExp t -> SmartExp r
    integral (VectorIntegralType _ vR) (VectorIntegralType m rR) x y
      | TupRsingle (NumScalarType (IntegralNumType (VectorIntegralType m' iR))) <- vecR @m @i
      , Just Refl <- matchSingleIntegralType vR rR
      , Just Refl <- sameNat' m m'
      = SmartExp $ Shuffle (NumScalarType (IntegralNumType (VectorIntegralType m vR))) iR x y i
    integral _ _ _ _ = error "impossible"

    floating :: FloatingType t -> FloatingType r -> SmartExp t -> SmartExp t -> SmartExp r
    floating (VectorFloatingType _ vR) (VectorFloatingType m rR) x y
      | TupRsingle (NumScalarType (IntegralNumType (VectorIntegralType m' iR))) <- vecR @m @i
      , Just Refl <- matchSingleFloatingType vR rR
      , Just Refl <- sameNat' m m'
      = SmartExp $ Shuffle (NumScalarType (FloatingNumType (VectorFloatingType m vR))) iR x y i
    floating _ _ _ _ = error "impossible"


-- | Choose one value based on a condition, without branching. This is strict in
-- both arguments.
--
-- @since 1.4.0.0
--
select :: forall n a. SIMD n a
       => Exp (Vec n Bool)
       -> Exp (Vec n a)
       -> Exp (Vec n a)
       -> Exp (Vec n a)
select (Exp mask) (Exp tt) (Exp ff) = Exp $ go (vecR @n @a) tt ff
  where
    go :: TypeR t -> SmartExp t -> SmartExp t -> SmartExp t
    go TupRunit           _ _ = SmartExp Nil
    go (TupRsingle vR)    t f = scalar vR t f
    go (TupRpair vR1 vR2) t f =
      let (t1, t2) = unPair t
          (f1, f2) = unPair f
       in SmartExp $ go vR1 t1 f1 `Pair` go vR2 t2 f2

    scalar :: ScalarType t -> SmartExp t -> SmartExp t -> SmartExp t
    scalar (NumScalarType vR) = num vR
    scalar (BitScalarType vR) = bit vR

    bit :: BitType t -> SmartExp t -> SmartExp t -> SmartExp t
    bit (TypeMask n)
      | Just Refl <- sameNat' n (proxy# :: Proxy# n)
      = SmartExp $$ Select mask
    bit _ = error "impossible"

    num :: NumType t -> SmartExp t -> SmartExp t -> SmartExp t
    num (IntegralNumType vR) = integral vR
    num (FloatingNumType vR) = floating vR

    integral :: IntegralType t -> SmartExp t -> SmartExp t -> SmartExp t
    integral (VectorIntegralType n _)
      | Just Refl <- sameNat' n (proxy# :: Proxy# n)
      = SmartExp $$ Select mask
    integral _ = error "impossible"

    floating :: FloatingType t -> SmartExp t -> SmartExp t -> SmartExp t
    floating (VectorFloatingType n _)
      | Just Refl <- sameNat' n (proxy# :: Proxy# n)
      = SmartExp $$ Select mask
    floating _ = error "impossible"


-- Smart constructors for primitive applications
--

-- Operators from Floating

mkSin :: IsFloating (EltR t) => Exp t -> Exp t
mkSin = mkPrimUnary $ PrimSin floatingType

mkCos :: IsFloating (EltR t) => Exp t -> Exp t
mkCos = mkPrimUnary $ PrimCos floatingType

mkTan :: IsFloating (EltR t) => Exp t -> Exp t
mkTan = mkPrimUnary $ PrimTan floatingType

mkAsin :: IsFloating (EltR t) => Exp t -> Exp t
mkAsin = mkPrimUnary $ PrimAsin floatingType

mkAcos :: IsFloating (EltR t) => Exp t -> Exp t
mkAcos = mkPrimUnary $ PrimAcos floatingType

mkAtan :: IsFloating (EltR t) => Exp t -> Exp t
mkAtan = mkPrimUnary $ PrimAtan floatingType

mkSinh :: IsFloating (EltR t) => Exp t -> Exp t
mkSinh = mkPrimUnary $ PrimSinh floatingType

mkCosh :: IsFloating (EltR t) => Exp t -> Exp t
mkCosh = mkPrimUnary $ PrimCosh floatingType

mkTanh :: IsFloating (EltR t) => Exp t -> Exp t
mkTanh = mkPrimUnary $ PrimTanh floatingType

mkAsinh :: IsFloating (EltR t) => Exp t -> Exp t
mkAsinh = mkPrimUnary $ PrimAsinh floatingType

mkAcosh :: IsFloating (EltR t) => Exp t -> Exp t
mkAcosh = mkPrimUnary $ PrimAcosh floatingType

mkAtanh :: IsFloating (EltR t) => Exp t -> Exp t
mkAtanh = mkPrimUnary $ PrimAtanh floatingType

mkExpFloating :: IsFloating (EltR t) => Exp t -> Exp t
mkExpFloating = mkPrimUnary $ PrimExpFloating floatingType

mkSqrt :: IsFloating (EltR t) => Exp t -> Exp t
mkSqrt = mkPrimUnary $ PrimSqrt floatingType

mkLog :: IsFloating (EltR t) => Exp t -> Exp t
mkLog = mkPrimUnary $ PrimLog floatingType

mkFPow :: IsFloating (EltR t) => Exp t -> Exp t -> Exp t
mkFPow = mkPrimBinary $ PrimFPow floatingType

mkLogBase :: IsFloating (EltR t) => Exp t -> Exp t -> Exp t
mkLogBase = mkPrimBinary $ PrimLogBase floatingType

-- Operators from Num

mkAdd :: IsNum (EltR t) => Exp t -> Exp t -> Exp t
mkAdd = mkPrimBinary $ PrimAdd numType

mkSub :: IsNum (EltR t) => Exp t -> Exp t -> Exp t
mkSub = mkPrimBinary $ PrimSub numType

mkMul :: IsNum (EltR t) => Exp t -> Exp t -> Exp t
mkMul = mkPrimBinary $ PrimMul numType

mkNeg :: IsNum (EltR t) => Exp t -> Exp t
mkNeg = mkPrimUnary $ PrimNeg numType

mkAbs :: IsNum (EltR t) => Exp t -> Exp t
mkAbs = mkPrimUnary $ PrimAbs numType

mkSig :: IsNum (EltR t) => Exp t -> Exp t
mkSig = mkPrimUnary $ PrimSig numType

-- Operators from Integral

mkQuot :: IsIntegral (EltR t) => Exp t -> Exp t -> Exp t
mkQuot = mkPrimBinary $ PrimQuot integralType

mkRem :: IsIntegral (EltR t) => Exp t -> Exp t -> Exp t
mkRem = mkPrimBinary $ PrimRem integralType

mkQuotRem :: IsIntegral (EltR t) => Exp t -> Exp t -> (Exp t, Exp t)
mkQuotRem (Exp x) (Exp y) =
  let pair = SmartExp $ PrimQuotRem integralType `PrimApp` SmartExp (Pair x y)
   in ( mkExp $ Prj PairIdxLeft  pair
      , mkExp $ Prj PairIdxRight pair)

mkIDiv :: IsIntegral (EltR t) => Exp t -> Exp t -> Exp t
mkIDiv = mkPrimBinary $ PrimIDiv integralType

mkMod :: IsIntegral (EltR t) => Exp t -> Exp t -> Exp t
mkMod = mkPrimBinary $ PrimMod integralType

mkDivMod :: IsIntegral (EltR t) => Exp t -> Exp t -> (Exp t, Exp t)
mkDivMod (Exp x) (Exp y) =
  let pair = SmartExp $ PrimDivMod integralType `PrimApp` SmartExp (Pair x y)
   in ( mkExp $ Prj PairIdxLeft  pair
      , mkExp $ Prj PairIdxRight pair)

-- Operators from Bits and FiniteBits

mkBAnd :: IsIntegral (EltR t) => Exp t -> Exp t -> Exp t
mkBAnd = mkPrimBinary $ PrimBAnd integralType

mkBOr :: IsIntegral (EltR t) => Exp t -> Exp t -> Exp t
mkBOr = mkPrimBinary $ PrimBOr integralType

mkBXor :: IsIntegral (EltR t) => Exp t -> Exp t -> Exp t
mkBXor = mkPrimBinary $ PrimBXor integralType

mkBNot :: IsIntegral (EltR t) => Exp t -> Exp t
mkBNot = mkPrimUnary $ PrimBNot integralType

mkBShiftL :: IsIntegral (EltR t) => Exp t -> Exp t -> Exp t
mkBShiftL = mkPrimBinary $ PrimBShiftL integralType

mkBShiftR :: IsIntegral (EltR t) => Exp t -> Exp t -> Exp t
mkBShiftR = mkPrimBinary $ PrimBShiftR integralType

mkBRotateL :: IsIntegral (EltR t) => Exp t -> Exp t -> Exp t
mkBRotateL = mkPrimBinary $ PrimBRotateL integralType

mkBRotateR :: IsIntegral (EltR t) => Exp t -> Exp t -> Exp t
mkBRotateR = mkPrimBinary $ PrimBRotateR integralType

mkPopCount :: IsIntegral (EltR t) => Exp t -> Exp t
mkPopCount = mkPrimUnary $ PrimPopCount integralType

mkCountLeadingZeros :: IsIntegral (EltR t) => Exp t -> Exp t
mkCountLeadingZeros = mkPrimUnary $ PrimCountLeadingZeros integralType

mkCountTrailingZeros :: IsIntegral (EltR t) => Exp t -> Exp t
mkCountTrailingZeros = mkPrimUnary $ PrimCountTrailingZeros integralType

-- Operators from Fractional

mkFDiv :: IsFloating (EltR t) => Exp t -> Exp t -> Exp t
mkFDiv = mkPrimBinary $ PrimFDiv floatingType

mkRecip :: IsFloating (EltR t) => Exp t -> Exp t
mkRecip = mkPrimUnary $ PrimRecip floatingType

-- Operators from RealFrac

mkTruncate :: (IsFloating (EltR a), IsIntegral (EltR b)) => Exp a -> Exp b
mkTruncate = mkPrimUnary $ PrimTruncate floatingType integralType

mkRound :: (IsFloating (EltR a), IsIntegral (EltR b)) => Exp a -> Exp b
mkRound = mkPrimUnary $ PrimRound floatingType integralType

mkFloor :: (IsFloating (EltR a), IsIntegral (EltR b)) => Exp a -> Exp b
mkFloor = mkPrimUnary $ PrimFloor floatingType integralType

mkCeiling :: (IsFloating (EltR a), IsIntegral (EltR b)) => Exp a -> Exp b
mkCeiling = mkPrimUnary $ PrimCeiling floatingType integralType

-- Operators from RealFloat

mkAtan2 :: IsFloating (EltR t) => Exp t -> Exp t -> Exp t
mkAtan2 = mkPrimBinary $ PrimAtan2 floatingType

mkIsNaN :: (IsFloating (EltR t), BitOrMask (EltR t) ~ EltR b) => Exp t -> Exp b
mkIsNaN = mkPrimUnary $ PrimIsNaN floatingType

mkIsInfinite :: (IsFloating (EltR t), BitOrMask (EltR t) ~ EltR b) => Exp t -> Exp b
mkIsInfinite = mkPrimUnary $ PrimIsInfinite floatingType

-- Relational and equality operators

mkLt :: (IsScalar (EltR t), BitOrMask (EltR t) ~ EltR b) => Exp t -> Exp t -> Exp b
mkLt = mkPrimBinary $ PrimLt scalarType

mkGt :: (IsScalar (EltR t), BitOrMask (EltR t) ~ EltR b) => Exp t -> Exp t -> Exp b
mkGt = mkPrimBinary $ PrimGt scalarType

mkLtEq :: (IsScalar (EltR t), BitOrMask (EltR t) ~ EltR b) => Exp t -> Exp t -> Exp b
mkLtEq = mkPrimBinary $ PrimLtEq scalarType

mkGtEq :: (IsScalar (EltR t), BitOrMask (EltR t) ~ EltR b) => Exp t -> Exp t -> Exp b
mkGtEq = mkPrimBinary $ PrimGtEq scalarType

mkEq :: (IsScalar (EltR t), BitOrMask (EltR t) ~ EltR b) => Exp t -> Exp t -> Exp b
mkEq = mkPrimBinary $ PrimEq scalarType

mkNEq :: (IsScalar (EltR t), BitOrMask (EltR t) ~ EltR b) => Exp t -> Exp t -> Exp b
mkNEq = mkPrimBinary $ PrimNEq scalarType

mkMax :: IsScalar (EltR t) => Exp t -> Exp t -> Exp t
mkMax = mkPrimBinary $ PrimMax scalarType

mkMin :: IsScalar (EltR t) => Exp t -> Exp t -> Exp t
mkMin = mkPrimBinary $ PrimMin scalarType

-- Logical operators

mkLAnd :: IsBit (EltR t) => Exp t -> Exp t -> Exp t
mkLAnd = mkPrimBinary $ PrimLAnd bitType

mkLOr :: IsBit (EltR t) => Exp t -> Exp t -> Exp t
mkLOr = mkPrimBinary $ PrimLOr bitType

mkLNot :: IsBit (EltR t) => Exp t -> Exp t
mkLNot = mkPrimUnary $ PrimLNot bitType

-- Numeric conversions

mkFromIntegral :: (IsIntegral (EltR a), IsNum (EltR b)) => Exp a -> Exp b
mkFromIntegral = mkPrimUnary $ PrimFromIntegral integralType numType

mkToFloating :: (IsNum (EltR a), IsFloating (EltR b)) => Exp a -> Exp b
mkToFloating = mkPrimUnary $ PrimToFloating numType floatingType

mkToBool :: (IsSingleIntegral (EltR a), BitOrMask (EltR a) ~ Bit) => Exp a -> Exp Bool
mkToBool = mkPrimUnary $ PrimToBool (SingleIntegralType singleIntegralType) bitType

mkFromBool :: (IsSingleIntegral (EltR a), BitOrMask (EltR a) ~ Bit) => Exp Bool -> Exp a
mkFromBool = mkPrimUnary $ PrimFromBool bitType (SingleIntegralType singleIntegralType)

-- Other conversions

mkBitcast :: forall b a. (IsScalar (EltR a), IsScalar (EltR b), BitSizeEq (EltR a) (EltR b)) => Exp a -> Exp b
mkBitcast (Exp a) = mkExp $ Coerce (scalarType @(EltR a)) (scalarType @(EltR b)) a

mkCoerce :: Coerce (EltR a) (EltR b) => Exp a -> Exp b
mkCoerce (Exp a) = Exp $ mkCoerce' a

class Coerce a b where
  mkCoerce' :: SmartExp a -> SmartExp b

instance {-# OVERLAPS #-} (IsScalar a, IsScalar b, BitSizeEq a b) => Coerce a b where
  mkCoerce' = SmartExp . Coerce (scalarType @a) (scalarType @b)

instance (Coerce a1 b1, Coerce a2 b2) => Coerce (a1, a2) (b1, b2) where
  mkCoerce' a = SmartExp $ Pair (mkCoerce' $ SmartExp $ Prj PairIdxLeft a) (mkCoerce' $ SmartExp $ Prj PairIdxRight a)

instance Coerce a a where
  mkCoerce' = id

instance Coerce ((), a) a where
  mkCoerce' a = SmartExp $ Prj PairIdxRight a

instance Coerce a ((), a) where
  mkCoerce' = SmartExp . Pair (SmartExp Nil)

instance Coerce (a, ()) a where
  mkCoerce' a = SmartExp $ Prj PairIdxLeft a

instance Coerce a (a, ()) where
  mkCoerce' a = SmartExp (Pair a (SmartExp Nil))


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

unAcc :: Arrays a => Acc a -> SmartAcc (Sugar.ArraysR a)
unAcc (Acc a) = a

unAccFunction :: (Arrays a, Arrays b) => (Acc a -> Acc b) -> SmartAcc (Sugar.ArraysR a) -> SmartAcc (Sugar.ArraysR b)
unAccFunction f = unAcc . f . Acc

mkExp :: PreSmartExp SmartAcc SmartExp (EltR t) -> Exp t
mkExp = Exp . SmartExp

unExp :: Exp e -> SmartExp (EltR e)
unExp (Exp e) = e

unExpFunction :: (Exp a -> Exp b) -> SmartExp (EltR a) -> SmartExp (EltR b)
unExpFunction f = unExp . f . Exp

unExpBinaryFunction :: (Exp a -> Exp b -> Exp c) -> SmartExp (EltR a) -> SmartExp (EltR b) -> SmartExp (EltR c)
unExpBinaryFunction f a b = unExp $ f (Exp a) (Exp b)

mkPrimUnary :: PrimFun (EltR a -> EltR b) -> Exp a -> Exp b
mkPrimUnary prim (Exp a) = mkExp $ PrimApp prim a

mkPrimBinary :: PrimFun ((EltR a, EltR b) -> EltR c) -> Exp a -> Exp b -> Exp c
mkPrimBinary prim (Exp a) (Exp b) = mkExp $ PrimApp prim (SmartExp $ Pair a b)

unPair :: SmartExp (a, b) -> (SmartExp a, SmartExp b)
unPair e = (SmartExp $ Prj PairIdxLeft e, SmartExp $ Prj PairIdxRight e)

mkPairToTuple :: SmartAcc (a, b) -> SmartAcc (((), a), b)
mkPairToTuple e = SmartAcc Anil `pair` a `pair` b
  where
    a = SmartAcc $ Aprj PairIdxLeft e
    b = SmartAcc $ Aprj PairIdxRight e
    pair x y = SmartAcc $ Apair x y

class ApplyAcc a where
  type FromApplyAcc a
  applyAcc :: FromApplyAcc a -> a

instance ApplyAcc (SmartAcc a) where
  type FromApplyAcc (SmartAcc a) = PreSmartAcc SmartAcc SmartExp a
  applyAcc = SmartAcc

instance (Arrays a, ApplyAcc t) => ApplyAcc (Acc a -> t) where
  type FromApplyAcc (Acc a -> t) = SmartAcc (Sugar.ArraysR a) -> FromApplyAcc t
  applyAcc f a = applyAcc $ f (unAcc a)

instance (Elt a, ApplyAcc t) => ApplyAcc (Exp a -> t) where
  type FromApplyAcc (Exp a -> t) = SmartExp (EltR a) -> FromApplyAcc t
  applyAcc f a = applyAcc $ f (unExp a)

instance (Elt a, Elt b, ApplyAcc t) => ApplyAcc ((Exp a -> Exp b) -> t) where
  type FromApplyAcc ((Exp a -> Exp b) -> t) = (SmartExp (EltR a) -> SmartExp (EltR b)) -> FromApplyAcc t
  applyAcc f a = applyAcc $ f (unExpFunction a)

instance (Elt a, Elt b, Elt c, ApplyAcc t) => ApplyAcc ((Exp a -> Exp b -> Exp c) -> t) where
  type FromApplyAcc ((Exp a -> Exp b -> Exp c) -> t) = (SmartExp (EltR a) -> SmartExp (EltR b) -> SmartExp (EltR c)) -> FromApplyAcc t
  applyAcc f a = applyAcc $ f (unExpBinaryFunction a)

instance (Arrays a, Arrays b, ApplyAcc t) => ApplyAcc ((Acc a -> Acc b) -> t) where
  type FromApplyAcc ((Acc a -> Acc b) -> t) = (SmartAcc (Sugar.ArraysR a) -> SmartAcc (Sugar.ArraysR b)) -> FromApplyAcc t
  applyAcc f a = applyAcc $ f (unAccFunction a)


-- Debugging
-- ---------

formatDirection :: Format r (Direction -> r)
formatDirection = later $ \case
  LeftToRight -> singleton 'l'
  RightToLeft -> singleton 'r'

formatPreAccOp :: Format r (PreSmartAcc acc exp arrs -> r)
formatPreAccOp = later $ \case
  Atag _ i            -> bformat ("Atag " % int) i
  Use aR a            -> bformat ("Use " % string) (showArrayShort 5 (showsElt (arrayRtype aR)) aR a)
  Pipe{}              -> "Pipe"
  Acond{}             -> "Acond"
  Awhile{}            -> "Awhile"
  Apair{}             -> "Apair"
  Anil{}              -> "Anil"
  Aprj{}              -> "Aprj"
  Atrace{}            -> "Atrace"
  Unit{}              -> "Unit"
  Generate{}          -> "Generate"
  Reshape{}           -> "Reshape"
  Replicate{}         -> "Replicate"
  Slice{}             -> "Slice"
  Map{}               -> "Map"
  ZipWith{}           -> "ZipWith"
  Fold _ _ z _        -> bformat ("Fold" % maybed "1" (fconst mempty)) z
  FoldSeg _ _ _ z _ _ -> bformat ("Fold" % maybed "1" (fconst mempty) % "Seg") z
  Scan d _ _ z _      -> bformat ("Scan" % formatDirection % maybed "1" (fconst mempty)) d z
  Scan' d _ _ _ _     -> bformat ("Scan" % formatDirection % "\'") d
  Permute{}           -> "Permute"
  Backpermute{}       -> "Backpermute"
  Stencil{}           -> "Stencil"
  Stencil2{}          -> "Stencil2"
  Aforeign{}          -> "Aforeign"

formatPreExpOp :: Format r (PreSmartExp acc exp t -> r)
formatPreExpOp = later $ \case
  Tag _ i       -> bformat ("Tag " % int) i
  Const t c     -> bformat ("Const " % string) (showElt (TupRsingle t) c)
  Match{}       -> "Match"
  Undef{}       -> "Undef"
  Nil{}         -> "Nil"
  Pair{}        -> "Pair"
  Prj{}         -> "Prj"
  Extract{}     -> "Extract"
  Insert{}      -> "Insert"
  Shuffle{}     -> "Shuffle"
  Select{}      -> "Select"
  ToIndex{}     -> "ToIndex"
  FromIndex{}   -> "FromIndex"
  Case{}        -> "Case"
  Cond{}        -> "Cond"
  While{}       -> "While"
  PrimApp{}     -> "PrimApp"
  Index{}       -> "Index"
  LinearIndex{} -> "LinearIndex"
  Shape{}       -> "Shape"
  ShapeSize{}   -> "ShapeSize"
  Foreign{}     -> "Foreign"
  Coerce{}      -> "Coerce"

