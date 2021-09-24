{-# LANGUAGE ImplicitParams   #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
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

  -- ** Smart constructors for constants
  mkMinBound, mkMaxBound, mkPi,
  mkSin, mkCos, mkTan,
  mkAsin, mkAcos, mkAtan,
  mkSinh, mkCosh, mkTanh,
  mkAsinh, mkAcosh, mkAtanh,
  mkExpFloating, mkSqrt, mkLog,
  mkFPow, mkLogBase,
  mkTruncate, mkRound, mkFloor, mkCeiling,
  mkAtan2,

  -- ** Smart constructors for primitive functions
  mkAdd, mkSub, mkMul, mkNeg, mkAbs, mkSig, mkQuot, mkRem, mkQuotRem, mkIDiv, mkMod, mkDivMod,
  mkBAnd, mkBOr, mkBXor, mkBNot, mkBShiftL, mkBShiftR, mkBRotateL, mkBRotateR, mkPopCount, mkCountLeadingZeros, mkCountTrailingZeros,
  mkFDiv, mkRecip, mkLt, mkGt, mkLtEq, mkGtEq, mkEq, mkNEq, mkMax, mkMin,
  mkLAnd, mkLOr, mkLNot, mkIsNaN, mkIsInfinite,

  -- ** Smart constructors for type coercion functions
  mkFromIntegral, mkToFloating, mkBitcast, mkCoerce, Coerce(..),

  -- ** Auxiliary functions
  ($$), ($$$), ($$$$), ($$$$$),
  ApplyAcc(..),
  unAcc, unAccFunction, mkExp, unExp, unExpFunction, unExpBinaryFunction, unPair, mkPairToTuple,

  -- ** Miscellaneous
  formatPreAccOp,
  formatPreExpOp,

) where


import Data.Array.Accelerate.Annotations
import Data.Array.Accelerate.AST.Idx
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Representation.Array
import Data.Array.Accelerate.Representation.Elt
import Data.Array.Accelerate.Representation.Shape
import Data.Array.Accelerate.Representation.Slice
import Data.Array.Accelerate.Representation.Stencil                 hiding ( StencilR, stencilR )
import Data.Array.Accelerate.Representation.Tag
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Representation.Vec
import Data.Array.Accelerate.Sugar.Array                            ( Arrays )
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Sugar.Foreign
import Data.Array.Accelerate.Sugar.Shape                            ( (:.)(..) )
import Data.Array.Accelerate.Type
import qualified Data.Array.Accelerate.Representation.Stencil       as R
import qualified Data.Array.Accelerate.Sugar.Array                  as Sugar
import qualified Data.Array.Accelerate.Sugar.Shape                  as Sugar

import Data.Array.Accelerate.AST                                    ( Direction(..), Message(..)
                                                                    , PrimBool, PrimMaybe
                                                                    , PrimFun(..), primFunType
                                                                    , PrimConst(..), primConstType )
import Data.Primitive.Vec

import Data.Kind
import Data.Text.Lazy.Builder
import Formatting
import Lens.Micro                                                   ( (<&>) )

import GHC.TypeLits


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
-- TODO: Add annotations to most of the constructors
data PreSmartAcc acc exp as where
    -- Needed for conversion to de Bruijn form
  Atag          :: ArraysR as
                -> Level                        -- environment size at defining occurrence
                -> PreSmartAcc acc exp as

  Pipe          :: Ann
                -> ArraysR as
                -> ArraysR bs
                -> ArraysR cs
                -> (SmartAcc as -> acc bs)
                -> (SmartAcc bs -> acc cs)
                -> acc as
                -> PreSmartAcc acc exp cs

  Aforeign      :: Foreign asm
                => Ann
                -> ArraysR bs
                -> asm (as -> bs)
                -> (SmartAcc as -> SmartAcc bs)
                -> acc as
                -> PreSmartAcc acc exp bs

  Acond         :: Ann
                -> exp PrimBool
                -> acc as
                -> acc as
                -> PreSmartAcc acc exp as

  Awhile        :: Ann
                -> ArraysR arrs
                -> (SmartAcc arrs -> acc (Scalar PrimBool))
                -> (SmartAcc arrs -> acc arrs)
                -> acc arrs
                -> PreSmartAcc acc exp arrs

  Anil          :: Ann
                -> PreSmartAcc acc exp ()

  Apair         :: Ann
                -> acc arrs1
                -> acc arrs2
                -> PreSmartAcc acc exp (arrs1, arrs2)

  Aprj          :: Ann
                -> PairIdx (arrs1, arrs2) arrs
                -> acc (arrs1, arrs2)
                -> PreSmartAcc acc exp arrs

  Atrace        :: Ann
                -> Message arrs1
                -> acc arrs1
                -> acc arrs2
                -> PreSmartAcc acc exp arrs2

  Use           :: Ann
                -> ArrayR (Array sh e)
                -> Array sh e
                -> PreSmartAcc acc exp (Array sh e)

  Unit          :: Ann
                -> TypeR e
                -> exp e
                -> PreSmartAcc acc exp (Scalar e)

  Generate      :: Ann
                -> ArrayR (Array sh e)
                -> exp sh
                -> (SmartExp sh -> exp e)
                -> PreSmartAcc acc exp (Array sh e)

  Reshape       :: Ann
                -> ShapeR sh
                -> exp sh
                -> acc (Array sh' e)
                -> PreSmartAcc acc exp (Array sh e)

  Replicate     :: Ann
                -> SliceIndex slix sl co sh
                -> exp slix
                -> acc                 (Array sl e)
                -> PreSmartAcc acc exp (Array sh e)

  Slice         :: Ann
                -> SliceIndex slix sl co sh
                -> acc                 (Array sh e)
                -> exp slix
                -> PreSmartAcc acc exp (Array sl e)

  Map           :: Ann
                -> TypeR e
                -> TypeR e'
                -> (SmartExp e -> exp e')
                -> acc (Array sh e)
                -> PreSmartAcc acc exp (Array sh e')

  ZipWith       :: Ann
                -> TypeR e1
                -> TypeR e2
                -> TypeR e3
                -> (SmartExp e1 -> SmartExp e2 -> exp e3)
                -> acc (Array sh e1)
                -> acc (Array sh e2)
                -> PreSmartAcc acc exp (Array sh e3)

  Fold          :: Ann
                -> TypeR e
                -> (SmartExp e -> SmartExp e -> exp e)
                -> Maybe (exp e)
                -> acc (Array (sh, Int) e)
                -> PreSmartAcc acc exp (Array sh e)

  FoldSeg       :: Ann
                -> IntegralType i
                -> TypeR e
                -> (SmartExp e -> SmartExp e -> exp e)
                -> Maybe (exp e)
                -> acc (Array (sh, Int) e)
                -> acc (Segments i)
                -> PreSmartAcc acc exp (Array (sh, Int) e)

  Scan          :: Ann
                -> Direction
                -> TypeR e
                -> (SmartExp e -> SmartExp e -> exp e)
                -> Maybe (exp e)
                -> acc (Array (sh, Int) e)
                -> PreSmartAcc acc exp (Array (sh, Int) e)

  Scan'         :: Ann
                -> Direction
                -> TypeR e
                -> (SmartExp e -> SmartExp e -> exp e)
                -> exp e
                -> acc (Array (sh, Int) e)
                -> PreSmartAcc acc exp (Array (sh, Int) e, Array sh e)

  Permute       :: Ann
                -> ArrayR (Array sh e)
                -> (SmartExp e -> SmartExp e -> exp e)
                -> acc (Array sh' e)
                -> (SmartExp sh -> exp (PrimMaybe sh'))
                -> acc (Array sh e)
                -> PreSmartAcc acc exp (Array sh' e)

  Backpermute   :: Ann
                -> ShapeR sh'
                -> exp sh'
                -> (SmartExp sh' -> exp sh)
                -> acc (Array sh e)
                -> PreSmartAcc acc exp (Array sh' e)

  Stencil       :: Ann
                -> R.StencilR sh a stencil
                -> TypeR b
                -> (SmartExp stencil -> exp b)
                -> PreBoundary acc exp (Array sh a)
                -> acc (Array sh a)
                -> PreSmartAcc acc exp (Array sh b)

  Stencil2      :: Ann
                -> R.StencilR sh a stencil1
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
  Const         :: Ann
                -> ScalarType t
                -> t
                -> PreSmartExp acc exp t

  Nil           :: Ann
                -> PreSmartExp acc exp ()

  Pair          :: Ann
                -> exp t1
                -> exp t2
                -> PreSmartExp acc exp (t1, t2)

  Prj           :: Ann
                -> PairIdx (t1, t2) t
                -> exp (t1, t2)
                -> PreSmartExp acc exp t

  VecPack       :: KnownNat n
                => Ann
                -> VecR n s tup
                -> exp tup
                -> PreSmartExp acc exp (Vec n s)

  VecUnpack     :: KnownNat n
                => Ann
                -> VecR n s tup
                -> exp (Vec n s)
                -> PreSmartExp acc exp tup

  ToIndex       :: Ann
                -> ShapeR sh
                -> exp sh
                -> exp sh
                -> PreSmartExp acc exp Int

  FromIndex     :: Ann
                -> ShapeR sh
                -> exp sh
                -> exp Int
                -> PreSmartExp acc exp sh

  Case          :: Ann
                -> exp a
                -> [(TagR a, exp b)]
                -> PreSmartExp acc exp b

  Cond          :: Ann
                -> exp PrimBool
                -> exp t
                -> exp t
                -> PreSmartExp acc exp t

  While         :: Ann
                -> TypeR t
                -> (SmartExp t -> exp PrimBool)
                -> (SmartExp t -> exp t)
                -> exp t
                -> PreSmartExp acc exp t

  PrimConst     :: Ann
                -> PrimConst t
                -> PreSmartExp acc exp t

  PrimApp       :: Ann
                -> PrimFun (a -> r)
                -> exp a
                -> PreSmartExp acc exp r

  Index         :: Ann
                -> TypeR t
                -> acc (Array sh t)
                -> exp sh
                -> PreSmartExp acc exp t

  LinearIndex   :: Ann
                -> TypeR t
                -> acc (Array sh t)
                -> exp Int
                -> PreSmartExp acc exp t

  Shape         :: Ann
                -> ShapeR sh
                -> acc (Array sh e)
                -> PreSmartExp acc exp sh

  ShapeSize     :: Ann
                -> ShapeR sh
                -> exp sh
                -> PreSmartExp acc exp Int

  Foreign       :: Foreign asm
                => Ann
                -> TypeR y
                -> asm (x -> y)
                -> (SmartExp x -> SmartExp y) -- RCE: Using SmartExp instead of exp to aid in sharing recovery.
                -> exp x
                -> PreSmartExp acc exp y

  Undef         :: Ann
                -> ScalarType t
                -> PreSmartExp acc exp t

  Coerce        :: BitSizeEq a b
                => Ann
                -> ScalarType a
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

  stencilR   :: HasCallStack => R.StencilR (EltR sh) (EltR e) (StencilR sh stencil)
  stencilPrj :: HasCallStack => SmartExp (StencilR sh stencil) -> stencil

-- DIM1
instance Elt e => Stencil Sugar.DIM1 e (Exp e, Exp e, Exp e) where
  type StencilR Sugar.DIM1 (Exp e, Exp e, Exp e)
    = EltR (e, e, e)
  stencilR = sourceMap $ StencilRunit3 @(EltR e) $ eltR @e
  stencilPrj s = sourceMap (Exp $ prj2 s,
                            Exp $ prj1 s,
                            Exp $ prj0 s)

instance Elt e => Stencil Sugar.DIM1 e (Exp e, Exp e, Exp e, Exp e, Exp e) where
  type StencilR Sugar.DIM1 (Exp e, Exp e, Exp e, Exp e, Exp e)
    = EltR (e, e, e, e, e)
  stencilR = sourceMap $ StencilRunit5 $ eltR @e
  stencilPrj s = sourceMap (Exp $ prj4 s,
                            Exp $ prj3 s,
                            Exp $ prj2 s,
                            Exp $ prj1 s,
                            Exp $ prj0 s)

instance Elt e => Stencil Sugar.DIM1 e (Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e) where
  type StencilR Sugar.DIM1 (Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e)
    = EltR (e, e, e, e, e, e, e)
  stencilR = sourceMap $ StencilRunit7 $ eltR @e
  stencilPrj s = sourceMap (Exp $ prj6 s,
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
  stencilR = sourceMap $ StencilRunit9 $ eltR @e
  stencilPrj s = sourceMap (Exp $ prj8 s,
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
  stencilR = sourceMap $ StencilRtup3 (stencilR @(sh:.Int) @a @row2) (stencilR @(sh:.Int) @a @row1) (stencilR @(sh:.Int) @a @row0)
  stencilPrj s = sourceMap (stencilPrj @(sh:.Int) @a $ prj2 s,
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
  stencilR = sourceMap $ StencilRtup5 (stencilR @(sh:.Int) @a @row4) (stencilR @(sh:.Int) @a @row3)
                                      (stencilR @(sh:.Int) @a @row2) (stencilR @(sh:.Int) @a @row1) (stencilR @(sh:.Int) @a @row0)
  stencilPrj s = sourceMap (stencilPrj @(sh:.Int) @a $ prj4 s,
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
  stencilR = sourceMap $ StencilRtup7 (stencilR @(sh:.Int) @a @row6)
                                      (stencilR @(sh:.Int) @a @row5) (stencilR @(sh:.Int) @a @row4) (stencilR @(sh:.Int) @a @row3)
                                      (stencilR @(sh:.Int) @a @row2) (stencilR @(sh:.Int) @a @row1) (stencilR @(sh:.Int) @a @row0)
  stencilPrj s = sourceMap (stencilPrj @(sh:.Int) @a $ prj6 s,
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
  stencilR = sourceMap $ StencilRtup9
                         (stencilR @(sh:.Int) @a @row8) (stencilR @(sh:.Int) @a @row7) (stencilR @(sh:.Int) @a @row6)
                         (stencilR @(sh:.Int) @a @row5) (stencilR @(sh:.Int) @a @row4) (stencilR @(sh:.Int) @a @row3)
                         (stencilR @(sh:.Int) @a @row2) (stencilR @(sh:.Int) @a @row1) (stencilR @(sh:.Int) @a @row0)
  stencilPrj s = sourceMap (stencilPrj @(sh:.Int) @a $ prj8 s,
                            stencilPrj @(sh:.Int) @a $ prj7 s,
                            stencilPrj @(sh:.Int) @a $ prj6 s,
                            stencilPrj @(sh:.Int) @a $ prj5 s,
                            stencilPrj @(sh:.Int) @a $ prj4 s,
                            stencilPrj @(sh:.Int) @a $ prj3 s,
                            stencilPrj @(sh:.Int) @a $ prj2 s,
                            stencilPrj @(sh:.Int) @a $ prj1 s,
                            stencilPrj @(sh:.Int) @a $ prj0 s)

prjTail :: SourceMapped => SmartExp (t, a) -> SmartExp t
prjTail = SmartExp . Prj mkAnn PairIdxLeft

prj0 :: SourceMapped => SmartExp (t, a) -> SmartExp a
prj0 = SmartExp . Prj mkAnn PairIdxRight

prj1 :: SourceMapped => SmartExp ((t, a), s0) -> SmartExp a
prj1 = prj0 . prjTail

prj2 :: SourceMapped => SmartExp (((t, a), s1), s0) -> SmartExp a
prj2 = prj1 . prjTail

prj3 :: SourceMapped => SmartExp ((((t, a), s2), s1), s0) -> SmartExp a
prj3 = prj2 . prjTail

prj4 :: SourceMapped => SmartExp (((((t, a), s3), s2), s1), s0) -> SmartExp a
prj4 = prj3 . prjTail

prj5 :: SourceMapped => SmartExp ((((((t, a), s4), s3), s2), s1), s0) -> SmartExp a
prj5 = prj4 . prjTail

prj6 :: SourceMapped => SmartExp (((((((t, a), s5), s4), s3), s2), s1), s0) -> SmartExp a
prj6 = prj5 . prjTail

prj7 :: SourceMapped => SmartExp ((((((((t, a), s6), s5), s4), s3), s2), s1), s0) -> SmartExp a
prj7 = prj6 . prjTail

prj8 :: SourceMapped => SmartExp (((((((((t, a), s7), s6), s5), s4), s3), s2), s1), s0) -> SmartExp a
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

-- TODO: Reformat
instance HasArraysR acc => HasArraysR (PreSmartAcc acc exp) where
  arraysR = \case
    Atag repr _               -> repr
    Pipe _ _ _ repr  _ _ _    -> repr
    Aforeign _ repr _ _ _     -> repr
    Acond _ _ a _             -> arraysR a
    Awhile _ _ _ _ a          -> arraysR a
    Anil _                    -> TupRunit
    Apair _ a1 a2             -> arraysR a1 `TupRpair` arraysR a2
    Aprj _ idx a | TupRpair t1 t2 <- arraysR a
                              -> case idx of
                                   PairIdxLeft  -> t1
                                   PairIdxRight -> t2
    Aprj _ _ _                -> error "Ejector seat? You're joking!"
    Atrace _ _ _ a            -> arraysR a
    Use _ repr _              -> TupRsingle repr
    Unit _ tp _               -> TupRsingle $ ArrayR ShapeRz $ tp
    Generate _ repr _ _       -> TupRsingle repr
    Reshape _ shr _ a         -> let ArrayR _ tp = arrayR a
                                 in  TupRsingle $ ArrayR shr tp
    Replicate _ si _ a        -> let ArrayR _ tp = arrayR a
                                 in  TupRsingle $ ArrayR (sliceDomainR si) tp
    Slice _ si a _            -> let ArrayR _ tp = arrayR a
                                 in  TupRsingle $ ArrayR (sliceShapeR si) tp
    Map _ _ tp _ a            -> let ArrayR shr _ = arrayR a
                                 in  TupRsingle $ ArrayR shr tp
    ZipWith _ _ _ tp _ a _    -> let ArrayR shr _ = arrayR a
                                 in  TupRsingle $ ArrayR shr tp
    Fold _ _ _ _ a            -> let ArrayR (ShapeRsnoc shr) tp = arrayR a
                                 in  TupRsingle (ArrayR shr tp)
    FoldSeg _ _ _ _ _ a _     -> arraysR a
    Scan _ _ _ _ _ a          -> arraysR a
    Scan' _ _ _ _ _ a         -> let repr@(ArrayR (ShapeRsnoc shr) tp) = arrayR a
                                 in  TupRsingle repr `TupRpair` TupRsingle (ArrayR shr tp)
    Permute _ _ _ a _ _       -> arraysR a
    Backpermute _ shr _ _ a   -> let ArrayR _ tp = arrayR a
                                 in  TupRsingle (ArrayR shr tp)
    Stencil _ s tp _ _ _      -> TupRsingle $ ArrayR (stencilShapeR s) tp
    Stencil2 _ s _ tp _ _ _ _ _ -> TupRsingle $ ArrayR (stencilShapeR s) tp


class HasTypeR f where
  typeR :: HasCallStack => f t -> TypeR t

instance HasTypeR SmartExp where
  typeR (SmartExp e) = typeR e

instance HasTypeR exp => HasTypeR (PreSmartExp acc exp) where
  typeR = \case
    Tag tp _                        -> tp
    Match _ e                       -> typeR e
    Const _ tp _                    -> TupRsingle tp
    Nil _                           -> TupRunit
    Pair _ e1 e2                    -> typeR e1 `TupRpair` typeR e2
    Prj _ idx e
      | TupRpair t1 t2 <- typeR e   -> case idx of
                                         PairIdxLeft  -> t1
                                         PairIdxRight -> t2
    Prj _ _ _                       -> error "I never joke about my work"
    VecPack   _ vecR _              -> TupRsingle $ VectorScalarType $ vecRvector vecR
    VecUnpack _ vecR _              -> vecRtuple vecR
    ToIndex _ _ _ _                 -> TupRsingle scalarTypeInt
    FromIndex _ shr _ _             -> shapeType shr
    Case _ _ ((_,c):_)              -> typeR c
    Case{}                          -> internalError "encountered empty case"
    Cond _ _ e _                    -> typeR e
    While _ t _ _ _                 -> t
    PrimConst _ c                   -> TupRsingle $ SingleScalarType $ primConstType c
    PrimApp _ f _                   -> snd $ primFunType f
    Index _ tp _ _                  -> tp
    LinearIndex _ tp _ _            -> tp
    Shape _ shr _                   -> shapeType shr
    ShapeSize _ _ _                 -> TupRsingle scalarTypeInt
    Foreign _ tp _ _ _              -> tp
    Undef _ tp                      -> TupRsingle tp
    Coerce _ _ tp _                 -> TupRsingle tp


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
constant = sourceMap $ Exp . go (eltR @e) . fromElt
  where
    go :: SourceMapped => TypeR t -> t -> SmartExp t
    go TupRunit         ()       = SmartExp $ (Nil mkAnn)
    go (TupRsingle tp)  c        = SmartExp $ Const mkAnn tp c
    go (TupRpair t1 t2) (c1, c2) = SmartExp $ Pair mkAnn (go t1 c1) (go t2 c2)

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
undef :: forall e. (HasCallStack, Elt e) => Exp e
undef = sourceMap $ Exp . go $ eltR @e
  where
    go :: SourceMapped => TypeR t -> SmartExp t
    go TupRunit         = SmartExp $ Nil mkAnn
    go (TupRsingle t)   = SmartExp $ Undef mkAnn t
    go (TupRpair t1 t2) = SmartExp $ Pair mkAnn (go t1) (go t2)

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
indexHead :: (HasCallStack, Elt sh, Elt a) => Exp (sh :. a) -> Exp a
indexHead (Exp x) = sourceMap $ mkExp $ Prj mkAnn PairIdxRight x

-- | Get all but the innermost element of a shape
--
indexTail :: (HasCallStack, Elt sh, Elt a) => Exp (sh :. a) -> Exp sh
indexTail (Exp x) = sourceMap $ mkExp $ Prj mkAnn PairIdxLeft x


-- Smart constructor for constants
--

mkMinBound :: (SourceMapped, Elt t, IsBounded (EltR t)) => Exp t
mkMinBound = mkExp $ PrimConst mkAnn (PrimMinBound boundedType)

mkMaxBound :: (SourceMapped, Elt t, IsBounded (EltR t)) => Exp t
mkMaxBound = mkExp $ PrimConst mkAnn (PrimMaxBound boundedType)

mkPi :: (SourceMapped, Elt r, IsFloating (EltR r)) => Exp r
mkPi = mkExp $ PrimConst mkAnn (PrimPi floatingType)


-- Smart constructors for primitive applications
--

-- Operators from Floating

mkSin :: (SourceMapped, Elt t, IsFloating (EltR t)) => Exp t -> Exp t
mkSin = mkPrimUnary $ PrimSin floatingType

mkCos :: (SourceMapped, Elt t, IsFloating (EltR t)) => Exp t -> Exp t
mkCos = mkPrimUnary $ PrimCos floatingType

mkTan :: (SourceMapped, Elt t, IsFloating (EltR t)) => Exp t -> Exp t
mkTan = mkPrimUnary $ PrimTan floatingType

mkAsin :: (SourceMapped, Elt t, IsFloating (EltR t)) => Exp t -> Exp t
mkAsin = mkPrimUnary $ PrimAsin floatingType

mkAcos :: (SourceMapped, Elt t, IsFloating (EltR t)) => Exp t -> Exp t
mkAcos = mkPrimUnary $ PrimAcos floatingType

mkAtan :: (SourceMapped, Elt t, IsFloating (EltR t)) => Exp t -> Exp t
mkAtan = mkPrimUnary $ PrimAtan floatingType

mkSinh :: (SourceMapped, Elt t, IsFloating (EltR t)) => Exp t -> Exp t
mkSinh = mkPrimUnary $ PrimSinh floatingType

mkCosh :: (SourceMapped, Elt t, IsFloating (EltR t)) => Exp t -> Exp t
mkCosh = mkPrimUnary $ PrimCosh floatingType

mkTanh :: (SourceMapped, Elt t, IsFloating (EltR t)) => Exp t -> Exp t
mkTanh = mkPrimUnary $ PrimTanh floatingType

mkAsinh :: (SourceMapped, Elt t, IsFloating (EltR t)) => Exp t -> Exp t
mkAsinh = mkPrimUnary $ PrimAsinh floatingType

mkAcosh :: (SourceMapped, Elt t, IsFloating (EltR t)) => Exp t -> Exp t
mkAcosh = mkPrimUnary $ PrimAcosh floatingType

mkAtanh :: (SourceMapped, Elt t, IsFloating (EltR t)) => Exp t -> Exp t
mkAtanh = mkPrimUnary $ PrimAtanh floatingType

mkExpFloating :: (SourceMapped, Elt t, IsFloating (EltR t)) => Exp t -> Exp t
mkExpFloating = mkPrimUnary $ PrimExpFloating floatingType

mkSqrt :: (SourceMapped, Elt t, IsFloating (EltR t)) => Exp t -> Exp t
mkSqrt = mkPrimUnary $ PrimSqrt floatingType

mkLog :: (SourceMapped, Elt t, IsFloating (EltR t)) => Exp t -> Exp t
mkLog = mkPrimUnary $ PrimLog floatingType

mkFPow :: (SourceMapped, Elt t, IsFloating (EltR t)) => Exp t -> Exp t -> Exp t
mkFPow = mkPrimBinary $ PrimFPow floatingType

mkLogBase :: (SourceMapped, Elt t, IsFloating (EltR t)) => Exp t -> Exp t -> Exp t
mkLogBase = mkPrimBinary $ PrimLogBase floatingType

-- Operators from Num

mkAdd :: (SourceMapped, Elt t, IsNum (EltR t)) => Exp t -> Exp t -> Exp t
mkAdd = mkPrimBinary $ PrimAdd numType

mkSub :: (SourceMapped, Elt t, IsNum (EltR t)) => Exp t -> Exp t -> Exp t
mkSub = mkPrimBinary $ PrimSub numType

mkMul :: (SourceMapped, Elt t, IsNum (EltR t)) => Exp t -> Exp t -> Exp t
mkMul = mkPrimBinary $ PrimMul numType

mkNeg :: (SourceMapped, Elt t, IsNum (EltR t)) => Exp t -> Exp t
mkNeg = mkPrimUnary $ PrimNeg numType

mkAbs :: (SourceMapped, Elt t, IsNum (EltR t)) => Exp t -> Exp t
mkAbs = mkPrimUnary $ PrimAbs numType

mkSig :: (SourceMapped, Elt t, IsNum (EltR t)) => Exp t -> Exp t
mkSig = mkPrimUnary $ PrimSig numType

-- Operators from Integral

mkQuot :: (SourceMapped, Elt t, IsIntegral (EltR t)) => Exp t -> Exp t -> Exp t
mkQuot = mkPrimBinary $ PrimQuot integralType

mkRem :: (SourceMapped, Elt t, IsIntegral (EltR t)) => Exp t -> Exp t -> Exp t
mkRem = mkPrimBinary $ PrimRem integralType

mkQuotRem :: (SourceMapped, Elt t, IsIntegral (EltR t)) => Exp t -> Exp t -> (Exp t, Exp t)
mkQuotRem (Exp x) (Exp y) =
  let pair = SmartExp $ PrimApp mkAnn (PrimQuotRem integralType) (SmartExp (Pair mkAnn x y))
  in  (mkExp $ Prj mkAnn PairIdxLeft pair, mkExp $ Prj mkAnn PairIdxRight pair)

mkIDiv :: (SourceMapped, Elt t, IsIntegral (EltR t)) => Exp t -> Exp t -> Exp t
mkIDiv = mkPrimBinary $ PrimIDiv integralType

mkMod :: (SourceMapped, Elt t, IsIntegral (EltR t)) => Exp t -> Exp t -> Exp t
mkMod = mkPrimBinary $ PrimMod integralType

mkDivMod :: (SourceMapped, Elt t, IsIntegral (EltR t)) => Exp t -> Exp t -> (Exp t, Exp t)
mkDivMod (Exp x) (Exp y) =
  let pair = SmartExp $ PrimApp mkAnn (PrimDivMod integralType) (SmartExp (Pair mkAnn x y))
  in  (mkExp $ Prj mkAnn PairIdxLeft pair, mkExp $ Prj mkAnn PairIdxRight pair)

-- Operators from Bits and FiniteBits

mkBAnd :: (SourceMapped, Elt t, IsIntegral (EltR t)) => Exp t -> Exp t -> Exp t
mkBAnd = mkPrimBinary $ PrimBAnd integralType

mkBOr :: (SourceMapped, Elt t, IsIntegral (EltR t)) => Exp t -> Exp t -> Exp t
mkBOr = mkPrimBinary $ PrimBOr integralType

mkBXor :: (SourceMapped, Elt t, IsIntegral (EltR t)) => Exp t -> Exp t -> Exp t
mkBXor = mkPrimBinary $ PrimBXor integralType

mkBNot :: (SourceMapped, Elt t, IsIntegral (EltR t)) => Exp t -> Exp t
mkBNot = mkPrimUnary $ PrimBNot integralType

mkBShiftL :: (SourceMapped, Elt t, IsIntegral (EltR t)) => Exp t -> Exp Int -> Exp t
mkBShiftL = mkPrimBinary $ PrimBShiftL integralType

mkBShiftR :: (SourceMapped, Elt t, IsIntegral (EltR t)) => Exp t -> Exp Int -> Exp t
mkBShiftR = mkPrimBinary $ PrimBShiftR integralType

mkBRotateL :: (SourceMapped, Elt t, IsIntegral (EltR t)) => Exp t -> Exp Int -> Exp t
mkBRotateL = mkPrimBinary $ PrimBRotateL integralType

mkBRotateR :: (SourceMapped, Elt t, IsIntegral (EltR t)) => Exp t -> Exp Int -> Exp t
mkBRotateR = mkPrimBinary $ PrimBRotateR integralType

mkPopCount :: (SourceMapped, Elt t, IsIntegral (EltR t)) => Exp t -> Exp Int
mkPopCount = mkPrimUnary $ PrimPopCount integralType

mkCountLeadingZeros :: (SourceMapped, Elt t, IsIntegral (EltR t)) => Exp t -> Exp Int
mkCountLeadingZeros = mkPrimUnary $ PrimCountLeadingZeros integralType

mkCountTrailingZeros :: (SourceMapped, Elt t, IsIntegral (EltR t)) => Exp t -> Exp Int
mkCountTrailingZeros = mkPrimUnary $ PrimCountTrailingZeros integralType


-- Operators from Fractional

mkFDiv :: (SourceMapped, Elt t, IsFloating (EltR t)) => Exp t -> Exp t -> Exp t
mkFDiv = mkPrimBinary $ PrimFDiv floatingType

mkRecip :: (SourceMapped, Elt t, IsFloating (EltR t)) => Exp t -> Exp t
mkRecip = mkPrimUnary $ PrimRecip floatingType

-- Operators from RealFrac

mkTruncate :: (SourceMapped, Elt a, Elt b, IsFloating (EltR a), IsIntegral (EltR b)) => Exp a -> Exp b
mkTruncate = mkPrimUnary $ PrimTruncate floatingType integralType

mkRound :: (SourceMapped, Elt a, Elt b, IsFloating (EltR a), IsIntegral (EltR b)) => Exp a -> Exp b
mkRound = mkPrimUnary $ PrimRound floatingType integralType

mkFloor :: (SourceMapped, Elt a, Elt b, IsFloating (EltR a), IsIntegral (EltR b)) => Exp a -> Exp b
mkFloor = mkPrimUnary $ PrimFloor floatingType integralType

mkCeiling :: (SourceMapped, Elt a, Elt b, IsFloating (EltR a), IsIntegral (EltR b)) => Exp a -> Exp b
mkCeiling = mkPrimUnary $ PrimCeiling floatingType integralType

-- Operators from RealFloat

mkAtan2 :: (SourceMapped, Elt t, IsFloating (EltR t)) => Exp t -> Exp t -> Exp t
mkAtan2 = mkPrimBinary $ PrimAtan2 floatingType

mkIsNaN :: (SourceMapped, Elt t, IsFloating (EltR t)) => Exp t -> Exp Bool
mkIsNaN = mkPrimUnaryBool $ PrimIsNaN floatingType

mkIsInfinite :: (SourceMapped, Elt t, IsFloating (EltR t)) => Exp t -> Exp Bool
mkIsInfinite = mkPrimUnaryBool $ PrimIsInfinite floatingType

-- FIXME: add missing operations from Floating, RealFrac & RealFloat

-- Relational and equality operators

mkLt :: (SourceMapped, Elt t, IsSingle (EltR t)) => Exp t -> Exp t -> Exp Bool
mkLt = mkPrimBinaryBool $ PrimLt singleType

mkGt :: (SourceMapped, Elt t, IsSingle (EltR t)) => Exp t -> Exp t -> Exp Bool
mkGt = mkPrimBinaryBool $ PrimGt singleType

mkLtEq :: (SourceMapped, Elt t, IsSingle (EltR t)) => Exp t -> Exp t -> Exp Bool
mkLtEq = mkPrimBinaryBool $ PrimLtEq singleType

mkGtEq :: (SourceMapped, Elt t, IsSingle (EltR t)) => Exp t -> Exp t -> Exp Bool
mkGtEq = mkPrimBinaryBool $ PrimGtEq singleType

mkEq :: (SourceMapped, Elt t, IsSingle (EltR t)) => Exp t -> Exp t -> Exp Bool
mkEq = mkPrimBinaryBool $ PrimEq singleType

mkNEq :: (SourceMapped, Elt t, IsSingle (EltR t)) => Exp t -> Exp t -> Exp Bool
mkNEq = mkPrimBinaryBool $ PrimNEq singleType

mkMax :: (SourceMapped, Elt t, IsSingle (EltR t)) => Exp t -> Exp t -> Exp t
mkMax = mkPrimBinary $ PrimMax singleType

mkMin :: (SourceMapped, Elt t, IsSingle (EltR t)) => Exp t -> Exp t -> Exp t
mkMin = mkPrimBinary $ PrimMin singleType

-- Logical operators

mkLAnd :: SourceMapped  => Exp Bool -> Exp Bool -> Exp Bool
mkLAnd (Exp a) (Exp b) = mkExp $ Pair mkAnn (SmartExp (PrimApp mkAnn PrimLAnd (SmartExp $ Pair mkAnn x y))) (SmartExp (Nil mkAnn))
  where
    x = SmartExp $ Prj mkAnn PairIdxLeft a
    y = SmartExp $ Prj mkAnn PairIdxLeft b

mkLOr :: SourceMapped => Exp Bool -> Exp Bool -> Exp Bool
mkLOr (Exp a) (Exp b) = mkExp $ Pair mkAnn (SmartExp (PrimApp mkAnn PrimLOr (SmartExp $ Pair mkAnn x y))) (SmartExp (Nil mkAnn))
  where
    x = SmartExp $ Prj mkAnn PairIdxLeft a
    y = SmartExp $ Prj mkAnn PairIdxLeft b

mkLNot :: SourceMapped => Exp Bool -> Exp Bool
mkLNot (Exp a) = mkExp $ Pair mkAnn (SmartExp (PrimApp mkAnn PrimLNot x)) (SmartExp (Nil mkAnn))
  where
    x = SmartExp $ Prj mkAnn PairIdxLeft a

-- Numeric conversions

mkFromIntegral :: (SourceMapped, Elt a, Elt b, IsIntegral (EltR a), IsNum (EltR b)) => Exp a -> Exp b
mkFromIntegral = mkPrimUnary $ PrimFromIntegral integralType numType

mkToFloating :: (SourceMapped, Elt a, Elt b, IsNum (EltR a), IsFloating (EltR b)) => Exp a -> Exp b
mkToFloating = mkPrimUnary $ PrimToFloating numType floatingType

-- Other conversions

-- NOTE: Restricted to scalar types with a type-level BitSizeEq constraint to
-- make this version "safe"
mkBitcast :: forall b a. (SourceMapped, Elt a, Elt b, IsScalar (EltR a), IsScalar (EltR b), BitSizeEq (EltR a) (EltR b)) => Exp a -> Exp b
mkBitcast (Exp a) = mkExp $ Coerce mkAnn (scalarType @(EltR a)) (scalarType @(EltR b)) a

mkCoerce :: (SourceMapped, Coerce (EltR a) (EltR b)) => Exp a -> Exp b
mkCoerce (Exp a) = sourceMap $ Exp $ mkCoerce' a

class Coerce a b where
  mkCoerce' :: SourceMapped => SmartExp a -> SmartExp b

instance {-# OVERLAPS #-} (IsScalar a, IsScalar b, BitSizeEq a b) => Coerce a b where
  mkCoerce' = SmartExp . Coerce mkAnn (scalarType @a) (scalarType @b)

instance (Coerce a1 b1, Coerce a2 b2) => Coerce (a1, a2) (b1, b2) where
  mkCoerce' a = SmartExp $ Pair mkAnn (mkCoerce' $ SmartExp $ Prj mkAnn PairIdxLeft a) (mkCoerce' $ SmartExp $ Prj mkAnn PairIdxRight a)

instance Coerce a a where
  mkCoerce' = id

instance Coerce ((), a) a where
  mkCoerce' a = SmartExp $ Prj mkAnn PairIdxRight a

instance Coerce a ((), a) where
  mkCoerce' = SmartExp . Pair mkAnn (SmartExp (Nil mkAnn))

instance Coerce (a, ()) a where
  mkCoerce' a = SmartExp $ Prj mkAnn PairIdxLeft a

instance Coerce a (a, ()) where
  mkCoerce' a = SmartExp (Pair mkAnn a (SmartExp (Nil mkAnn)))


-- Annotations
-- -----------

instance FieldAnn (Acc a) where
  _ann k (Acc (SmartAcc pacc)) = Acc . SmartAcc <$> case pacc of
    (Pipe ann reprA reprB reprC afun1 afun2 acc)                -> k (Just ann) <&> \(Just ann') -> Pipe ann' reprA reprB reprC afun1 afun2 acc
    (Aforeign ann repr ff afun acc)                             -> k (Just ann) <&> \(Just ann') -> Aforeign ann' repr ff afun acc
    (Acond ann b acc1 acc2)                                     -> k (Just ann) <&> \(Just ann') -> Acond ann' b acc1 acc2
    (Awhile ann reprA pred' iter' init')                        -> k (Just ann) <&> \(Just ann') -> Awhile ann' reprA pred' iter' init'
    (Anil ann)                                                  -> k (Just ann) <&> \(Just ann') -> Anil ann'
    (Apair ann acc1 acc2)                                       -> k (Just ann) <&> \(Just ann') -> Apair ann' acc1 acc2
    (Aprj ann ix a)                                             -> k (Just ann) <&> \(Just ann') -> Aprj ann' ix a
    (Use ann repr array)                                        -> k (Just ann) <&> \(Just ann') -> Use ann' repr array
    (Unit ann tp e)                                             -> k (Just ann) <&> \(Just ann') -> Unit ann' tp e
    (Generate ann repr sh f)                                    -> k (Just ann) <&> \(Just ann') -> Generate ann' repr sh f
    (Reshape ann shr e acc)                                     -> k (Just ann) <&> \(Just ann') -> Reshape ann' shr e acc
    (Replicate ann si ix acc)                                   -> k (Just ann) <&> \(Just ann') -> Replicate ann' si ix acc
    (Slice ann si acc ix)                                       -> k (Just ann) <&> \(Just ann') -> Slice ann' si acc ix
    (Map ann t1 t2 f acc)                                       -> k (Just ann) <&> \(Just ann') -> Map ann' t1 t2 f acc
    (ZipWith ann t1 t2 t3 f acc1 acc2)                          -> k (Just ann) <&> \(Just ann') -> ZipWith ann' t1 t2 t3 f acc1 acc2
    (Fold ann tp f e acc)                                       -> k (Just ann) <&> \(Just ann') -> Fold ann' tp f e acc
    (FoldSeg ann i tp f e acc1 acc2)                            -> k (Just ann) <&> \(Just ann') -> FoldSeg ann' i tp f e acc1 acc2
    (Scan ann d tp f e acc)                                     -> k (Just ann) <&> \(Just ann') -> Scan ann' d tp f e acc
    (Scan' ann d tp f e acc)                                    -> k (Just ann) <&> \(Just ann') -> Scan' ann' d tp f e acc
    (Permute ann repr f dftAcc perm acc)                        -> k (Just ann) <&> \(Just ann') -> Permute ann' repr f dftAcc perm acc
    (Backpermute ann shr newDim perm acc)                       -> k (Just ann) <&> \(Just ann') -> Backpermute ann' shr newDim perm acc
    (Stencil ann stencil tp f boundary acc)                     -> k (Just ann) <&> \(Just ann') -> Stencil ann' stencil tp f boundary acc
    (Stencil2 ann stencil1 stencil2 tp f bndy1 acc1 bndy2 acc2) -> k (Just ann) <&> \(Just ann') -> Stencil2 ann' stencil1 stencil2 tp f bndy1 acc1 bndy2 acc2
    _                                                           -> pacc <$ k Nothing

instance FieldAnn (Exp a) where
  _ann k (Exp (SmartExp pexp)) = Exp . SmartExp <$> case pexp of
    (Const ann tp v)          -> k (Just ann) <&> \(Just ann') -> Const ann' tp v
    (Undef ann tp)            -> k (Just ann) <&> \(Just ann') -> Undef ann' tp
    (Prj ann idx e)           -> k (Just ann) <&> \(Just ann') -> Prj ann' idx e
    (Nil ann)                 -> k (Just ann) <&> \(Just ann') -> Nil ann'
    (Pair ann e1 e2)          -> k (Just ann) <&> \(Just ann') -> Pair ann' e1 e2
    (VecPack   ann vec e)     -> k (Just ann) <&> \(Just ann') -> VecPack   ann' vec e
    (VecUnpack ann vec e)     -> k (Just ann) <&> \(Just ann') -> VecUnpack ann' vec e
    (ToIndex   ann shr sh ix) -> k (Just ann) <&> \(Just ann') -> ToIndex   ann' shr sh ix
    (FromIndex ann shr sh e)  -> k (Just ann) <&> \(Just ann') -> FromIndex ann' shr sh e
    (Case ann e rhs)          -> k (Just ann) <&> \(Just ann') -> Case ann' e rhs
    (Cond ann e1 e2 e3)       -> k (Just ann) <&> \(Just ann') -> Cond ann' e1 e2 e3
    (While ann tp p it i)     -> k (Just ann) <&> \(Just ann') -> While ann' tp p it i
    (PrimConst ann c)         -> k (Just ann) <&> \(Just ann') -> PrimConst ann' c
    (PrimApp ann f e)         -> k (Just ann) <&> \(Just ann') -> PrimApp ann' f e
    (Index ann tp a e)        -> k (Just ann) <&> \(Just ann') -> Index ann' tp a e
    (LinearIndex ann tp a i)  -> k (Just ann) <&> \(Just ann') -> LinearIndex ann' tp a i
    (Shape ann shr a)         -> k (Just ann) <&> \(Just ann') -> Shape ann' shr a
    (ShapeSize ann shr e)     -> k (Just ann) <&> \(Just ann') -> ShapeSize ann' shr e
    (Foreign ann repr ff f e) -> k (Just ann) <&> \(Just ann') -> Foreign ann' repr ff f e
    (Coerce ann t1 t2 e)      -> k (Just ann) <&> \(Just ann') -> Coerce ann' t1 t2 e
    _                         -> pexp <$ k Nothing


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

unExpFunction :: (Elt a, Elt b) => (Exp a -> Exp b) -> SmartExp (EltR a) -> SmartExp (EltR b)
unExpFunction f = unExp . f . Exp

unExpBinaryFunction :: (Elt a, Elt b, Elt c) => (Exp a -> Exp b -> Exp c) -> SmartExp (EltR a) -> SmartExp (EltR b) -> SmartExp (EltR c)
unExpBinaryFunction f a b = unExp $ f (Exp a) (Exp b)

mkPrimUnary :: (SourceMapped, Elt a, Elt b) => PrimFun (EltR a -> EltR b) -> Exp a -> Exp b
mkPrimUnary prim (Exp a) = mkExp $ PrimApp mkAnn prim a

mkPrimBinary :: (SourceMapped, Elt a, Elt b, Elt c) => PrimFun ((EltR a, EltR b) -> EltR c) -> Exp a -> Exp b -> Exp c
mkPrimBinary prim (Exp a) (Exp b) = mkExp $ PrimApp mkAnn prim (SmartExp $ Pair mkAnn a b)

mkPrimUnaryBool :: (SourceMapped, Elt a) => PrimFun (EltR a -> PrimBool) -> Exp a -> Exp Bool
mkPrimUnaryBool = mkCoerce @PrimBool $$ mkPrimUnary

mkPrimBinaryBool :: (SourceMapped, Elt a, Elt b) => PrimFun ((EltR a, EltR b) -> PrimBool) -> Exp a -> Exp b -> Exp Bool
mkPrimBinaryBool = mkCoerce @PrimBool $$$ mkPrimBinary

unPair :: SourceMapped => SmartExp (a, b) -> (SmartExp a, SmartExp b)
unPair e = (SmartExp $ Prj mkAnn PairIdxLeft e, SmartExp $ Prj mkAnn PairIdxRight e)

mkPairToTuple :: SourceMapped => SmartAcc (a, b) -> SmartAcc (((), a), b)
mkPairToTuple e = SmartAcc (Anil mkAnn) `pair` a `pair` b
  where
    a = SmartAcc $ Aprj mkAnn PairIdxLeft e
    b = SmartAcc $ Aprj mkAnn PairIdxRight e
    pair x y = SmartAcc $ Apair mkAnn x y

class ApplyAcc a where
  type FromApplyAcc a
  applyAcc :: HasCallStack => FromApplyAcc a -> a

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

-- TODO: Reformat
formatPreAccOp :: Format r (PreSmartAcc acc exp arrs -> r)
formatPreAccOp = later $ \case
  Atag _ i            -> bformat ("Atag " % int) i
  Use _ aR a          -> bformat ("Use " % string) (showArrayShort 5 (showsElt (arrayRtype aR)) aR a)
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
  Fold _ _ _ z _      -> bformat ("Fold" % maybed "1" (fconst mempty)) z
  FoldSeg _ _ _ _ z _ _ -> bformat ("Fold" % maybed "1" (fconst mempty) % "Seg") z
  Scan _ d _ _ z _    -> bformat ("Scan" % formatDirection % maybed "1" (fconst mempty)) d z
  Scan' _ d _ _ _ _   -> bformat ("Scan" % formatDirection % "\'") d
  Permute{}           -> "Permute"
  Backpermute{}       -> "Backpermute"
  Stencil{}           -> "Stencil"
  Stencil2{}          -> "Stencil2"
  Aforeign{}          -> "Aforeign"

-- TODO: Show annotations
formatPreExpOp :: Format r (PreSmartExp acc exp t -> r)
formatPreExpOp = later $ \case
  Tag _ i       -> bformat ("Tag " % int) i
  Const _ t c   -> bformat ("Const " % string) (showElt (TupRsingle t) c)
  Match{}       -> "Match"
  Undef{}       -> "Undef"
  Nil{}         -> "Nil"
  Pair{}        -> "Pair"
  Prj{}         -> "Prj"
  VecPack{}     -> "VecPack"
  VecUnpack{}   -> "VecUnpack"
  ToIndex{}     -> "ToIndex"
  FromIndex{}   -> "FromIndex"
  Case{}        -> "Case"
  Cond{}        -> "Cond"
  While{}       -> "While"
  PrimConst{}   -> "PrimConst"
  PrimApp{}     -> "PrimApp"
  Index{}       -> "Index"
  LinearIndex{} -> "LinearIndex"
  Shape{}       -> "Shape"
  ShapeSize{}   -> "ShapeSize"
  Foreign{}     -> "Foreign"
  Coerce{}      -> "Coerce"

