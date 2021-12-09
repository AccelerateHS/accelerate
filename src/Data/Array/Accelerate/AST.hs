{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.AST
-- Copyright   : [2008..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- /Scalar versus collective operations/
--
-- The embedded array processing language is a two-level language.  It
-- combines a language of scalar expressions and functions with a language of
-- collective array operations.  Scalar expressions are used to compute
-- arguments for collective operations and scalar functions are used to
-- parametrise higher-order, collective array operations.  The two-level
-- structure, in particular, ensures that collective operations cannot be
-- parametrised with collective operations; hence, we are following a flat
-- data-parallel model.  The collective operations manipulate
-- multi-dimensional arrays whose shape is explicitly tracked in their types.
-- In fact, collective operations cannot produce any values other than
-- multi-dimensional arrays; when they yield a scalar, this is in the form of
-- a 0-dimensional, singleton array.  Similarly, scalar expression can -as
-- their name indicates- only produce tuples of scalar, but not arrays.
--
-- There are, however, two expression forms that take arrays as arguments.  As
-- a result scalar and array expressions are recursively dependent.  As we
-- cannot and don't want to compute arrays in the middle of scalar
-- computations, array computations will always be hoisted out of scalar
-- expressions.  So that this is always possible, these array expressions may
-- not contain any free scalar variables.  To express that condition in the
-- type structure, we use separate environments for scalar and array variables.
--
-- /Programs/
--
-- Collective array programs comprise closed expressions of array operations.
-- There is no explicit sharing in the initial AST form, but sharing is
-- introduced subsequently by common subexpression elimination and floating
-- of array computations.
--
-- /Functions/
--
-- The array expression language is first-order and only provides limited
-- control structures to ensure that it can be efficiently executed on
-- compute-acceleration hardware, such as GPUs.  To restrict functions to
-- first-order, we separate function abstraction from the main expression
-- type.  Functions are represented using de Bruijn indices.
--
-- /Parametric and ad-hoc polymorphism/
--
-- The array language features paramatric polymophism (e.g., pairing and
-- projections) as well as ad-hoc polymorphism (e.g., arithmetic operations).
-- All ad-hoc polymorphic constructs include reified dictionaries (c.f.,
-- module 'Types').  Reified dictionaries also ensure that constants
-- (constructor 'Const') are representable on compute acceleration hardware.
--
-- The AST contains both reified dictionaries and type class constraints.
-- Type classes are used for array-related functionality that is uniformly
-- available for all supported types.  In contrast, reified dictionaries are
-- used for functionality that is only available for certain types, such as
-- arithmetic operations.
--

module Data.Array.Accelerate.AST (

  -- * Internal AST
  -- ** Array computations
  Afun, PreAfun, OpenAfun, PreOpenAfun(..),
  Acc, OpenAcc(..), PreOpenAcc(..), Direction(..), Message(..),
  ALeftHandSide, ArrayVar, ArrayVars,

  -- ** Scalar expressions
  ELeftHandSide, ExpVar, ExpVars, expVars,
  Fun, OpenFun(..),
  Exp, OpenExp(..),
  Boundary(..),
  PrimConst(..),
  PrimFun(..),
  PrimBool,
  PrimMaybe,

  -- ** Extracting type information
  HasArraysR(..), arrayR,
  expType,
  primConstType,
  primFunType,

  -- ** Normal-form
  NFDataAcc,
  rnfOpenAfun, rnfPreOpenAfun,
  rnfOpenAcc, rnfPreOpenAcc,
  rnfALeftHandSide,
  rnfArrayVar,
  rnfOpenFun,
  rnfOpenExp,
  rnfELeftHandSide,
  rnfExpVar,
  rnfBoundary,
  rnfConst,
  rnfPrimConst,
  rnfPrimFun,

  -- ** Template Haskell
  LiftAcc,
  liftPreOpenAfun,
  liftPreOpenAcc,
  liftALeftHandSide,
  liftArrayVar,
  liftOpenFun,
  liftOpenExp,
  liftELeftHandSide,
  liftExpVar,
  liftBoundary,
  liftPrimConst,
  liftPrimFun,
  liftMessage,

  -- ** Miscellaneous
  formatPreAccOp,
  formatExpOp,

) where

import Data.Array.Accelerate.Annotations
import Data.Array.Accelerate.AST.Idx
import Data.Array.Accelerate.AST.LeftHandSide
import Data.Array.Accelerate.AST.Var
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Representation.Array
import Data.Array.Accelerate.Representation.Elt
import Data.Array.Accelerate.Representation.Shape
import Data.Array.Accelerate.Representation.Slice
import Data.Array.Accelerate.Representation.Stencil
import Data.Array.Accelerate.Representation.Tag
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Representation.Vec
import Data.Array.Accelerate.Sugar.Foreign
import Data.Array.Accelerate.Type
import Data.Primitive.Vec

import Control.DeepSeq
import Data.Kind
import Data.Maybe
import Data.Text                                                    ( Text )
import Data.Text.Lazy.Builder
import Formatting
import Language.Haskell.TH.Extra                                    ( CodeQ )
import qualified Language.Haskell.TH.Extra                          as TH
import qualified Language.Haskell.TH.Syntax                         as TH
import Lens.Micro                                                   ( (<&>) )

import GHC.TypeLits


-- Array expressions
-- -----------------

-- | Function abstraction over parametrised array computations
--
data PreOpenAfun acc aenv t where
  Abody ::                               acc             aenv  t -> PreOpenAfun acc aenv t
  Alam  :: ALeftHandSide a aenv aenv' -> PreOpenAfun acc aenv' t -> PreOpenAfun acc aenv (a -> t)

-- Function abstraction over vanilla open array computations
--
type OpenAfun = PreOpenAfun OpenAcc

-- | Parametrised array-computation function without free array variables
--
type PreAfun acc = PreOpenAfun acc ()

-- | Vanilla array-computation function without free array variables
--
type Afun = OpenAfun ()

-- Vanilla open array computations
--
newtype OpenAcc aenv t = OpenAcc (PreOpenAcc OpenAcc aenv t)

-- | Closed array expression aka an array program
--
type Acc = OpenAcc ()

-- Types for array binders
--
type ALeftHandSide  = LeftHandSide ArrayR
type ArrayVar       = Var ArrayR
type ArrayVars aenv = Vars ArrayR aenv

-- Bool is not a primitive type
type PrimBool    = TAG
type PrimMaybe a = (TAG, ((), a))

-- Trace messages
data Message a where
  Message :: (a -> String)                    -- embedded show
          -> Maybe (CodeQ (a -> String))      -- lifted version of show, for TH
          -> Text
          -> Message a

-- | Collective array computations parametrised over array variables
-- represented with de Bruijn indices.
--
-- * Scalar functions and expressions embedded in well-formed array
--   computations cannot contain free scalar variable indices. The latter
--   cannot be bound in array computations, and hence, cannot appear in any
--   well-formed program.
--
-- * The let-form is used to represent the sharing discovered by common
--   subexpression elimination as well as to control evaluation order. (We
--   need to hoist array expressions out of scalar expressions---they occur
--   in scalar indexing and in determining an arrays shape.)
--
-- The data type is parameterised over the representation types (not the
-- surface type).
--
-- We use a non-recursive variant parametrised over the recursive closure,
-- to facilitate attribute calculation in the backend.
--
data PreOpenAcc (acc :: Type -> Type -> Type) aenv a where

  -- Local non-recursive binding to represent sharing and demand
  -- explicitly. Note this is an eager binding!
  --
  Alet        :: Ann
              -> ALeftHandSide bndArrs aenv aenv'
              -> acc            aenv  bndArrs         -- bound expression
              -> acc            aenv' bodyArrs        -- the bound expression scope
              -> PreOpenAcc acc aenv  bodyArrs

  -- Variable bound by a 'Let', represented by a de Bruijn index
  --
  Avar        :: -- 'ArrayVar' already contains an 'Ann' field
                 ArrayVar       aenv (Array sh e)
              -> PreOpenAcc acc aenv (Array sh e)

  -- Tuples of arrays
  --
  Apair       :: Ann
              -> acc            aenv as
              -> acc            aenv bs
              -> PreOpenAcc acc aenv (as, bs)

  Anil        :: Ann
              -> PreOpenAcc acc aenv ()

  -- Array-function application.
  --
  -- The array function is not closed at the core level because we need access
  -- to free variables introduced by 'run1' style evaluators. See Issue#95.
  --
  Apply       :: Ann
              -> ArraysR arrs2
              -> PreOpenAfun acc aenv (arrs1 -> arrs2)
              -> acc             aenv arrs1
              -> PreOpenAcc  acc aenv arrs2

  -- Apply a backend-specific foreign function to an array, with a pure
  -- Accelerate version for use with other backends. The functions must be
  -- closed.
  --
  Aforeign    :: Foreign asm
              => Ann
              -> ArraysR bs
              -> asm                   (as -> bs) -- The foreign function for a given backend
              -> PreAfun      acc      (as -> bs) -- Fallback implementation(s)
              -> acc              aenv as         -- Arguments to the function
              -> PreOpenAcc   acc aenv bs

  -- If-then-else for array-level computations
  --
  Acond       :: Ann
              -> Exp            aenv PrimBool
              -> acc            aenv arrs
              -> acc            aenv arrs
              -> PreOpenAcc acc aenv arrs

  -- Value-recursion for array-level computations
  --
  Awhile      :: Ann
              -> PreOpenAfun acc aenv (arrs -> Scalar PrimBool) -- continue iteration while true
              -> PreOpenAfun acc aenv (arrs -> arrs)            -- function to iterate
              -> acc             aenv arrs                      -- initial value
              -> PreOpenAcc  acc aenv arrs

  Atrace      :: Ann
              -> Message              arrs1
              -> acc             aenv arrs1
              -> acc             aenv arrs2
              -> PreOpenAcc  acc aenv arrs2

  -- Array inlet. Triggers (possibly) asynchronous host->device transfer if
  -- necessary.
  --
  Use         :: Ann
              -> ArrayR (Array sh e)
              -> Array sh e
              -> PreOpenAcc acc aenv (Array sh e)

  -- Capture a scalar (or a tuple of scalars) in a singleton array
  --
  Unit        :: Ann
              -> TypeR e
              -> Exp            aenv e
              -> PreOpenAcc acc aenv (Scalar e)

  -- Change the shape of an array without altering its contents.
  -- Precondition (this may not be checked!):
  --
  -- > dim == size dim'
  --
  Reshape     :: Ann
              -> ShapeR sh
              -> Exp            aenv sh                         -- new shape
              -> acc            aenv (Array sh' e)              -- array to be reshaped
              -> PreOpenAcc acc aenv (Array sh e)

  -- Construct a new array by applying a function to each index.
  --
  Generate    :: Ann
              -> ArrayR (Array sh e)
              -> Exp            aenv sh                         -- output shape
              -> Fun            aenv (sh -> e)                  -- representation function
              -> PreOpenAcc acc aenv (Array sh e)

  -- Hybrid map/backpermute, where we separate the index and value
  -- transformations.
  --
  Transform   :: Ann
              -> ArrayR (Array sh' b)
              -> Exp            aenv sh'                        -- dimension of the result
              -> Fun            aenv (sh' -> sh)                -- index permutation function
              -> Fun            aenv (a   -> b)                 -- function to apply at each element
              ->            acc aenv (Array sh  a)              -- source array
              -> PreOpenAcc acc aenv (Array sh' b)

  -- Replicate an array across one or more dimensions as given by the first
  -- argument
  --
  Replicate   :: Ann
              -> SliceIndex slix sl co sh                       -- slice type specification
              -> Exp            aenv slix                       -- slice value specification
              -> acc            aenv (Array sl e)               -- data to be replicated
              -> PreOpenAcc acc aenv (Array sh e)

  -- Index a sub-array out of an array; i.e., the dimensions not indexed
  -- are returned whole
  --
  Slice       :: Ann
              -> SliceIndex slix sl co sh                       -- slice type specification
              -> acc            aenv (Array sh e)               -- array to be indexed
              -> Exp            aenv slix                       -- slice value specification
              -> PreOpenAcc acc aenv (Array sl e)

  -- Apply the given unary function to all elements of the given array
  --
  Map         :: Ann
              -> TypeR e'
              -> Fun            aenv (e -> e')
              -> acc            aenv (Array sh e)
              -> PreOpenAcc acc aenv (Array sh e')

  -- Apply a given binary function pairwise to all elements of the given
  -- arrays. The length of the result is the length of the shorter of the
  -- two argument arrays.
  --
  ZipWith     :: Ann
              -> TypeR e3
              -> Fun            aenv (e1 -> e2 -> e3)
              -> acc            aenv (Array sh e1)
              -> acc            aenv (Array sh e2)
              -> PreOpenAcc acc aenv (Array sh e3)

  -- Fold along the innermost dimension of an array with a given
  -- /associative/ function.
  --
  Fold        :: Ann
              -> Fun            aenv (e -> e -> e)              -- combination function
              -> Maybe     (Exp aenv e)                         -- default value
              -> acc            aenv (Array (sh, Int) e)        -- folded array
              -> PreOpenAcc acc aenv (Array sh e)

  -- Segmented fold along the innermost dimension of an array with a given
  -- /associative/ function
  --
  FoldSeg     :: Ann
              -> IntegralType i
              -> Fun            aenv (e -> e -> e)              -- combination function
              -> Maybe     (Exp aenv e)                         -- default value
              -> acc            aenv (Array (sh, Int) e)        -- folded array
              -> acc            aenv (Segments i)               -- segment descriptor
              -> PreOpenAcc acc aenv (Array (sh, Int) e)

  -- Haskell-style scan of a linear array with a given
  -- /associative/ function and optionally an initial element
  -- (which does not need to be the neutral of the associative operations)
  -- If no initial value is given, this is a scan1
  --
  Scan        :: Ann
              -> Direction
              -> Fun            aenv (e -> e -> e)              -- combination function
              -> Maybe     (Exp aenv e)                         -- initial value
              -> acc            aenv (Array (sh, Int) e)
              -> PreOpenAcc acc aenv (Array (sh, Int) e)

  -- Like 'Scan', but produces a rightmost (in case of a left-to-right scan)
  -- fold value and an array with the same length as the input array (the
  -- fold value would be the rightmost element in a Haskell-style scan)
  --
  Scan'       :: Ann
              -> Direction
              -> Fun            aenv (e -> e -> e)              -- combination function
              -> Exp            aenv e                          -- initial value
              -> acc            aenv (Array (sh, Int) e)
              -> PreOpenAcc acc aenv (Array (sh, Int) e, Array sh e)

  -- Generalised forward permutation is characterised by a permutation function
  -- that determines for each element of the source array where it should go in
  -- the output. The permutation can be between arrays of varying shape and
  -- dimensionality.
  --
  -- Other characteristics of the permutation function 'f':
  --
  --   1. 'f' is a (morally) partial function: only the elements of the domain
  --      for which the function evaluates to a 'Just' value are mapped in the
  --      result. Other elements are dropped.
  --
  --   2. 'f' is not surjective: positions in the target array need not be
  --      picked up by the permutation function, so the target array must first
  --      be initialised from an array of default values.
  --
  --   3. 'f' is not injective: distinct elements of the domain may map to the
  --      same position in the target array. In this case the combination
  --      function is used to combine elements, which needs to be /associative/
  --      and /commutative/.
  --
  Permute     :: Ann
              -> Fun            aenv (e -> e -> e)              -- combination function
              -> acc            aenv (Array sh' e)              -- default values
              -> Fun            aenv (sh -> PrimMaybe sh')      -- permutation function
              -> acc            aenv (Array sh e)               -- source array
              -> PreOpenAcc acc aenv (Array sh' e)

  -- Generalised multi-dimensional backwards permutation; the permutation can
  -- be between arrays of varying shape; the permutation function must be total
  --
  Backpermute :: Ann
              -> ShapeR sh'
              -> Exp            aenv sh'                        -- dimensions of the result
              -> Fun            aenv (sh' -> sh)                -- permutation function
              -> acc            aenv (Array sh e)               -- source array
              -> PreOpenAcc acc aenv (Array sh' e)

  -- Map a stencil over an array.  In contrast to 'map', the domain of
  -- a stencil function is an entire /neighbourhood/ of each array element.
  --
  Stencil     :: Ann
              -> StencilR sh e stencil
              -> TypeR e'
              -> Fun             aenv (stencil -> e')           -- stencil function
              -> Boundary        aenv (Array sh e)              -- boundary condition
              -> acc             aenv (Array sh e)              -- source array
              -> PreOpenAcc  acc aenv (Array sh e')

  -- Map a binary stencil over an array.
  --
  Stencil2    :: Ann
              -> StencilR sh a stencil1
              -> StencilR sh b stencil2
              -> TypeR c
              -> Fun             aenv (stencil1 -> stencil2 -> c) -- stencil function
              -> Boundary        aenv (Array sh a)                -- boundary condition #1
              -> acc             aenv (Array sh a)                -- source array #1
              -> Boundary        aenv (Array sh b)                -- boundary condition #2
              -> acc             aenv (Array sh b)                -- source array #2
              -> PreOpenAcc acc  aenv (Array sh c)


data Direction = LeftToRight | RightToLeft
  deriving Eq


-- | Vanilla boundary condition specification for stencil operations
--
data Boundary aenv t where
  -- Clamp coordinates to the extent of the array
  Clamp     :: Boundary aenv t

  -- Mirror coordinates beyond the array extent
  Mirror    :: Boundary aenv t

  -- Wrap coordinates around on each dimension
  Wrap      :: Boundary aenv t

  -- Use a constant value for outlying coordinates
  Constant  :: e
            -> Boundary aenv (Array sh e)

  -- Apply the given function to outlying coordinates
  Function  :: Fun aenv (sh -> e)
            -> Boundary aenv (Array sh e)


-- Embedded expressions
-- --------------------

-- | Vanilla open function abstraction
--
data OpenFun env aenv t where
  Body ::                             OpenExp env  aenv t -> OpenFun env aenv t
  Lam  :: ELeftHandSide a env env' -> OpenFun env' aenv t -> OpenFun env aenv (a -> t)

-- | Vanilla function without free scalar variables
--
type Fun = OpenFun ()

-- | Vanilla expression without free scalar variables
--
type Exp = OpenExp ()

-- Types for scalar bindings
--
type ELeftHandSide = LeftHandSide ScalarType
type ExpVar        = Var ScalarType
type ExpVars env   = Vars ScalarType env

-- TODO: Are the annotations in the 'Pair' and 'Nil' nodes significant here?
--       Probably not, but if they are, we can take them from the 'Tag'
--       constructor. Same goes for 'avarsIn'.
expVars :: ExpVars env t -> OpenExp env aenv t
expVars TupRunit         = Nil mkDummyAnn
expVars (TupRsingle var) = Evar var
expVars (TupRpair v1 v2) = Pair mkDummyAnn (expVars v1) (expVars v2)


-- | Vanilla open expressions using de Bruijn indices for variables ranging
-- over tuples of scalars and arrays of tuples. All code, except Cond, is
-- evaluated eagerly. N-tuples are represented as nested pairs.
--
-- The data type is parametrised over the representation type (not the
-- surface types).
--
data OpenExp env aenv t where
  -- Local binding of a scalar expression
  Let           :: Ann
                -> ELeftHandSide bnd_t env env'
                -> OpenExp env  aenv bnd_t
                -> OpenExp env' aenv body_t
                -> OpenExp env  aenv body_t

  -- Variable index, ranging only over tuples or scalars
  Evar          :: -- 'ExpVar' already contains an 'Ann' field
                   ExpVar env t
                -> OpenExp env aenv t

  -- Apply a backend-specific foreign function
  Foreign       :: Foreign asm
                => Ann
                -> TypeR y
                -> asm    (x -> y)    -- foreign function
                -> Fun () (x -> y)    -- alternate implementation (for other backends)
                -> OpenExp env aenv x
                -> OpenExp env aenv y

  -- Tuples
  Pair          :: Ann
                -> OpenExp env aenv t1
                -> OpenExp env aenv t2
                -> OpenExp env aenv (t1, t2)

  Nil           :: Ann
                -> OpenExp env aenv ()

  -- SIMD vectors
  VecPack       :: KnownNat n
                => Ann
                -> VecR n s tup
                -> OpenExp env aenv tup
                -> OpenExp env aenv (Vec n s)

  VecUnpack     :: KnownNat n
                => Ann
                -> VecR n s tup
                -> OpenExp env aenv (Vec n s)
                -> OpenExp env aenv tup

  -- Array indices & shapes
  IndexSlice    :: Ann
                -> SliceIndex slix sl co sh
                -> OpenExp env aenv slix
                -> OpenExp env aenv sh
                -> OpenExp env aenv sl

  IndexFull     :: Ann
                -> SliceIndex slix sl co sh
                -> OpenExp env aenv slix
                -> OpenExp env aenv sl
                -> OpenExp env aenv sh

  -- Shape and index conversion
  ToIndex       :: Ann
                -> ShapeR sh
                -> OpenExp env aenv sh           -- shape of the array
                -> OpenExp env aenv sh           -- index into the array
                -> OpenExp env aenv Int

  FromIndex     :: Ann
                -> ShapeR sh
                -> OpenExp env aenv sh           -- shape of the array
                -> OpenExp env aenv Int          -- index into linear representation
                -> OpenExp env aenv sh

  -- Case statement
  Case          :: Ann
                -> OpenExp env aenv TAG
                -> [(TAG, OpenExp env aenv b)]      -- list of equations
                -> Maybe (OpenExp env aenv b)       -- default case
                -> OpenExp env aenv b

  -- Conditional expression (non-strict in 2nd and 3rd argument)
  Cond          :: Ann
                -> OpenExp env aenv PrimBool
                -> OpenExp env aenv t
                -> OpenExp env aenv t
                -> OpenExp env aenv t

  -- Value recursion
  While         :: Ann
                -> OpenFun env aenv (a -> PrimBool) -- continue while true
                -> OpenFun env aenv (a -> a)        -- function to iterate
                -> OpenExp env aenv a               -- initial value
                -> OpenExp env aenv a

  -- Constant values
  Const         :: Ann
                -> ScalarType t
                -> t
                -> OpenExp env aenv t

  PrimConst     :: Ann
                -> PrimConst t
                -> OpenExp env aenv t

  -- Primitive scalar operations
  PrimApp       :: Ann
                -> PrimFun (a -> r)
                -> OpenExp env aenv a
                -> OpenExp env aenv r

  -- Project a single scalar from an array.
  -- The array expression can not contain any free scalar variables.
  Index         :: Ann
                -> ArrayVar    aenv (Array dim t)
                -> OpenExp env aenv dim
                -> OpenExp env aenv t

  LinearIndex   :: Ann
                -> ArrayVar    aenv (Array dim t)
                -> OpenExp env aenv Int
                -> OpenExp env aenv t

  -- Array shape.
  -- The array expression can not contain any free scalar variables.
  Shape         :: Ann
                -> ArrayVar    aenv (Array dim e)
                -> OpenExp env aenv dim

  -- Number of elements of an array given its shape
  ShapeSize     :: Ann
                -> ShapeR dim
                -> OpenExp env aenv dim
                -> OpenExp env aenv Int

  -- Unsafe operations (may fail or result in undefined behaviour)
  -- An unspecified bit pattern
  Undef         :: Ann
                -> ScalarType t
                -> OpenExp env aenv t

  -- Reinterpret the bits of a value as a different type
  Coerce        :: BitSizeEq a b
                => Ann
                -> ScalarType a
                -> ScalarType b
                -> OpenExp env aenv a
                -> OpenExp env aenv b

-- |Primitive constant values
--
data PrimConst ty where

  -- constants from Bounded
  PrimMinBound  :: BoundedType a -> PrimConst a
  PrimMaxBound  :: BoundedType a -> PrimConst a

  -- constant from Floating
  PrimPi        :: FloatingType a -> PrimConst a


-- |Primitive scalar operations
--
data PrimFun sig where

  -- operators from Num
  PrimAdd  :: NumType a -> PrimFun ((a, a) -> a)
  PrimSub  :: NumType a -> PrimFun ((a, a) -> a)
  PrimMul  :: NumType a -> PrimFun ((a, a) -> a)
  PrimNeg  :: NumType a -> PrimFun (a      -> a)
  PrimAbs  :: NumType a -> PrimFun (a      -> a)
  PrimSig  :: NumType a -> PrimFun (a      -> a)

  -- operators from Integral
  PrimQuot     :: IntegralType a -> PrimFun ((a, a)   -> a)
  PrimRem      :: IntegralType a -> PrimFun ((a, a)   -> a)
  PrimQuotRem  :: IntegralType a -> PrimFun ((a, a)   -> (a, a))
  PrimIDiv     :: IntegralType a -> PrimFun ((a, a)   -> a)
  PrimMod      :: IntegralType a -> PrimFun ((a, a)   -> a)
  PrimDivMod   :: IntegralType a -> PrimFun ((a, a)   -> (a, a))

  -- operators from Bits & FiniteBits
  PrimBAnd               :: IntegralType a -> PrimFun ((a, a)   -> a)
  PrimBOr                :: IntegralType a -> PrimFun ((a, a)   -> a)
  PrimBXor               :: IntegralType a -> PrimFun ((a, a)   -> a)
  PrimBNot               :: IntegralType a -> PrimFun (a        -> a)
  PrimBShiftL            :: IntegralType a -> PrimFun ((a, Int) -> a)
  PrimBShiftR            :: IntegralType a -> PrimFun ((a, Int) -> a)
  PrimBRotateL           :: IntegralType a -> PrimFun ((a, Int) -> a)
  PrimBRotateR           :: IntegralType a -> PrimFun ((a, Int) -> a)
  PrimPopCount           :: IntegralType a -> PrimFun (a -> Int)
  PrimCountLeadingZeros  :: IntegralType a -> PrimFun (a -> Int)
  PrimCountTrailingZeros :: IntegralType a -> PrimFun (a -> Int)

  -- operators from Fractional and Floating
  PrimFDiv        :: FloatingType a -> PrimFun ((a, a) -> a)
  PrimRecip       :: FloatingType a -> PrimFun (a      -> a)
  PrimSin         :: FloatingType a -> PrimFun (a      -> a)
  PrimCos         :: FloatingType a -> PrimFun (a      -> a)
  PrimTan         :: FloatingType a -> PrimFun (a      -> a)
  PrimAsin        :: FloatingType a -> PrimFun (a      -> a)
  PrimAcos        :: FloatingType a -> PrimFun (a      -> a)
  PrimAtan        :: FloatingType a -> PrimFun (a      -> a)
  PrimSinh        :: FloatingType a -> PrimFun (a      -> a)
  PrimCosh        :: FloatingType a -> PrimFun (a      -> a)
  PrimTanh        :: FloatingType a -> PrimFun (a      -> a)
  PrimAsinh       :: FloatingType a -> PrimFun (a      -> a)
  PrimAcosh       :: FloatingType a -> PrimFun (a      -> a)
  PrimAtanh       :: FloatingType a -> PrimFun (a      -> a)
  PrimExpFloating :: FloatingType a -> PrimFun (a      -> a)
  PrimSqrt        :: FloatingType a -> PrimFun (a      -> a)
  PrimLog         :: FloatingType a -> PrimFun (a      -> a)
  PrimFPow        :: FloatingType a -> PrimFun ((a, a) -> a)
  PrimLogBase     :: FloatingType a -> PrimFun ((a, a) -> a)

  -- FIXME: add missing operations from RealFrac & RealFloat

  -- operators from RealFrac
  PrimTruncate :: FloatingType a -> IntegralType b -> PrimFun (a -> b)
  PrimRound    :: FloatingType a -> IntegralType b -> PrimFun (a -> b)
  PrimFloor    :: FloatingType a -> IntegralType b -> PrimFun (a -> b)
  PrimCeiling  :: FloatingType a -> IntegralType b -> PrimFun (a -> b)
  -- PrimProperFraction :: FloatingType a -> IntegralType b -> PrimFun (a -> (b, a))

  -- operators from RealFloat
  PrimAtan2      :: FloatingType a -> PrimFun ((a, a) -> a)
  PrimIsNaN      :: FloatingType a -> PrimFun (a -> PrimBool)
  PrimIsInfinite :: FloatingType a -> PrimFun (a -> PrimBool)

  -- relational and equality operators
  PrimLt   :: SingleType a -> PrimFun ((a, a) -> PrimBool)
  PrimGt   :: SingleType a -> PrimFun ((a, a) -> PrimBool)
  PrimLtEq :: SingleType a -> PrimFun ((a, a) -> PrimBool)
  PrimGtEq :: SingleType a -> PrimFun ((a, a) -> PrimBool)
  PrimEq   :: SingleType a -> PrimFun ((a, a) -> PrimBool)
  PrimNEq  :: SingleType a -> PrimFun ((a, a) -> PrimBool)
  PrimMax  :: SingleType a -> PrimFun ((a, a) -> a)
  PrimMin  :: SingleType a -> PrimFun ((a, a) -> a)

  -- logical operators
  --
  -- Note that these operators are strict in both arguments. That is, the
  -- second argument of PrimLAnd is always evaluated even when the first
  -- argument is false.
  --
  -- We define (surface level) (&&) and (||) using if-then-else to enable
  -- short-circuiting, while (&&!) and (||!) are strict versions of these
  -- operators, which are defined using PrimLAnd and PrimLOr.
  --
  PrimLAnd :: PrimFun ((PrimBool, PrimBool) -> PrimBool)
  PrimLOr  :: PrimFun ((PrimBool, PrimBool) -> PrimBool)
  PrimLNot :: PrimFun (PrimBool             -> PrimBool)

  -- general conversion between types
  PrimFromIntegral :: IntegralType a -> NumType b -> PrimFun (a -> b)
  PrimToFloating   :: NumType a -> FloatingType b -> PrimFun (a -> b)


-- Type utilities
-- --------------

class HasArraysR f where
  arraysR :: f aenv a -> ArraysR a

instance HasArraysR OpenAcc where
  arraysR (OpenAcc a) = arraysR a

arrayR :: HasArraysR f => f aenv (Array sh e) -> ArrayR (Array sh e)
arrayR a = case arraysR a of
  TupRsingle aR -> aR

instance HasArraysR acc => HasArraysR (PreOpenAcc acc) where
  arraysR (Alet _ _ _ body)             = arraysR body
  arraysR (Avar (Var _ aR _))           = TupRsingle aR
  arraysR (Apair _ as bs)               = TupRpair (arraysR as) (arraysR bs)
  arraysR Anil{}                        = TupRunit
  arraysR (Atrace _ _ _ bs)             = arraysR bs
  arraysR (Apply _ aR _ _)              = aR
  arraysR (Aforeign _ r _ _ _)          = r
  arraysR (Acond _ _ a _)               = arraysR a
  arraysR (Awhile _ _ (Alam lhs _) _)   = lhsToTupR lhs
  arraysR Awhile{}                      = error "I want my, I want my MTV!"
  arraysR (Use _ aR _)                  = TupRsingle aR
  arraysR (Unit _ tR _)                 = arraysRarray ShapeRz tR
  arraysR (Reshape _ sh _ a)            = let ArrayR _ tR = arrayR a
                                           in arraysRarray sh tR
  arraysR (Generate _ aR _ _)           = TupRsingle aR
  arraysR (Transform _ aR _ _ _ _)      = TupRsingle aR
  arraysR (Replicate _ slice _ a)       = let ArrayR _ tR = arrayR a
                                           in arraysRarray (sliceDomainR slice) tR
  arraysR (Slice _ slice a _)           = let ArrayR _ tR = arrayR a
                                           in arraysRarray (sliceShapeR slice) tR
  arraysR (Map _  tR _ a)               = let ArrayR sh _ = arrayR a
                                           in arraysRarray sh tR
  arraysR (ZipWith _ tR _ a _)          = let ArrayR sh _ = arrayR a
                                           in arraysRarray sh tR
  arraysR (Fold _ _ _ a)                = let ArrayR (ShapeRsnoc sh) tR = arrayR a
                                           in arraysRarray sh tR
  arraysR (FoldSeg _ _ _ _ a _)         = arraysR a
  arraysR (Scan  _ _ _ _ a)             = arraysR a
  arraysR (Scan' _ _ _ _ a)             = let aR@(ArrayR (ShapeRsnoc sh) tR) = arrayR a
                                           in TupRsingle aR `TupRpair` TupRsingle (ArrayR sh tR)
  arraysR (Permute _ _ a _ _)           = arraysR a
  arraysR (Backpermute _ sh _ _ a)      = let ArrayR _ tR = arrayR a
                                           in arraysRarray sh tR
  arraysR (Stencil _ _ tR _ _ a)        = let ArrayR sh _ = arrayR a
                                           in arraysRarray sh tR
  arraysR (Stencil2 _ _ _ tR _ _ a _ _) = let ArrayR sh _ = arrayR a
                                           in arraysRarray sh tR

expType :: HasCallStack => OpenExp aenv env t -> TypeR t
expType = \case
  Let _ _ _ body                 -> expType body
  Evar (Var _ tR _)              -> TupRsingle tR
  Foreign _ tR _ _ _             -> tR
  Pair _ e1 e2                   -> TupRpair (expType e1) (expType e2)
  Nil _                          -> TupRunit
  VecPack   _ vecR _             -> TupRsingle $ VectorScalarType $ vecRvector vecR
  VecUnpack _ vecR _             -> vecRtuple vecR
  IndexSlice _ si _ _            -> shapeType $ sliceShapeR si
  IndexFull  _ si _ _            -> shapeType $ sliceDomainR si
  ToIndex{}                      -> TupRsingle scalarTypeInt
  FromIndex _ shr _ _            -> shapeType shr
  Case _ _ ((_,e):_) _           -> expType e
  Case _ _ [] (Just e)           -> expType e
  Case{}                         -> internalError "empty case encountered"
  Cond _ _ e _                   -> expType e
  While _ _ (Lam lhs _) _        -> lhsToTupR lhs
  While{}                        -> error "What's the matter, you're running in the shadows"
  Const _ tR _                   -> TupRsingle tR
  PrimConst _ c                  -> TupRsingle $ SingleScalarType $ primConstType c
  PrimApp _ f _                  -> snd $ primFunType f
  Index _ (Var _ repr _) _       -> arrayRtype repr
  LinearIndex _ (Var _ repr _) _ -> arrayRtype repr
  Shape _ (Var _ repr _)         -> shapeType $ arrayRshape repr
  ShapeSize{}                    -> TupRsingle scalarTypeInt
  Undef _ tR                     -> TupRsingle tR
  Coerce _ _ tR _                -> TupRsingle tR

primConstType :: PrimConst a -> SingleType a
primConstType = \case
  PrimMinBound t -> bounded t
  PrimMaxBound t -> bounded t
  PrimPi       t -> floating t
  where
    bounded :: BoundedType a -> SingleType a
    bounded (IntegralBoundedType t) = NumSingleType $ IntegralNumType t

    floating :: FloatingType t -> SingleType t
    floating = NumSingleType . FloatingNumType

primFunType :: PrimFun (a -> b) -> (TypeR a, TypeR b)
primFunType = \case
  -- Num
  PrimAdd t                 -> binary' $ num t
  PrimSub t                 -> binary' $ num t
  PrimMul t                 -> binary' $ num t
  PrimNeg t                 -> unary'  $ num t
  PrimAbs t                 -> unary'  $ num t
  PrimSig t                 -> unary'  $ num t

  -- Integral
  PrimQuot t                -> binary' $ integral t
  PrimRem  t                -> binary' $ integral t
  PrimQuotRem t             -> unary' $ integral t `TupRpair` integral t
  PrimIDiv t                -> binary' $ integral t
  PrimMod  t                -> binary' $ integral t
  PrimDivMod t              -> unary' $ integral t `TupRpair` integral t

  -- Bits & FiniteBits
  PrimBAnd t                -> binary' $ integral t
  PrimBOr t                 -> binary' $ integral t
  PrimBXor t                -> binary' $ integral t
  PrimBNot t                -> unary' $ integral t
  PrimBShiftL t             -> (integral t `TupRpair` tint, integral t)
  PrimBShiftR t             -> (integral t `TupRpair` tint, integral t)
  PrimBRotateL t            -> (integral t `TupRpair` tint, integral t)
  PrimBRotateR t            -> (integral t `TupRpair` tint, integral t)
  PrimPopCount t            -> unary (integral t) tint
  PrimCountLeadingZeros t   -> unary (integral t) tint
  PrimCountTrailingZeros t  -> unary (integral t) tint

  -- Fractional, Floating
  PrimFDiv t                -> binary' $ floating t
  PrimRecip t               -> unary'  $ floating t
  PrimSin t                 -> unary'  $ floating t
  PrimCos t                 -> unary'  $ floating t
  PrimTan t                 -> unary'  $ floating t
  PrimAsin t                -> unary'  $ floating t
  PrimAcos t                -> unary'  $ floating t
  PrimAtan t                -> unary'  $ floating t
  PrimSinh t                -> unary'  $ floating t
  PrimCosh t                -> unary'  $ floating t
  PrimTanh t                -> unary'  $ floating t
  PrimAsinh t               -> unary'  $ floating t
  PrimAcosh t               -> unary'  $ floating t
  PrimAtanh t               -> unary'  $ floating t
  PrimExpFloating t         -> unary'  $ floating t
  PrimSqrt t                -> unary'  $ floating t
  PrimLog t                 -> unary'  $ floating t
  PrimFPow t                -> binary' $ floating t
  PrimLogBase t             -> binary' $ floating t

  -- RealFrac
  PrimTruncate a b          -> unary (floating a) (integral b)
  PrimRound a b             -> unary (floating a) (integral b)
  PrimFloor a b             -> unary (floating a) (integral b)
  PrimCeiling a b           -> unary (floating a) (integral b)

  -- RealFloat
  PrimAtan2 t               -> binary' $ floating t
  PrimIsNaN t               -> unary (floating t) tbool
  PrimIsInfinite t          -> unary (floating t) tbool

  -- Relational and equality
  PrimLt t                  -> compare' t
  PrimGt t                  -> compare' t
  PrimLtEq t                -> compare' t
  PrimGtEq t                -> compare' t
  PrimEq t                  -> compare' t
  PrimNEq t                 -> compare' t
  PrimMax t                 -> binary' $ single t
  PrimMin t                 -> binary' $ single t

  -- Logical
  PrimLAnd                  -> binary' tbool
  PrimLOr                   -> binary' tbool
  PrimLNot                  -> unary' tbool

  -- general conversion between types
  PrimFromIntegral a b      -> unary (integral a) (num b)
  PrimToFloating   a b      -> unary (num a) (floating b)

  where
    unary a b  = (a, b)
    unary' a   = unary a a
    binary a b = (a `TupRpair` a, b)
    binary' a  = binary a a
    compare' a = binary (single a) tbool

    single   = TupRsingle . SingleScalarType
    num      = TupRsingle . SingleScalarType . NumSingleType
    integral = num . IntegralNumType
    floating = num . FloatingNumType

    tbool    = TupRsingle scalarTypeWord8
    tint     = TupRsingle scalarTypeInt


-- Annotations
-- -----------

instance FieldAnn (OpenAcc aenv t) where
  _ann k (OpenAcc pacc) = OpenAcc <$> _ann k pacc

instance FieldAnn (PreOpenAcc acc aenv t) where
  _ann k (Alet ann lhs a b)                    = k (Just ann) <&> \(Just ann') -> Alet ann' lhs a b
  _ann k (Avar (Var ann repr ix))              = k (Just ann) <&> \(Just ann') -> Avar (Var ann' repr ix)
  _ann k (Apair ann as bs)                     = k (Just ann) <&> \(Just ann') -> Apair ann' as bs
  _ann k (Anil ann)                            = k (Just ann) <&> \(Just ann') -> Anil ann'
  _ann k (Apply ann repr f a)                  = k (Just ann) <&> \(Just ann') -> Apply ann' repr f a
  _ann k (Aforeign ann repr ff afun as)        = k (Just ann) <&> \(Just ann') -> Aforeign ann' repr ff afun as
  _ann k (Acond ann p t e)                     = k (Just ann) <&> \(Just ann') -> Acond ann' p t e
  _ann k (Awhile ann p f a)                    = k (Just ann) <&> \(Just ann') -> Awhile ann' p f a
  _ann k (Atrace ann msg as bs)                = k (Just ann) <&> \(Just ann') -> Atrace ann' msg as bs
  _ann k (Use ann repr a)                      = k (Just ann) <&> \(Just ann') -> Use ann' repr a
  _ann k (Unit ann tp e)                       = k (Just ann) <&> \(Just ann') -> Unit ann' tp e
  _ann k (Reshape ann shr e a)                 = k (Just ann) <&> \(Just ann') -> Reshape ann' shr e a
  _ann k (Generate ann repr e f)               = k (Just ann) <&> \(Just ann') -> Generate ann' repr e f
  _ann k (Transform ann repr sh ix f a)        = k (Just ann) <&> \(Just ann') -> Transform ann' repr sh ix f a
  _ann k (Replicate ann sl slix a)             = k (Just ann) <&> \(Just ann') -> Replicate ann' sl slix a
  _ann k (Slice ann sl a slix)                 = k (Just ann) <&> \(Just ann') -> Slice ann' sl a slix
  _ann k (Map ann tp f a)                      = k (Just ann) <&> \(Just ann') -> Map ann' tp f a
  _ann k (ZipWith ann tp f a1 a2)              = k (Just ann) <&> \(Just ann') -> ZipWith ann' tp f a1 a2
  _ann k (Fold ann f z a)                      = k (Just ann) <&> \(Just ann') -> Fold ann' f z a
  _ann k (FoldSeg ann itp f z a s)             = k (Just ann) <&> \(Just ann') -> FoldSeg ann' itp f z a s
  _ann k (Scan  ann d f z a)                   = k (Just ann) <&> \(Just ann') -> Scan  ann' d f z a
  _ann k (Scan' ann d f z a)                   = k (Just ann) <&> \(Just ann') -> Scan' ann' d f z a
  _ann k (Permute ann f1 a1 f2 a2)             = k (Just ann) <&> \(Just ann') -> Permute ann' f1 a1 f2 a2
  _ann k (Backpermute ann shr sh f a)          = k (Just ann) <&> \(Just ann') -> Backpermute ann' shr sh f a
  _ann k (Stencil ann sr tp f b a)             = k (Just ann) <&> \(Just ann') -> Stencil ann' sr tp f b a
  _ann k (Stencil2 ann s1 s2 tp f b1 a1 b2 a2) = k (Just ann) <&> \(Just ann') -> Stencil2 ann' s1 s2 tp f b1 a1 b2 a2

instance FieldAnn (OpenExp env aenv t) where
  _ann k (Let ann lhs bnd body)         = k (Just ann) <&> \(Just ann') -> Let ann' lhs bnd body
  _ann k (Evar (Var ann tp ix))         = k (Just ann) <&> \(Just ann') -> Evar (Var ann' tp ix)
  _ann k (Foreign ann tp asm f x)       = k (Just ann) <&> \(Just ann') -> Foreign ann' tp asm f x
  _ann k (Const ann tp c)               = k (Just ann) <&> \(Just ann') -> Const ann' tp c
  _ann k (Undef ann tp)                 = k (Just ann) <&> \(Just ann') -> Undef ann' tp
  _ann k (Pair ann a b)                 = k (Just ann) <&> \(Just ann') -> Pair ann' a b
  _ann k (Nil ann)                      = k (Just ann) <&> \(Just ann') -> Nil ann'
  _ann k (VecPack   ann vecr e)         = k (Just ann) <&> \(Just ann') -> VecPack   ann' vecr e
  _ann k (VecUnpack ann vecr e)         = k (Just ann) <&> \(Just ann') -> VecUnpack ann' vecr e
  _ann k (IndexSlice ann slice slix sh) = k (Just ann) <&> \(Just ann') -> IndexSlice ann' slice slix sh
  _ann k (IndexFull ann slice slix sl)  = k (Just ann) <&> \(Just ann') -> IndexFull ann' slice slix sl
  _ann k (ToIndex ann shr sh ix)        = k (Just ann) <&> \(Just ann') -> ToIndex ann' shr sh ix
  _ann k (FromIndex ann shr sh ix)      = k (Just ann) <&> \(Just ann') -> FromIndex ann' shr sh ix
  _ann k (Case ann e rhs def)           = k (Just ann) <&> \(Just ann') -> Case ann' e rhs def
  _ann k (Cond ann p e1 e2)             = k (Just ann) <&> \(Just ann') -> Cond ann' p e1 e2
  _ann k (While ann p f x)              = k (Just ann) <&> \(Just ann') -> While ann' p f x
  _ann k (PrimConst ann c)              = k (Just ann) <&> \(Just ann') -> PrimConst ann' c
  _ann k (PrimApp ann f x)              = k (Just ann) <&> \(Just ann') -> PrimApp ann' f x
  _ann k (Index ann a ix)               = k (Just ann) <&> \(Just ann') -> Index ann' a ix
  _ann k (LinearIndex ann a ix)         = k (Just ann) <&> \(Just ann') -> LinearIndex ann' a ix
  _ann k (Shape ann a)                  = k (Just ann) <&> \(Just ann') -> Shape ann' a
  _ann k (ShapeSize ann shr sh)         = k (Just ann) <&> \(Just ann') -> ShapeSize ann' shr sh
  _ann k (Coerce ann t1 t2 e)           = k (Just ann) <&> \(Just ann') -> Coerce ann' t1 t2 e


-- Normal form data
-- ================

instance NFData (OpenAfun aenv f) where
  rnf = rnfOpenAfun

instance NFData (OpenAcc aenv t) where
  rnf = rnfOpenAcc

instance NFData (OpenExp env aenv t) where
  rnf = rnfOpenExp

instance NFData (OpenFun env aenv t) where
  rnf = rnfOpenFun


type NFDataAcc acc = forall aenv t. acc aenv t -> ()

rnfOpenAfun :: OpenAfun aenv t -> ()
rnfOpenAfun = rnfPreOpenAfun rnfOpenAcc

rnfPreOpenAfun :: NFDataAcc acc -> PreOpenAfun acc aenv t -> ()
rnfPreOpenAfun rnfA (Abody b) = rnfA b
rnfPreOpenAfun rnfA (Alam lhs f) = rnfALeftHandSide lhs `seq` rnfPreOpenAfun rnfA f

rnfOpenAcc :: OpenAcc aenv t -> ()
rnfOpenAcc (OpenAcc pacc) = rnfPreOpenAcc rnfOpenAcc pacc

rnfPreOpenAcc :: forall acc aenv t. HasArraysR acc => NFDataAcc acc -> PreOpenAcc acc aenv t -> ()
rnfPreOpenAcc rnfA pacc =
  let
      rnfAF :: PreOpenAfun acc aenv' t' -> ()
      rnfAF = rnfPreOpenAfun rnfA

      rnfE :: OpenExp env' aenv' t' -> ()
      rnfE = rnfOpenExp

      rnfF :: OpenFun env' aenv' t' -> ()
      rnfF = rnfOpenFun

      rnfB :: ArrayR (Array sh e) -> Boundary aenv' (Array sh e) -> ()
      rnfB = rnfBoundary

      rnfM :: Message a -> ()
      rnfM (Message f g msg) = f `seq` rnfMaybe (\x -> x `seq` ()) g `seq` rnf msg
  in
  case pacc of
    Alet ann lhs bnd body        -> rnfAnn ann `seq` rnfALeftHandSide lhs `seq` rnfA bnd `seq` rnfA body
    Avar var                     -> rnfArrayVar var
    Apair ann as bs              -> rnfAnn ann `seq` rnfA as `seq` rnfA bs
    Anil ann                     -> rnfAnn ann
    Atrace ann msg as bs         -> rnfAnn ann `seq` rnfM msg `seq` rnfA as `seq` rnfA bs
    Apply ann repr afun acc      -> rnfAnn ann `seq` rnfTupR rnfArrayR repr `seq` rnfAF afun `seq` rnfA acc
    Aforeign ann repr asm afun a -> rnfAnn ann `seq` rnfTupR rnfArrayR repr `seq` rnf (strForeign asm) `seq` rnfAF afun `seq` rnfA a
    Acond ann p a1 a2            -> rnfAnn ann `seq` rnfE p `seq` rnfA a1 `seq` rnfA a2
    Awhile ann p f a             -> rnfAnn ann `seq` rnfAF p `seq` rnfAF f `seq` rnfA a
    Use ann repr arr             -> rnfAnn ann `seq` rnfArray repr arr
    Unit ann tp x                -> rnfAnn ann `seq` rnfTypeR tp `seq` rnfE x
    Reshape ann shr sh a         -> rnfAnn ann `seq` rnfShapeR shr `seq` rnfE sh `seq` rnfA a
    Generate ann repr sh f       -> rnfAnn ann `seq` rnfArrayR repr `seq` rnfE sh `seq` rnfF f
    Transform ann repr sh p f a  -> rnfAnn ann `seq` rnfArrayR repr `seq` rnfE sh `seq` rnfF p `seq` rnfF f `seq` rnfA a
    Replicate ann slice sh a     -> rnfAnn ann `seq` rnfSliceIndex slice `seq` rnfE sh `seq` rnfA a
    Slice ann slice a sh         -> rnfAnn ann `seq` rnfSliceIndex slice `seq` rnfE sh `seq` rnfA a
    Map ann tp f a               -> rnfAnn ann `seq` rnfTypeR tp `seq` rnfF f `seq` rnfA a
    ZipWith ann tp f a1 a2       -> rnfAnn ann `seq` rnfTypeR tp `seq` rnfF f `seq` rnfA a1 `seq` rnfA a2
    Fold ann f z a               -> rnfAnn ann `seq` rnfF f `seq` rnfMaybe rnfE z `seq` rnfA a
    FoldSeg ann i f z a s        -> rnfAnn ann `seq` rnfIntegralType i `seq` rnfF f `seq` rnfMaybe rnfE z `seq` rnfA a `seq` rnfA s
    Scan  ann d f z a            -> rnfAnn ann `seq` d `seq` rnfF f `seq` rnfMaybe rnfE z `seq` rnfA a
    Scan' ann d f z a            -> rnfAnn ann `seq` d `seq` rnfF f `seq` rnfE z `seq` rnfA a
    Permute ann f d p a          -> rnfAnn ann `seq` rnfF f `seq` rnfA d `seq` rnfF p `seq` rnfA a
    Backpermute ann shr sh f a   -> rnfAnn ann `seq` rnfShapeR shr `seq` rnfE sh `seq` rnfF f `seq` rnfA a
    Stencil ann sr tp f b a      ->
      let
        TupRsingle (ArrayR shr _) = arraysR a
        repr                      = ArrayR shr $ stencilEltR sr
      in rnfAnn ann `seq` rnfStencilR sr `seq` rnfTupR rnfScalarType tp `seq` rnfF f `seq` rnfB repr b  `seq` rnfA a
    Stencil2 ann sr1 sr2 tp f b1 a1 b2 a2 ->
      let
        TupRsingle (ArrayR shr _) = arraysR a1
        repr1 = ArrayR shr $ stencilEltR sr1
        repr2 = ArrayR shr $ stencilEltR sr2
      in rnfAnn ann `seq` rnfStencilR sr1 `seq` rnfStencilR sr2 `seq` rnfTupR rnfScalarType tp `seq` rnfF f `seq` rnfB repr1 b1 `seq` rnfB repr2 b2 `seq` rnfA a1 `seq` rnfA a2

rnfArrayVar :: ArrayVar aenv a -> ()
rnfArrayVar = rnfVar rnfArrayR

rnfALeftHandSide :: ALeftHandSide arrs aenv aenv' -> ()
rnfALeftHandSide = rnfLeftHandSide rnfArrayR

rnfBoundary :: forall aenv sh e. ArrayR (Array sh e) -> Boundary aenv (Array sh e) -> ()
rnfBoundary _             Clamp        = ()
rnfBoundary _             Mirror       = ()
rnfBoundary _             Wrap         = ()
rnfBoundary (ArrayR _ tR) (Constant c) = rnfConst tR c
rnfBoundary _             (Function f) = rnfOpenFun f

rnfMaybe :: (a -> ()) -> Maybe a -> ()
rnfMaybe _ Nothing  = ()
rnfMaybe f (Just x) = f x

rnfList :: (a -> ()) -> [a] -> ()
rnfList r = go
  where
    go []     = ()
    go (x:xs) = r x `seq` go xs

rnfOpenFun :: OpenFun env aenv t -> ()
rnfOpenFun (Body b)    = rnfOpenExp b
rnfOpenFun (Lam lhs f) = rnfELeftHandSide lhs `seq` rnfOpenFun f

rnfOpenExp :: forall env aenv t. OpenExp env aenv t -> ()
rnfOpenExp topExp =
  let
      rnfF :: OpenFun env' aenv' t' -> ()
      rnfF = rnfOpenFun

      rnfE :: OpenExp env' aenv' t' -> ()
      rnfE = rnfOpenExp
  in
  case topExp of
    Let ann lhs bnd body         -> rnfAnn ann `seq` rnfELeftHandSide lhs `seq` rnfE bnd `seq` rnfE body
    Evar v                       -> rnfExpVar v
    Foreign ann tp asm f x       -> rnfAnn ann `seq` rnfTypeR tp `seq` rnf (strForeign asm) `seq` rnfF f `seq` rnfE x
    Const ann tp c               -> rnfAnn ann `seq` c `seq` rnfScalarType tp -- scalars should have (nf == whnf)
    Undef ann tp                 -> rnfAnn ann `seq` rnfScalarType tp
    Pair ann a b                 -> rnfAnn ann `seq` rnfE a `seq` rnfE b
    Nil ann                      -> rnfAnn ann
    VecPack   ann vecr e         -> rnfAnn ann `seq` rnfVecR vecr `seq` rnfE e
    VecUnpack ann vecr e         -> rnfAnn ann `seq` rnfVecR vecr `seq` rnfE e
    IndexSlice ann slice slix sh -> rnfAnn ann `seq` rnfSliceIndex slice `seq` rnfE slix `seq` rnfE sh
    IndexFull ann slice slix sl  -> rnfAnn ann `seq` rnfSliceIndex slice `seq` rnfE slix `seq` rnfE sl
    ToIndex ann shr sh ix        -> rnfAnn ann `seq` rnfShapeR shr `seq` rnfE sh `seq` rnfE ix
    FromIndex ann shr sh ix      -> rnfAnn ann `seq` rnfShapeR shr `seq` rnfE sh `seq` rnfE ix
    Case ann e rhs def           -> rnfAnn ann `seq` rnfE e `seq` rnfList (\(t,c) -> t `seq` rnfE c) rhs `seq` rnfMaybe rnfE def
    Cond ann p e1 e2             -> rnfAnn ann `seq` rnfE p `seq` rnfE e1 `seq` rnfE e2
    While ann p f x              -> rnfAnn ann `seq` rnfF p `seq` rnfF f `seq` rnfE x
    PrimConst ann c              -> rnfAnn ann `seq` rnfPrimConst c
    PrimApp ann f x              -> rnfAnn ann `seq` rnfPrimFun f `seq` rnfE x
    Index ann a ix               -> rnfAnn ann `seq` rnfArrayVar a `seq` rnfE ix
    LinearIndex ann a ix         -> rnfAnn ann `seq` rnfArrayVar a `seq` rnfE ix
    Shape ann a                  -> rnfAnn ann `seq` rnfArrayVar a
    ShapeSize ann shr sh         -> rnfAnn ann `seq` rnfShapeR shr `seq` rnfE sh
    Coerce ann t1 t2 e           -> rnfAnn ann `seq` rnfScalarType t1 `seq` rnfScalarType t2 `seq` rnfE e

rnfExpVar :: ExpVar env t -> ()
rnfExpVar = rnfVar rnfScalarType

rnfELeftHandSide :: ELeftHandSide t env env' -> ()
rnfELeftHandSide= rnfLeftHandSide rnfScalarType

rnfConst :: TypeR t -> t -> ()
rnfConst TupRunit          ()    = ()
rnfConst (TupRsingle t)    !_    = rnfScalarType t  -- scalars should have (nf == whnf)
rnfConst (TupRpair ta tb)  (a,b) = rnfConst ta a `seq` rnfConst tb b

rnfPrimConst :: PrimConst c -> ()
rnfPrimConst (PrimMinBound t) = rnfBoundedType t
rnfPrimConst (PrimMaxBound t) = rnfBoundedType t
rnfPrimConst (PrimPi t)       = rnfFloatingType t

rnfPrimFun :: PrimFun f -> ()
rnfPrimFun (PrimAdd t)                = rnfNumType t
rnfPrimFun (PrimSub t)                = rnfNumType t
rnfPrimFun (PrimMul t)                = rnfNumType t
rnfPrimFun (PrimNeg t)                = rnfNumType t
rnfPrimFun (PrimAbs t)                = rnfNumType t
rnfPrimFun (PrimSig t)                = rnfNumType t
rnfPrimFun (PrimQuot t)               = rnfIntegralType t
rnfPrimFun (PrimRem t)                = rnfIntegralType t
rnfPrimFun (PrimQuotRem t)            = rnfIntegralType t
rnfPrimFun (PrimIDiv t)               = rnfIntegralType t
rnfPrimFun (PrimMod t)                = rnfIntegralType t
rnfPrimFun (PrimDivMod t)             = rnfIntegralType t
rnfPrimFun (PrimBAnd t)               = rnfIntegralType t
rnfPrimFun (PrimBOr t)                = rnfIntegralType t
rnfPrimFun (PrimBXor t)               = rnfIntegralType t
rnfPrimFun (PrimBNot t)               = rnfIntegralType t
rnfPrimFun (PrimBShiftL t)            = rnfIntegralType t
rnfPrimFun (PrimBShiftR t)            = rnfIntegralType t
rnfPrimFun (PrimBRotateL t)           = rnfIntegralType t
rnfPrimFun (PrimBRotateR t)           = rnfIntegralType t
rnfPrimFun (PrimPopCount t)           = rnfIntegralType t
rnfPrimFun (PrimCountLeadingZeros t)  = rnfIntegralType t
rnfPrimFun (PrimCountTrailingZeros t) = rnfIntegralType t
rnfPrimFun (PrimFDiv t)               = rnfFloatingType t
rnfPrimFun (PrimRecip t)              = rnfFloatingType t
rnfPrimFun (PrimSin t)                = rnfFloatingType t
rnfPrimFun (PrimCos t)                = rnfFloatingType t
rnfPrimFun (PrimTan t)                = rnfFloatingType t
rnfPrimFun (PrimAsin t)               = rnfFloatingType t
rnfPrimFun (PrimAcos t)               = rnfFloatingType t
rnfPrimFun (PrimAtan t)               = rnfFloatingType t
rnfPrimFun (PrimSinh t)               = rnfFloatingType t
rnfPrimFun (PrimCosh t)               = rnfFloatingType t
rnfPrimFun (PrimTanh t)               = rnfFloatingType t
rnfPrimFun (PrimAsinh t)              = rnfFloatingType t
rnfPrimFun (PrimAcosh t)              = rnfFloatingType t
rnfPrimFun (PrimAtanh t)              = rnfFloatingType t
rnfPrimFun (PrimExpFloating t)        = rnfFloatingType t
rnfPrimFun (PrimSqrt t)               = rnfFloatingType t
rnfPrimFun (PrimLog t)                = rnfFloatingType t
rnfPrimFun (PrimFPow t)               = rnfFloatingType t
rnfPrimFun (PrimLogBase t)            = rnfFloatingType t
rnfPrimFun (PrimTruncate f i)         = rnfFloatingType f `seq` rnfIntegralType i
rnfPrimFun (PrimRound f i)            = rnfFloatingType f `seq` rnfIntegralType i
rnfPrimFun (PrimFloor f i)            = rnfFloatingType f `seq` rnfIntegralType i
rnfPrimFun (PrimCeiling f i)          = rnfFloatingType f `seq` rnfIntegralType i
rnfPrimFun (PrimIsNaN t)              = rnfFloatingType t
rnfPrimFun (PrimIsInfinite t)         = rnfFloatingType t
rnfPrimFun (PrimAtan2 t)              = rnfFloatingType t
rnfPrimFun (PrimLt t)                 = rnfSingleType t
rnfPrimFun (PrimGt t)                 = rnfSingleType t
rnfPrimFun (PrimLtEq t)               = rnfSingleType t
rnfPrimFun (PrimGtEq t)               = rnfSingleType t
rnfPrimFun (PrimEq t)                 = rnfSingleType t
rnfPrimFun (PrimNEq t)                = rnfSingleType t
rnfPrimFun (PrimMax t)                = rnfSingleType t
rnfPrimFun (PrimMin t)                = rnfSingleType t
rnfPrimFun PrimLAnd                   = ()
rnfPrimFun PrimLOr                    = ()
rnfPrimFun PrimLNot                   = ()
rnfPrimFun (PrimFromIntegral i n)     = rnfIntegralType i `seq` rnfNumType n
rnfPrimFun (PrimToFloating n f)       = rnfNumType n `seq` rnfFloatingType f


-- Template Haskell
-- ================

type LiftAcc acc = forall aenv a. acc aenv a -> CodeQ (acc aenv a)

liftPreOpenAfun :: LiftAcc acc -> PreOpenAfun acc aenv t -> CodeQ (PreOpenAfun acc aenv t)
liftPreOpenAfun liftA (Alam lhs f) = [|| Alam $$(liftALeftHandSide lhs) $$(liftPreOpenAfun liftA f) ||]
liftPreOpenAfun liftA (Abody b)    = [|| Abody $$(liftA b) ||]

liftPreOpenAcc
    :: forall acc aenv a.
       HasArraysR acc
    => LiftAcc acc
    -> PreOpenAcc acc aenv a
    -> CodeQ (PreOpenAcc acc aenv a)
liftPreOpenAcc liftA pacc =
  let
      liftE :: OpenExp env aenv t -> CodeQ (OpenExp env aenv t)
      liftE = liftOpenExp

      liftF :: OpenFun env aenv t -> CodeQ (OpenFun env aenv t)
      liftF = liftOpenFun

      liftAF :: PreOpenAfun acc aenv f -> CodeQ (PreOpenAfun acc aenv f)
      liftAF = liftPreOpenAfun liftA

      liftB :: ArrayR (Array sh e) -> Boundary aenv (Array sh e) -> CodeQ (Boundary aenv (Array sh e))
      liftB = liftBoundary
  in
  case pacc of
    Alet ann lhs bnd body                 -> [|| Alet $$(liftAnn ann) $$(liftALeftHandSide lhs) $$(liftA bnd) $$(liftA body) ||]
    Avar var                              -> [|| Avar $$(liftArrayVar var) ||]
    Apair ann as bs                       -> [|| Apair $$(liftAnn ann) $$(liftA as) $$(liftA bs) ||]
    Anil ann                              -> [|| Anil $$(liftAnn ann) ||]
    Atrace ann msg as bs                  -> [|| Atrace $$(liftAnn ann) $$(liftMessage (arraysR as) msg) $$(liftA as) $$(liftA bs) ||]
    Apply ann repr f a                    -> [|| Apply $$(liftAnn ann) $$(liftArraysR repr) $$(liftAF f) $$(liftA a) ||]
    Aforeign ann repr asm f a             -> [|| Aforeign $$(liftAnn ann) $$(liftArraysR repr) $$(liftForeign asm) $$(liftPreOpenAfun liftA f) $$(liftA a) ||]
    Acond ann p t e                       -> [|| Acond $$(liftAnn ann) $$(liftE p) $$(liftA t) $$(liftA e) ||]
    Awhile ann p f a                      -> [|| Awhile $$(liftAnn ann) $$(liftAF p) $$(liftAF f) $$(liftA a) ||]
    Use ann repr a                        -> [|| Use $$(liftAnn ann) $$(liftArrayR repr) $$(liftArray repr a) ||]
    Unit ann tp e                         -> [|| Unit $$(liftAnn ann) $$(liftTypeR tp) $$(liftE e) ||]
    Reshape ann shr sh a                  -> [|| Reshape $$(liftAnn ann) $$(liftShapeR shr) $$(liftE sh) $$(liftA a) ||]
    Generate ann repr sh f                -> [|| Generate $$(liftAnn ann) $$(liftArrayR repr) $$(liftE sh) $$(liftF f) ||]
    Transform ann repr sh p f a           -> [|| Transform $$(liftAnn ann) $$(liftArrayR repr) $$(liftE sh) $$(liftF p) $$(liftF f) $$(liftA a) ||]
    Replicate ann slix sl a               -> [|| Replicate $$(liftAnn ann) $$(liftSliceIndex slix) $$(liftE sl) $$(liftA a) ||]
    Slice ann slix a sh                   -> [|| Slice $$(liftAnn ann) $$(liftSliceIndex slix) $$(liftA a) $$(liftE sh) ||]
    Map ann tp f a                        -> [|| Map $$(liftAnn ann) $$(liftTypeR tp) $$(liftF f) $$(liftA a) ||]
    ZipWith ann tp f a b                  -> [|| ZipWith $$(liftAnn ann) $$(liftTypeR tp) $$(liftF f) $$(liftA a) $$(liftA b) ||]
    Fold ann f z a                        -> [|| Fold $$(liftAnn ann) $$(liftF f) $$(liftMaybe liftE z) $$(liftA a) ||]
    FoldSeg ann i f z a s                 -> [|| FoldSeg $$(liftAnn ann) $$(liftIntegralType i) $$(liftF f) $$(liftMaybe liftE z) $$(liftA a) $$(liftA s) ||]
    Scan  ann d f z a                     -> [|| Scan $$(liftAnn ann)  $$(liftDirection d) $$(liftF f) $$(liftMaybe liftE z) $$(liftA a) ||]
    Scan' ann d f z a                     -> [|| Scan' $$(liftAnn ann) $$(liftDirection d) $$(liftF f) $$(liftE z) $$(liftA a) ||]
    Permute ann f d p a                   -> [|| Permute $$(liftAnn ann) $$(liftF f) $$(liftA d) $$(liftF p) $$(liftA a) ||]
    Backpermute ann shr sh p a            -> [|| Backpermute $$(liftAnn ann) $$(liftShapeR shr) $$(liftE sh) $$(liftF p) $$(liftA a) ||]
    Stencil ann sr tp f b a               ->
      let TupRsingle (ArrayR shr _) = arraysR a
          repr = ArrayR shr $ stencilEltR sr
       in [|| Stencil $$(liftAnn ann) $$(liftStencilR sr) $$(liftTypeR tp) $$(liftF f) $$(liftB repr b) $$(liftA a) ||]
    Stencil2 ann sr1 sr2 tp f b1 a1 b2 a2 ->
      let TupRsingle (ArrayR shr _) = arraysR a1
          repr1 = ArrayR shr $ stencilEltR sr1
          repr2 = ArrayR shr $ stencilEltR sr2
       in [|| Stencil2 $$(liftAnn ann) $$(liftStencilR sr1) $$(liftStencilR sr2) $$(liftTypeR tp) $$(liftF f) $$(liftB repr1 b1) $$(liftA a1) $$(liftB repr2 b2) $$(liftA a2) ||]


liftALeftHandSide :: ALeftHandSide arrs aenv aenv' -> CodeQ (ALeftHandSide arrs aenv aenv')
liftALeftHandSide = liftLeftHandSide liftArrayR

liftArrayVar :: ArrayVar aenv a -> CodeQ (ArrayVar aenv a)
liftArrayVar = liftVar liftArrayR

liftDirection :: Direction -> CodeQ Direction
liftDirection LeftToRight = [|| LeftToRight ||]
liftDirection RightToLeft = [|| RightToLeft ||]

liftMessage :: ArraysR a -> Message a -> CodeQ (Message a)
liftMessage aR (Message _ fmt msg) =
  let
      -- We (ironically?) can't lift TExp, so nested occurrences must fall
      -- back to displaying in representation format
      fmtR :: ArraysR arrs' -> CodeQ (arrs' -> String)
      fmtR TupRunit                         = [|| \() -> "()" ||]
      fmtR (TupRsingle (ArrayR ShapeRz eR)) = [|| \as -> showElt $$(liftTypeR eR) $ linearIndexArray $$(liftTypeR eR) as 0 ||]
      fmtR (TupRsingle (ArrayR shR eR))     = [|| \as -> showArray (showsElt $$(liftTypeR eR)) (ArrayR $$(liftShapeR shR) $$(liftTypeR eR)) as ||]
      fmtR aR'                              = [|| \as -> showArrays $$(liftArraysR aR') as ||]
  in
  [|| Message $$(fromMaybe (fmtR aR) fmt) Nothing $$(TH.unsafeCodeCoerce (TH.lift msg)) ||]

liftMaybe :: (a -> CodeQ a) -> Maybe a -> CodeQ (Maybe a)
liftMaybe _ Nothing  = [|| Nothing ||]
liftMaybe f (Just x) = [|| Just $$(f x) ||]

liftList :: (a -> CodeQ a) -> [a] -> CodeQ [a]
liftList _ []     = [|| [] ||]
liftList f (x:xs) = [|| $$(f x) : $$(liftList f xs) ||]

liftOpenFun
    :: OpenFun env aenv t
    -> CodeQ (OpenFun env aenv t)
liftOpenFun (Lam lhs f)  = [|| Lam $$(liftELeftHandSide lhs) $$(liftOpenFun f) ||]
liftOpenFun (Body b)     = [|| Body $$(liftOpenExp b) ||]

liftOpenExp
    :: forall env aenv t.
       OpenExp env aenv t
    -> CodeQ (OpenExp env aenv t)
liftOpenExp pexp =
  let
      liftE :: OpenExp env aenv e -> CodeQ (OpenExp env aenv e)
      liftE = liftOpenExp

      liftF :: OpenFun env aenv f -> CodeQ (OpenFun env aenv f)
      liftF = liftOpenFun
  in
  case pexp of
    Let ann lhs bnd body         -> [|| Let $$(liftAnn ann) $$(liftELeftHandSide lhs) $$(liftOpenExp bnd) $$(liftOpenExp body) ||]
    Evar var                     -> [|| Evar $$(liftExpVar var) ||]
    Foreign ann repr asm f x     -> [|| Foreign $$(liftAnn ann) $$(liftTypeR repr) $$(liftForeign asm) $$(liftOpenFun f) $$(liftE x) ||]
    Const ann tp c               -> [|| Const $$(liftAnn ann) $$(liftScalarType tp) $$(liftElt (TupRsingle tp) c) ||]
    Undef ann tp                 -> [|| Undef $$(liftAnn ann) $$(liftScalarType tp) ||]
    Pair ann a b                 -> [|| Pair $$(liftAnn ann) $$(liftE a) $$(liftE b) ||]
    Nil ann                      -> [|| Nil $$(liftAnn ann) ||]
    VecPack   ann vecr e         -> [|| VecPack   $$(liftAnn ann) $$(liftVecR vecr) $$(liftE e) ||]
    VecUnpack ann vecr e         -> [|| VecUnpack $$(liftAnn ann) $$(liftVecR vecr) $$(liftE e) ||]
    IndexSlice ann slice slix sh -> [|| IndexSlice $$(liftAnn ann) $$(liftSliceIndex slice) $$(liftE slix) $$(liftE sh) ||]
    IndexFull ann slice slix sl  -> [|| IndexFull $$(liftAnn ann) $$(liftSliceIndex slice) $$(liftE slix) $$(liftE sl) ||]
    ToIndex ann shr sh ix        -> [|| ToIndex $$(liftAnn ann) $$(liftShapeR shr) $$(liftE sh) $$(liftE ix) ||]
    FromIndex ann shr sh ix      -> [|| FromIndex $$(liftAnn ann) $$(liftShapeR shr) $$(liftE sh) $$(liftE ix) ||]
    Case ann p rhs def           -> [|| Case $$(liftAnn ann) $$(liftE p) $$(liftList (\(t,c) -> [|| (t, $$(liftE c)) ||]) rhs) $$(liftMaybe liftE def) ||]
    Cond ann p t e               -> [|| Cond $$(liftAnn ann) $$(liftE p) $$(liftE t) $$(liftE e) ||]
    While ann p f x              -> [|| While $$(liftAnn ann) $$(liftF p) $$(liftF f) $$(liftE x) ||]
    PrimConst ann t              -> [|| PrimConst $$(liftAnn ann) $$(liftPrimConst t) ||]
    PrimApp ann f x              -> [|| PrimApp $$(liftAnn ann) $$(liftPrimFun f) $$(liftE x) ||]
    Index ann a ix               -> [|| Index $$(liftAnn ann) $$(liftArrayVar a) $$(liftE ix) ||]
    LinearIndex ann a ix         -> [|| LinearIndex $$(liftAnn ann) $$(liftArrayVar a) $$(liftE ix) ||]
    Shape ann a                  -> [|| Shape $$(liftAnn ann) $$(liftArrayVar a) ||]
    ShapeSize ann shr ix         -> [|| ShapeSize $$(liftAnn ann) $$(liftShapeR shr) $$(liftE ix) ||]
    Coerce ann t1 t2 e           -> [|| Coerce $$(liftAnn ann) $$(liftScalarType t1) $$(liftScalarType t2) $$(liftE e) ||]

liftELeftHandSide :: ELeftHandSide t env env' -> CodeQ (ELeftHandSide t env env')
liftELeftHandSide = liftLeftHandSide liftScalarType

liftExpVar :: ExpVar env t -> CodeQ (ExpVar env t)
liftExpVar = liftVar liftScalarType

liftBoundary
    :: forall aenv sh e.
       ArrayR (Array sh e)
    -> Boundary aenv (Array sh e)
    -> CodeQ (Boundary aenv (Array sh e))
liftBoundary _             Clamp        = [|| Clamp ||]
liftBoundary _             Mirror       = [|| Mirror ||]
liftBoundary _             Wrap         = [|| Wrap ||]
liftBoundary (ArrayR _ tp) (Constant v) = [|| Constant $$(liftElt tp v) ||]
liftBoundary _             (Function f) = [|| Function $$(liftOpenFun f) ||]

liftPrimConst :: PrimConst c -> CodeQ (PrimConst c)
liftPrimConst (PrimMinBound t) = [|| PrimMinBound $$(liftBoundedType t) ||]
liftPrimConst (PrimMaxBound t) = [|| PrimMaxBound $$(liftBoundedType t) ||]
liftPrimConst (PrimPi t)       = [|| PrimPi $$(liftFloatingType t) ||]

liftPrimFun :: PrimFun f -> CodeQ (PrimFun f)
liftPrimFun (PrimAdd t)                = [|| PrimAdd $$(liftNumType t) ||]
liftPrimFun (PrimSub t)                = [|| PrimSub $$(liftNumType t) ||]
liftPrimFun (PrimMul t)                = [|| PrimMul $$(liftNumType t) ||]
liftPrimFun (PrimNeg t)                = [|| PrimNeg $$(liftNumType t) ||]
liftPrimFun (PrimAbs t)                = [|| PrimAbs $$(liftNumType t) ||]
liftPrimFun (PrimSig t)                = [|| PrimSig $$(liftNumType t) ||]
liftPrimFun (PrimQuot t)               = [|| PrimQuot $$(liftIntegralType t) ||]
liftPrimFun (PrimRem t)                = [|| PrimRem $$(liftIntegralType t) ||]
liftPrimFun (PrimQuotRem t)            = [|| PrimQuotRem $$(liftIntegralType t) ||]
liftPrimFun (PrimIDiv t)               = [|| PrimIDiv $$(liftIntegralType t) ||]
liftPrimFun (PrimMod t)                = [|| PrimMod $$(liftIntegralType t) ||]
liftPrimFun (PrimDivMod t)             = [|| PrimDivMod $$(liftIntegralType t) ||]
liftPrimFun (PrimBAnd t)               = [|| PrimBAnd $$(liftIntegralType t) ||]
liftPrimFun (PrimBOr t)                = [|| PrimBOr $$(liftIntegralType t) ||]
liftPrimFun (PrimBXor t)               = [|| PrimBXor $$(liftIntegralType t) ||]
liftPrimFun (PrimBNot t)               = [|| PrimBNot $$(liftIntegralType t) ||]
liftPrimFun (PrimBShiftL t)            = [|| PrimBShiftL $$(liftIntegralType t) ||]
liftPrimFun (PrimBShiftR t)            = [|| PrimBShiftR $$(liftIntegralType t) ||]
liftPrimFun (PrimBRotateL t)           = [|| PrimBRotateL $$(liftIntegralType t) ||]
liftPrimFun (PrimBRotateR t)           = [|| PrimBRotateR $$(liftIntegralType t) ||]
liftPrimFun (PrimPopCount t)           = [|| PrimPopCount $$(liftIntegralType t) ||]
liftPrimFun (PrimCountLeadingZeros t)  = [|| PrimCountLeadingZeros $$(liftIntegralType t) ||]
liftPrimFun (PrimCountTrailingZeros t) = [|| PrimCountTrailingZeros $$(liftIntegralType t) ||]
liftPrimFun (PrimFDiv t)               = [|| PrimFDiv $$(liftFloatingType t) ||]
liftPrimFun (PrimRecip t)              = [|| PrimRecip $$(liftFloatingType t) ||]
liftPrimFun (PrimSin t)                = [|| PrimSin $$(liftFloatingType t) ||]
liftPrimFun (PrimCos t)                = [|| PrimCos $$(liftFloatingType t) ||]
liftPrimFun (PrimTan t)                = [|| PrimTan $$(liftFloatingType t) ||]
liftPrimFun (PrimAsin t)               = [|| PrimAsin $$(liftFloatingType t) ||]
liftPrimFun (PrimAcos t)               = [|| PrimAcos $$(liftFloatingType t) ||]
liftPrimFun (PrimAtan t)               = [|| PrimAtan $$(liftFloatingType t) ||]
liftPrimFun (PrimSinh t)               = [|| PrimSinh $$(liftFloatingType t) ||]
liftPrimFun (PrimCosh t)               = [|| PrimCosh $$(liftFloatingType t) ||]
liftPrimFun (PrimTanh t)               = [|| PrimTanh $$(liftFloatingType t) ||]
liftPrimFun (PrimAsinh t)              = [|| PrimAsinh $$(liftFloatingType t) ||]
liftPrimFun (PrimAcosh t)              = [|| PrimAcosh $$(liftFloatingType t) ||]
liftPrimFun (PrimAtanh t)              = [|| PrimAtanh $$(liftFloatingType t) ||]
liftPrimFun (PrimExpFloating t)        = [|| PrimExpFloating $$(liftFloatingType t) ||]
liftPrimFun (PrimSqrt t)               = [|| PrimSqrt $$(liftFloatingType t) ||]
liftPrimFun (PrimLog t)                = [|| PrimLog $$(liftFloatingType t) ||]
liftPrimFun (PrimFPow t)               = [|| PrimFPow $$(liftFloatingType t) ||]
liftPrimFun (PrimLogBase t)            = [|| PrimLogBase $$(liftFloatingType t) ||]
liftPrimFun (PrimTruncate ta tb)       = [|| PrimTruncate $$(liftFloatingType ta) $$(liftIntegralType tb) ||]
liftPrimFun (PrimRound ta tb)          = [|| PrimRound $$(liftFloatingType ta) $$(liftIntegralType tb) ||]
liftPrimFun (PrimFloor ta tb)          = [|| PrimFloor $$(liftFloatingType ta) $$(liftIntegralType tb) ||]
liftPrimFun (PrimCeiling ta tb)        = [|| PrimCeiling $$(liftFloatingType ta) $$(liftIntegralType tb) ||]
liftPrimFun (PrimIsNaN t)              = [|| PrimIsNaN $$(liftFloatingType t) ||]
liftPrimFun (PrimIsInfinite t)         = [|| PrimIsInfinite $$(liftFloatingType t) ||]
liftPrimFun (PrimAtan2 t)              = [|| PrimAtan2 $$(liftFloatingType t) ||]
liftPrimFun (PrimLt t)                 = [|| PrimLt $$(liftSingleType t) ||]
liftPrimFun (PrimGt t)                 = [|| PrimGt $$(liftSingleType t) ||]
liftPrimFun (PrimLtEq t)               = [|| PrimLtEq $$(liftSingleType t) ||]
liftPrimFun (PrimGtEq t)               = [|| PrimGtEq $$(liftSingleType t) ||]
liftPrimFun (PrimEq t)                 = [|| PrimEq $$(liftSingleType t) ||]
liftPrimFun (PrimNEq t)                = [|| PrimNEq $$(liftSingleType t) ||]
liftPrimFun (PrimMax t)                = [|| PrimMax $$(liftSingleType t) ||]
liftPrimFun (PrimMin t)                = [|| PrimMin $$(liftSingleType t) ||]
liftPrimFun PrimLAnd                   = [|| PrimLAnd ||]
liftPrimFun PrimLOr                    = [|| PrimLOr ||]
liftPrimFun PrimLNot                   = [|| PrimLNot ||]
liftPrimFun (PrimFromIntegral ta tb)   = [|| PrimFromIntegral $$(liftIntegralType ta) $$(liftNumType tb) ||]
liftPrimFun (PrimToFloating ta tb)     = [|| PrimToFloating $$(liftNumType ta) $$(liftFloatingType tb) ||]


formatDirection :: Format r (Direction -> r)
formatDirection = later $ \case
  LeftToRight -> singleton 'l'
  RightToLeft -> singleton 'r'

-- TODO: Should we print anything from the annotations here?
formatPreAccOp :: Format r (PreOpenAcc acc aenv arrs -> r)
formatPreAccOp = later $ \case
  Alet{}              -> "Alet"
  Avar (Var _ _ ix)   -> bformat ("Avar a" % int) (idxToInt ix)
  Use _ aR a          -> bformat ("Use " % string) (showArrayShort 5 (showsElt (arrayRtype aR)) aR a)
  Atrace{}            -> "Atrace"
  Apply{}             -> "Apply"
  Aforeign{}          -> "Aforeign"
  Acond{}             -> "Acond"
  Awhile{}            -> "Awhile"
  Apair{}             -> "Apair"
  Anil{}              -> "Anil"
  Unit{}              -> "Unit"
  Generate{}          -> "Generate"
  Transform{}         -> "Transform"
  Reshape{}           -> "Reshape"
  Replicate{}         -> "Replicate"
  Slice{}             -> "Slice"
  Map{}               -> "Map"
  ZipWith{}           -> "ZipWith"
  Fold _ _ z _        -> bformat ("Fold" % maybed "1" (fconst mempty)) z
  FoldSeg _ _ _ z _ _ -> bformat ("Fold" % maybed "1" (fconst mempty) % "Seg") z
  Scan  _ d _ z _     -> bformat ("Scan" % formatDirection % maybed "1" (fconst mempty)) d z
  Scan' _ d _ _ _     -> bformat ("Scan" % formatDirection % "\'") d
  Permute{}           -> "Permute"
  Backpermute{}       -> "Backpermute"
  Stencil{}           -> "Stencil"
  Stencil2{}          -> "Stencil2"

-- TODO: Same as with 'formatPreAccOp'
formatExpOp :: Format r (OpenExp aenv env t -> r)
formatExpOp = later $ \case
  Let{}             -> "Let"
  Evar (Var _ _ ix) -> bformat ("Var x" % int) (idxToInt ix)
  Const _ tp c      -> bformat ("Const " % string) (showElt (TupRsingle tp) c)
  Undef{}           -> "Undef"
  Foreign{}         -> "Foreign"
  Pair{}            -> "Pair"
  Nil{}             -> "Nil"
  VecPack{}         -> "VecPack"
  VecUnpack{}       -> "VecUnpack"
  IndexSlice{}      -> "IndexSlice"
  IndexFull{}       -> "IndexFull"
  ToIndex{}         -> "ToIndex"
  FromIndex{}       -> "FromIndex"
  Case{}            -> "Case"
  Cond{}            -> "Cond"
  While{}           -> "While"
  PrimConst{}       -> "PrimConst"
  PrimApp{}         -> "PrimApp"
  Index{}           -> "Index"
  LinearIndex{}     -> "LinearIndex"
  Shape{}           -> "Shape"
  ShapeSize{}       -> "ShapeSize"
  Coerce{}          -> "Coerce"

