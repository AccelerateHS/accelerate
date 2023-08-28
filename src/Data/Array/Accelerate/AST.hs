{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
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
  Acc, OpenAcc(..), PreOpenAcc(..), Direction(..), Message(..), RescaleFactor,
  ALeftHandSide, ArrayVar, ArrayVars,

  -- ** Scalar expressions
  ELeftHandSide, ExpVar, ExpVars, expVars,
  Fun, OpenFun(..),
  Exp, OpenExp(..),
  Boundary(..),
  PrimFun(..),
  PrimBool, PrimMask,
  PrimMaybe,
  BitOrMask,

  -- ** Extracting type information
  HasArraysR(..), arrayR,
  expType,
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
  liftPrimFun,
  liftMessage,

  -- ** Miscellaneous
  formatPreAccOp,
  formatExpOp,

) where

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

type PrimBool    = Bit
type PrimMask n  = Vec n Bit
type PrimMaybe a = (TAG, ((), a))

-- Trace messages
data Message a where
  Message :: (a -> String)                    -- embedded show
          -> Maybe (CodeQ (a -> String))      -- lifted version of show, for TH
          -> Text
          -> Message a

-- Coercing an array to a different type may involve scaling the size of the
-- array by the given factor. Positive values mean the size of the innermost
-- dimension is multiplied by that value (i.e. the number of elements in the
-- array grows by that factor), negative meaning it shrinks.
--
type RescaleFactor = INT

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
  Alet        :: ALeftHandSide bndArrs aenv aenv'
              -> acc            aenv  bndArrs         -- bound expression
              -> acc            aenv' bodyArrs        -- the bound expression scope
              -> PreOpenAcc acc aenv  bodyArrs

  -- Variable bound by a 'Let', represented by a de Bruijn index
  --
  Avar        :: ArrayVar       aenv (Array sh e)
              -> PreOpenAcc acc aenv (Array sh e)

  -- Tuples of arrays
  --
  Apair       :: acc            aenv as
              -> acc            aenv bs
              -> PreOpenAcc acc aenv (as, bs)

  Anil        :: PreOpenAcc acc aenv ()

  -- Array-function application.
  --
  -- The array function is not closed at the core level because we need access
  -- to free variables introduced by 'run1' style evaluators. See Issue#95.
  --
  Apply       :: ArraysR arrs2
              -> PreOpenAfun acc aenv (arrs1 -> arrs2)
              -> acc             aenv arrs1
              -> PreOpenAcc  acc aenv arrs2

  -- Apply a backend-specific foreign function to an array, with a pure
  -- Accelerate version for use with other backends. The functions must be
  -- closed.
  --
  Aforeign    :: Foreign asm
              => ArraysR bs
              -> asm                   (as -> bs) -- The foreign function for a given backend
              -> PreAfun      acc      (as -> bs) -- Fallback implementation(s)
              -> acc              aenv as         -- Arguments to the function
              -> PreOpenAcc   acc aenv bs

  -- If-then-else for array-level computations
  --
  Acond       :: Exp            aenv PrimBool
              -> acc            aenv arrs
              -> acc            aenv arrs
              -> PreOpenAcc acc aenv arrs

  -- Value-recursion for array-level computations
  --
  Awhile      :: PreOpenAfun acc aenv (arrs -> Scalar PrimBool) -- continue iteration while true
              -> PreOpenAfun acc aenv (arrs -> arrs)            -- function to iterate
              -> acc             aenv arrs                      -- initial value
              -> PreOpenAcc  acc aenv arrs

  Atrace      :: Message              arrs1
              -> acc             aenv arrs1
              -> acc             aenv arrs2
              -> PreOpenAcc  acc aenv arrs2

  -- Reinterpret the bits of the array as a different type. The size of the
  -- innermost dimension is adjusted as necessary. The old and new sizes must be
  -- compatible, but this may not be checked; e.g. in the conversion
  --
  -- > Array (Z :. n) Float <~~> Array (Z :. m) (Vec 3 Float)
  --
  -- then we require
  --
  -- > (m, r) = quotRem n 3 and r == 0
  --
  Acoerce     :: RescaleFactor
              -> TypeR b
              -> acc            aenv (Array (sh, INT) a)
              -> PreOpenAcc acc aenv (Array (sh, INT) b)

  -- Array inlet. Triggers (possibly) asynchronous host->device transfer if
  -- necessary.
  --
  Use         :: ArrayR (Array sh e)
              -> Array sh e
              -> PreOpenAcc acc aenv (Array sh e)

  -- Capture a scalar (or a tuple of scalars) in a singleton array
  --
  Unit        :: TypeR e
              -> Exp            aenv e
              -> PreOpenAcc acc aenv (Scalar e)

  -- Change the shape of an array without altering its contents.
  -- Precondition (this may not be checked!):
  --
  -- > dim == size dim'
  --
  Reshape     :: ShapeR sh
              -> Exp            aenv sh                         -- new shape
              -> acc            aenv (Array sh' e)              -- array to be reshaped
              -> PreOpenAcc acc aenv (Array sh e)

  -- Construct a new array by applying a function to each index.
  --
  Generate    :: ArrayR (Array sh e)
              -> Exp            aenv sh                         -- output shape
              -> Fun            aenv (sh -> e)                  -- representation function
              -> PreOpenAcc acc aenv (Array sh e)

  -- Hybrid map/backpermute, where we separate the index and value
  -- transformations.
  --
  Transform   :: ArrayR (Array sh' b)
              -> Exp            aenv sh'                        -- dimension of the result
              -> Fun            aenv (sh' -> sh)                -- index permutation function
              -> Fun            aenv (a   -> b)                 -- function to apply at each element
              ->            acc aenv (Array sh  a)              -- source array
              -> PreOpenAcc acc aenv (Array sh' b)

  -- Replicate an array across one or more dimensions as given by the first
  -- argument
  --
  Replicate   :: SliceIndex slix sl co sh                       -- slice type specification
              -> Exp            aenv slix                       -- slice value specification
              -> acc            aenv (Array sl e)               -- data to be replicated
              -> PreOpenAcc acc aenv (Array sh e)

  -- Index a sub-array out of an array; i.e., the dimensions not indexed
  -- are returned whole
  --
  Slice       :: SliceIndex slix sl co sh                       -- slice type specification
              -> acc            aenv (Array sh e)               -- array to be indexed
              -> Exp            aenv slix                       -- slice value specification
              -> PreOpenAcc acc aenv (Array sl e)

  -- Apply the given unary function to all elements of the given array
  --
  Map         :: TypeR e'
              -> Fun            aenv (e -> e')
              -> acc            aenv (Array sh e)
              -> PreOpenAcc acc aenv (Array sh e')

  -- Apply a given binary function pairwise to all elements of the given
  -- arrays. The length of the result is the length of the shorter of the
  -- two argument arrays.
  --
  ZipWith     :: TypeR e3
              -> Fun            aenv (e1 -> e2 -> e3)
              -> acc            aenv (Array sh e1)
              -> acc            aenv (Array sh e2)
              -> PreOpenAcc acc aenv (Array sh e3)

  -- Fold along the innermost dimension of an array with a given
  -- /associative/ function.
  --
  Fold        :: Fun            aenv (e -> e -> e)              -- combination function
              -> Maybe     (Exp aenv e)                         -- default value
              -> acc            aenv (Array (sh, INT) e)        -- folded array
              -> PreOpenAcc acc aenv (Array sh e)

  -- Segmented fold along the innermost dimension of an array with a given
  -- /associative/ function
  --
  FoldSeg     :: SingleIntegralType i
              -> Fun            aenv (e -> e -> e)              -- combination function
              -> Maybe     (Exp aenv e)                         -- default value
              -> acc            aenv (Array (sh, INT) e)        -- folded array
              -> acc            aenv (Segments i)               -- segment descriptor
              -> PreOpenAcc acc aenv (Array (sh, INT) e)

  -- Haskell-style scan of a linear array with a given
  -- /associative/ function and optionally an initial element
  -- (which does not need to be the neutral of the associative operations)
  -- If no initial value is given, this is a scan1
  --
  Scan        :: Direction
              -> Fun            aenv (e -> e -> e)              -- combination function
              -> Maybe     (Exp aenv e)                         -- initial value
              -> acc            aenv (Array (sh, INT) e)
              -> PreOpenAcc acc aenv (Array (sh, INT) e)

  -- Like 'Scan', but produces a rightmost (in case of a left-to-right scan)
  -- fold value and an array with the same length as the input array (the
  -- fold value would be the rightmost element in a Haskell-style scan)
  --
  Scan'       :: Direction
              -> Fun            aenv (e -> e -> e)              -- combination function
              -> Exp            aenv e                          -- initial value
              -> acc            aenv (Array (sh, INT) e)
              -> PreOpenAcc acc aenv (Array (sh, INT) e, Array sh e)

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
  Permute     :: Fun            aenv (e -> e -> e)              -- combination function
              -> acc            aenv (Array sh' e)              -- default values
              -> Fun            aenv (sh -> PrimMaybe sh')      -- permutation function
              -> acc            aenv (Array sh e)               -- source array
              -> PreOpenAcc acc aenv (Array sh' e)

  -- Generalised multi-dimensional backwards permutation; the permutation can
  -- be between arrays of varying shape; the permutation function must be total
  --
  Backpermute :: ShapeR sh'
              -> Exp            aenv sh'                        -- dimensions of the result
              -> Fun            aenv (sh' -> sh)                -- permutation function
              -> acc            aenv (Array sh e)               -- source array
              -> PreOpenAcc acc aenv (Array sh' e)

  -- Map a stencil over an array.  In contrast to 'map', the domain of
  -- a stencil function is an entire /neighbourhood/ of each array element.
  --
  Stencil     :: StencilR sh e stencil
              -> TypeR e'
              -> Fun             aenv (stencil -> e')           -- stencil function
              -> Boundary        aenv (Array sh e)              -- boundary condition
              -> acc             aenv (Array sh e)              -- source array
              -> PreOpenAcc  acc aenv (Array sh e')

  -- Map a binary stencil over an array.
  --
  Stencil2    :: StencilR sh a stencil1
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

expVars :: ExpVars env t -> OpenExp env aenv t
expVars TupRunit         = Nil
expVars (TupRsingle var) = Evar var
expVars (TupRpair v1 v2) = expVars v1 `Pair` expVars v2


-- | Vanilla open expressions using de Bruijn indices for variables ranging
-- over tuples of scalars and arrays of tuples. All code, except Cond, is
-- evaluated eagerly. N-tuples are represented as nested pairs.
--
-- The data type is parametrised over the representation type (not the
-- surface types).
--
data OpenExp env aenv t where

  -- Local binding of a scalar expression
  Let           :: ELeftHandSide bnd_t env env'
                -> OpenExp env  aenv bnd_t
                -> OpenExp env' aenv body_t
                -> OpenExp env  aenv body_t

  -- Variable index, ranging _only_ over scalars
  Evar          :: ExpVar env t
                -> OpenExp env aenv t

  -- Apply a backend-specific foreign function
  Foreign       :: Foreign asm
                => TypeR y
                -> asm    (x -> y)    -- foreign function
                -> Fun () (x -> y)    -- alternate implementation (for other backends)
                -> OpenExp env aenv x
                -> OpenExp env aenv y

  -- Tuples
  Pair          :: OpenExp env aenv t1
                -> OpenExp env aenv t2
                -> OpenExp env aenv (t1, t2)

  Nil           :: OpenExp env aenv ()

  -- SIMD vectors
  Extract       :: ScalarType (Vec n a)
                -> SingleIntegralType i
                -> OpenExp env aenv (Vec n a)
                -> OpenExp env aenv i
                -> OpenExp env aenv a

  Insert        :: ScalarType (Vec n a)
                -> SingleIntegralType i
                -> OpenExp env aenv (Vec n a)
                -> OpenExp env aenv i
                -> OpenExp env aenv a
                -> OpenExp env aenv (Vec n a)

  Shuffle       :: ScalarType (Vec m a)
                -> SingleIntegralType i
                -> OpenExp env aenv (Vec n a)
                -> OpenExp env aenv (Vec n a)
                -> OpenExp env aenv (Vec m i)
                -> OpenExp env aenv (Vec m a)

  Select        :: ScalarType (Vec n a)
                -> OpenExp env aenv (PrimMask n)
                -> OpenExp env aenv (Vec n a)
                -> OpenExp env aenv (Vec n a)
                -> OpenExp env aenv (Vec n a)

  -- Array indices & shapes
  IndexSlice    :: SliceIndex slix sl co sh
                -> OpenExp env aenv slix
                -> OpenExp env aenv sh
                -> OpenExp env aenv sl

  IndexFull     :: SliceIndex slix sl co sh
                -> OpenExp env aenv slix
                -> OpenExp env aenv sl
                -> OpenExp env aenv sh

  -- Shape and index conversion
  ToIndex       :: ShapeR sh
                -> OpenExp env aenv sh           -- shape of the array
                -> OpenExp env aenv sh           -- index into the array
                -> OpenExp env aenv INT

  FromIndex     :: ShapeR sh
                -> OpenExp env aenv sh           -- shape of the array
                -> OpenExp env aenv INT          -- index into linear representation
                -> OpenExp env aenv sh

  -- Case statement
  Case          :: TagType tag
                -> OpenExp env aenv tag
                -> [(tag, OpenExp env aenv b)]      -- list of equations
                -> Maybe (OpenExp env aenv b)       -- default case
                -> OpenExp env aenv b

  -- Conditional expression (non-strict in 2nd and 3rd argument)
  Cond          :: OpenExp env aenv PrimBool
                -> OpenExp env aenv t
                -> OpenExp env aenv t
                -> OpenExp env aenv t

  -- Value recursion
  While         :: OpenFun env aenv (a -> PrimBool) -- continue while true
                -> OpenFun env aenv (a -> a)        -- function to iterate
                -> OpenExp env aenv a               -- initial value
                -> OpenExp env aenv a

  -- Constant values
  Const         :: ScalarType t
                -> t
                -> OpenExp env aenv t

  -- Primitive scalar operations
  PrimApp       :: PrimFun (a -> r)
                -> OpenExp env aenv a
                -> OpenExp env aenv r

  -- Project a single scalar from an array.
  -- The array expression can not contain any free scalar variables.
  Index         :: ArrayVar    aenv (Array dim t)
                -> OpenExp env aenv dim
                -> OpenExp env aenv t

  LinearIndex   :: ArrayVar    aenv (Array dim t)
                -> OpenExp env aenv INT
                -> OpenExp env aenv t

  -- Array shape.
  -- The array expression can not contain any free scalar variables.
  Shape         :: ArrayVar    aenv (Array dim e)
                -> OpenExp env aenv dim

  -- Number of elements of an array given its shape
  ShapeSize     :: ShapeR dim
                -> OpenExp env aenv dim
                -> OpenExp env aenv INT

  -- Unsafe operations (may fail or result in undefined behaviour)
  -- An unspecified bit pattern
  Undef         :: ScalarType t
                -> OpenExp env aenv t

  -- Reinterpret the bits of a value as a different type.
  --
  -- The types must have the same bit size, but that constraint is not include
  -- at this point because GHC's typelits solver is often not powerful enough to
  -- discharge that constraint. ---TLM 2022-09-20
  Bitcast       :: ScalarType a
                -> ScalarType b
                -> OpenExp env aenv a
                -> OpenExp env aenv b


-- | A bit mask at the width of the corresponding type
--
type family BitOrMask a where
  BitOrMask (Vec n _) = PrimMask n
  BitOrMask _         = PrimBool

-- | Primitive scalar operations
--
data PrimFun sig where

  -- operators from Num
  PrimAdd  :: NumType a -> PrimFun ((a, a) -> a)
  PrimSub  :: NumType a -> PrimFun ((a, a) -> a)
  PrimMul  :: NumType a -> PrimFun ((a, a) -> a)
  PrimNeg  :: NumType a -> PrimFun (a      -> a)
  PrimAbs  :: NumType a -> PrimFun (a      -> a)
  PrimSig  :: NumType a -> PrimFun (a      -> a)
  PrimVAdd :: NumType (Vec n a) -> PrimFun (Vec n a -> a)
  PrimVMul :: NumType (Vec n a) -> PrimFun (Vec n a -> a)

  -- operators from Integral
  PrimQuot     :: IntegralType a -> PrimFun ((a, a)   -> a)
  PrimRem      :: IntegralType a -> PrimFun ((a, a)   -> a)
  PrimQuotRem  :: IntegralType a -> PrimFun ((a, a)   -> (a, a))
  PrimIDiv     :: IntegralType a -> PrimFun ((a, a)   -> a)
  PrimMod      :: IntegralType a -> PrimFun ((a, a)   -> a)
  PrimDivMod   :: IntegralType a -> PrimFun ((a, a)   -> (a, a))

  -- operators from Bits & FiniteBits
  PrimBAnd               :: IntegralType a -> PrimFun ((a, a) -> a)
  PrimBOr                :: IntegralType a -> PrimFun ((a, a) -> a)
  PrimBXor               :: IntegralType a -> PrimFun ((a, a) -> a)
  PrimBNot               :: IntegralType a -> PrimFun (a      -> a)
  PrimBShiftL            :: IntegralType a -> PrimFun ((a, a) -> a)
  PrimBShiftR            :: IntegralType a -> PrimFun ((a, a) -> a)
  PrimBRotateL           :: IntegralType a -> PrimFun ((a, a) -> a)
  PrimBRotateR           :: IntegralType a -> PrimFun ((a, a) -> a)
  PrimPopCount           :: IntegralType a -> PrimFun (a -> a)
  PrimCountLeadingZeros  :: IntegralType a -> PrimFun (a -> a)
  PrimCountTrailingZeros :: IntegralType a -> PrimFun (a -> a)
  PrimBReverse           :: IntegralType a -> PrimFun (a -> a)
  PrimBSwap              :: IntegralType a -> PrimFun (a -> a)  -- prerequisite: BitSize a % 16 == 0
  PrimVBAnd              :: IntegralType (Vec n a) -> PrimFun (Vec n a -> a)
  PrimVBOr               :: IntegralType (Vec n a) -> PrimFun (Vec n a -> a)
  PrimVBXor              :: IntegralType (Vec n a) -> PrimFun (Vec n a -> a)

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

  -- operators from RealFrac
  PrimTruncate :: FloatingType a -> IntegralType b -> PrimFun (a -> b)
  PrimRound    :: FloatingType a -> IntegralType b -> PrimFun (a -> b)
  PrimFloor    :: FloatingType a -> IntegralType b -> PrimFun (a -> b)
  PrimCeiling  :: FloatingType a -> IntegralType b -> PrimFun (a -> b)
  -- PrimProperFraction :: FloatingType a -> IntegralType b -> PrimFun (a -> (b, a))

  -- operators from RealFloat
  PrimAtan2      :: FloatingType a -> PrimFun ((a, a) -> a)
  PrimIsNaN      :: FloatingType a -> PrimFun (a -> BitOrMask a)
  PrimIsInfinite :: FloatingType a -> PrimFun (a -> BitOrMask a)

  -- relational and equality operators
  PrimLt    :: ScalarType a -> PrimFun ((a, a) -> BitOrMask a)
  PrimGt    :: ScalarType a -> PrimFun ((a, a) -> BitOrMask a)
  PrimLtEq  :: ScalarType a -> PrimFun ((a, a) -> BitOrMask a)
  PrimGtEq  :: ScalarType a -> PrimFun ((a, a) -> BitOrMask a)
  PrimEq    :: ScalarType a -> PrimFun ((a, a) -> BitOrMask a)
  PrimNEq   :: ScalarType a -> PrimFun ((a, a) -> BitOrMask a)
  PrimMin   :: ScalarType a -> PrimFun ((a, a) -> a)
  PrimMax   :: ScalarType a -> PrimFun ((a, a) -> a)
  PrimVMin  :: ScalarType (Vec n a) -> PrimFun (Vec n a -> a)
  PrimVMax  :: ScalarType (Vec n a) -> PrimFun (Vec n a -> a)

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
  PrimLAnd  :: BitType a -> PrimFun ((a, a) -> a)
  PrimLOr   :: BitType a -> PrimFun ((a, a) -> a)
  PrimLNot  :: BitType a -> PrimFun (a      -> a)
  PrimVLAnd :: BitType (Vec n a) -> PrimFun (Vec n a -> a)
  PrimVLOr  :: BitType (Vec n a) -> PrimFun (Vec n a -> a)

  -- general conversion between types
  PrimFromIntegral :: IntegralType a -> NumType b -> PrimFun (a -> b)
  PrimToFloating   :: NumType a -> FloatingType b -> PrimFun (a -> b)
  PrimToBool       :: IntegralType a -> BitType b -> PrimFun (a -> b)
  PrimFromBool     :: BitType a -> IntegralType b -> PrimFun (a -> b)


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
  arraysR (Alet _ _ body)             = arraysR body
  arraysR (Avar (Var aR _))           = TupRsingle aR
  arraysR (Apair as bs)               = TupRpair (arraysR as) (arraysR bs)
  arraysR Anil                        = TupRunit
  arraysR (Atrace _ _ bs)             = arraysR bs
  arraysR (Acoerce _ bR as)           = let ArrayR shR _ = arrayR as
                                         in arraysRarray shR bR
  arraysR (Apply aR _ _)              = aR
  arraysR (Aforeign r _ _ _)          = r
  arraysR (Acond _ a _)               = arraysR a
  arraysR (Awhile _ (Alam lhs _) _)   = lhsToTupR lhs
  arraysR Awhile{}                    = error "I want my, I want my MTV!"
  arraysR (Use aR _)                  = TupRsingle aR
  arraysR (Unit tR _)                 = arraysRarray ShapeRz tR
  arraysR (Reshape sh _ a)            = let ArrayR _ tR = arrayR a
                                         in arraysRarray sh tR
  arraysR (Generate aR _ _)           = TupRsingle aR
  arraysR (Transform aR _ _ _ _)      = TupRsingle aR
  arraysR (Replicate slice _ a)       = let ArrayR _ tR = arrayR a
                                         in arraysRarray (sliceDomainR slice) tR
  arraysR (Slice slice a _)           = let ArrayR _ tR = arrayR a
                                         in arraysRarray (sliceShapeR slice) tR
  arraysR (Map tR _ a)                = let ArrayR sh _ = arrayR a
                                         in arraysRarray sh tR
  arraysR (ZipWith tR _ a _)          = let ArrayR sh _ = arrayR a
                                         in arraysRarray sh tR
  arraysR (Fold _ _ a)                = let ArrayR (ShapeRsnoc sh) tR = arrayR a
                                         in arraysRarray sh tR
  arraysR (FoldSeg _ _ _ a _)         = arraysR a
  arraysR (Scan _ _ _ a)              = arraysR a
  arraysR (Scan' _ _ _ a)             = let aR@(ArrayR (ShapeRsnoc sh) tR) = arrayR a
                                         in TupRsingle aR `TupRpair` TupRsingle (ArrayR sh tR)
  arraysR (Permute _ a _ _)           = arraysR a
  arraysR (Backpermute sh _ _ a)      = let ArrayR _ tR = arrayR a
                                         in arraysRarray sh tR
  arraysR (Stencil _ tR _ _ a)        = let ArrayR sh _ = arrayR a
                                         in arraysRarray sh tR
  arraysR (Stencil2 _ _ tR _ _ a _ _) = let ArrayR sh _ = arrayR a
                                         in arraysRarray sh tR

expType :: HasCallStack => OpenExp aenv env t -> TypeR t
expType = \case
  Let _ _ body                 -> expType body
  Evar (Var tR _)              -> TupRsingle tR
  Foreign tR _ _ _             -> tR
  Pair e1 e2                   -> TupRpair (expType e1) (expType e2)
  Nil                          -> TupRunit
  Extract tR _ _ _             -> TupRsingle (scalar tR)
    where
      scalar :: ScalarType (Vec n a) -> ScalarType a
      scalar (NumScalarType t) = NumScalarType (num t)
      scalar (BitScalarType t) = BitScalarType (bit t)

      bit :: BitType (Vec n a) -> BitType a
      bit TypeMask{} = TypeBit

      num :: NumType (Vec n a) -> NumType a
      num (IntegralNumType t) = IntegralNumType (integral t)
      num (FloatingNumType t) = FloatingNumType (floating t)

      integral :: IntegralType (Vec n a) -> IntegralType a
      integral (SingleIntegralType   t) = case t of
      integral (VectorIntegralType _ t) = SingleIntegralType t

      floating :: FloatingType (Vec n a) -> FloatingType a
      floating (SingleFloatingType   t) = case t of
      floating (VectorFloatingType _ t) = SingleFloatingType t
  --
  Insert t _ _ _ _             -> TupRsingle t
  Shuffle t _ _ _ _            -> TupRsingle t
  Select t _ _ _               -> TupRsingle t
  IndexSlice si _ _            -> shapeType $ sliceShapeR si
  IndexFull  si _ _            -> shapeType $ sliceDomainR si
  ToIndex{}                    -> TupRsingle scalarType
  FromIndex shR _ _            -> shapeType shR
  Case _ _ ((_,e):_) _         -> expType e
  Case _ _ [] (Just e)         -> expType e
  Case{}                       -> internalError "empty case encountered"
  Cond _ e _                   -> expType e
  While _ (Lam lhs _) _        -> lhsToTupR lhs
  While{}                      -> error "What's the matter, you're running in the shadows"
  Const tR _                   -> TupRsingle tR
  PrimApp f _                  -> snd $ primFunType f
  Index (Var repr _) _         -> arrayRtype repr
  LinearIndex (Var repr _) _   -> arrayRtype repr
  Shape (Var repr _)           -> shapeType $ arrayRshape repr
  ShapeSize{}                  -> TupRsingle (scalarType @INT)
  Undef tR                     -> TupRsingle tR
  Bitcast _ tR _               -> TupRsingle tR


primFunType :: PrimFun (a -> b) -> (TypeR a, TypeR b)
primFunType = \case
  -- Num
  PrimAdd t                 -> binary' $ num t
  PrimSub t                 -> binary' $ num t
  PrimMul t                 -> binary' $ num t
  PrimNeg t                 -> unary'  $ num t
  PrimAbs t                 -> unary'  $ num t
  PrimSig t                 -> unary'  $ num t
  PrimVAdd t                -> unary (num t) (scalar_num t)
  PrimVMul t                -> unary (num t) (scalar_num t)

  -- Integral
  PrimQuot t                -> binary' $ integral t
  PrimRem  t                -> binary' $ integral t
  PrimQuotRem t             -> unary'  $ integral t `TupRpair` integral t
  PrimIDiv t                -> binary' $ integral t
  PrimMod  t                -> binary' $ integral t
  PrimDivMod t              -> unary'  $ integral t `TupRpair` integral t

  -- Bits & FiniteBits
  PrimBAnd t                -> binary' $ integral t
  PrimBOr t                 -> binary' $ integral t
  PrimBXor t                -> binary' $ integral t
  PrimBNot t                -> unary'  $ integral t
  PrimBShiftL t             -> binary' $ integral t
  PrimBShiftR t             -> binary' $ integral t
  PrimBRotateL t            -> binary' $ integral t
  PrimBRotateR t            -> binary' $ integral t
  PrimPopCount t            -> unary'  $ integral t
  PrimCountLeadingZeros t   -> unary'  $ integral t
  PrimCountTrailingZeros t  -> unary'  $ integral t
  PrimBSwap t               -> unary'  $ integral t
  PrimBReverse t            -> unary'  $ integral t
  PrimVBAnd t               -> unary (integral t) (scalar_integral t)
  PrimVBOr t                -> unary (integral t) (scalar_integral t)
  PrimVBXor t               -> unary (integral t) (scalar_integral t)

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
  PrimIsNaN t               -> unary (floating t) (mask_floating t)
  PrimIsInfinite t          -> unary (floating t) (mask_floating t)

  -- Relational and equality
  PrimLt t                  -> compare' t
  PrimGt t                  -> compare' t
  PrimLtEq t                -> compare' t
  PrimGtEq t                -> compare' t
  PrimEq t                  -> compare' t
  PrimNEq t                 -> compare' t
  PrimMin t                 -> binary' $ single t
  PrimMax t                 -> binary' $ single t
  PrimVMin t                -> unary (single t) (scalar_scalar t)
  PrimVMax t                -> unary (single t) (scalar_scalar t)

  -- Logical
  PrimLAnd t                -> binary' (bit t)
  PrimLOr t                 -> binary' (bit t)
  PrimLNot t                -> unary' (bit t)
  PrimVLAnd t               -> unary (bit t) (scalar_bit t)
  PrimVLOr t                -> unary (bit t) (scalar_bit t)

  -- general conversion between types
  PrimFromIntegral a b      -> unary (integral a) (num b)
  PrimToFloating   a b      -> unary (num a) (floating b)
  PrimToBool a b            -> unary (integral a) (bit b)
  PrimFromBool a b          -> unary (bit a) (integral b)

  where
    unary a b  = (a, b)
    unary' a   = unary a a
    binary a b = (a `TupRpair` a, b)
    binary' a  = binary a a
    compare' a = binary (single a) (mask_scalar a)

    single     = TupRsingle
    num        = single . NumScalarType
    bit        = single . BitScalarType
    integral   = num . IntegralNumType
    floating   = num . FloatingNumType

    mask_scalar :: ScalarType t -> TypeR (BitOrMask t)
    mask_scalar (NumScalarType t) = mask_num t
    mask_scalar (BitScalarType t) = mask_bit t

    mask_bit :: BitType t -> TypeR (BitOrMask t)
    mask_bit TypeBit      = bit TypeBit
    mask_bit (TypeMask n) = bit (TypeMask n)

    mask_num :: NumType t -> TypeR (BitOrMask t)
    mask_num (IntegralNumType t) = mask_integral t
    mask_num (FloatingNumType t) = mask_floating t

    mask_integral :: IntegralType t -> TypeR (BitOrMask t)
    mask_integral (VectorIntegralType n _) = single (BitScalarType (TypeMask n))
    mask_integral (SingleIntegralType   t) = case t of
      TypeInt8    -> single (scalarType @Bit)
      TypeInt16   -> single (scalarType @Bit)
      TypeInt32   -> single (scalarType @Bit)
      TypeInt64   -> single (scalarType @Bit)
      TypeInt128  -> single (scalarType @Bit)
      TypeWord8   -> single (scalarType @Bit)
      TypeWord16  -> single (scalarType @Bit)
      TypeWord32  -> single (scalarType @Bit)
      TypeWord64  -> single (scalarType @Bit)
      TypeWord128 -> single (scalarType @Bit)

    mask_floating :: FloatingType t -> TypeR (BitOrMask t)
    mask_floating (VectorFloatingType n _) = single (BitScalarType (TypeMask n))
    mask_floating (SingleFloatingType   t) = case t of
      TypeFloat16  -> single (scalarType @Bit)
      TypeFloat32  -> single (scalarType @Bit)
      TypeFloat64  -> single (scalarType @Bit)
      TypeFloat128 -> single (scalarType @Bit)

    scalar_scalar :: ScalarType (Vec n a) -> TypeR a
    scalar_scalar (NumScalarType t) = scalar_num t
    scalar_scalar (BitScalarType t) = scalar_bit t

    scalar_bit :: BitType (Vec n a) -> TypeR a
    scalar_bit TypeMask{} = bit TypeBit

    scalar_num :: NumType (Vec n a) -> TypeR a
    scalar_num (IntegralNumType t) = scalar_integral t
    scalar_num (FloatingNumType t) = scalar_floating t

    scalar_integral :: IntegralType (Vec n a) -> TypeR a
    scalar_integral = \case
      SingleIntegralType t   -> case t of
      VectorIntegralType _ t -> integral (SingleIntegralType t)

    scalar_floating :: FloatingType (Vec n a) -> TypeR a
    scalar_floating = \case
      SingleFloatingType t   -> case t of
      VectorFloatingType _ t -> floating (SingleFloatingType t)


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
    Alet lhs bnd body         -> rnfALeftHandSide lhs `seq` rnfA bnd `seq` rnfA body
    Avar var                  -> rnfArrayVar var
    Apair as bs               -> rnfA as `seq` rnfA bs
    Anil                      -> ()
    Atrace msg as bs          -> rnfM msg `seq` rnfA as `seq` rnfA bs
    Acoerce scale bR as       -> scale `seq` rnfTypeR bR `seq` rnfA as
    Apply repr afun acc       -> rnfTupR rnfArrayR repr `seq` rnfAF afun `seq` rnfA acc
    Aforeign repr asm afun a  -> rnfTupR rnfArrayR repr `seq` rnf (strForeign asm) `seq` rnfAF afun `seq` rnfA a
    Acond p a1 a2             -> rnfE p `seq` rnfA a1 `seq` rnfA a2
    Awhile p f a              -> rnfAF p `seq` rnfAF f `seq` rnfA a
    Use repr arr              -> rnfArray repr arr
    Unit tp x                 -> rnfTypeR tp `seq` rnfE x
    Reshape shr sh a          -> rnfShapeR shr `seq` rnfE sh `seq` rnfA a
    Generate repr sh f        -> rnfArrayR repr `seq` rnfE sh `seq` rnfF f
    Transform repr sh p f a   -> rnfArrayR repr `seq` rnfE sh `seq` rnfF p `seq` rnfF f `seq` rnfA a
    Replicate slice sh a      -> rnfSliceIndex slice `seq` rnfE sh `seq` rnfA a
    Slice slice a sh          -> rnfSliceIndex slice `seq` rnfE sh `seq` rnfA a
    Map tp f a                -> rnfTypeR tp `seq` rnfF f `seq` rnfA a
    ZipWith tp f a1 a2        -> rnfTypeR tp `seq` rnfF f `seq` rnfA a1 `seq` rnfA a2
    Fold f z a                -> rnfF f `seq` rnfMaybe rnfE z `seq` rnfA a
    FoldSeg i f z a s         -> rnfSingleIntegralType i `seq` rnfF f `seq` rnfMaybe rnfE z `seq` rnfA a `seq` rnfA s
    Scan d f z a              -> d `seq` rnfF f `seq` rnfMaybe rnfE z `seq` rnfA a
    Scan' d f z a             -> d `seq` rnfF f `seq` rnfE z `seq` rnfA a
    Permute f d p a           -> rnfF f `seq` rnfA d `seq` rnfF p `seq` rnfA a
    Backpermute shr sh f a    -> rnfShapeR shr `seq` rnfE sh `seq` rnfF f `seq` rnfA a
    Stencil sr tp f b a       ->
      let
        TupRsingle (ArrayR shr _) = arraysR a
        repr                      = ArrayR shr $ stencilEltR sr
      in rnfStencilR sr `seq` rnfTupR rnfScalarType tp `seq` rnfF f `seq` rnfB repr b  `seq` rnfA a
    Stencil2 sr1 sr2 tp f b1 a1 b2 a2 ->
      let
        TupRsingle (ArrayR shr _) = arraysR a1
        repr1 = ArrayR shr $ stencilEltR sr1
        repr2 = ArrayR shr $ stencilEltR sr2
      in rnfStencilR sr1 `seq` rnfStencilR sr2 `seq` rnfTupR rnfScalarType tp `seq` rnfF f `seq` rnfB repr1 b1 `seq` rnfB repr2 b2 `seq` rnfA a1 `seq` rnfA a2

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
    Let lhs bnd body          -> rnfELeftHandSide lhs `seq` rnfE bnd `seq` rnfE body
    Evar v                    -> rnfExpVar v
    Foreign tp asm f x        -> rnfTypeR tp `seq` rnf (strForeign asm) `seq` rnfF f `seq` rnfE x
    Const tp c                -> c `seq` rnfScalarType tp -- scalars should have (nf == whnf)
    Undef tp                  -> rnfScalarType tp
    Pair a b                  -> rnfE a `seq` rnfE b
    Nil                       -> ()
    Extract vR iR v i         -> rnfScalarType vR `seq` rnfSingleIntegralType iR `seq` rnfE v `seq` rnfE i
    Insert vR iR v i x        -> rnfScalarType vR `seq` rnfSingleIntegralType iR `seq` rnfE v `seq` rnfE i `seq` rnfE x
    Shuffle eR iR x y i       -> rnfScalarType eR `seq` rnfSingleIntegralType iR `seq` rnfE x `seq` rnfE y `seq` rnfE i
    Select eR m x y           -> rnfScalarType eR `seq` rnfE m `seq` rnfE x `seq` rnfE y
    IndexSlice slice slix sh  -> rnfSliceIndex slice `seq` rnfE slix `seq` rnfE sh
    IndexFull slice slix sl   -> rnfSliceIndex slice `seq` rnfE slix `seq` rnfE sl
    ToIndex shr sh ix         -> rnfShapeR shr `seq` rnfE sh `seq` rnfE ix
    FromIndex shr sh ix       -> rnfShapeR shr `seq` rnfE sh `seq` rnfE ix
    Case pR p rhs def         -> rnfTagType pR `seq` rnfE p `seq` rnfList (\(t,c) -> t `seq` rnfE c) rhs `seq` rnfMaybe rnfE def
    Cond p e1 e2              -> rnfE p `seq` rnfE e1 `seq` rnfE e2
    While p f x               -> rnfF p `seq` rnfF f `seq` rnfE x
    PrimApp f x               -> rnfPrimFun f `seq` rnfE x
    Index a ix                -> rnfArrayVar a `seq` rnfE ix
    LinearIndex a ix          -> rnfArrayVar a `seq` rnfE ix
    Shape a                   -> rnfArrayVar a
    ShapeSize shr sh          -> rnfShapeR shr `seq` rnfE sh
    Bitcast t1 t2 e           -> rnfScalarType t1 `seq` rnfScalarType t2 `seq` rnfE e

rnfExpVar :: ExpVar env t -> ()
rnfExpVar = rnfVar rnfScalarType

rnfELeftHandSide :: ELeftHandSide t env env' -> ()
rnfELeftHandSide= rnfLeftHandSide rnfScalarType

rnfConst :: TypeR t -> t -> ()
rnfConst TupRunit          ()    = ()
rnfConst (TupRsingle t)    !_    = rnfScalarType t  -- scalars should have (nf == whnf)
rnfConst (TupRpair ta tb)  (a,b) = rnfConst ta a `seq` rnfConst tb b

rnfPrimFun :: PrimFun f -> ()
rnfPrimFun = \case
  PrimAdd t                -> rnfNumType t
  PrimSub t                -> rnfNumType t
  PrimMul t                -> rnfNumType t
  PrimNeg t                -> rnfNumType t
  PrimAbs t                -> rnfNumType t
  PrimSig t                -> rnfNumType t
  PrimVAdd t               -> rnfNumType t
  PrimVMul t               -> rnfNumType t
  PrimQuot t               -> rnfIntegralType t
  PrimRem t                -> rnfIntegralType t
  PrimQuotRem t            -> rnfIntegralType t
  PrimIDiv t               -> rnfIntegralType t
  PrimMod t                -> rnfIntegralType t
  PrimDivMod t             -> rnfIntegralType t
  PrimBAnd t               -> rnfIntegralType t
  PrimBOr t                -> rnfIntegralType t
  PrimBXor t               -> rnfIntegralType t
  PrimBNot t               -> rnfIntegralType t
  PrimBShiftL t            -> rnfIntegralType t
  PrimBShiftR t            -> rnfIntegralType t
  PrimBRotateL t           -> rnfIntegralType t
  PrimBRotateR t           -> rnfIntegralType t
  PrimPopCount t           -> rnfIntegralType t
  PrimCountLeadingZeros t  -> rnfIntegralType t
  PrimCountTrailingZeros t -> rnfIntegralType t
  PrimBReverse t           -> rnfIntegralType t
  PrimBSwap t              -> rnfIntegralType t
  PrimVBAnd t              -> rnfIntegralType t
  PrimVBOr t               -> rnfIntegralType t
  PrimVBXor t              -> rnfIntegralType t
  PrimFDiv t               -> rnfFloatingType t
  PrimRecip t              -> rnfFloatingType t
  PrimSin t                -> rnfFloatingType t
  PrimCos t                -> rnfFloatingType t
  PrimTan t                -> rnfFloatingType t
  PrimAsin t               -> rnfFloatingType t
  PrimAcos t               -> rnfFloatingType t
  PrimAtan t               -> rnfFloatingType t
  PrimSinh t               -> rnfFloatingType t
  PrimCosh t               -> rnfFloatingType t
  PrimTanh t               -> rnfFloatingType t
  PrimAsinh t              -> rnfFloatingType t
  PrimAcosh t              -> rnfFloatingType t
  PrimAtanh t              -> rnfFloatingType t
  PrimExpFloating t        -> rnfFloatingType t
  PrimSqrt t               -> rnfFloatingType t
  PrimLog t                -> rnfFloatingType t
  PrimFPow t               -> rnfFloatingType t
  PrimLogBase t            -> rnfFloatingType t
  PrimTruncate f i         -> rnfFloatingType f `seq` rnfIntegralType i
  PrimRound f i            -> rnfFloatingType f `seq` rnfIntegralType i
  PrimFloor f i            -> rnfFloatingType f `seq` rnfIntegralType i
  PrimCeiling f i          -> rnfFloatingType f `seq` rnfIntegralType i
  PrimAtan2 t              -> rnfFloatingType t
  PrimIsNaN t              -> rnfFloatingType t
  PrimIsInfinite t         -> rnfFloatingType t
  PrimLt t                 -> rnfScalarType t
  PrimGt t                 -> rnfScalarType t
  PrimLtEq t               -> rnfScalarType t
  PrimGtEq t               -> rnfScalarType t
  PrimEq t                 -> rnfScalarType t
  PrimNEq t                -> rnfScalarType t
  PrimMin t                -> rnfScalarType t
  PrimMax t                -> rnfScalarType t
  PrimVMin t               -> rnfScalarType t
  PrimVMax t               -> rnfScalarType t
  PrimLAnd t               -> rnfBitType t
  PrimLOr t                -> rnfBitType t
  PrimLNot t               -> rnfBitType t
  PrimVLAnd t              -> rnfBitType t
  PrimVLOr t               -> rnfBitType t
  PrimFromIntegral i n     -> rnfIntegralType i `seq` rnfNumType n
  PrimToFloating n f       -> rnfNumType n `seq` rnfFloatingType f
  PrimToBool i b           -> rnfIntegralType i `seq` rnfBitType b
  PrimFromBool b i         -> rnfBitType b `seq` rnfIntegralType i


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
    Alet lhs bnd body         -> [|| Alet $$(liftALeftHandSide lhs) $$(liftA bnd) $$(liftA body) ||]
    Avar var                  -> [|| Avar $$(liftArrayVar var) ||]
    Apair as bs               -> [|| Apair $$(liftA as) $$(liftA bs) ||]
    Anil                      -> [|| Anil ||]
    Atrace msg as bs          -> [|| Atrace $$(liftMessage (arraysR as) msg) $$(liftA as) $$(liftA bs) ||]
    Acoerce scale bR a        -> [|| Acoerce scale $$(liftTypeR bR) $$(liftA a) ||]
    Apply repr f a            -> [|| Apply $$(liftArraysR repr) $$(liftAF f) $$(liftA a) ||]
    Aforeign repr asm f a     -> [|| Aforeign $$(liftArraysR repr) $$(liftForeign asm) $$(liftPreOpenAfun liftA f) $$(liftA a) ||]
    Acond p t e               -> [|| Acond $$(liftE p) $$(liftA t) $$(liftA e) ||]
    Awhile p f a              -> [|| Awhile $$(liftAF p) $$(liftAF f) $$(liftA a) ||]
    Use repr a                -> [|| Use $$(liftArrayR repr) $$(liftArray repr a) ||]
    Unit tp e                 -> [|| Unit $$(liftTypeR tp) $$(liftE e) ||]
    Reshape shr sh a          -> [|| Reshape $$(liftShapeR shr) $$(liftE sh) $$(liftA a) ||]
    Generate repr sh f        -> [|| Generate $$(liftArrayR repr) $$(liftE sh) $$(liftF f) ||]
    Transform repr sh p f a   -> [|| Transform $$(liftArrayR repr) $$(liftE sh) $$(liftF p) $$(liftF f) $$(liftA a) ||]
    Replicate slix sl a       -> [|| Replicate $$(liftSliceIndex slix) $$(liftE sl) $$(liftA a) ||]
    Slice slix a sh           -> [|| Slice $$(liftSliceIndex slix) $$(liftA a) $$(liftE sh) ||]
    Map tp f a                -> [|| Map $$(liftTypeR tp) $$(liftF f) $$(liftA a) ||]
    ZipWith tp f a b          -> [|| ZipWith $$(liftTypeR tp) $$(liftF f) $$(liftA a) $$(liftA b) ||]
    Fold f z a                -> [|| Fold $$(liftF f) $$(liftMaybe liftE z) $$(liftA a) ||]
    FoldSeg i f z a s         -> [|| FoldSeg $$(liftSingleIntegralType i) $$(liftF f) $$(liftMaybe liftE z) $$(liftA a) $$(liftA s) ||]
    Scan d f z a              -> [|| Scan  $$(liftDirection d) $$(liftF f) $$(liftMaybe liftE z) $$(liftA a) ||]
    Scan' d f z a             -> [|| Scan' $$(liftDirection d) $$(liftF f) $$(liftE z) $$(liftA a) ||]
    Permute f d p a           -> [|| Permute $$(liftF f) $$(liftA d) $$(liftF p) $$(liftA a) ||]
    Backpermute shr sh p a    -> [|| Backpermute $$(liftShapeR shr) $$(liftE sh) $$(liftF p) $$(liftA a) ||]
    Stencil sr tp f b a       ->
      let TupRsingle (ArrayR shr _) = arraysR a
          repr = ArrayR shr $ stencilEltR sr
       in [|| Stencil $$(liftStencilR sr) $$(liftTypeR tp) $$(liftF f) $$(liftB repr b) $$(liftA a) ||]
    Stencil2 sr1 sr2 tp f b1 a1 b2 a2 ->
      let TupRsingle (ArrayR shr _) = arraysR a1
          repr1 = ArrayR shr $ stencilEltR sr1
          repr2 = ArrayR shr $ stencilEltR sr2
       in [|| Stencil2 $$(liftStencilR sr1) $$(liftStencilR sr2) $$(liftTypeR tp) $$(liftF f) $$(liftB repr1 b1) $$(liftA a1) $$(liftB repr2 b2) $$(liftA a2) ||]


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
    Let lhs bnd body          -> [|| Let $$(liftELeftHandSide lhs) $$(liftOpenExp bnd) $$(liftOpenExp body) ||]
    Evar var                  -> [|| Evar $$(liftExpVar var) ||]
    Foreign repr asm f x      -> [|| Foreign $$(liftTypeR repr) $$(liftForeign asm) $$(liftOpenFun f) $$(liftE x) ||]
    Const tp c                -> [|| Const $$(liftScalarType tp) $$(liftElt (TupRsingle tp) c) ||]
    Undef tp                  -> [|| Undef $$(liftScalarType tp) ||]
    Pair a b                  -> [|| Pair $$(liftE a) $$(liftE b) ||]
    Nil                       -> [|| Nil ||]
    Extract vR iR v i         -> [|| Extract $$(liftScalarType vR) $$(liftSingleIntegralType iR) $$(liftE v) $$(liftE i) ||]
    Insert vR iR v i x        -> [|| Insert $$(liftScalarType vR) $$(liftSingleIntegralType iR) $$(liftE v) $$(liftE i) $$(liftE x) ||]
    Shuffle eR iR x y i       -> [|| Shuffle $$(liftScalarType eR) $$(liftSingleIntegralType iR) $$(liftE x) $$(liftE y) $$(liftE i) ||]
    Select eR m x y           -> [|| Select $$(liftScalarType eR) $$(liftE m) $$(liftE x) $$(liftE y) ||]
    IndexSlice slice slix sh  -> [|| IndexSlice $$(liftSliceIndex slice) $$(liftE slix) $$(liftE sh) ||]
    IndexFull slice slix sl   -> [|| IndexFull $$(liftSliceIndex slice) $$(liftE slix) $$(liftE sl) ||]
    ToIndex shr sh ix         -> [|| ToIndex $$(liftShapeR shr) $$(liftE sh) $$(liftE ix) ||]
    FromIndex shr sh ix       -> [|| FromIndex $$(liftShapeR shr) $$(liftE sh) $$(liftE ix) ||]
    Case pR p rhs def         -> [|| Case $$(liftTagType pR) $$(liftE p) $$(liftList (\(t,c) -> [|| ($$(liftTag pR t), $$(liftE c)) ||]) rhs) $$(liftMaybe liftE def) ||]
    Cond p t e                -> [|| Cond $$(liftE p) $$(liftE t) $$(liftE e) ||]
    While p f x               -> [|| While $$(liftF p) $$(liftF f) $$(liftE x) ||]
    PrimApp f x               -> [|| PrimApp $$(liftPrimFun f) $$(liftE x) ||]
    Index a ix                -> [|| Index $$(liftArrayVar a) $$(liftE ix) ||]
    LinearIndex a ix          -> [|| LinearIndex $$(liftArrayVar a) $$(liftE ix) ||]
    Shape a                   -> [|| Shape $$(liftArrayVar a) ||]
    ShapeSize shr ix          -> [|| ShapeSize $$(liftShapeR shr) $$(liftE ix) ||]
    Bitcast t1 t2 e           -> [|| Bitcast $$(liftScalarType t1) $$(liftScalarType t2) $$(liftE e) ||]

liftELeftHandSide :: ELeftHandSide t env env' -> CodeQ (ELeftHandSide t env env')
liftELeftHandSide = liftLeftHandSide liftScalarType

liftExpVar :: ExpVar env t -> CodeQ (ExpVar env t)
liftExpVar = liftVar liftScalarType

liftBoundary
    :: forall aenv sh e.
       ArrayR (Array sh e)
    -> Boundary aenv (Array sh e)
    -> CodeQ (Boundary aenv (Array sh e))
liftBoundary (ArrayR _ tR) = \case
  Clamp      -> [|| Clamp ||]
  Mirror     -> [|| Mirror ||]
  Wrap       -> [|| Wrap ||]
  Constant v -> [|| Constant $$(liftElt tR v) ||]
  Function f -> [|| Function $$(liftOpenFun f) ||]

liftPrimFun :: PrimFun f -> CodeQ (PrimFun f)
liftPrimFun = \case
  PrimAdd t                -> [|| PrimAdd $$(liftNumType t) ||]
  PrimSub t                -> [|| PrimSub $$(liftNumType t) ||]
  PrimMul t                -> [|| PrimMul $$(liftNumType t) ||]
  PrimNeg t                -> [|| PrimNeg $$(liftNumType t) ||]
  PrimAbs t                -> [|| PrimAbs $$(liftNumType t) ||]
  PrimSig t                -> [|| PrimSig $$(liftNumType t) ||]
  PrimVAdd t               -> [|| PrimVAdd $$(liftNumType t) ||]
  PrimVMul t               -> [|| PrimVMul $$(liftNumType t) ||]
  PrimQuot t               -> [|| PrimQuot $$(liftIntegralType t) ||]
  PrimRem t                -> [|| PrimRem $$(liftIntegralType t) ||]
  PrimQuotRem t            -> [|| PrimQuotRem $$(liftIntegralType t) ||]
  PrimIDiv t               -> [|| PrimIDiv $$(liftIntegralType t) ||]
  PrimMod t                -> [|| PrimMod $$(liftIntegralType t) ||]
  PrimDivMod t             -> [|| PrimDivMod $$(liftIntegralType t) ||]
  PrimBAnd t               -> [|| PrimBAnd $$(liftIntegralType t) ||]
  PrimBOr t                -> [|| PrimBOr $$(liftIntegralType t) ||]
  PrimBXor t               -> [|| PrimBXor $$(liftIntegralType t) ||]
  PrimBNot t               -> [|| PrimBNot $$(liftIntegralType t) ||]
  PrimBShiftL t            -> [|| PrimBShiftL $$(liftIntegralType t) ||]
  PrimBShiftR t            -> [|| PrimBShiftR $$(liftIntegralType t) ||]
  PrimBRotateL t           -> [|| PrimBRotateL $$(liftIntegralType t) ||]
  PrimBRotateR t           -> [|| PrimBRotateR $$(liftIntegralType t) ||]
  PrimPopCount t           -> [|| PrimPopCount $$(liftIntegralType t) ||]
  PrimCountLeadingZeros t  -> [|| PrimCountLeadingZeros $$(liftIntegralType t) ||]
  PrimCountTrailingZeros t -> [|| PrimCountTrailingZeros $$(liftIntegralType t) ||]
  PrimBReverse t           -> [|| PrimBReverse $$(liftIntegralType t) ||]
  PrimBSwap t              -> [|| PrimBSwap $$(liftIntegralType t) ||]
  PrimVBAnd t              -> [|| PrimVBAnd $$(liftIntegralType t) ||]
  PrimVBOr t               -> [|| PrimVBOr $$(liftIntegralType t) ||]
  PrimVBXor t              -> [|| PrimVBXor $$(liftIntegralType t) ||]
  PrimFDiv t               -> [|| PrimFDiv $$(liftFloatingType t) ||]
  PrimRecip t              -> [|| PrimRecip $$(liftFloatingType t) ||]
  PrimSin t                -> [|| PrimSin $$(liftFloatingType t) ||]
  PrimCos t                -> [|| PrimCos $$(liftFloatingType t) ||]
  PrimTan t                -> [|| PrimTan $$(liftFloatingType t) ||]
  PrimAsin t               -> [|| PrimAsin $$(liftFloatingType t) ||]
  PrimAcos t               -> [|| PrimAcos $$(liftFloatingType t) ||]
  PrimAtan t               -> [|| PrimAtan $$(liftFloatingType t) ||]
  PrimSinh t               -> [|| PrimSinh $$(liftFloatingType t) ||]
  PrimCosh t               -> [|| PrimCosh $$(liftFloatingType t) ||]
  PrimTanh t               -> [|| PrimTanh $$(liftFloatingType t) ||]
  PrimAsinh t              -> [|| PrimAsinh $$(liftFloatingType t) ||]
  PrimAcosh t              -> [|| PrimAcosh $$(liftFloatingType t) ||]
  PrimAtanh t              -> [|| PrimAtanh $$(liftFloatingType t) ||]
  PrimExpFloating t        -> [|| PrimExpFloating $$(liftFloatingType t) ||]
  PrimSqrt t               -> [|| PrimSqrt $$(liftFloatingType t) ||]
  PrimLog t                -> [|| PrimLog $$(liftFloatingType t) ||]
  PrimFPow t               -> [|| PrimFPow $$(liftFloatingType t) ||]
  PrimLogBase t            -> [|| PrimLogBase $$(liftFloatingType t) ||]
  PrimTruncate ta tb       -> [|| PrimTruncate $$(liftFloatingType ta) $$(liftIntegralType tb) ||]
  PrimRound ta tb          -> [|| PrimRound $$(liftFloatingType ta) $$(liftIntegralType tb) ||]
  PrimFloor ta tb          -> [|| PrimFloor $$(liftFloatingType ta) $$(liftIntegralType tb) ||]
  PrimCeiling ta tb        -> [|| PrimCeiling $$(liftFloatingType ta) $$(liftIntegralType tb) ||]
  PrimIsNaN t              -> [|| PrimIsNaN $$(liftFloatingType t) ||]
  PrimIsInfinite t         -> [|| PrimIsInfinite $$(liftFloatingType t) ||]
  PrimAtan2 t              -> [|| PrimAtan2 $$(liftFloatingType t) ||]
  PrimLt t                 -> [|| PrimLt $$(liftScalarType t) ||]
  PrimGt t                 -> [|| PrimGt $$(liftScalarType t) ||]
  PrimLtEq t               -> [|| PrimLtEq $$(liftScalarType t) ||]
  PrimGtEq t               -> [|| PrimGtEq $$(liftScalarType t) ||]
  PrimEq t                 -> [|| PrimEq $$(liftScalarType t) ||]
  PrimNEq t                -> [|| PrimNEq $$(liftScalarType t) ||]
  PrimMin t                -> [|| PrimMin $$(liftScalarType t) ||]
  PrimMax t                -> [|| PrimMax $$(liftScalarType t) ||]
  PrimVMin t               -> [|| PrimVMin $$(liftScalarType t) ||]
  PrimVMax t               -> [|| PrimVMax $$(liftScalarType t) ||]
  PrimLAnd t               -> [|| PrimLAnd $$(liftBitType t) ||]
  PrimLOr t                -> [|| PrimLOr $$(liftBitType t) ||]
  PrimLNot t               -> [|| PrimLNot $$(liftBitType t) ||]
  PrimVLAnd t              -> [|| PrimVLAnd $$(liftBitType t) ||]
  PrimVLOr t               -> [|| PrimVLOr $$(liftBitType t) ||]
  PrimFromIntegral ta tb   -> [|| PrimFromIntegral $$(liftIntegralType ta) $$(liftNumType tb) ||]
  PrimToFloating ta tb     -> [|| PrimToFloating $$(liftNumType ta) $$(liftFloatingType tb) ||]
  PrimToBool ta tb         -> [|| PrimToBool $$(liftIntegralType ta) $$(liftBitType tb) ||]
  PrimFromBool ta tb       -> [|| PrimFromBool $$(liftBitType ta) $$(liftIntegralType tb) ||]


formatDirection :: Format r (Direction -> r)
formatDirection = later $ \case
  LeftToRight -> singleton 'l'
  RightToLeft -> singleton 'r'

formatPreAccOp :: Format r (PreOpenAcc acc aenv arrs -> r)
formatPreAccOp = later $ \case
  Alet{}            -> "Alet"
  Avar (Var _ ix)   -> bformat ("Avar a" % int) (idxToInt ix)
  Use aR a          -> bformat ("Use " % string) (showArrayShort 5 (showsElt (arrayRtype aR)) aR a)
  Atrace{}          -> "Atrace"
  Acoerce{}         -> "Acoerce"
  Apply{}           -> "Apply"
  Aforeign{}        -> "Aforeign"
  Acond{}           -> "Acond"
  Awhile{}          -> "Awhile"
  Apair{}           -> "Apair"
  Anil              -> "Anil"
  Unit{}            -> "Unit"
  Generate{}        -> "Generate"
  Transform{}       -> "Transform"
  Reshape{}         -> "Reshape"
  Replicate{}       -> "Replicate"
  Slice{}           -> "Slice"
  Map{}             -> "Map"
  ZipWith{}         -> "ZipWith"
  Fold _ z _        -> bformat ("Fold" % maybed "1" (fconst mempty)) z
  FoldSeg _ _ z _ _ -> bformat ("Fold" % maybed "1" (fconst mempty) % "Seg") z
  Scan d _ z _      -> bformat ("Scan" % formatDirection % maybed "1" (fconst mempty)) d z
  Scan' d _ _ _     -> bformat ("Scan" % formatDirection % "\'") d
  Permute{}         -> "Permute"
  Backpermute{}     -> "Backpermute"
  Stencil{}         -> "Stencil"
  Stencil2{}        -> "Stencil2"

formatExpOp :: Format r (OpenExp aenv env t -> r)
formatExpOp = later $ \case
  Let{}           -> "Let"
  Evar (Var _ ix) -> bformat ("Var x" % int) (idxToInt ix)
  Const tp c      -> bformat ("Const " % string) (showElt (TupRsingle tp) c)
  Undef{}         -> "Undef"
  Foreign{}       -> "Foreign"
  Pair{}          -> "Pair"
  Nil{}           -> "Nil"
  Insert{}        -> "Insert"
  Extract{}       -> "Extract"
  Shuffle{}       -> "Shuffle"
  Select{}        -> "Select"
  IndexSlice{}    -> "IndexSlice"
  IndexFull{}     -> "IndexFull"
  ToIndex{}       -> "ToIndex"
  FromIndex{}     -> "FromIndex"
  Case{}          -> "Case"
  Cond{}          -> "Cond"
  While{}         -> "While"
  PrimApp{}       -> "PrimApp"
  Index{}         -> "Index"
  LinearIndex{}   -> "LinearIndex"
  Shape{}         -> "Shape"
  ShapeSize{}     -> "ShapeSize"
  Bitcast{}       -> "Coerce"

