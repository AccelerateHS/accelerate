{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UnboxedTuples         #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.AST
-- Copyright   : [2008..2019] The Accelerate Team
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

  -- * Typed de Bruijn indices
  Idx(..), idxToInt, tupleIdxToInt, Vars(..), ArrayVar(..), ScalarVars(..), ArrayVars, ScalarVars,

  -- * Valuation environment
  Val(..), push, prj,

  -- * Accelerated array expressions
  PreOpenAfun(..), OpenAfun, PreAfun, Afun, PreOpenAcc(..), OpenAcc(..), Acc,
  PreBoundary(..), Boundary, Stencil(..), StencilR(..),
  LeftHandSide(..), HasArraysRepr(..), lhsToTupR,

  -- * Accelerated sequences
  -- PreOpenSeq(..), Seq,
  -- Producer(..), Consumer(..),

  -- * Scalar expressions
  PreOpenFun(..), OpenFun, PreFun, Fun, PreOpenExp(..), OpenExp, PreExp, Exp, PrimConst(..),
  PrimFun(..),

  -- NFData
  NFDataAcc,
  rnfPreOpenAfun, rnfPreOpenAcc, rnfPreOpenFun, rnfPreOpenExp,
  rnfArrays,

  -- TemplateHaskell
  LiftAcc,
  liftIdx, liftTupleIdx,
  liftConst, liftSliceIndex, liftPrimConst, liftPrimFun,
  liftPreOpenAfun, liftPreOpenAcc, liftPreOpenFun, liftPreOpenExp,
  liftALhs, liftELhs, liftArray,

  -- Utilities
  Exists(..), weakenWithLHS, (:>),

  -- debugging
  showPreAccOp, showPreExpOp,

) where

--standard library
import Control.DeepSeq
import Control.Monad.ST
import Data.Typeable
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe
import Language.Haskell.TH                                          ( Q, TExp )
import qualified Language.Haskell.TH                                as TH
import qualified Language.Haskell.TH.Syntax                         as TH
#if __GLASGOW_HASKELL__ <= 708
import Instances.TH.Lift                                            () -- Int8, Int16...
#endif

import GHC.Base                                                     ( Int#, isTrue# )
import GHC.Int                                                      ( Int(..) )
import GHC.Prim                                                     ( (<#), (+#), indexWord8Array#, sizeofByteArray# )
import GHC.Ptr                                                      ( Ptr(..) )
import GHC.Word                                                     ( Word8(..) )

-- friends
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Representation
import qualified Data.Array.Accelerate.Array.Sugar                   as Sugar
import Data.Array.Accelerate.Array.Unique
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Type
#if __GLASGOW_HASKELL__ < 800
import Data.Array.Accelerate.Error
#endif


-- Typed de Bruijn indices
-- -----------------------

-- De Bruijn variable index projecting a specific type from a type
-- environment.  Type environments are nested pairs (..((), t1), t2, ..., tn).
--
data Idx env t where
  ZeroIdx ::              Idx (env, t) t
  SuccIdx :: Idx env t -> Idx (env, s) t

-- de Bruijn Index to Int conversion
--
idxToInt :: Idx env t -> Int
idxToInt ZeroIdx       = 0
idxToInt (SuccIdx idx) = 1 + idxToInt idx

tupleIdxToInt :: TupleIdx tup e -> Int
tupleIdxToInt ZeroTupIdx       = 0
tupleIdxToInt (SuccTupIdx idx) = 1 + tupleIdxToInt idx


-- Environments
-- ------------

-- Valuation for an environment
--
data Val env where
  Empty :: Val ()
  Push  :: Val env -> t -> Val (env, t)
deriving instance Typeable Val

push :: Val env -> (LeftHandSide s arrs env env', arrs) -> Val env'
push env (LeftHandSideWildcard _, _     ) = env
push env (LeftHandSideSingle _  , a     ) = env `Push` a
push env (LeftHandSidePair l1 l2, (a, b)) = push env (l1, a) `push` (l2, b)

-- Projection of a value from a valuation using a de Bruijn index
--
prj :: Idx env t -> Val env -> t
prj ZeroIdx       (Push _   v) = v
prj (SuccIdx idx) (Push val _) = prj idx val
#if __GLASGOW_HASKELL__ < 800
prj _             _            = $internalError "prj" "inconsistent valuation"
#endif

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

deriving instance Typeable PreOpenAcc
deriving instance Typeable OpenAcc

type ALeftHandSide = LeftHandSide ArrayR

type ELeftHandSide = LeftHandSide ScalarType

data LeftHandSide (s :: * -> *) arrs env env' where
  LeftHandSideSingle
    :: s arrs
    -> LeftHandSide s arrs env (env, arrs)

  -- Note: a unit is represented as LeftHandSideWildcard TupRunit
  LeftHandSideWildcard
    :: TupR s arrs
    -> LeftHandSide s arrs env env

  LeftHandSidePair
    :: LeftHandSide s arrs1          env  env'
    -> LeftHandSide s arrs2          env' env''
    -> LeftHandSide s (arrs1, arrs2) env  env''

lhsToTupR :: LeftHandSide s arrs aenv aenv' -> TupR s arrs
lhsToTupR (LeftHandSideSingle s)   = TupRsingle s
lhsToTupR (LeftHandSideWildcard r) = r
lhsToTupR (LeftHandSidePair as bs) = TupRpair (lhsToTupR as) (lhsToTupR bs)

-- The type of shifting terms from one context into another
--
type env :> env' = forall t'. Idx env t' -> Idx env' t'

weakenWithLHS :: LeftHandSide s arrs env env' -> env :> env'
weakenWithLHS (LeftHandSideWildcard _)     = id
weakenWithLHS (LeftHandSideSingle _)       = SuccIdx
weakenWithLHS (LeftHandSidePair lhs1 lhs2) = weakenWithLHS lhs2 . weakenWithLHS lhs1

-- Often useful when working with LeftHandSide, when you need to
-- existentially quantify on the resulting environment type.
data Exists f where
  Exists :: f a -> Exists f

type ArrayVar = Var ArrayR
type ArrayVars = Vars ArrayR

type ScalarVar = Var ScalarType
type ScalarVars = Vars ScalarType

data Var s env t = Var (s t) (Idx env t)
data Vars s env t where
  VarsSingle :: Var s env a -> Vars s env a
  VarsNil    :: Vars s aenv ()
  VarsPair   :: Vars s aenv a -> Vars s aenv b -> Vars s aenv (a, b)

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
-- The data type is parameterised over the surface types (not the
-- representation type).
--
-- We use a non-recursive variant parametrised over the recursive closure,
-- to facilitate attribute calculation in the backend.
--
data PreOpenAcc acc aenv a where

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
  Apply       :: PreOpenAfun acc aenv (arrs1 -> arrs2)
              -> acc             aenv arrs1
              -> PreOpenAcc  acc aenv arrs2

  -- Apply a backend-specific foreign function to an array, with a pure
  -- Accelerate version for use with other backends. The functions must be
  -- closed.
  --
  Aforeign    :: (Sugar.Arrays as, Sugar.Arrays bs, Sugar.Foreign asm)
              => asm                   (as -> bs)                 -- The foreign function for a given backend
              -> PreAfun      acc      (Sugar.ArrRepr as -> Sugar.ArrRepr bs) -- Fallback implementation(s)
              -> acc              aenv (Sugar.ArrRepr as)               -- Arguments to the function
              -> PreOpenAcc   acc aenv (Sugar.ArrRepr bs)

  -- If-then-else for array-level computations
  --
  Acond       :: PreExp     acc aenv Bool
              -> acc            aenv arrs
              -> acc            aenv arrs
              -> PreOpenAcc acc aenv arrs

  -- Value-recursion for array-level computations
  --
  Awhile      :: PreOpenAfun acc aenv (arrs -> Scalar Bool)     -- continue iteration while true
              -> PreOpenAfun acc aenv (arrs -> arrs)            -- function to iterate
              -> acc             aenv arrs                      -- initial value
              -> PreOpenAcc  acc aenv arrs


  -- Array inlet. Triggers (possibly) asynchronous host->device transfer if
  -- necessary.
  --
  Use         :: ArrayR (Array sh e)
              -> Array sh e
              -> PreOpenAcc acc aenv (Array sh e)

  -- Capture a scalar (or a tuple of scalars) in a singleton array
  --
  Unit        :: TupleType e
              => PreExp     acc aenv e
              -> PreOpenAcc acc aenv (Scalar e)

  -- Change the shape of an array without altering its contents.
  -- Precondition (this may not be checked!):
  --
  -- > dim == size dim'
  --
  Reshape     :: ShapeR sh
              -> PreExp     acc aenv sh                         -- new shape
              -> acc            aenv (Array sh' e)              -- array to be reshaped
              -> PreOpenAcc acc aenv (Array sh e)

  -- Construct a new array by applying a function to each index.
  --
  Generate    :: ArrayR (Array sh e)
              => PreExp     acc aenv sh                         -- output shape
              -> PreFun     acc aenv (sh -> e)                  -- representation function
              -> PreOpenAcc acc aenv (Array sh e)

  -- Hybrid map/backpermute, where we separate the index and value
  -- transformations.
  --
  Transform   :: ArrayR (Array sh' b)
              => PreExp     acc aenv sh'                        -- dimension of the result
              -> PreFun     acc aenv (sh' -> sh)                -- index permutation function
              -> PreFun     acc aenv (a   -> b)                 -- function to apply at each element
              ->            acc aenv (Array sh  a)              -- source array
              -> PreOpenAcc acc aenv (Array sh' b)

  -- Replicate an array across one or more dimensions as given by the first
  -- argument
  --
  Replicate   :: SliceIndex (EltRepr slix)                      -- slice type specification
                            (EltRepr sl)
                            co
                            (EltRepr sh)
              -> PreExp     acc aenv slix                       -- slice value specification
              -> acc            aenv (Array sl e)               -- data to be replicated
              -> PreOpenAcc acc aenv (Array sh e)

  -- Index a sub-array out of an array; i.e., the dimensions not indexed
  -- are returned whole
  --
  Slice       :: SliceIndex (EltRepr slix)                      -- slice type specification
                            (EltRepr sl)
                            co
                            (EltRepr sh)
              -> acc            aenv (Array sh e)               -- array to be indexed
              -> PreExp     acc aenv slix                       -- slice value specification
              -> PreOpenAcc acc aenv (Array sl e)

  -- Apply the given unary function to all elements of the given array
  --
  Map         :: TupleType e'
              -> PreFun     acc aenv (e -> e')
              -> acc            aenv (Array sh e)
              -> PreOpenAcc acc aenv (Array sh e')

  -- Apply a given binary function pairwise to all elements of the given
  -- arrays. The length of the result is the length of the shorter of the
  -- two argument arrays.
  --
  ZipWith     :: TupleType e3
              -> PreFun     acc aenv (e1 -> e2 -> e3)
              -> acc            aenv (Array sh e1)
              -> acc            aenv (Array sh e2)
              -> PreOpenAcc acc aenv (Array sh e3)

  -- Fold along the innermost dimension of an array with a given
  -- /associative/ function.
  --
  Fold        :: PreFun     acc aenv (e -> e -> e)              -- combination function
              -> PreExp     acc aenv e                          -- default value
              -> acc            aenv (Array (sh:.Int) e)        -- folded array
              -> PreOpenAcc acc aenv (Array sh e)

  -- As 'Fold' without a default value
  --
  Fold1       :: PreFun     acc aenv (e -> e -> e)              -- combination function
              -> acc            aenv (Array (sh:.Int) e)        -- folded array
              -> PreOpenAcc acc aenv (Array sh e)

  -- Segmented fold along the innermost dimension of an array with a given
  -- /associative/ function
  --
  FoldSeg     :: IntegralType i
              -> PreFun     acc aenv (e -> e -> e)              -- combination function
              -> PreExp     acc aenv e                          -- default value
              -> acc            aenv (Array (sh:.Int) e)        -- folded array
              -> acc            aenv (Segments i)               -- segment descriptor
              -> PreOpenAcc acc aenv (Array (sh:.Int) e)

  -- As 'FoldSeg' without a default value
  --
  Fold1Seg    :: IntegralType i
              -> PreFun     acc aenv (e -> e -> e)              -- combination function
              -> acc            aenv (Array (sh:.Int) e)        -- folded array
              -> acc            aenv (Segments i)               -- segment descriptor
              -> PreOpenAcc acc aenv (Array (sh:.Int) e)

  -- Left-to-right Haskell-style scan of a linear array with a given
  -- /associative/ function and an initial element (which does not need to
  -- be the neutral of the associative operations)
  --
  Scanl       :: PreFun     acc aenv (e -> e -> e)              -- combination function
              -> PreExp     acc aenv e                          -- initial value
              -> acc            aenv (Array (sh:.Int) e)
              -> PreOpenAcc acc aenv (Array (sh:.Int) e)

  -- Like 'Scan', but produces a rightmost fold value and an array with the
  -- same length as the input array (the fold value would be the rightmost
  -- element in a Haskell-style scan)
  --
  Scanl'      :: PreFun     acc aenv (e -> e -> e)              -- combination function
              -> PreExp     acc aenv e                          -- initial value
              -> acc            aenv (Array (sh:.Int) e)
              -> PreOpenAcc acc aenv (((), Array (sh:.Int) e), Array sh e)

  -- Haskell-style scan without an initial value
  --
  Scanl1      :: PreFun     acc aenv (e -> e -> e)              -- combination function
              -> acc            aenv (Array (sh:.Int) e)
              -> PreOpenAcc acc aenv (Array (sh:.Int) e)

  -- Right-to-left version of 'Scanl'
  --
  Scanr       :: PreFun     acc aenv (e -> e -> e)              -- combination function
              -> PreExp     acc aenv e                          -- initial value
              -> acc            aenv (Array (sh:.Int) e)
              -> PreOpenAcc acc aenv (Array (sh:.Int) e)

  -- Right-to-left version of 'Scanl\''
  --
  Scanr'      :: PreFun     acc aenv (e -> e -> e)              -- combination function
              -> PreExp     acc aenv e                          -- initial value
              -> acc            aenv (Array (sh:.Int) e)
              -> PreOpenAcc acc aenv (((), Array (sh:.Int) e), Array sh e)

  -- Right-to-left version of 'Scanl1'
  --
  Scanr1      :: PreFun     acc aenv (e -> e -> e)              -- combination function
              -> acc            aenv (Array (sh:.Int) e)
              -> PreOpenAcc acc aenv (Array (sh:.Int) e)

  -- Generalised forward permutation is characterised by a permutation function
  -- that determines for each element of the source array where it should go in
  -- the output. The permutation can be between arrays of varying shape and
  -- dimensionality.
  --
  -- Other characteristics of the permutation function 'f':
  --
  --   1. 'f' is a partial function: if it evaluates to the magic value 'ignore'
  --      (i.e. a tuple of -1 values) then those elements of the domain are
  --      dropped.
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
  Permute     :: ShapeR sh'
              => PreFun     acc aenv (e -> e -> e)              -- combination function
              -> acc            aenv (Array sh' e)              -- default values
              -> PreFun     acc aenv (sh -> sh')                -- permutation function
              -> acc            aenv (Array sh e)               -- source array
              -> PreOpenAcc acc aenv (Array sh' e)

  -- Generalised multi-dimensional backwards permutation; the permutation can
  -- be between arrays of varying shape; the permutation function must be total
  --
  Backpermute :: ShapeR sh'
              => PreExp     acc aenv sh'                        -- dimensions of the result
              -> PreFun     acc aenv (sh' -> sh)                -- permutation function
              -> acc            aenv (Array sh e)               -- source array
              -> PreOpenAcc acc aenv (Array sh' e)

  -- Map a stencil over an array.  In contrast to 'map', the domain of
  -- a stencil function is an entire /neighbourhood/ of each array element.
  --
  Stencil     :: StencilR sh e stencil
              -> TupleType e'
              -> PreFun      acc aenv (stencil -> e')           -- stencil function
              -> PreBoundary acc aenv (Array sh e)              -- boundary condition
              -> acc             aenv (Array sh e)              -- source array
              -> PreOpenAcc  acc aenv (Array sh e')

  -- Map a binary stencil over an array.
  --
  Stencil2    :: StencilR sh a stencil1
              -> StencilR sh b stencil2
              -> TupleType c
              => PreFun      acc aenv (stencil1 -> stencil2 -> c) -- stencil function
              -> PreBoundary acc aenv (Array sh a)                -- boundary condition #1
              -> acc             aenv (Array sh a)                -- source array #1
              -> PreBoundary acc aenv (Array sh b)                -- boundary condition #2
              -> acc             aenv (Array sh b)                -- source array #2
              -> PreOpenAcc acc  aenv (Array sh c)

  -- A sequence of operations.
  -- Collect     :: Arrays arrs
  --             => PreOpenSeq acc aenv () arrs
  --             -> PreOpenAcc acc aenv arrs

{--
data PreOpenSeq acc aenv senv arrs where
  Producer :: Arrays a
           => Producer acc aenv senv a
           -> PreOpenSeq acc aenv (senv, a) arrs
           -> PreOpenSeq acc aenv senv arrs

  Consumer :: Arrays arrs
           => Consumer acc aenv senv arrs
           -> PreOpenSeq acc aenv senv arrs

  Reify    :: Arrays arrs
           => Idx senv arrs
           -> PreOpenSeq acc aenv senv [arrs]

data Producer acc aenv senv a where
  -- Convert the given Haskell-list of arrays to a sequence.
  StreamIn :: Arrays a
           => [a]
           -> Producer acc aenv senv a

  -- Convert the given array to a sequence.
  ToSeq :: (Elt slix, Shape sl, Shape sh, Elt e)
           => SliceIndex  (EltRepr slix)
                          (EltRepr sl)
                          co
                          (EltRepr sh)
           -> proxy slix
           -> acc aenv (Array sh e)
           -> Producer acc aenv senv (Array sl e)

  -- Apply the given the given function to all elements of the given
  -- sequence.
  MapSeq :: (Arrays a, Arrays b)
         => PreOpenAfun acc aenv (a -> b)
         -> Idx senv a
         -> Producer acc aenv senv b

  -- Apply the given the given function to all elements of the given
  -- sequence.
  ChunkedMapSeq :: (Arrays a, Arrays b)
                => PreOpenAfun acc aenv (Vector' a -> Vector' b)
                -> Idx senv a
                -> Producer acc aenv senv b

  -- Apply a given binary function pairwise to all elements of the
  -- given sequences.
  ZipWithSeq :: (Arrays a, Arrays b, Arrays c)
             => PreOpenAfun acc aenv (a -> b -> c)
             -> Idx senv a
             -> Idx senv b
             -> Producer acc aenv senv c

  -- ScanSeq (+) a0 x. Scan a sequence x by combining each element
  -- using the given binary operation (+). (+) must be associative:
  --
  --   Forall a b c. (a + b) + c = a + (b + c),
  --
  -- and a0 must be the identity element for (+):
  --
  --   Forall a. a0 + a = a = a + a0.
  --
  ScanSeq :: Elt e
          => PreFun acc aenv (e -> e -> e)
          -> PreExp acc aenv e
          -> Idx senv (Scalar e)
          -> Producer acc aenv senv (Scalar e)

data Consumer acc aenv senv a where

  -- FoldSeq (+) a0 x. Fold a sequence x by combining each element
  -- using the given binary operation (+). (+) must be associative:
  --
  --   Forall a b c. (a + b) + c = a + (b + c),
  --
  -- and a0 must be the identity element for (+):
  --
  --   Forall a. a0 + a = a = a + a0.
  --
  FoldSeq :: Elt a
          => PreFun acc aenv (a -> a -> a)
          -> PreExp acc aenv a
          -> Idx senv (Scalar a)
          -> Consumer acc aenv senv (Scalar a)

  -- FoldSeqFlatten f a0 x. A specialized version of FoldSeqAct where
  -- reduction with the companion operator corresponds to
  -- flattening. f must be semi-associative, with vecotor append (++)
  -- as the companion operator:
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
  FoldSeqFlatten :: (Arrays a, Shape sh, Elt e)
                 => PreOpenAfun acc aenv (a -> Vector sh -> Vector e -> a)
                 -> acc aenv a
                 -> Idx senv (Array sh e)
                 -> Consumer acc aenv senv a

  Stuple :: (Arrays a, IsAtuple a)
         => Atuple (Consumer acc aenv senv) (TupleRepr a)
         -> Consumer acc aenv senv a

-- |Closed sequence computation
--
type Seq = PreOpenSeq OpenAcc () ()
--}


-- | Vanilla stencil boundary condition
--
type Boundary = PreBoundary OpenAcc

-- | Boundary condition specification for stencil operations
--
data PreBoundary acc aenv t where
  -- Clamp coordinates to the extent of the array
  Clamp     :: PreBoundary acc aenv t

  -- Mirror coordinates beyond the array extent
  Mirror    :: PreBoundary acc aenv t

  -- Wrap coordinates around on each dimension
  Wrap      :: PreBoundary acc aenv t

  -- Use a constant value for outlying coordinates
  Constant  :: Elt e
            => EltRepr e
            -> PreBoundary acc aenv (Array sh e)

  -- Apply the given function to outlying coordinates
  Function  :: (Shape sh, Elt e)
            => PreFun acc aenv (sh -> e)
            -> PreBoundary acc aenv (Array sh e)


class HasArraysRepr f where
  arraysRepr :: f aenv a -> ArraysR a

instance HasArraysRepr acc => HasArraysRepr (PreOpenAcc acc) where
  arraysRepr (Alet _ _ body)                    = arraysRepr body
  arraysRepr (Avar ArrayVar{})                  = arraysRarray
  arraysRepr (Apair as bs)                      = TupRpair (arraysRepr as) (arraysRepr bs)
  arraysRepr Anil                               = TupRunit
  arraysRepr (Apply (Alam _ (Abody a)) _)       = arraysRepr a
  arraysRepr (Apply _ _)                        = error "Tomorrow will arrive, on time"
  arraysRepr (Aforeign _ (Alam _ (Abody a)) _)  = arraysRepr a
  arraysRepr (Aforeign _ (Abody _) _)           = error "And what have you got, at the end of the day?"
  arraysRepr (Aforeign _ (Alam _ (Alam _ _)) _) = error "A bottle of whisky. And a new set of lies."
  arraysRepr (Acond _ whenTrue _)               = arraysRepr whenTrue
  arraysRepr (Awhile _ (Alam lhs _) _)          = lhsToTupR lhs
  arraysRepr (Awhile _ _ _)                     = error "I want my, I want my MTV!"
  arraysRepr Use{}                              = arraysRarray
  arraysRepr Unit{}                             = arraysRarray
  arraysRepr Reshape{}                          = arraysRarray
  arraysRepr Generate{}                         = arraysRarray
  arraysRepr Transform{}                        = arraysRarray
  arraysRepr Replicate{}                        = arraysRarray
  arraysRepr Slice{}                            = arraysRarray
  arraysRepr Map{}                              = arraysRarray
  arraysRepr ZipWith{}                          = arraysRarray
  arraysRepr Fold{}                             = arraysRarray
  arraysRepr Fold1{}                            = arraysRarray
  arraysRepr FoldSeg{}                          = arraysRarray
  arraysRepr Fold1Seg{}                         = arraysRarray
  arraysRepr Scanl{}                            = arraysRarray
  arraysRepr Scanl'{}                           = arraysRtuple2
  arraysRepr Scanl1{}                           = arraysRarray
  arraysRepr Scanr{}                            = arraysRarray
  arraysRepr Scanr'{}                           = arraysRtuple2
  arraysRepr Scanr1{}                           = arraysRarray
  arraysRepr Permute{}                          = arraysRarray
  arraysRepr Backpermute{}                      = arraysRarray
  arraysRepr Stencil{}                          = arraysRarray
  arraysRepr Stencil2{}                         = arraysRarray

instance HasArraysRepr OpenAcc where
  arraysRepr (OpenAcc a) = arraysRepr a
-- Embedded expressions
-- --------------------

-- |Parametrised open function abstraction
--
data PreOpenFun acc env aenv t where
  Body ::                             PreOpenExp acc env  aenv t -> PreOpenFun acc env aenv t
  Lam  :: ELeftHandSide a env env' -> PreOpenFun acc env' aenv t -> PreOpenFun acc env aenv (a -> t)

-- |Vanilla open function abstraction
--
type OpenFun = PreOpenFun OpenAcc

-- |Parametrised function without free scalar variables
--
type PreFun acc = PreOpenFun acc ()

-- |Vanilla function without free scalar variables
--
type Fun = OpenFun ()

-- |Vanilla open expression
--
type OpenExp = PreOpenExp OpenAcc

-- |Parametrised expression without free scalar variables
--
type PreExp acc = PreOpenExp acc ()

-- |Vanilla expression without free scalar variables
--
type Exp = OpenExp ()

-- |Parametrised open expressions using de Bruijn indices for variables ranging over tuples
-- of scalars and arrays of tuples.  All code, except Cond, is evaluated eagerly.  N-tuples are
-- represented as nested pairs.
--
-- The data type is parametrised over the representation type (not the surface types).
--
data PreOpenExp acc env aenv t where

  -- Local binding of a scalar expression
  Let           :: ELeftHandSide bnd_t env env'
                -> PreOpenExp acc env  aenv bnd_t
                -> PreOpenExp acc env' aenv body_t
                -> PreOpenExp acc env  aenv body_t

  -- Variable index, ranging only over tuples or scalars
  EVar          :: ScalarVar env t
                -> PreOpenExp acc env aenv t

  -- Apply a backend-specific foreign function
  Foreign       :: (Sugar.Foreign asm, Sugar.Elt x, Sugar.Elt y)
                => asm           (x -> y)                             -- foreign function
                -> PreFun acc () (Sugar.EltRepr x -> Sugar.EltRepr y) -- alternate implementation (for other backends)
                -> PreOpenExp acc env aenv (Sugar.EltRepr x)
                -> PreOpenExp acc env aenv (Sugar.EltRepr y)

  -- Tuples
  Pair          :: PreOpenExp acc env aenv t1
                -> PreOpenExp acc env aenv t2
                -> PreOpenExp acc env aenv (t1, t2)

  Nil           :: PreOpenExp acc env aenv ()

  -- Array indices & shapes
  -- TODO: IndexIgnore?
  IndexAny      :: Shape sh
                => PreOpenExp acc env aenv (Any sh)

  IndexSlice    :: (Shape sh, Shape sl)
                => SliceIndex slix sl co sh
                -> PreOpenExp acc env aenv slix
                -> PreOpenExp acc env aenv sh
                -> PreOpenExp acc env aenv sl

  IndexFull     :: (Shape sh, Shape sl)
                => SliceIndex slix sl co sh
                -> PreOpenExp acc env aenv slix
                -> PreOpenExp acc env aenv sl
                -> PreOpenExp acc env aenv sh

  -- Shape and index conversion
  ToIndex       :: Shape sh
                => PreOpenExp acc env aenv sh           -- shape of the array
                -> PreOpenExp acc env aenv sh           -- index into the array
                -> PreOpenExp acc env aenv Int

  FromIndex     :: Shape sh
                => PreOpenExp acc env aenv sh           -- shape of the array
                -> PreOpenExp acc env aenv Int          -- index into linear representation
                -> PreOpenExp acc env aenv sh

  -- Conditional expression (non-strict in 2nd and 3rd argument)
  Cond          :: PreOpenExp acc env aenv Bool
                -> PreOpenExp acc env aenv t
                -> PreOpenExp acc env aenv t
                -> PreOpenExp acc env aenv t

  -- Value recursion
  While         :: PreOpenFun acc env aenv (a -> Bool)  -- continue while true
                -> PreOpenFun acc env aenv (a -> a)     -- function to iterate
                -> PreOpenExp acc env aenv a            -- initial value
                -> PreOpenExp acc env aenv a

  -- Constant values
  Const         :: ScalarType t
                -> t
                -> PreOpenExp acc env aenv t

  PrimConst     :: PrimConst t
                -> PreOpenExp acc env aenv t

  -- Primitive scalar operations
  PrimApp       :: PrimFun (a -> r)
                -> PreOpenExp acc env aenv a
                -> PreOpenExp acc env aenv r

  -- Project a single scalar from an array.
  -- The array expression can not contain any free scalar variables.
  Index         :: acc                aenv (Array dim t)
                -> PreOpenExp acc env aenv dim
                -> PreOpenExp acc env aenv t

  LinearIndex   :: acc                aenv (Array dim t)
                -> PreOpenExp acc env aenv Int
                -> PreOpenExp acc env aenv t

  -- Array shape.
  -- The array expression can not contain any free scalar variables.
  Shape         :: acc                aenv (Array dim e)
                -> PreOpenExp acc env aenv dim

  -- Number of elements of an array given its shape
  ShapeSize     :: PreOpenExp acc env aenv dim
                -> PreOpenExp acc env aenv Int

  {-
  -- Intersection of two shapes
  Intersect     :: Shape dim
                => PreOpenExp acc env aenv dim
                -> PreOpenExp acc env aenv dim
                -> PreOpenExp acc env aenv dim

  -- Union of two shapes
  Union         :: Shape dim
                => PreOpenExp acc env aenv dim
                -> PreOpenExp acc env aenv dim
                -> PreOpenExp acc env aenv dim
-}
  -- Unsafe operations (may fail or result in undefined behaviour)
  -- An unspecified bit pattern
  Undef         :: ScalarType t
                -> PreOpenExp acc env aenv t

  -- Reinterpret the bits of a value as a different type
  Coerce        :: BitSizeEq a b
                => ScalarType a
                -> ScalarType b
                -> PreOpenExp acc env aenv a
                -> PreOpenExp acc env aenv b


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
  PrimAtan2          :: FloatingType a -> PrimFun ((a, a) -> a)
  PrimIsNaN          :: FloatingType a -> PrimFun (a -> Bool)
  PrimIsInfinite     :: FloatingType a -> PrimFun (a -> Bool)

  -- relational and equality operators
  PrimLt   :: SingleType a -> PrimFun ((a, a) -> Bool)
  PrimGt   :: SingleType a -> PrimFun ((a, a) -> Bool)
  PrimLtEq :: SingleType a -> PrimFun ((a, a) -> Bool)
  PrimGtEq :: SingleType a -> PrimFun ((a, a) -> Bool)
  PrimEq   :: SingleType a -> PrimFun ((a, a) -> Bool)
  PrimNEq  :: SingleType a -> PrimFun ((a, a) -> Bool)
  PrimMax  :: SingleType a -> PrimFun ((a, a) -> a   )
  PrimMin  :: SingleType a -> PrimFun ((a, a) -> a   )

  -- logical operators
  PrimLAnd :: PrimFun ((Bool, Bool) -> Bool)
  PrimLOr  :: PrimFun ((Bool, Bool) -> Bool)
  PrimLNot :: PrimFun (Bool         -> Bool)

  -- character conversions
  PrimOrd  :: PrimFun (Char -> Int)
  PrimChr  :: PrimFun (Int  -> Char)

  -- boolean conversion
  PrimBoolToInt :: PrimFun (Bool -> Int)

  -- general conversion between types
  PrimFromIntegral :: IntegralType a -> NumType b -> PrimFun (a -> b)
  PrimToFloating   :: NumType a -> FloatingType b -> PrimFun (a -> b)

primConstType :: PrimConst a -> TupleType a
primConstType prim = case prim of
  PrimMinBound t -> boundedType t
  PrimMaxBound t -> boundedType t
  PrimPi       t -> floatingType t
  where
    boundedType :: BoundedType a -> TupleType a
    boundedType (IntegralBoundedType t) = SingleType $ NumSingleType $ IntegralNumType t
    boundedType (NonNumBoundedType t)   = SingleType $ NonNumSingleType t

    floatingType :: FloatingType t -> TupleType t
    floatingType = numType . FloatingNumType

primFunType :: PrimFun (a -> b) -> (TupleType a, TupleType b)
primFunType prim = case prim of
  -- Num
  PrimAdd t                 -> binary' $ numType t
  PrimSub t                 -> binary' $ numType t
  PrimMul t                 -> binary' $ numType t
  PrimNeg t                 -> unary'  $ numType t
  PrimAbs t                 -> unary'  $ numType t
  PrimSig t                 -> unary'  $ numType t

  -- Integral
  PrimQuot t                -> binary' $ integralType t
  PrimRem  t                -> binary' $ integralType t
  PrimQuotRem t             -> divMod t
  PrimIDiv t                -> binary' $ integralType t
  PrimMod  t                -> binary' $ integralType t
  PrimDivMod t              -> divMod t

  -- Bits & FiniteBits
  PrimBAnd t                -> binary' $ integralType t
  PrimBOr t                 -> binary' $ integralType t
  PrimBXor t                -> binary' $ integralType t
  PrimBNot t                -> unary' $ integralType t
  PrimBShiftL t             -> (integralType t `TupRpair` typeInt, integralType t)
  PrimBShiftR t             -> (integralType t `TupRpair` typeInt, integralType t)
  PrimBRotateL t            -> (integralType t `TupRpair` typeInt, integralType t)
  PrimBRotateR t            -> (integralType t `TupRpair` typeInt, integralType t)
  PrimPopCount t            -> unary (integralType t) typeInt
  PrimCountLeadingZeros t   -> unary (integralType t) typeInt
  PrimCountTrailingZeros t  -> unary (integralType t) typeInt

  -- Fractional, Floating
  PrimFDiv t                -> binary' $ floatingType t
  PrimRecip t               -> unary' $ floatingType t
  PrimSin t                 -> unary' $ floatingType t
  PrimCos t                 -> unary' $ floatingType t
  PrimTan t                 -> unary' $ floatingType t
  PrimAsin t                -> unary' $ floatingType t
  PrimAcos t                -> unary' $ floatingType t
  PrimAtan t                -> unary' $ floatingType t
  PrimSinh t                -> unary' $ floatingType t
  PrimCosh t                -> unary' $ floatingType t
  PrimTanh t                -> unary' $ floatingType t
  PrimAsinh t               -> unary' $ floatingType t
  PrimAcosh t               -> unary' $ floatingType t
  PrimAtanh t               -> unary' $ floatingType t
  PrimExpFloating t         -> unary' $ floatingType t
  PrimSqrt t                -> unary' $ floatingType t
  PrimLog t                 -> unary' $ floatingType t
  PrimFPow t                -> binary' $ floatingType t
  PrimLogBase t             -> binary' $ floatingType t

  -- RealFrac
  PrimTruncate a b          -> unary (floatingType a) (integralType b)
  PrimRound a b             -> unary (floatingType a) (integralType b)
  PrimFloor a b             -> unary (floatingType a) (integralType b)
  PrimCeiling a b           -> unary (floatingType a) (integralType b)

  -- RealFloat
  PrimAtan2 t               -> binary' $ floatingType t
  PrimIsNaN t               -> unary (floatingType t) typeBool
  PrimIsInfinite t          -> unary (floatingType t) typeBool

  -- Relational and equality
  PrimLt t                  -> compare t
  PrimGt t                  -> compare t
  PrimLtEq t                -> compare t
  PrimGtEq t                -> compare t
  PrimEq t                  -> compare t
  PrimMax t                 -> binary $ singleType t
  PrimMin t                 -> binary $ singleType t

  -- Logical
  PrimLAnd                  -> binary' typeBool
  PrimLOr                   -> binary' typeBool
  PrimLNot                  -> unary' typeBool

  -- character conversions
  PrimOrd                   -> unary typeChar typeInt
  PrimChr                   -> unary typeInt  typeChar

  -- boolean conversion
  PrimBoolToInt             -> unary typeBool typeInt

  -- general conversion between types
  PrimFromIntegral a b      -> unary (integralType a) (numType b)
  PrimToFloating   a b      -> unary (numType a) (floatingType b)

  where
    unary :: TupleType a -> TupleType b -> (TupleType a, TupleType b)
    unary a b = (a, b)

    unary' :: TupleType a -> (TupleType a, TupleType a)
    unary' a = unary a a

    binary :: TupleType a -> TupleType b -> (TupleType (a, a), TupleType b)
    binary a b = (a `TupRpair` a, b)

    binary' :: TupleType a -> (TupleType (a, a), TupleType a)
    binary' a = binary a a

    compare :: SingleType a -> (TupleType (a, a), TupleType Bool)
    compare a = binary (singleType a) typeBool

    singleType :: SingleType t -> TupleType t
    singleType = SingleScalarType

    numType :: NumType t -> TupleType t
    numType = SingleScalarType . NumSingleType

    integralType :: IntegralType t -> TupleType t
    integralType = numType . IntegralNumType

    floatingType :: FloatingType t -> TupleType t
    floatingType = numType . FloatingNumType

    divMod :: IntegralType t -> (TupleType (t, t), TupleType (t, t))
    divMod t = unary' $ integralType t `TupRpair` integralType t

    typeBool :: TupleType Bool
    typeBool = SingleScalarType $ NonNumSingleType $ TypeBool

    typeChar :: TupleType Char
    typeChar = SingleScalarType $ NonNumSingleType $ TypeChar

    typeInt :: TupleType Int
    typeInt = SingleScalarType $ NumSingleType $ IntegralNumType TypeInt

-- NFData instances
-- ================

instance NFData (OpenAfun aenv f) where
  rnf = rnfOpenAfun

instance NFData (OpenAcc aenv t) where
  rnf = rnfOpenAcc

-- instance NFData (Seq t) where
--   rnf = rnfPreOpenSeq rnfOpenAcc

instance NFData (OpenExp env aenv t) where
  rnf = rnfPreOpenExp rnfOpenAcc

instance NFData (OpenFun env aenv t) where
  rnf = rnfPreOpenFun rnfOpenAcc


-- Array expressions
-- -----------------

type NFDataAcc acc = forall aenv t. acc aenv t -> ()

rnfIdx :: Idx env t -> ()
rnfIdx ZeroIdx      = ()
rnfIdx (SuccIdx ix) = rnfIdx ix

rnfTupleIdx :: TupleIdx t e -> ()
rnfTupleIdx ZeroTupIdx       = ()
rnfTupleIdx (SuccTupIdx tix) = rnfTupleIdx tix

rnfOpenAfun :: OpenAfun aenv t -> ()
rnfOpenAfun = rnfPreOpenAfun rnfOpenAcc

rnfOpenAcc :: OpenAcc aenv t -> ()
rnfOpenAcc (OpenAcc pacc) = rnfPreOpenAcc rnfOpenAcc pacc

rnfPreOpenAfun :: NFDataAcc acc -> PreOpenAfun acc aenv t -> ()
rnfPreOpenAfun rnfA (Abody b) = rnfA b
rnfPreOpenAfun rnfA (Alam lhs f) = rnfALhs lhs `seq` rnfPreOpenAfun rnfA f

rnfPreOpenAcc :: forall acc aenv t. NFDataAcc acc -> PreOpenAcc acc aenv t -> ()
rnfPreOpenAcc rnfA pacc =
  let
      rnfAF :: PreOpenAfun acc aenv' t' -> ()
      rnfAF = rnfPreOpenAfun rnfA

      rnfE :: PreOpenExp acc env' aenv' t' -> ()
      rnfE = rnfPreOpenExp rnfA

      rnfF :: PreOpenFun acc env' aenv' t' -> ()
      rnfF = rnfPreOpenFun rnfA

      -- rnfS :: PreOpenSeq acc aenv' senv' t' -> ()
      -- rnfS = rnfPreOpenSeq rnfA

      rnfB :: PreBoundary acc aenv' (Array sh e) -> ()
      rnfB = rnfBoundary rnfA
  in
  case pacc of
    Alet lhs bnd body         -> rnfALhs lhs `seq` rnfA bnd `seq` rnfA body
    Avar (ArrayVar ix)        -> rnfIdx ix
    Apair as bs               -> rnfA as `seq` rnfA bs
    Anil                      -> ()
    Apply afun acc            -> rnfAF afun `seq` rnfA acc
    Aforeign asm afun a       -> rnf (strForeign asm) `seq` rnfAF afun `seq` rnfA a
    Acond p a1 a2             -> rnfE p `seq` rnfA a1 `seq` rnfA a2
    Awhile p f a              -> rnfAF p `seq` rnfAF f `seq` rnfA a
    Use arr                   -> rnf arr
    Unit x                    -> rnfE x
    Reshape sh a              -> rnfE sh `seq` rnfA a
    Generate sh f             -> rnfE sh `seq` rnfF f
    Transform sh p f a        -> rnfE sh `seq` rnfF p `seq` rnfF f `seq` rnfA a
    Replicate slice sh a      -> rnfSliceIndex slice `seq` rnfE sh `seq` rnfA a
    Slice slice a sh          -> rnfSliceIndex slice `seq` rnfE sh `seq` rnfA a
    Map f a                   -> rnfF f `seq` rnfA a
    ZipWith f a1 a2           -> rnfF f `seq` rnfA a1 `seq` rnfA a2
    Fold f z a                -> rnfF f `seq` rnfE z `seq` rnfA a
    Fold1 f a                 -> rnfF f `seq` rnfA a
    FoldSeg f z a s           -> rnfF f `seq` rnfE z `seq` rnfA a `seq` rnfA s
    Fold1Seg f a s            -> rnfF f `seq` rnfA a `seq` rnfA s
    Scanl f z a               -> rnfF f `seq` rnfE z `seq` rnfA a
    Scanl1 f a                -> rnfF f `seq` rnfA a
    Scanl' f z a              -> rnfF f `seq` rnfE z `seq` rnfA a
    Scanr f z a               -> rnfF f `seq` rnfE z `seq` rnfA a
    Scanr1 f a                -> rnfF f `seq` rnfA a
    Scanr' f z a              -> rnfF f `seq` rnfE z `seq` rnfA a
    Permute f d p a           -> rnfF f `seq` rnfA d `seq` rnfF p `seq` rnfA a
    Backpermute sh f a        -> rnfE sh `seq` rnfF f `seq` rnfA a
    Stencil f b a             -> rnfF f `seq` rnfB b  `seq` rnfA a
    Stencil2 f b1 a1 b2 a2    -> rnfF f `seq` rnfB b1 `seq` rnfB b2 `seq` rnfA a1 `seq` rnfA a2
    -- Collect s                 -> rnfS s

rnfLhs :: (forall b. s b -> ()) -> LeftHandSide s arrs env env' -> ()
rnfLhs rnfS (LeftHandSideWildcard r)   = rnfTupR rnfS r
rnfLhs rnfS (LeftHandSideSingle s)     = rnfS s
rnfLhs rnfS (LeftHandSidePair ar1 ar2) = rnfLhs rnfS ar1 `seq` rnfLhs rnfS ar2

rnfALhs :: ALeftHandSide arrs aenv aenv' -> ()
rnfALhs = rnfLhs rnfArrayR

rnfELhs :: ELeftHandSide t env env' -> ()
rnfELhs = rnfLhs rnfScalarType

rnfTupR :: (forall b. s b -> ()) -> TupR s a -> ()
rnfTupR _    TupRunit = ()
rnfTupR rnfS (TupRsingle s) = rnfS s
rnfTupR rnfS (TupRpair t1 t2) = rnfTupR rnfS t1 `seq` rnfTupR rnfS t2

rnfArrayR :: ArrayR arr -> ()
rnfArrayR ArrayR = ()

rnfArrays :: ArraysR arrs -> arrs -> ()
rnfArrays TupRunit            ()      = ()
rnfArrays (TupRsingle ArrayR) arr     = rnf arr
rnfArrays (TupRpair ar1 ar2)  (a1,a2) = rnfArrays ar1 a1 `seq` rnfArrays ar2 a2

rnfBoundary :: forall acc aenv sh e. NFDataAcc acc -> PreBoundary acc aenv (Array sh e) -> ()
rnfBoundary _    Clamp        = ()
rnfBoundary _    Mirror       = ()
rnfBoundary _    Wrap         = ()
rnfBoundary _    (Constant c) = rnfConst (eltType @e) c
rnfBoundary rnfA (Function f) = rnfPreOpenFun rnfA f


{--
-- Sequence expressions
-- --------------------

rnfPreOpenSeq :: forall acc aenv senv t. NFDataAcc acc -> PreOpenSeq acc aenv senv t -> ()
rnfPreOpenSeq rnfA topSeq =
  let
      rnfS :: PreOpenSeq acc aenv' senv' t' -> ()
      rnfS = rnfPreOpenSeq rnfA

      rnfP :: Producer acc aenv' senv' t' -> ()
      rnfP = rnfSeqProducer rnfA

      rnfC :: Consumer acc aenv' senv' t' -> ()
      rnfC = rnfSeqConsumer rnfA
  in
  case topSeq of
    Producer p s              -> rnfP p `seq` rnfS s
    Consumer c                -> rnfC c
    Reify ix                  -> rnfIdx ix

rnfSeqProducer :: forall acc aenv senv t. NFDataAcc acc -> Producer acc aenv senv t -> ()
rnfSeqProducer rnfA topSeq =
  let
      rnfArrs :: forall a. Arrays a => [a] -> ()
      rnfArrs []     = ()
      rnfArrs (a:as) = rnfArrays (arrays @a) (fromArr a) `seq` rnfArrs as

      rnfAF :: PreOpenAfun acc aenv' t' -> ()
      rnfAF = rnfPreOpenAfun rnfA

      rnfF :: PreOpenFun acc env' aenv' t' -> ()
      rnfF = rnfPreOpenFun rnfA

      rnfE :: PreOpenExp acc env' aenv' t' -> ()
      rnfE = rnfPreOpenExp rnfA
  in
  case topSeq of
    StreamIn as               -> rnfArrs as
    ToSeq slice _ a           -> rnfSliceIndex slice `seq` rnfA a
    MapSeq f ix               -> rnfAF f `seq` rnfIdx ix
    ChunkedMapSeq f ix        -> rnfAF f `seq` rnfIdx ix
    ZipWithSeq f ix1 ix2      -> rnfAF f `seq` rnfIdx ix1 `seq` rnfIdx ix2
    ScanSeq f z ix            -> rnfF f `seq` rnfE z `seq` rnfIdx ix

rnfSeqConsumer :: forall acc aenv senv t. NFDataAcc acc -> Consumer acc aenv senv t -> ()
rnfSeqConsumer rnfA topSeq =
  let
      rnfAF :: PreOpenAfun acc aenv' t' -> ()
      rnfAF = rnfPreOpenAfun rnfA

      rnfF :: PreOpenFun acc env' aenv' t' -> ()
      rnfF = rnfPreOpenFun rnfA

      rnfE :: PreOpenExp acc env' aenv' t' -> ()
      rnfE = rnfPreOpenExp rnfA
  in
  case topSeq of
    FoldSeq f z ix            -> rnfF f `seq` rnfE z `seq` rnfIdx ix
    FoldSeqFlatten f a ix     -> rnfAF f `seq` rnfA a `seq` rnfIdx ix
    Stuple stup               -> rnfStuple rnfA stup

rnfStuple :: NFDataAcc acc -> Atuple (Consumer acc aenv senv) t -> ()
rnfStuple _    NilAtup          = ()
rnfStuple rnfA (SnocAtup tup c) = rnfStuple rnfA tup `seq` rnfSeqConsumer rnfA c
--}

-- Scalar expressions
-- ------------------

rnfPreOpenFun :: NFDataAcc acc -> PreOpenFun acc env aenv t -> ()
rnfPreOpenFun rnfA (Body b) = rnfPreOpenExp rnfA b
rnfPreOpenFun rnfA (Lam f)  = rnfPreOpenFun rnfA f

rnfPreOpenExp :: forall acc env aenv t. NFDataAcc acc -> PreOpenExp acc env aenv t -> ()
rnfPreOpenExp rnfA topExp =
  let
      rnfF :: PreOpenFun acc env' aenv' t' -> ()
      rnfF = rnfPreOpenFun rnfA

      rnfE :: PreOpenExp acc env' aenv' t' -> ()
      rnfE = rnfPreOpenExp rnfA
  in
  case topExp of
    Let bnd body              -> rnfE bnd `seq` rnfE body
    Var ix                    -> rnfIdx ix
    Foreign asm f x           -> rnf (strForeign asm) `seq` rnfF f `seq` rnfE x
    Const t                   -> rnfConst (eltType @t) t
    Undef                     -> ()
    Pair a b                  -> rnfE a `seq` rnfE b
    Nil                       -> ()
    IndexAny                  -> ()
    IndexSlice slice slix sh  -> rnfSliceIndex slice `seq` rnfE slix `seq` rnfE sh
    IndexFull slice slix sl   -> rnfSliceIndex slice `seq` rnfE slix `seq` rnfE sl
    ToIndex sh ix             -> rnfE sh `seq` rnfE ix
    FromIndex sh ix           -> rnfE sh `seq` rnfE ix
    Cond p e1 e2              -> rnfE p `seq` rnfE e1 `seq` rnfE e2
    While p f x               -> rnfF p `seq` rnfF f `seq` rnfE x
    PrimConst c               -> rnfPrimConst c
    PrimApp f x               -> rnfPrimFun f `seq` rnfE x
    Index a ix                -> rnfA a `seq` rnfE ix
    LinearIndex a ix          -> rnfA a `seq` rnfE ix
    Shape a                   -> rnfA a
    ShapeSize sh              -> rnfE sh
    Coerce e                  -> rnfE e

rnfConst :: TupleType t -> t -> ()
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
rnfPrimFun PrimOrd                    = ()
rnfPrimFun PrimChr                    = ()
rnfPrimFun PrimBoolToInt              = ()
rnfPrimFun (PrimFromIntegral i n)     = rnfIntegralType i `seq` rnfNumType n
rnfPrimFun (PrimToFloating n f)       = rnfNumType n `seq` rnfFloatingType f

rnfSliceIndex :: SliceIndex ix slice co sh -> ()
rnfSliceIndex SliceNil        = ()
rnfSliceIndex (SliceAll sh)   = rnfSliceIndex sh
rnfSliceIndex (SliceFixed sh) = rnfSliceIndex sh

rnfScalarType :: ScalarType t -> ()
rnfScalarType (SingleScalarType t) = rnfSingleType t
rnfScalarType (VectorScalarType t) = rnfVectorType t

rnfSingleType :: SingleType t -> ()
rnfSingleType (NumSingleType t)    = rnfNumType t
rnfSingleType (NonNumSingleType t) = rnfNonNumType t

rnfVectorType :: VectorType t -> ()
rnfVectorType (VectorType !_ t) = rnfSingleType t

rnfBoundedType :: BoundedType t -> ()
rnfBoundedType (IntegralBoundedType t) = rnfIntegralType t
rnfBoundedType (NonNumBoundedType t)   = rnfNonNumType t

rnfNumType :: NumType t -> ()
rnfNumType (IntegralNumType t) = rnfIntegralType t
rnfNumType (FloatingNumType t) = rnfFloatingType t

rnfNonNumType :: NonNumType t -> ()
rnfNonNumType (TypeBool   NonNumDict) = ()
rnfNonNumType (TypeChar   NonNumDict) = ()

rnfIntegralType :: IntegralType t -> ()
rnfIntegralType (TypeInt     IntegralDict) = ()
rnfIntegralType (TypeInt8    IntegralDict) = ()
rnfIntegralType (TypeInt16   IntegralDict) = ()
rnfIntegralType (TypeInt32   IntegralDict) = ()
rnfIntegralType (TypeInt64   IntegralDict) = ()
rnfIntegralType (TypeWord    IntegralDict) = ()
rnfIntegralType (TypeWord8   IntegralDict) = ()
rnfIntegralType (TypeWord16  IntegralDict) = ()
rnfIntegralType (TypeWord32  IntegralDict) = ()
rnfIntegralType (TypeWord64  IntegralDict) = ()

rnfFloatingType :: FloatingType t -> ()
rnfFloatingType (TypeHalf    FloatingDict) = ()
rnfFloatingType (TypeFloat   FloatingDict) = ()
rnfFloatingType (TypeDouble  FloatingDict) = ()


-- Template Haskell
-- ================

type LiftAcc acc = forall aenv a. acc aenv a -> Q (TExp (acc aenv a))

liftIdx :: Idx env t -> Q (TExp (Idx env t))
liftIdx ZeroIdx      = [|| ZeroIdx ||]
liftIdx (SuccIdx ix) = [|| SuccIdx $$(liftIdx ix) ||]

liftTupleIdx :: TupleIdx t e -> Q (TExp (TupleIdx t e))
liftTupleIdx ZeroTupIdx       = [|| ZeroTupIdx ||]
liftTupleIdx (SuccTupIdx tix) = [|| SuccTupIdx $$(liftTupleIdx tix) ||]


liftPreOpenAfun :: LiftAcc acc -> PreOpenAfun acc aenv t -> Q (TExp (PreOpenAfun acc aenv t))
liftPreOpenAfun liftA (Alam lhs f) = [|| Alam $$(liftALhs lhs) $$(liftPreOpenAfun liftA f) ||]
liftPreOpenAfun liftA (Abody b)    = [|| Abody $$(liftA b) ||]

liftPreOpenAcc
    :: forall acc aenv a.
       LiftAcc acc
    -> PreOpenAcc acc aenv a
    -> Q (TExp (PreOpenAcc acc aenv a))
liftPreOpenAcc liftA pacc =
  let
      liftE :: PreOpenExp acc env aenv t -> Q (TExp (PreOpenExp acc env aenv t))
      liftE = liftPreOpenExp liftA

      liftF :: PreOpenFun acc env aenv t -> Q (TExp (PreOpenFun acc env aenv t))
      liftF = liftPreOpenFun liftA

      liftAF :: PreOpenAfun acc aenv f -> Q (TExp (PreOpenAfun acc aenv f))
      liftAF = liftPreOpenAfun liftA

      liftB :: PreBoundary acc aenv (Array sh e) -> Q (TExp (PreBoundary acc aenv (Array sh e)))
      liftB = liftBoundary liftA

  in
  case pacc of
    Alet lhs bnd body         -> [|| Alet $$(liftALhs lhs) $$(liftA bnd) $$(liftA body) ||]
    Avar (ArrayVar ix)        -> [|| Avar (ArrayVar $$(liftIdx ix)) ||]
    Apair as bs               -> [|| Apair $$(liftA as) $$(liftA bs) ||]
    Anil                      -> [|| Anil ||]
    Apply f a                 -> [|| Apply $$(liftAF f) $$(liftA a) ||]
    Aforeign asm f a          -> [|| Aforeign $$(liftForeign asm) $$(liftPreOpenAfun liftA f) $$(liftA a) ||]
    Acond p t e               -> [|| Acond $$(liftE p) $$(liftA t) $$(liftA e) ||]
    Awhile p f a              -> [|| Awhile $$(liftAF p) $$(liftAF f) $$(liftA a) ||]
    Use a                     -> [|| Use $$(liftArray a) ||]
    Unit e                    -> [|| Unit $$(liftE e) ||]
    Reshape sh a              -> [|| Reshape $$(liftE sh) $$(liftA a) ||]
    Generate sh f             -> [|| Generate $$(liftE sh) $$(liftF f) ||]
    Transform sh p f a        -> [|| Transform $$(liftE sh) $$(liftF p) $$(liftF f) $$(liftA a) ||]
    Replicate slix sl a       -> [|| Replicate $$(liftSliceIndex slix) $$(liftE sl) $$(liftA a) ||]
    Slice slix a sh           -> [|| Slice $$(liftSliceIndex slix) $$(liftA a) $$(liftE sh) ||]
    Map f a                   -> [|| Map $$(liftF f) $$(liftA a) ||]
    ZipWith f a b             -> [|| ZipWith $$(liftF f) $$(liftA a) $$(liftA b) ||]
    Fold f z a                -> [|| Fold $$(liftF f) $$(liftE z) $$(liftA a) ||]
    Fold1 f a                 -> [|| Fold1 $$(liftF f) $$(liftA a) ||]
    FoldSeg f z a s           -> [|| FoldSeg $$(liftF f) $$(liftE z) $$(liftA a) $$(liftA s) ||]
    Fold1Seg f a s            -> [|| Fold1Seg $$(liftF f) $$(liftA a) $$(liftA s) ||]
    Scanl f z a               -> [|| Scanl $$(liftF f) $$(liftE z) $$(liftA a) ||]
    Scanl1 f a                -> [|| Scanl1 $$(liftF f) $$(liftA a) ||]
    Scanl' f z a              -> [|| Scanl' $$(liftF f) $$(liftE z) $$(liftA a) ||]
    Scanr f z a               -> [|| Scanr $$(liftF f) $$(liftE z) $$(liftA a) ||]
    Scanr1 f a                -> [|| Scanr1 $$(liftF f) $$(liftA a) ||]
    Scanr' f z a              -> [|| Scanr' $$(liftF f) $$(liftE z) $$(liftA a) ||]
    Permute f d p a           -> [|| Permute $$(liftF f) $$(liftA d) $$(liftF p) $$(liftA a) ||]
    Backpermute sh p a        -> [|| Backpermute $$(liftE sh) $$(liftF p) $$(liftA a) ||]
    Stencil f b a             -> [|| Stencil $$(liftF f) $$(liftB b) $$(liftA a) ||]
    Stencil2 f b1 a1 b2 a2    -> [|| Stencil2 $$(liftF f) $$(liftB b1) $$(liftA a1) $$(liftB b2) $$(liftA a2) ||]

liftALhs :: ALeftHandSide arrs aenv aenv' -> Q (TExp (ALeftHandSide arrs aenv aenv'))
liftALhs (LeftHandSideWildcard r)    = [|| LeftHandSideWildcard $$(liftArraysR r) ||]
liftALhs (LeftHandSideSingle ArrayR) = [|| LeftHandSideSingle ArrayR ||]
liftALhs (LeftHandSidePair a b)      = [|| LeftHandSidePair $$(liftALhs a) $$(liftALhs b) ||]

liftELhs :: ELeftHandSide t aenv aenv' -> Q (TExp (ELeftHandSide t aenv aenv'))
liftELhs (LeftHandSideWildcard r)    = [|| LeftHandSideWildcard $$(liftTupleType r) ||]
liftELhs (LeftHandSideSingle t)      = [|| LeftHandSideSingle $$(liftScalarType t) ||]
liftELhs (LeftHandSidePair a b)      = [|| LeftHandSidePair $$(liftELhs a) $$(liftELhs b) ||]

liftArraysR :: ArraysR arrs -> Q (TExp (ArraysR arrs))
liftArraysR TupRunit            = [|| TupRunit ||]
liftArraysR (TupRsingle ArrayR) = [|| TupRsingle ArrayR ||]
liftArraysR (TupRpair a b)      = [|| TupRpair $$(liftArraysR a) $$(liftArraysR b) ||]

liftPreOpenFun
    :: LiftAcc acc
    -> PreOpenFun acc env aenv t
    -> Q (TExp (PreOpenFun acc env aenv t))
liftPreOpenFun liftA (Lam f)  = [|| Lam  $$(liftPreOpenFun liftA f) ||]
liftPreOpenFun liftA (Body b) = [|| Body $$(liftPreOpenExp liftA b) ||]

liftPreOpenExp
    :: forall acc env aenv t.
       LiftAcc acc
    -> PreOpenExp acc env aenv t
    -> Q (TExp (PreOpenExp acc env aenv t))
liftPreOpenExp liftA pexp =
  let
      liftE :: PreOpenExp acc env aenv e -> Q (TExp (PreOpenExp acc env aenv e))
      liftE = liftPreOpenExp liftA

      liftF :: PreOpenFun acc env aenv f -> Q (TExp (PreOpenFun acc env aenv f))
      liftF = liftPreOpenFun liftA
  in
  case pexp of
    Let lhs bnd body          -> [|| Let $$(liftElhs lhs) $$(liftPreOpenExp liftA bnd) $$(liftPreOpenExp liftA body) ||]
    Var var                   -> [|| Var $$(liftScalarVar var) ||]
    Foreign asm f x           -> [|| Foreign $$(liftForeign asm) $$(liftPreOpenFun liftA f) $$(liftE x) ||]
    Const c                   -> [|| Const $$(liftConst (eltType @t) c) ||]
    Undef                     -> [|| Undef ||]
    Pair a b                  -> [|| Pair $$(liftE a) $$(liftE b) ||]
    Nil                       -> [|| Nil ||]
    IndexAny                  -> [|| IndexAny ||]
    IndexSlice slice slix sh  -> [|| IndexSlice $$(liftSliceIndex slice) $$(liftE slix) $$(liftE sh) ||]
    IndexFull slice slix sl   -> [|| IndexFull $$(liftSliceIndex slice) $$(liftE slix) $$(liftE sl) ||]
    ToIndex sh ix             -> [|| ToIndex $$(liftE sh) $$(liftE ix) ||]
    FromIndex sh ix           -> [|| FromIndex $$(liftE sh) $$(liftE ix) ||]
    Cond p t e                -> [|| Cond $$(liftE p) $$(liftE t) $$(liftE e) ||]
    While p f x               -> [|| While $$(liftF p) $$(liftF f) $$(liftE x) ||]
    PrimConst t               -> [|| PrimConst $$(liftPrimConst t) ||]
    PrimApp f x               -> [|| PrimApp $$(liftPrimFun f) $$(liftE x) ||]
    Index a ix                -> [|| Index $$(liftA a) $$(liftE ix) ||]
    LinearIndex a ix          -> [|| LinearIndex $$(liftA a) $$(liftE ix) ||]
    Shape a                   -> [|| Shape $$(liftA a) ||]
    ShapeSize ix              -> [|| ShapeSize $$(liftE ix) ||]
    Coerce e                  -> [|| Coerce $$(liftE e) ||]

liftScalarVar :: ScalarVar env t -> Q (TExp (ScalarVar env t))
liftScalarVar (ScalarVar tp ix) = [|| ScalarVar $$(liftScalarType tp) $$(liftIdx ix) ||]

liftArray :: forall sh e. Shape sh => TupleType -> Array sh e -> Q (TExp (Array sh e))
liftArray tp (Array sh adata) =
  [|| Array $$(liftConst (eltType @sh) sh) $$(go arrayElt adata) ||] `sigE` typeRepToType (typeOf (undefined::Array sh e))
  where
    sz :: Int
    sz = size sh

    sigE :: Q (TExp t) -> Q TH.Type -> Q (TExp t)
    sigE e t = TH.unsafeTExpCoerce $ TH.sigE (TH.unTypeQ e) t

    typeRepToType :: TypeRep -> Q TH.Type
    typeRepToType trep = do
      let (con, args)     = splitTyConApp trep
          name            = TH.Name (TH.OccName (tyConName con)) (TH.NameG TH.TcClsName (TH.PkgName (tyConPackage con)) (TH.ModName (tyConModule con)))
          --
          appsT x []      = x
          appsT x (y:xs)  = appsT (TH.AppT x y) xs
          --
      resultArgs <- mapM typeRepToType args
      return (appsT (TH.ConT name) resultArgs)

    -- TODO: make sure that the resulting array is 16-byte aligned...
    arr :: forall a. Storable a => UniqueArray a -> Q (TExp (UniqueArray a))
    arr ua = do
      bytes <- TH.runIO $ peekArray (sizeOf (undefined::a) * sz) (castPtr (unsafeUniqueArrayPtr ua) :: Ptr Word8)
      [|| unsafePerformIO $ do
           fp  <- newForeignPtr_ $$( TH.unsafeTExpCoerce [| Ptr $(TH.litE (TH.StringPrimL bytes)) |] )
           ua' <- newUniqueArray (castForeignPtr fp)
           return ua'
       ||]

    go :: ArrayEltR e' -> ArrayData e' -> Q (TExp (ArrayData e'))
    go ArrayEltRunit         AD_Unit         = [|| AD_Unit ||]
    go ArrayEltRint          (AD_Int ua)     = [|| AD_Int $$(arr ua) ||]
    go ArrayEltRint8         (AD_Int8 ua)    = [|| AD_Int8 $$(arr ua) ||]
    go ArrayEltRint16        (AD_Int16 ua)   = [|| AD_Int16 $$(arr ua) ||]
    go ArrayEltRint32        (AD_Int32 ua)   = [|| AD_Int32 $$(arr ua) ||]
    go ArrayEltRint64        (AD_Int64 ua)   = [|| AD_Int64 $$(arr ua) ||]
    go ArrayEltRword         (AD_Word ua)    = [|| AD_Word $$(arr ua) ||]
    go ArrayEltRword8        (AD_Word8 ua)   = [|| AD_Word8 $$(arr ua) ||]
    go ArrayEltRword16       (AD_Word16 ua)  = [|| AD_Word16 $$(arr ua) ||]
    go ArrayEltRword32       (AD_Word32 ua)  = [|| AD_Word32 $$(arr ua) ||]
    go ArrayEltRword64       (AD_Word64 ua)  = [|| AD_Word64 $$(arr ua) ||]
    go ArrayEltRhalf         (AD_Half ua)    = [|| AD_Half $$(arr ua) ||]
    go ArrayEltRfloat        (AD_Float ua)   = [|| AD_Float $$(arr ua) ||]
    go ArrayEltRdouble       (AD_Double ua)  = [|| AD_Double $$(arr ua) ||]
    go ArrayEltRbool         (AD_Bool ua)    = [|| AD_Bool $$(arr ua) ||]
    go ArrayEltRchar         (AD_Char ua)    = [|| AD_Char $$(arr ua) ||]
    go (ArrayEltRpair r1 r2) (AD_Pair a1 a2) = [|| AD_Pair $$(go r1 a1) $$(go r2 a2) ||]
    go (ArrayEltRvec r)      (AD_Vec w# a)   = TH.unsafeTExpCoerce $ [| AD_Vec $(liftInt# w#) $(TH.unTypeQ (go r a)) |]


liftBoundary
    :: forall acc aenv sh e.
       LiftAcc acc
    -> PreBoundary acc aenv (Array sh e)
    -> Q (TExp (PreBoundary acc aenv (Array sh e)))
liftBoundary _     Clamp        = [|| Clamp ||]
liftBoundary _     Mirror       = [|| Mirror ||]
liftBoundary _     Wrap         = [|| Wrap ||]
liftBoundary _     (Constant v) = [|| Constant $$(liftConst (eltType @e) v) ||]
liftBoundary liftA (Function f) = [|| Function $$(liftPreOpenFun liftA f) ||]

liftSliceIndex :: SliceIndex ix slice coSlice sliceDim -> Q (TExp (SliceIndex ix slice coSlice sliceDim))
liftSliceIndex SliceNil          = [|| SliceNil ||]
liftSliceIndex (SliceAll rest)   = [|| SliceAll $$(liftSliceIndex rest) ||]
liftSliceIndex (SliceFixed rest) = [|| SliceFixed $$(liftSliceIndex rest) ||]

liftPrimConst :: PrimConst c -> Q (TExp (PrimConst c))
liftPrimConst (PrimMinBound t) = [|| PrimMinBound $$(liftBoundedType t) ||]
liftPrimConst (PrimMaxBound t) = [|| PrimMaxBound $$(liftBoundedType t) ||]
liftPrimConst (PrimPi t)       = [|| PrimPi $$(liftFloatingType t) ||]

liftPrimFun :: PrimFun f -> Q (TExp (PrimFun f))
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
liftPrimFun PrimOrd                    = [|| PrimOrd ||]
liftPrimFun PrimChr                    = [|| PrimChr ||]
liftPrimFun PrimBoolToInt              = [|| PrimBoolToInt ||]
liftPrimFun (PrimFromIntegral ta tb)   = [|| PrimFromIntegral $$(liftIntegralType ta) $$(liftNumType tb) ||]
liftPrimFun (PrimToFloating ta tb)     = [|| PrimToFloating $$(liftNumType ta) $$(liftFloatingType tb) ||]

liftTupleType :: TupleType t -> Q (TExp (TupleType t))
liftTupleType TupRunit         = [|| TupRunit ||]
liftTupleType (TupRsingle t)   = [|| TupRsingle $$(liftScalarType t) ||]
liftTupleType (TupRpair ta tb) = [|| TupRpair $$(liftTupleType ta) $$(liftTupleType tb) ||]

liftConst :: TupleType t -> t -> Q (TExp t)
liftConst TupRunit         ()    = [|| () ||]
liftConst (TupRsingle t)   x     = [|| $$(liftScalar t x) ||]
liftConst (TupRpair ta tb) (a,b) = [|| ($$(liftConst ta a), $$(liftConst tb b)) ||]

liftScalar :: ScalarType t -> t -> Q (TExp t)
liftScalar (SingleScalarType t) x = liftSingle t x
liftScalar (VectorScalarType t) x = liftVector t x

liftSingle :: SingleType t -> t -> Q (TExp t)
liftSingle (NumSingleType t)    x = liftNum t x
liftSingle (NonNumSingleType t) x = liftNonNum t x

liftVector :: VectorType t -> t -> Q (TExp t)
liftVector VectorType{} x = liftVec x

-- O(n) at runtime to copy from the Addr# to the ByteArray#. We should be able
-- to do this without copying, but I don't think the definition of ByteArray# is
-- exported (or it is deeply magical).
--
liftVec :: Vec n a -> Q (TExp (Vec n a))
liftVec (Vec ba#)
  = TH.unsafeTExpCoerce
  $ [| runST $ \s ->
         case newByteArray# $(liftInt# n#) s                                                   of { (# s1, mba# #) ->
         case copyAddrToByteArray# $(TH.litE (TH.StringPrimL bytes)) mba# 0# $(liftInt# n#) s1 of { s2             ->
         case unsafeFreezeByteArray# mba# s2                                                   of { (# s3, ba'# #) ->
           (# s3, Vec ba'# #)
        }}}
     |]
  where
      bytes :: [Word8]
      bytes = go 0#
        where
          go i# | isTrue# (i# <# n#) = W8# (indexWord8Array# ba# i#) : go (i# +# 1#)
                | otherwise          = []

      n# = sizeofByteArray# ba#

-- XXX: Typed TH does not support unlifted types
--
liftInt# :: Int# -> TH.ExpQ
liftInt# i# = TH.litE (TH.IntPrimL (toInteger (I# i#)))

liftNum :: NumType t -> t -> Q (TExp t)
liftNum (IntegralNumType t) x = liftIntegral t x
liftNum (FloatingNumType t) x = liftFloating t x

liftNonNum :: NonNumType t -> t -> Q (TExp t)
liftNonNum TypeBool{} x = [|| x ||]
liftNonNum TypeChar{} x = [|| x ||]

liftIntegral :: IntegralType t -> t -> Q (TExp t)
liftIntegral TypeInt{}    x = [|| x ||]
liftIntegral TypeInt8{}   x = [|| x ||]
liftIntegral TypeInt16{}  x = [|| x ||]
liftIntegral TypeInt32{}  x = [|| x ||]
liftIntegral TypeInt64{}  x = [|| x ||]
#if __GLASGOW_HASKELL__ >= 710
liftIntegral TypeWord{}   x = [|| x ||]
#else
liftIntegral TypeWord{}   x = return (TH.TExp (TH.LitE (TH.IntegerL (toInteger x))))
#endif
liftIntegral TypeWord8{}  x = [|| x ||]
liftIntegral TypeWord16{} x = [|| x ||]
liftIntegral TypeWord32{} x = [|| x ||]
liftIntegral TypeWord64{} x = [|| x ||]

liftFloating :: FloatingType t -> t -> Q (TExp t)
liftFloating TypeHalf{}   x = [|| x ||]
liftFloating TypeFloat{}  x = [|| x ||]
liftFloating TypeDouble{} x = [|| x ||]


liftIntegralType :: IntegralType t -> Q (TExp (IntegralType t))
liftIntegralType TypeInt{}    = [|| TypeInt IntegralDict ||]
liftIntegralType TypeInt8{}   = [|| TypeInt8 IntegralDict ||]
liftIntegralType TypeInt16{}  = [|| TypeInt16 IntegralDict ||]
liftIntegralType TypeInt32{}  = [|| TypeInt32 IntegralDict ||]
liftIntegralType TypeInt64{}  = [|| TypeInt64 IntegralDict ||]
liftIntegralType TypeWord{}   = [|| TypeWord IntegralDict ||]
liftIntegralType TypeWord8{}  = [|| TypeWord8 IntegralDict ||]
liftIntegralType TypeWord16{} = [|| TypeWord16 IntegralDict ||]
liftIntegralType TypeWord32{} = [|| TypeWord32 IntegralDict ||]
liftIntegralType TypeWord64{} = [|| TypeWord64 IntegralDict ||]

liftFloatingType :: FloatingType t -> Q (TExp (FloatingType t))
liftFloatingType TypeHalf{}   = [|| TypeHalf FloatingDict ||]
liftFloatingType TypeFloat{}  = [|| TypeFloat FloatingDict ||]
liftFloatingType TypeDouble{} = [|| TypeDouble FloatingDict ||]

liftNonNumType :: NonNumType t -> Q (TExp (NonNumType t))
liftNonNumType TypeBool{} = [|| TypeBool NonNumDict ||]
liftNonNumType TypeChar{} = [|| TypeChar NonNumDict ||]

liftNumType :: NumType t -> Q (TExp (NumType t))
liftNumType (IntegralNumType t) = [|| IntegralNumType $$(liftIntegralType t) ||]
liftNumType (FloatingNumType t) = [|| FloatingNumType $$(liftFloatingType t) ||]

liftBoundedType :: BoundedType t -> Q (TExp (BoundedType t))
liftBoundedType (IntegralBoundedType t) = [|| IntegralBoundedType $$(liftIntegralType t) ||]
liftBoundedType (NonNumBoundedType t)   = [|| NonNumBoundedType $$(liftNonNumType t) ||]

liftScalarType :: ScalarType t -> Q (TExp (ScalarType t))
liftScalarType (SingleScalarType t) = [|| SingleScalarType $$(liftSingleType t) ||]
liftScalarType (VectorScalarType t) = [|| VectorScalarType $$(liftVectorType t) ||]

liftSingleType :: SingleType t -> Q (TExp (SingleType t))
liftSingleType (NumSingleType t)    = [|| NumSingleType $$(liftNumType t) ||]
liftSingleType (NonNumSingleType t) = [|| NonNumSingleType $$(liftNonNumType t) ||]

liftVectorType :: VectorType t -> Q (TExp (VectorType t))
liftVectorType (VectorType n t) = [|| VectorType n $$(liftSingleType t) ||]


-- Debugging
-- =========

showPreAccOp :: forall acc aenv arrs. PreOpenAcc acc aenv arrs -> String
showPreAccOp Alet{}               = "Alet"
showPreAccOp (Avar (ArrayVar ix)) = "Avar a" ++ show (idxToInt ix)
showPreAccOp (Use a)              = "Use " ++ showShortendArr a
showPreAccOp Apply{}              = "Apply"
showPreAccOp Aforeign{}           = "Aforeign"
showPreAccOp Acond{}              = "Acond"
showPreAccOp Awhile{}             = "Awhile"
showPreAccOp Apair{}              = "Apair"
showPreAccOp Anil                 = "Anil"
showPreAccOp Unit{}               = "Unit"
showPreAccOp Generate{}           = "Generate"
showPreAccOp Transform{}          = "Transform"
showPreAccOp Reshape{}            = "Reshape"
showPreAccOp Replicate{}          = "Replicate"
showPreAccOp Slice{}              = "Slice"
showPreAccOp Map{}                = "Map"
showPreAccOp ZipWith{}            = "ZipWith"
showPreAccOp Fold{}               = "Fold"
showPreAccOp Fold1{}              = "Fold1"
showPreAccOp FoldSeg{}            = "FoldSeg"
showPreAccOp Fold1Seg{}           = "Fold1Seg"
showPreAccOp Scanl{}              = "Scanl"
showPreAccOp Scanl'{}             = "Scanl'"
showPreAccOp Scanl1{}             = "Scanl1"
showPreAccOp Scanr{}              = "Scanr"
showPreAccOp Scanr'{}             = "Scanr'"
showPreAccOp Scanr1{}             = "Scanr1"
showPreAccOp Permute{}            = "Permute"
showPreAccOp Backpermute{}        = "Backpermute"
showPreAccOp Stencil{}            = "Stencil"
showPreAccOp Stencil2{}           = "Stencil2"
-- showPreAccOp Collect{}          = "Collect"


showShortendArr :: TupleType -> Array sh e -> String
showShortendArr tp arr
  | length l > cutoff = "[" ++ elements ++ ", ..]"
  | otherwise         = "[" ++ elements ++ "]"
  where
    l      = toList arr
    cutoff = 5
    elements = intercalate ", " $ map (showElement tp) $ take cutoff l


showPreExpOp :: forall acc env aenv t. PreOpenExp acc env aenv t -> String
showPreExpOp Let{}             = "Let"
showPreExpOp (EVar (Var _ ix)) = "Var x" ++ show (idxToInt ix)
showPreExpOp (Const tp c)      = "Const " ++ showElement tp c
showPreExpOp Undef             = "Undef"
showPreExpOp Foreign{}         = "Foreign"
showPreExpOp Pair{}            = "Pair"
showPreExpOp Nil{}             = "Nil"
showPreExpOp IndexAny          = "IndexAny"
showPreExpOp IndexSlice{}      = "IndexSlice"
showPreExpOp IndexFull{}       = "IndexFull"
showPreExpOp ToIndex{}         = "ToIndex"
showPreExpOp FromIndex{}       = "FromIndex"
showPreExpOp Cond{}            = "Cond"
showPreExpOp While{}           = "While"
showPreExpOp PrimConst{}       = "PrimConst"
showPreExpOp PrimApp{}         = "PrimApp"
showPreExpOp Index{}           = "Index"
showPreExpOp LinearIndex{}     = "LinearIndex"
showPreExpOp Shape{}           = "Shape"
showPreExpOp ShapeSize{}       = "ShapeSize"
showPreExpOp Coerce{}          = "Coerce"

showElement :: TupleType e -> e -> String
showElement tuple value = showElement' tuple value ""
  where
    showElement' :: TupleType e -> e -> ShowS
    showElement' TupRunit () = showString "()"
    showElement' (TupRpair t1 t2) (e1, e2) = showString "(" . showElement' t1 e1 . showString ", " . showElement' t2 e2 . showString ")"
    showElement' (TupRsingle tp) val = showScalar tp val

    showScalar :: ScalarType e -> e -> ShowS
    showScalar (SingleScalarType t) e = showString $ showSingle t e
    showScalar (VectorScalarType t) e = showString $ showVector t e

    showSingle :: SingleType e -> e -> String
    showSingle (NumSingleType t) e = showNum t e
    showSingle (NonNumSingleType t) e = showNonNum t e

    showNum :: NumType e -> e -> String
    showNum (IntegralNumType t) e = showIntegral t e
    showNum (FloatingNumType t) e = showFloating t e

    showIntegral :: IntegralType e -> e -> String
    showIntegral TypeInt{}    e = show e
    showIntegral TypeInt8{}   e = show e
    showIntegral TypeInt16{}  e = show e
    showIntegral TypeInt32{}  e = show e
    showIntegral TypeInt64{}  e = show e
    showIntegral TypeWord{}   e = show e
    showIntegral TypeWord8{}  e = show e
    showIntegral TypeWord16{} e = show e
    showIntegral TypeWord32{} e = show e
    showIntegral TypeWord64{} e = show e

    showFloating :: FloatingType e -> e -> String
    showFloating TypeHalf{}   e = show e
    showFloating TypeFloat{}  e = show e
    showFloating TypeDouble{} e = show e

    showNonNum :: NonNumType e -> e -> String
    showNonNum TypeChar e = show e
    showNonNum TypeBool e = show e

    showVector :: VectorType (Vec n a) e -> String
    showVector (VectorType _ scalar) vec = "<" ++ (intercalate ", " $ showScalar scalar $ vecToArray vec) ++ ">"


