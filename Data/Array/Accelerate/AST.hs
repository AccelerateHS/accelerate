{-# LANGUAGE GADTs #-}

-- |Embedded array processing language: accelerate AST with de Bruijn indices
--
--  Copyright (c) [2008..2009] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
--
--  License: BSD3
--
--- Description ---------------------------------------------------------------
--
--  Scalar versus collective operations
--  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--  The embedded array processing language is a two-level language.  It
--  combines a language of scalar expressions and functions with a language of
--  collective array operations.  Scalar expressions are used to compute 
--  arguments for collective operations and scalar functions are used to
--  parametrise higher-order, collective array operations.  The two-level
--  structure, in particular, ensures that collective operations cannot be
--  parametrised with collective operations; hence, we are following a flat
--  data-parallel model.  The collective operations manipulate
--  multi-dimensional arrays whose shape is explicitly tracked in their
--  types.  In fact, collective operations cannot produce any values other
--  than multi-dimensional arrays; when they yield a scalar, this is in the
--  form of a 0-dimensional, singleton array. 
--
--  Programs
--  ~~~~~~~~
--  Collective array programs are monadic sequences of collective array 
--  operations.  The monadic framework provides for the explicit sharing of
--  intermediate results and orders the computations.  Programs are the
--  execution unit for array computations.
--
--  Functions
--  ~~~~~~~~~
--  The array expression language is first-order and only provides only limited
--  control structures, to ensure that it can be efficiently executed on
--  compute acceleration hardware, such as GPUs.  To restrict functions to
--  first-order, we separate function abstraction from the main expression
--  type.  Functions are represented using de Bruijn indices.
--
--  Parametric and ad-hoc polymorphism
--  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--  The array language features paramatric polymophism (e.g., pairing and
--  projections) as well as ad-hoc polymorphism (e.g., arithmetic
--  operations).  All ad-hoc polymorphic constructs include reified dictionaries
--  (c.f., module `Types').  Reified dictionaries also ensure that constants
--  (constructor `Const') are representable on compute acceleration hardware.
--
--  Host <-> device transfer
--  ~~~~~~~~~~~~~~~~~~~~~~~~
--  This is included in the array computations to enable the code generator to 
--  schedule transfers concurrently with array operations.

module Data.Array.Accelerate.AST (

  Comps(..), CompBinding(..), Comp(..), Index(..), Idx(..), Fun, OpenFun(..), 
  Exp, OpenExp(..), PrimConst(..), PrimFun(..),

) where

-- friends
import Data.Array.Accelerate.Type


-- |Abstract syntax of array computations
-- -

-- |A program of collective array operations is a sequence of collective array
-- operations 
newtype Comps = Comps [CompBinding]
  -- FIXME: we'd like conditionals, but then we need a binary tree rather than a
  --  sequence of collective array operations

-- |A binding of a collective array operation is such an operation with an
-- apporpriate number of `Arr' binders
--
data CompBinding where
  CompBinding :: CompResult a => a -> Comp a -> CompBinding

-- |The various variants of collective array operations
--
-- * We have no fold, only scan which returns the fold result and scan array.
--   We assume that the code generatoris clever enough to eliminate any dead
--   code, when only one of the two values is needed.
--
data Comp a where

  -- array inlet (triggers async host->device transfer if necessary)
  Use         :: Array dim a -> Comp (Arr dim a)

  -- capture a scalar (or a tuple of scalars) in a singleton array  
  Unit        :: Exp a -> Comp (Scalar a)

  -- collective operations
  --

  -- Change the shape of an array without altering its contents
  -- * precondition: size dim == size dim'
  Reshape     :: Exp dim                          -- ^new shape
              -> Arr dim' a                       -- ^array to be reshaped
              -> Comp (Arr dim a)

  -- Replicate an array across one or more dimensions as given by the first
  -- argument
  Replicate   :: Index dim' dim                   -- ^specifies new dimensions
              -> Arr dim a                        -- ^data to be replicated
              -> Comp (Arr dim' a)

  -- Index a subarray out of an array; i.e., the dimensions not indexed are 
  -- returned whole
  Index       :: Arr dim a                        -- ^array to be indexed
              -> Index dim dim'                   -- ^dimensions to indexed
              -> Comp (Arr dim' a)

  -- Pairwise combination of elements of two arrays with the same shape
  Zip         :: Arr dim a -> Arr dim b -> Comp (Arr dim (a, b))

  -- Apply the given function to all elements of the given array
  Map         :: Fun (a -> b) -> Arr dim a -> Comp (Arr dim b)
    -- FIXME: generalise to mapFold

  -- Remove all elements from a linear array that do not satisfy the given
  -- predicate
  Filter      :: Fun (a -> Bool) -> Arr DIM1 a -> Comp (Arr DIM1 a)

  -- Left-to-right prescan of a linear array with a given *associative*
  -- function and its neutral element; produces a rightmost fold value and a
  -- linear of the same shape (the fold value would be the rightmost element
  -- in a scan, as opposed to a prescan)
  Scan        :: Fun (a -> a -> a)                -- ^combination function
              -> Exp a                            -- ^default value
              -> Arr DIM1 a                       -- ^linear array
              -> Comp (Arr DIM0 a, Arr DIM1 a)
    -- FIXME: generalise multi-dimensional scan?  And/or a generalised mapScan?

  -- Generalised forward permutation is characterised by a permutation
  -- function that determines for each element of the source array where it
  -- should go in the target; the permutation can be between arrays of varying
  -- shape; the permutation function must be total.
  --
  -- The target array is initialised from an array of default values (in case
  -- some positions in the target array are never picked by the permutation
  -- functions).  Moroever, we have a combination function (in case some
  -- positions on the target array are picked multiple times by the
  -- permutation functions).  The combination functions needs to be
  -- *associative* and *commutative*.  
  Permute     :: Fun (a -> a -> a)                -- ^combination function
              -> Arr dim' a                       -- ^default values
              -> Fun (dim -> dim')                -- ^permutation function
              -> Arr dim a                        -- ^linear array to permute
              -> Comp (Arr dim' a)

  -- Generalised multi-dimensional backwards permutation; the permutation can
  -- be between arrays of varying shape; the permutation function must be total
  Backpermute :: Exp dim'                         -- ^dimensions of the result
              -> Fun (dim' -> dim)                -- ^permutation function
              -> Arr dim a                        -- ^source array
              -> Comp (Arr dim' a)

-- |Generalised array index, which may index only in a subset of the dimensions
-- of a shape.
--
data Index initialDim projectedDim where
  IndexNil   :: Index () ()
  IndexAll   :: Index init proj -> Index (init, Int) (proj, Int)
  IndexFixed :: Exp Int -> Index init proj -> Index (init, Int)  proj

-- De Bruijn variable index projecting a specific type from a type
-- environment.  Type envionments are nested pairs (..((), t1), t2, ..., tn). 
--
data Idx env t where
  ZeroIdx ::              Idx (env, t) t
  SuccIdx :: Idx env t -> Idx (env, s) t

-- |Closed function (may be nullary)
--
type Fun fun = OpenFun () fun

-- |Function abstraction
--
data OpenFun env t where
  Body :: OpenExp env t      -> OpenFun env t
  Lam  :: OpenFun (env, a) t -> OpenFun env (a -> t)

-- Closed expression
--
type Exp t = OpenExp () t

-- |Open expressions using de Bruijn indices for variables ranging over tuples
-- of scalars; they never produce an array.  All code, except Cond, is
-- evaluated eagerly.  N-tuples are represented as a nested pairs. 
--
data OpenExp env t where

  -- |Variable index
  Var         :: TupleType t -> Idx env t -> OpenExp env t

  -- |Constant values
  Const       :: TupleType t -> t -> OpenExp env t

  -- |Tuples
  Pair        :: OpenExp env s -> OpenExp env t -> OpenExp env (s, t)
  Fst         :: OpenExp env (s, t)             -> OpenExp env s
  Snd         :: OpenExp env (s, t)             -> OpenExp env t

  -- |Conditional expression (non-strict in 2nd and 3rd argument)
  Cond        :: OpenExp env Bool -> OpenExp env t -> OpenExp env t 
              -> OpenExp env t

  -- |Primitive constants
  PrimConst   :: PrimConst t -> OpenExp env t

  -- |Primitive scalar operations
  PrimApp     :: PrimFun (a -> r) -> OpenExp env a -> OpenExp env r

  -- |Project a single scalar from an array
  IndexScalar :: Arr dim t -> OpenExp env dim -> OpenExp env t

  -- |Array shape
  Shape       :: Arr dim e -> OpenExp env dim

-- |Primitive GPU constants
--
data PrimConst ty where

  -- constants from Bounded
  PrimMinBound  :: BoundedType a -> PrimConst a
  PrimMaxBound  :: BoundedType a -> PrimConst a

  -- constant from Floating
  PrimPi        :: FloatingType a -> PrimConst a

-- |Primitive GPU operations
--
data PrimFun sig where

  -- operators from Num
  PrimAdd  :: NumType a -> PrimFun ((a, a) -> a)
  PrimSub  :: NumType a -> PrimFun ((a, a) -> a)
  PrimMul  :: NumType a -> PrimFun ((a, a) -> a)
  PrimNeg  :: NumType a -> PrimFun (a      -> a)
  PrimAbs  :: NumType a -> PrimFun (a      -> a)
  PrimSig  :: NumType a -> PrimFun (a      -> a)

  -- operators from Integral & Bits
  PrimQuot :: IntegralType a -> PrimFun ((a, a) -> a)
  PrimRem  :: IntegralType a -> PrimFun ((a, a) -> a)
  PrimIDiv :: IntegralType a -> PrimFun ((a, a) -> a)
  PrimMod  :: IntegralType a -> PrimFun ((a, a) -> a)
  PrimBAnd :: IntegralType a -> PrimFun ((a, a) -> a)
  PrimBOr  :: IntegralType a -> PrimFun ((a, a) -> a)
  PrimBXor :: IntegralType a -> PrimFun ((a, a) -> a)
  PrimBNot :: IntegralType a -> PrimFun (a      -> a)
  -- FIXME: add shifts

  -- operators from Fractional, Floating, RealFrac & RealFloat
  PrimFDiv  :: FloatingType a -> PrimFun ((a, a) -> a)
  PrimRecip :: FloatingType a -> PrimFun (a      -> a)
  -- FIXME: add operations from Floating, RealFrac & RealFloat

  -- relational and equality operators
  PrimLt   :: ScalarType a -> PrimFun ((a, a) -> Bool)
  PrimGt   :: ScalarType a -> PrimFun ((a, a) -> Bool)
  PrimLtEq :: ScalarType a -> PrimFun ((a, a) -> Bool)
  PrimGtEq :: ScalarType a -> PrimFun ((a, a) -> Bool)
  PrimEq   :: ScalarType a -> PrimFun ((a, a) -> Bool)
  PrimNEq  :: ScalarType a -> PrimFun ((a, a) -> Bool)
  PrimMax  :: ScalarType a -> PrimFun ((a, a) -> a   )
  PrimMin  :: ScalarType a -> PrimFun ((a, a) -> a   )

  -- logical operators
  PrimLAnd :: PrimFun ((Bool, Bool) -> Bool)
  PrimLOr  :: PrimFun ((Bool, Bool) -> Bool)
  PrimLNot :: PrimFun (Bool         -> Bool)

  -- character conversions
  PrimOrd  :: PrimFun (Char -> Int)
  PrimChr  :: PrimFun (Int  -> Char)
  -- FIXME: use IntegralType?

  -- floating point conversions
  PrimRoundFloatInt :: PrimFun (Float -> Int)
  PrimTruncFloatInt :: PrimFun (Float -> Int)
  PrimIntFloat      :: PrimFun (Int -> Float)
  -- FIXME: variants for other integer types (and also for Double)
  --        ALSO: need to use overloading

  -- FIXME: conversions between various integer types

  -- FIXME: what do we want to do about Enum?  succ and pred are only
  --   moderatly useful without user-defined enumerations, but we want
  --   the range constructs for arrays (but that's not scalar primitives)
