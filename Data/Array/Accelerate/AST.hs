{-# LANGUAGE GADTs, EmptyDataDecls, DeriveDataTypeable #-}

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
--  multi-dimensional arrays whose shape is explicitly tracked in their types.
--  In fact, collective operations cannot produce any values other than
--  multi-dimensional arrays; when they yield a scalar, this is in the form of
--  a 0-dimensional, singleton array.  Similarly, scalar expression can -as
--  their name indicates- only produce tuples of scalar, but not arrays. 
--  (There are, however, two expression forms that take arrays as arguments.)
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
--  The array expression language is first-order and only provides limited
--  control structures to ensure that it can be efficiently executed on
--  compute-acceleration hardware, such as GPUs.  To restrict functions to
--  first-order, we separate function abstraction from the main expression
--  type.  Functions are represented using de Bruijn indices.
--
--  Parametric and ad-hoc polymorphism
--  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--  The array language features paramatric polymophism (e.g., pairing and
--  projections) as well as ad-hoc polymorphism (e.g., arithmetic operations).
--  All ad-hoc polymorphic constructs include reified dictionaries (c.f.,
--  module 'Types').  Reified dictionaries also ensure that constants
--  (constructor 'Const') are representable on compute acceleration hardware.
--
--  External vs. internal arrays
--  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--  Arrays in the embedded language have a type different from those of the
--  host language - the host language uses the type constructor 'Array', while
--  the embedded language uses 'Arr'.  External 'Array's are internalised with
--  a 'use' operation.  This makes the multiple use (sharing) of an
--  internalised array explicit and it indicates where host->device transfers
--  are needed when compute-acceleration hardware is used. (These transfers
--  may proceed concurrently to host and device computations.)

module Data.Array.Accelerate.AST (

  -- * Typed de Bruijn indices
  Idx(..),
  
  -- * Internal arrays
  Arr(..), Sca, Vec,
  
  -- * Monadic array computations
  Program, Comp(..),
  
  -- * Expressions
  Fun(..), Exp(..), PrimConst(..), PrimFun(..)

) where
  
-- standard library
import Data.Typeable

-- friends
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Data  (ArrayElem)
import Data.Array.Accelerate.Array.Representation
import Data.Array.Accelerate.Array.Sugar (Elem, ElemRepr, ElemRepr')


-- Typed de Bruijn indices
-- -----------------------

-- De Bruijn variable index projecting a specific type from a type
-- environment.  Type envionments are nested pairs (..((), t1), t2, ..., tn). 
--
data Idx env t where
  ZeroIdx ::              Idx (env, t) t
  SuccIdx :: Idx env t -> Idx (env, s) t


-- Internal arrays (using representation types)
-- --------------------------------------------

-- |Array representation inside collective computations; this is only to track
-- the array, not to represent it's value.
--
-- We require the dimensions and element type to be in `Typeable' as we don't
-- use type-indexed de Bruijn indices for array variables at the moment and
-- hence need to cast in the interpreter.
--
data Arr dim e {- where
  Arr :: (IxRepr dim, ArrayElem e, Typeable dim, Typeable e) 
      => TupleType e -> Int -> Arr dim e -}
  -- FIXME: Do we ever construct values of this type?  If not, the problem is
  -- that the restrictions imposed by the classes are not effective either.
  deriving Typeable
  
-- |Scalar results (both elementary scalars and tuples of scalars) are being
-- represented as 0-dimensional singleton arrays
--
type Sca a = Arr DIM0Repr a

-- |Vectors as 1-dimensional arrays
--
type Vec a = Arr DIM1Repr a


-- Monadic array computations
-- --------------------------

-- Abstract syntax of array computations
-- 

-- |Programs are closed array computations.
--
type Program a = Comp () a

-- |Collective array computations parametrised over array variables
-- represented with de Bruijn indices.
--
-- * We have no fold, only scan which returns the fold result and scan array.
--   We assume that the code generatoris clever enough to eliminate any dead
--   code, when only one of the two values is needed.
--
-- * Scalar functions and expressions embedded in well-formed array
--   computations cannot contain free scalar variable indices.  The latter
--   cannot be bound in array computations, and hence, cannot appear in any
--   well-formed program.
--
data Comp env a where
  
  -- Monadic return
  Return      :: Idx env (Arr dim e) -> Comp env (Arr dim e)

  -- Monadic return with pairing
  Return2     :: Idx env (Arr dim1 e1) 
              -> Idx env (Arr dim2 e2) 
              -> Comp env (Arr dim1 e1, Arr dim2 e2)
    -- FIXME: we ought to be able to return n-tuples (as nested pairs) of arrays
    --        without using multiple returns - in fact, it's very ugly to have
    --        two returns in the first place
    
  -- Monadic bind
  Bind        :: Comp env a -> Comp (env, a) b -> Comp env b
        
  -- Monadic bind with unpairing
  Bind2       :: Comp env (a, b) -> Comp ((env, a), b) c -> Comp env c
  -- FIXME: we ought to be able to match n-tuples (as nested pairs) of arrays
  --        without using multiple binds - in fact, it's very ugly to have
  --        two binds in the first place
        
  -- Array Inlet (Triggers Async Host->Device Transfer if Necessary)
  Use         :: Array dim e -> Comp env (Arr dim e)

  -- Capture a Scalar (or a tuple of Scalars) in a Singleton Array  
  Unit        :: Exp env e -> Comp env (Sca e)

  -- Change the shape of an array without altering its contents
  -- * precondition: size dim == size dim'
  Reshape     :: Exp env dim                     -- ^new shape
              -> Idx env (Arr dim' e)            -- ^array to be reshaped
              -> Comp env (Arr dim e)

  -- Replicate an array across one or more dimensions as given by the first
  -- argument
  Replicate   :: SliceIndex slix sl co dim        -- ^slice type specification
              -> Exp env slix                     -- ^slice value specification
              -> Idx env (Arr sl e)               -- ^data to be replicated
              -> Comp env (Arr dim e)

  -- Index a subarray out of an array; i.e., the dimensions not indexed are 
  -- returned whole
  Index       :: SliceIndex slix sl co dim        -- ^slice type specification
              -> Idx env (Arr dim e)              -- ^array to be indexed
              -> Exp env slix                     -- ^slice value specification
              -> Comp env (Arr sl e)

  -- Apply the given unary function to all elements of the given array
  Map         :: Fun env (e -> e') 
              -> Idx env (Arr dim e) 
              -> Comp env (Arr dim e')
    -- FIXME: generalise to mapFold

  -- Apply a given binary function pairwise to all elements of the given arrays.
  -- The length of the result is the length of the shorter of the two argument
  -- arrays.
  ZipWith     :: Fun env (e1 -> e2 -> e3) 
              -> Idx env (Arr dim e1)
              -> Idx env (Arr dim e2)
              -> Comp env (Arr dim e3)

  -- Remove all elements from a linear array that do not satisfy the given
  -- predicate
  Filter      :: Fun env (e -> ElemRepr Bool) 
              -> Idx env (Vec e)
              -> Comp env (Vec e)

  -- Left-to-right prescan of a linear array with a given *associative*
  -- function and its neutral element; produces a rightmost fold value and a
  -- linear of the same shape (the fold value would be the rightmost element
  -- in a scan, as opposed to a prescan)
  Scan        :: Fun env (e -> e -> e)            -- ^combination function
              -> Exp env e                        -- ^default value
              -> Idx env (Vec e)                 -- ^linear array
              -> Comp env (Sca e, Vec e)
    -- FIXME: generalised multi-dimensional scan?  And/or a generalised mapScan?

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
  Permute     :: Fun env (e -> e -> e)            -- ^combination function
              -> Idx env (Arr dim' e)             -- ^default values
              -> Fun env (dim -> dim')            -- ^permutation function
              -> Idx env (Arr dim e)              -- ^linear array to permute
              -> Comp env (Arr dim' e)

  -- Generalised multi-dimensional backwards permutation; the permutation can
  -- be between arrays of varying shape; the permutation function must be total
  Backpermute :: Exp env dim'                     -- ^dimensions of the result
              -> Fun env (dim' -> dim)            -- ^permutation function
              -> Idx env (Arr dim e)              -- ^source array
              -> Comp env (Arr dim' e)
              
              
-- Embedded expressions
-- --------------------

-- |Function abstraction
--
data Fun env t where
  Body :: Exp env t      -> Fun env t
  Lam  :: Fun (env, a) t -> Fun env (a -> t)

-- |Open expressions using de Bruijn indices for variables ranging over tuples
-- of scalars and arrays of tuples.  All code, except Cond, is evaluated
-- eagerly.  N-tuples are represented as a nested pairs. 
--
data Exp env t where

  -- |Variable index, ranging only over tuples or scalars
  Var         :: TupleType t -> Idx env t -> Exp env t

  -- |Constant values
  Const       :: TupleType t -> t -> Exp env t

  -- |Tuples
  Pair        :: (Elem s, Elem t)
              => s {- dummy to fix the type variable -}
              -> t {- dummy to fix the type variable -}
              -> Exp env (ElemRepr s) 
              -> Exp env (ElemRepr t) 
              -> Exp env (ElemRepr (s, t))
  Fst         :: (Elem s, Elem t)
              => s {- dummy to fix the type variable -}
              -> t {- dummy to fix the type variable -}
              -> Exp env (ElemRepr (s, t))
              -> Exp env (ElemRepr s)
  Snd         :: (Elem s, Elem t)
              => s {- dummy to fix the type variable -}
              -> t {- dummy to fix the type variable -}
              -> Exp env (ElemRepr (s, t))
              -> Exp env (ElemRepr t)

  -- |Conditional expression (non-strict in 2nd and 3rd argument)
  Cond        :: Exp env (ElemRepr Bool) 
              -> Exp env t 
              -> Exp env t 
              -> Exp env t

  -- |Primitive constants
  PrimConst   :: Elem t
              => PrimConst t -> Exp env (ElemRepr t)

  -- |Primitive scalar operations
  PrimApp     :: (Elem a, Elem r)
              => PrimFun (a -> r) 
              -> Exp env (ElemRepr a) 
              -> Exp env (ElemRepr r)

  -- |Project a single scalar from an array
  IndexScalar :: Idx env (Arr dim t) -> Exp env dim -> Exp env t

  -- |Array shape
  Shape       :: Idx env (Arr dim e) -> Exp env dim

-- |Primitive GPU constants
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
