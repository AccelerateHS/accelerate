{-# LANGUAGE GADTs, EmptyDataDecls, FlexibleContexts #-}

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
--
--  There are, however, two expression forms that take arrays as arguments.  As
--  a result scalar and array expressions are recursively dependent.  As we
--  cannot and don't want to compute arrays in the middle of scalar
--  computations, array computations will always be hoisted out of scalar
--  expressions.  So that this is always possible, these array expressions may
--  not contain any free scalar variables.  To express that condition in the
--  type structure, we use separate environments for scalar and array variables.
--
--  Programs
--  ~~~~~~~~
--  Collective array programs comprise closed expressions of array operations.
--  There is no explicit sharing in the initial AST form, but sharing is
--  introduced subsequently by common subexpression elimination and floating
--  of array computations.
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
--  The AST contains both reified dictionaries and type class constraints.  
--  Type classes are used for array-related functionality that is uniformly
--  available for all supported types.  In contrast, reified dictionaries are
--  used for functionality that is only available for certain types, such as
--  arithmetic operations.

module Data.Array.Accelerate.AST (

  -- * Typed de Bruijn indices
  Idx(..),
  
  -- * Accelerated array expressions
  OpenAcc(..), Acc,
  
  -- * Scalar expressions
  OpenFun(..), Fun, OpenExp(..), Exp, PrimConst(..), PrimFun(..)

) where
  
-- friends
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Representation (SliceIndex)
import Data.Array.Accelerate.Array.Sugar 


-- Typed de Bruijn indices
-- -----------------------

-- De Bruijn variable index projecting a specific type from a type
-- environment.  Type envionments are nested pairs (..((), t1), t2, ..., tn). 
--
data Idx env t where
  ZeroIdx ::              Idx (env, t) t
  SuccIdx :: Idx env t -> Idx (env, s) t


-- Array expressions
-- -----------------

-- |Collective array computations parametrised over array variables
-- represented with de Bruijn indices.
--
-- * We have no fold, only scan which returns the fold result and scan array.
--   We assume that the code generator is clever enough to eliminate any dead
--   code, when only one of the two values is needed.
--
-- * Scalar functions and expressions embedded in well-formed array
--   computations cannot contain free scalar variable indices.  The latter
--   cannot be bound in array computations, and hence, cannot appear in any
--   well-formed program.
--
-- * The let-form is used to represent the sharing discovered by common
--   subexpression elimination as well as to control evaluation order.  (We
--   need to hoist array expressions out of scalar expressions - they occur in
--   scalar indexing and in determining an arrays shape.)
--
-- The data type is parametrised over the surface types (not the representation
-- type).
--
data OpenAcc aenv a where
  
  -- Local binding to represent sharing and demand explicitly; this is an
  -- eager(!) binding
  Let         :: OpenAcc aenv (Array dim e)         -- bound expression
              -> OpenAcc (aenv, Array dim e) 
                         (Array dim' e')            -- the bound expr's scope           
              -> OpenAcc aenv (Array dim' e')

  -- Variant of 'Let' binding (and decomposing) a pair
  Let2        :: OpenAcc aenv (Array dim1 e1, 
                               Array dim2 e2)       -- bound expressions 
              -> OpenAcc ((aenv, Array dim1 e1), 
                                 Array dim2 e2)
                         (Array dim' e')            -- the bound expr's scope           
              -> OpenAcc aenv (Array dim' e')

  -- Variable bound by a 'Let', represented by a de Bruijn index              
  Avar        :: Idx     aenv (Array dim e)
              -> OpenAcc aenv (Array dim e)
  
  -- Array Inlet (Triggers Async Host->Device Transfer if Necessary)
  Use         :: Array dim e 
              -> OpenAcc aenv (Array dim e)

  -- Capture a Scalar (or a tuple of Scalars) in a Singleton Array  
  Unit        :: Elem e
              => Exp     aenv e 
              -> OpenAcc aenv (Scalar e)

  -- Change the shape of an array without altering its contents
  -- > precondition: size dim == size dim'
  Reshape     :: Ix dim
              => Exp     aenv dim                 -- new shape
              -> OpenAcc aenv (Array dim' e)      -- array to be reshaped
              -> OpenAcc aenv (Array dim e)

  -- Replicate an array across one or more dimensions as given by the first
  -- argument
  Replicate   :: (Ix dim, Elem slix)
              => SliceIndex (ElemRepr slix)       -- slice type specification
                            (ElemRepr sl) 
                            co'
                            (ElemRepr dim)
              -> Exp     aenv slix                -- slice value specification
              -> OpenAcc aenv (Array sl e)        -- data to be replicated
              -> OpenAcc aenv (Array dim e)

  -- Index a subarray out of an array; i.e., the dimensions not indexed are 
  -- returned whole
  Index       :: (Ix sl, Elem slix)
              => SliceIndex (ElemRepr slix)       -- slice type specification
                            (ElemRepr sl) 
                            co'
                            (ElemRepr dim)
              -> OpenAcc aenv (Array dim e)       -- array to be indexed
              -> Exp     aenv slix                -- slice value specification
              -> OpenAcc aenv (Array sl e)

  -- Apply the given unary function to all elements of the given array
  Map         :: Elem e'
              => Fun     aenv (e -> e') 
              -> OpenAcc aenv (Array dim e) 
              -> OpenAcc aenv (Array dim e')
    -- FIXME: generalise to mapFold

  -- Apply a given binary function pairwise to all elements of the given arrays.
  -- The length of the result is the length of the shorter of the two argument
  -- arrays.
  ZipWith     :: Elem e3
              => Fun     aenv (e1 -> e2 -> e3) 
              -> OpenAcc aenv (Array dim e1)
              -> OpenAcc aenv (Array dim e2)
              -> OpenAcc aenv (Array dim e3)

  -- Fold of an array with a given *associative* function and its neutral
  -- element
  Fold        :: Fun     aenv (e -> e -> e)          -- combination function
              -> Exp     aenv e                      -- default value
              -> OpenAcc aenv (Array dim e)          -- folded array
              -> OpenAcc aenv (Scalar e)
    -- FIXME: generalise to Gabi's mapFold

  -- Left-to-right prescan of a linear array with a given *associative*
  -- function and its neutral element; produces a rightmost fold value and a
  -- linear of the same shape (the fold value would be the rightmost element
  -- in a scan, as opposed to a prescan)
  Scan        :: Fun     aenv (e -> e -> e)          -- combination function
              -> Exp     aenv e                      -- default value
              -> OpenAcc aenv (Vector e)             -- linear array
              -> OpenAcc aenv (Vector e, Scalar e)
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
  -- permutation functions).  The combination function needs to be
  -- /associative/ and /commutative/ .  We drop every element for which the
  -- permutation function yields -1 (i.e., a tuple of -1 values).
  Permute     :: Fun     aenv (e -> e -> e)        -- combination function
              -> OpenAcc aenv (Array dim' e)       -- default values
              -> Fun     aenv (dim -> dim')        -- permutation function
              -> OpenAcc aenv (Array dim e)        -- source array
              -> OpenAcc aenv (Array dim' e)

  -- Generalised multi-dimensional backwards permutation; the permutation can
  -- be between arrays of varying shape; the permutation function must be total
  Backpermute :: Ix dim'
              => Exp     aenv dim'                 -- dimensions of the result
              -> Fun     aenv (dim' -> dim)        -- permutation function
              -> OpenAcc aenv (Array dim e)        -- source array
              -> OpenAcc aenv (Array dim' e)

-- |Closed array expression aka an array program
--
type Acc a = OpenAcc () a

              
-- Embedded expressions
-- --------------------

-- |Function abstraction
--
data OpenFun env aenv t where
  Body :: OpenExp env               aenv t -> OpenFun env aenv t
  Lam  :: Elem a
       => OpenFun (env, ElemRepr a) aenv t -> OpenFun env aenv (a -> t)

-- |Function without free scalar variables
--
type Fun aenv t = OpenFun () aenv t

-- |Open expressions using de Bruijn indices for variables ranging over tuples
-- of scalars and arrays of tuples.  All code, except Cond, is evaluated
-- eagerly.  N-tuples are represented as nested pairs. 
--
-- The data type is parametrised over the surface types (not the representation
-- type).
--
data OpenExp env aenv t where

  -- Variable index, ranging only over tuples or scalars
  Var         :: Elem t
              => Idx env (ElemRepr t)
              -> OpenExp env aenv t

  -- Constant values
  Const       :: Elem t
              => ElemRepr t
              -> OpenExp env aenv t

  -- Tuples
  Pair        :: (Elem s, Elem t)
              => OpenExp env aenv s 
              -> OpenExp env aenv t
              -> OpenExp env aenv (s, t)
  Fst         :: (Elem s, Elem t)
              => OpenExp env aenv (s, t)
              -> OpenExp env aenv s
  Snd         :: (Elem s, Elem t)
              => OpenExp env aenv (s, t)
              -> OpenExp env aenv t

  -- Conditional expression (non-strict in 2nd and 3rd argument)
  Cond        :: OpenExp env aenv Bool
              -> OpenExp env aenv t 
              -> OpenExp env aenv t 
              -> OpenExp env aenv t

  -- Primitive constants
  PrimConst   :: Elem t
              => PrimConst t -> OpenExp env aenv t

  -- Primitive scalar operations
  PrimApp     :: (Elem a, Elem r)
              => PrimFun (a -> r) 
              -> OpenExp env aenv a
              -> OpenExp env aenv r

  -- Project a single scalar from an array
  -- the array expression cannot contain any free scalar variables
  IndexScalar :: OpenAcc aenv (Array dim t)
              -> OpenExp env aenv dim 
              -> OpenExp env aenv t

  -- Array shape
  -- the array expression cannot contain any free scalar variables
  Shape       :: OpenAcc aenv (Array dim e) 
              -> OpenExp env aenv dim
            
-- |Expression without free scalar variables
--
type Exp aenv t = OpenExp () aenv t

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
  --        should we have an overloaded functions like 'toInt'?  
  --        (or 'fromEnum' for enums?)
  PrimBoolToInt     :: PrimFun (Bool -> Int)

  -- FIXME: what do we want to do about Enum?  succ and pred are only
  --   moderatly useful without user-defined enumerations, but we want
  --   the range constructs for arrays (but that's not scalar primitives)
