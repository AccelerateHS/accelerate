{-# LANGUAGE CPP, DeriveDataTypeable, StandaloneDeriving #-}
{-# LANGUAGE GADTs, EmptyDataDecls, FlexibleContexts, TypeFamilies, TypeOperators #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.AST
-- Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
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

module Data.Array.Accelerate.AST (

  -- * Typed de Bruijn indices
  Idx(..),

  -- * Valuation environment
  Val(..), prj,

  -- * Accelerated array expressions
  Arrays(..), ArraysR(..), OpenAfun(..), Afun, OpenAcc(..), Acc, Stencil(..), StencilR(..),

  -- * Scalar expressions
  OpenFun(..), Fun, OpenExp(..), Exp, PrimConst(..), PrimFun(..)

) where

--standard library
import Data.Typeable

-- friends
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Representation (SliceIndex)
import Data.Array.Accelerate.Array.Sugar as Sugar
import Data.Array.Accelerate.Array.Delayed        (Delayable)
import Data.Array.Accelerate.Tuple

#include "accelerate.h"


-- Typed de Bruijn indices
-- -----------------------

-- De Bruijn variable index projecting a specific type from a type
-- environment.  Type envionments are nested pairs (..((), t1), t2, ..., tn). 
--
data Idx env t where
  ZeroIdx ::              Idx (env, t) t
  SuccIdx :: Idx env t -> Idx (env, s) t


-- Environments
-- ------------

-- Valuation for an environment
--
data Val env where
  Empty :: Val ()
  Push  :: Val env -> t -> Val (env, t)

deriving instance Typeable1 Val


-- Projection of a value from a valuation using a de Bruijn index
--
prj :: Idx env t -> Val env -> t
prj ZeroIdx       (Push _   v) = v
prj (SuccIdx idx) (Push val _) = prj idx val
prj _             _            = INTERNAL_ERROR(error) "prj" "inconsistent valuation"


-- Array expressions
-- -----------------

-- |Tuples of arrays (of type 'Array dim e').  This characterises the domain of results of Accelerate
-- array computations.
--
class (Delayable arrs, Typeable arrs) => Arrays arrs where
  arrays :: ArraysR arrs
  
-- |GADT reifying the 'Arrays' class.
--
data ArraysR arrs where
  ArraysRunit  :: ArraysR ()
  ArraysRarray :: (Shape sh, Elt e) => ArraysR (Array sh e)
  ArraysRpair  :: ArraysR arrs1 -> ArraysR arrs2 -> ArraysR (arrs1, arrs2)
  
instance Arrays () where
  arrays = ArraysRunit
instance (Shape sh, Elt e) => Arrays (Array sh e) where
  arrays = ArraysRarray
instance (Arrays arrs1, Arrays arrs2) => Arrays (arrs1, arrs2) where
  arrays = ArraysRpair arrays arrays


-- |Function abstraction over array computations
--
data OpenAfun aenv t where
  Abody :: OpenAcc  aenv       t -> OpenAfun aenv t
  Alam  :: Arrays as
        => OpenAfun (aenv, as) t -> OpenAfun aenv (as -> t)

-- |Array computation function without free array variables
--
type Afun t = OpenAfun () t

-- |Collective array computations parametrised over array variables
-- represented with de Bruijn indices.
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
-- The data type is parameterised over the surface types (not the representation
-- type).
--
data OpenAcc aenv a where
  
  -- Local binding to represent sharing and demand explicitly; this is an
  -- eager(!) binding
  Let         :: (Arrays bndArrs, Arrays bodyArrs)
              => OpenAcc aenv bndArrs                  -- bound expression
              -> OpenAcc (aenv, bndArrs) 
                         bodyArrs                      -- the bound expr's scope           
              -> OpenAcc aenv bodyArrs

  -- Variant of 'Let' binding (and decomposing) a pair
  Let2        :: (Arrays bndArrs1, Arrays bndArrs2, Arrays bodyArrs)
              => OpenAcc aenv (bndArrs1, bndArrs2)     -- bound expressions 
              -> OpenAcc ((aenv, bndArrs1), bndArrs2)
                         bodyArrs                      -- the bound expr's scope           
              -> OpenAcc aenv bodyArrs

  -- Variable bound by a 'Let', represented by a de Bruijn index              
  Avar        :: Arrays arrs
              => Idx     aenv arrs
              -> OpenAcc aenv arrs
  
  -- Array inlet (triggers async host->device transfer if necessary)
  Use         :: Array dim e 
              -> OpenAcc aenv (Array dim e)

  -- Capture a scalar (or a tuple of scalars) in a singleton array  
  Unit        :: Elt e
              => Exp     aenv e 
              -> OpenAcc aenv (Scalar e)

  -- Change the shape of an array without altering its contents
  -- > precondition: size dim == size dim'
  Reshape     :: Shape sh
              => Exp     aenv sh                  -- new shape
              -> OpenAcc aenv (Array sh' e)       -- array to be reshaped
              -> OpenAcc aenv (Array sh e)

  -- Constuct a new array by applying a function to each index.
  Generate    :: (Shape sh, Elt e)
              => Exp     aenv sh                  -- output shape
              -> Fun     aenv (sh -> e)           -- representation function
              -> OpenAcc aenv (Array sh e)

  -- Replicate an array across one or more dimensions as given by the first
  -- argument
  Replicate   :: (Shape sh, Elt slix)
              => SliceIndex (EltRepr slix)        -- slice type specification
                            (EltRepr sl) 
                            co'
                            (EltRepr sh)
              -> Exp     aenv slix                -- slice value specification
              -> OpenAcc aenv (Array sl e)        -- data to be replicated
              -> OpenAcc aenv (Array sh e)

  -- Index a subarray out of an array; i.e., the dimensions not indexed are 
  -- returned whole
  Index       :: (Shape sl, Elt slix)
              => SliceIndex (EltRepr slix)       -- slice type specification
                            (EltRepr sl) 
                            co'
                            (EltRepr sh)
              -> OpenAcc aenv (Array sh e)        -- array to be indexed
              -> Exp     aenv slix                -- slice value specification
              -> OpenAcc aenv (Array sl e)

  -- Apply the given unary function to all elements of the given array
  Map         :: Elt e'
              => Fun     aenv (e -> e') 
              -> OpenAcc aenv (Array sh e) 
              -> OpenAcc aenv (Array sh e')

  -- Apply a given binary function pairwise to all elements of the given arrays.
  -- The length of the result is the length of the shorter of the two argument
  -- arrays.
  ZipWith     :: Elt e3
              => Fun     aenv (e1 -> e2 -> e3) 
              -> OpenAcc aenv (Array sh e1)
              -> OpenAcc aenv (Array sh e2)
              -> OpenAcc aenv (Array sh e3)

  -- Fold along the innermost dimension of an array with a given /associative/ function.
  Fold        :: Shape sh
              => Fun     aenv (e -> e -> e)          -- combination function
              -> Exp     aenv e                      -- default value
              -> OpenAcc aenv (Array (sh:.Int) e)    -- folded array
              -> OpenAcc aenv (Array sh e)

  -- 'Fold' without a default value
  Fold1       :: Shape sh
              => Fun     aenv (e -> e -> e)          -- combination function
              -> OpenAcc aenv (Array (sh:.Int) e)    -- folded array
              -> OpenAcc aenv (Array sh e)

  -- Segmented fold along the innermost dimension of an array with a given /associative/ function
  FoldSeg     :: Shape sh
              => Fun     aenv (e -> e -> e)           -- combination function
              -> Exp     aenv e                       -- default value
              -> OpenAcc aenv (Array (sh:.Int) e)     -- folded array
              -> OpenAcc aenv Segments                -- segment descriptor
              -> OpenAcc aenv (Array (sh:.Int) e)

  -- 'FoldSeg' without a default value
  Fold1Seg   :: Shape sh
             => Fun     aenv (e -> e -> e)            -- combination function
             -> OpenAcc aenv (Array (sh:.Int) e)      -- folded array
             -> OpenAcc aenv Segments                 -- segment descriptor
             -> OpenAcc aenv (Array (sh:.Int) e)

  -- Left-to-right Haskell-style scan of a linear array with a given *associative*
  -- function and an initial element (which does not need to be the neutral of the
  -- associative operations)
  Scanl       :: Fun     aenv (e -> e -> e)          -- combination function
              -> Exp     aenv e                      -- initial value
              -> OpenAcc aenv (Vector e)             -- linear array
              -> OpenAcc aenv (Vector e)
    -- FIXME: Make the scans rank-polymorphic?
  
  -- Like 'Scan', but produces a rightmost fold value and an array with the same length as the input
  -- array (the fold value would be the rightmost element in a Haskell-style scan)
  Scanl'      :: Fun     aenv (e -> e -> e)          -- combination function
              -> Exp     aenv e                      -- initial value
              -> OpenAcc aenv (Vector e)             -- linear array
              -> OpenAcc aenv (Vector e, Scalar e)

  -- Haskell-style scan without an initial value
  Scanl1      :: Fun     aenv (e -> e -> e)          -- combination function
              -> OpenAcc aenv (Vector e)             -- linear array
              -> OpenAcc aenv (Vector e)

  -- Right-to-left version of 'Scanl'
  Scanr       :: Fun     aenv (e -> e -> e)          -- combination function
              -> Exp     aenv e                      -- initial value
              -> OpenAcc aenv (Vector e)             -- linear array
              -> OpenAcc aenv (Vector e)
  
  -- Right-to-left version of 'Scanl\''
  Scanr'      :: Fun     aenv (e -> e -> e)          -- combination function
              -> Exp     aenv e                      -- initial value
              -> OpenAcc aenv (Vector e)             -- linear array
              -> OpenAcc aenv (Vector e, Scalar e)

  -- Right-to-left version of 'Scanl1'
  Scanr1      :: Fun     aenv (e -> e -> e)          -- combination function
              -> OpenAcc aenv (Vector e)             -- linear array
              -> OpenAcc aenv (Vector e)

  -- Generalised forward permutation is characterised by a permutation
  -- function that determines for each element of the source array where it
  -- should go in the target; the permutation can be between arrays of varying
  -- shape; the permutation function must be total.
  --
  -- The target array is initialised from an array of default values (in case
  -- some positions in the target array are never picked by the permutation
  -- functions).  Moreover, we have a combination function (in case some
  -- positions on the target array are picked multiple times by the
  -- permutation functions).  The combination function needs to be
  -- /associative/ and /commutative/ .  We drop every element for which the
  -- permutation function yields -1 (i.e., a tuple of -1 values).
  Permute     :: Fun     aenv (e -> e -> e)        -- combination function
              -> OpenAcc aenv (Array sh' e)        -- default values
              -> Fun     aenv (sh -> sh')          -- permutation function
              -> OpenAcc aenv (Array sh e)         -- source array
              -> OpenAcc aenv (Array sh' e)

  -- Generalised multi-dimensional backwards permutation; the permutation can
  -- be between arrays of varying shape; the permutation function must be total
  Backpermute :: Shape sh'
              => Exp     aenv sh'                  -- dimensions of the result
              -> Fun     aenv (sh' -> sh)          -- permutation function
              -> OpenAcc aenv (Array sh e)         -- source array
              -> OpenAcc aenv (Array sh' e)

  -- Map a stencil over an array.  In contrast to 'map', the domain of a stencil function is an
  -- entire /neighbourhood/ of each array element.
  Stencil :: (Elt e, Elt e', Stencil sh e stencil)
          => Fun      aenv (stencil -> e')         -- stencil function
          -> Boundary (EltRepr e)                  -- boundary condition
          -> OpenAcc  aenv (Array sh e)            -- source array
          -> OpenAcc  aenv (Array sh e')

  -- Map a binary stencil over an array.
  Stencil2 :: (Elt e1, Elt e2, Elt e', 
               Stencil sh e1 stencil1,
               Stencil sh e2 stencil2)
           => Fun      aenv (stencil1 -> 
                             stencil2 -> e')        -- stencil function
           -> Boundary (EltRepr e1)                 -- boundary condition #1
           -> OpenAcc  aenv (Array sh e1)           -- source array #1
           -> Boundary (EltRepr e2)                 -- boundary condition #2
           -> OpenAcc  aenv (Array sh e2)           -- source array #2
           -> OpenAcc  aenv (Array sh e')


deriving instance Typeable2 OpenAcc

-- |Closed array expression aka an array program
--
type Acc a = OpenAcc () a


-- | Operations on stencils.
--
class (Shape sh, Elt e, IsTuple stencil) => Stencil sh e stencil where
  stencil       :: StencilR sh e stencil
  stencilAccess :: (sh -> e) -> sh -> stencil


-- |GADT reifying the 'Stencil' class.
--
data StencilR sh e pat where
  StencilRunit3 :: (Elt e)
                => StencilR DIM1 e (e,e,e)
  StencilRunit5 :: (Elt e)
                => StencilR DIM1 e (e,e,e,e,e)
  StencilRunit7 :: (Elt e)
                => StencilR DIM1 e (e,e,e,e,e,e,e)
  StencilRunit9 :: (Elt e)
                => StencilR DIM1 e (e,e,e,e,e,e,e,e,e)
  StencilRtup3  :: (Shape sh, Elt e)
                => StencilR sh e pat1
                -> StencilR sh e pat2
                -> StencilR sh e pat3
                -> StencilR (sh:.Int) e (pat1,pat2,pat3)
  StencilRtup5  :: (Shape sh, Elt e)
                => StencilR sh e pat1
                -> StencilR sh e pat2
                -> StencilR sh e pat3
                -> StencilR sh e pat4
                -> StencilR sh e pat5
                -> StencilR (sh:.Int) e (pat1,pat2,pat3,pat4,pat5)
  StencilRtup7  :: (Shape sh, Elt e)
                => StencilR sh e pat1
                -> StencilR sh e pat2
                -> StencilR sh e pat3
                -> StencilR sh e pat4
                -> StencilR sh e pat5
                -> StencilR sh e pat6
                -> StencilR sh e pat7
                -> StencilR (sh:.Int) e (pat1,pat2,pat3,pat4,pat5,pat6,pat7)
  StencilRtup9  :: (Shape sh, Elt e)
                => StencilR sh e pat1
                -> StencilR sh e pat2
                -> StencilR sh e pat3
                -> StencilR sh e pat4
                -> StencilR sh e pat5
                -> StencilR sh e pat6
                -> StencilR sh e pat7
                -> StencilR sh e pat8
                -> StencilR sh e pat9
                -> StencilR (sh:.Int) e (pat1,pat2,pat3,pat4,pat5,pat6,pat7,pat8,pat9)


-- NB: We cannot start with 'DIM0'.  The 'IsTuple stencil' superclass would at 'DIM0' imply that
--     the types of individual array elements are in 'IsTuple'.  (That would only possible if we
--     could have (degenerate) 1-tuple, but we can't as we can't distinguish between a 1-tuple of a
--     pair and a simple pair.)  Hence, we need to start from 'DIM1' and use 'sh:.Int:.Int' in the
--     recursive case (to avoid overlapping instances).

-- DIM1
instance Elt e => Stencil DIM1 e (e, e, e) where
  stencil = StencilRunit3
  stencilAccess rf (Z:.y) = (rf' (y - 1),
                             rf' y      ,
                             rf' (y + 1))
    where
      rf' d = rf (Z:.d)
instance Elt e => Stencil DIM1 e (e, e, e, e, e) where
  stencil = StencilRunit5
  stencilAccess rf (Z:.y) = (rf' (y - 2),
                         rf' (y - 1),
                         rf' y      ,
                         rf' (y + 1),
                         rf' (y + 2))
    where
      rf' d = rf (Z:.d)
instance Elt e => Stencil DIM1 e (e, e, e, e, e, e, e) where
  stencil = StencilRunit7
  stencilAccess rf (Z:.y) = (rf' (y - 3),
                         rf' (y - 2),
                         rf' (y - 1),
                         rf' y      ,
                         rf' (y + 1),
                         rf' (y + 2),
                         rf' (y + 3))
    where
      rf' d = rf (Z:.d)
instance Elt e => Stencil DIM1 e (e, e, e, e, e, e, e, e, e) where
  stencil = StencilRunit9
  stencilAccess rf (Z:.y) = (rf' (y - 4),
                         rf' (y - 3),
                         rf' (y - 2),
                         rf' (y - 1),
                         rf' y      ,
                         rf' (y + 1),
                         rf' (y + 2),
                         rf' (y + 3),
                         rf' (y + 4))
    where
      rf' d = rf (Z:.d)

-- DIM(n+1), where n>0
instance (Stencil (sh:.Int) a row1,
          Stencil (sh:.Int) a row2,
          Stencil (sh:.Int) a row3) => Stencil (sh:.Int:.Int) a (row1, row2, row3) where
  stencil = StencilRtup3 stencil stencil stencil
  stencilAccess rf ix = (stencilAccess (rf' (y - 1)) xs',
                     stencilAccess (rf' y      ) xs',
                     stencilAccess (rf' (y + 1)) xs')

    where
      y : xs = reverse $ Sugar.shapeToList ix
      xs' = Sugar.listToShape $ reverse xs

      rf' :: Int -> (sh:.Int) -> a
      rf' d ds = rf $ Sugar.listToShape $ reverse $ Sugar.shapeToList (ds:.d)

instance (Stencil (sh:.Int) a row1,
          Stencil (sh:.Int) a row2,
          Stencil (sh:.Int) a row3,
          Stencil (sh:.Int) a row4,
          Stencil (sh:.Int) a row5) => Stencil (sh:.Int:.Int) a (row1, row2, row3, row4, row5) where
  stencil = StencilRtup5 stencil stencil stencil stencil stencil
  stencilAccess rf ix = (stencilAccess (rf' (y - 2)) xs',
                         stencilAccess (rf' (y - 1)) xs',
                         stencilAccess (rf' y      ) xs',
                         stencilAccess (rf' (y + 1)) xs',
                         stencilAccess (rf' (y + 2)) xs')
    where
      y : xs = reverse $ Sugar.shapeToList ix
      xs' = Sugar.listToShape $ reverse xs

      rf' :: Int -> (sh:.Int) -> a
      rf' d ds = rf $ Sugar.listToShape $ reverse $ Sugar.shapeToList (ds:.d)

instance (Stencil (sh:.Int) a row1,
          Stencil (sh:.Int) a row2,
          Stencil (sh:.Int) a row3,
          Stencil (sh:.Int) a row4,
          Stencil (sh:.Int) a row5,
          Stencil (sh:.Int) a row6,
          Stencil (sh:.Int) a row7)
  => Stencil (sh:.Int:.Int) a (row1, row2, row3, row4, row5, row6, row7) where
  stencil = StencilRtup7 stencil stencil stencil stencil stencil stencil stencil
  stencilAccess rf ix = (stencilAccess (rf' (y - 3)) xs',
                         stencilAccess (rf' (y - 2)) xs',
                         stencilAccess (rf' (y - 1)) xs',
                         stencilAccess (rf' y      ) xs',
                         stencilAccess (rf' (y + 1)) xs',
                         stencilAccess (rf' (y + 2)) xs',
                         stencilAccess (rf' (y + 3)) xs')
    where
      y : xs = reverse $ Sugar.shapeToList ix
      xs' = Sugar.listToShape $ reverse xs

      rf' :: Int -> (sh:.Int) -> a
      rf' d ds = rf $ Sugar.listToShape $ reverse $ Sugar.shapeToList (ds:.d)

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
  stencil = StencilRtup9 stencil stencil stencil stencil stencil stencil stencil stencil stencil
  stencilAccess rf ix = (stencilAccess (rf' (y - 4)) xs',
                         stencilAccess (rf' (y - 3)) xs',
                         stencilAccess (rf' (y - 2)) xs',
                         stencilAccess (rf' (y - 1)) xs',
                         stencilAccess (rf' y      ) xs',
                         stencilAccess (rf' (y + 1)) xs',
                         stencilAccess (rf' (y + 2)) xs',
                         stencilAccess (rf' (y + 3)) xs',
                         stencilAccess (rf' (y + 4)) xs')
    where
      y : xs = reverse $ Sugar.shapeToList ix
      xs' = Sugar.listToShape $ reverse xs

      rf' :: Int -> (sh:.Int) -> a
      rf' d ds = rf $ Sugar.listToShape $ reverse $ Sugar.shapeToList (ds:.d)


-- Embedded expressions
-- --------------------

-- |Function abstraction
--
data OpenFun env aenv t where
  Body :: OpenExp env              aenv t -> OpenFun env aenv t
  Lam  :: Elt a
       => OpenFun (env, EltRepr a) aenv t -> OpenFun env aenv (a -> t)

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
  Var         :: Elt t
              => Idx env (EltRepr t)
              -> OpenExp env aenv t

  -- Constant values
  Const       :: Elt t
              => EltRepr t
              -> OpenExp env aenv t
              
  -- Tuples
  Tuple       :: (Elt t, IsTuple t)
              => Tuple (OpenExp env aenv) (TupleRepr t)
              -> OpenExp env aenv t
  Prj         :: (Elt t, IsTuple t)
              => TupleIdx (TupleRepr t) e
              -> OpenExp env aenv t
              -> OpenExp env aenv e

  -- Array indices & shapes
  IndexNil    :: OpenExp env aenv Z
  IndexCons   :: Shape sh
              => OpenExp env aenv sh 
              -> OpenExp env aenv Int
              -> OpenExp env aenv (sh:.Int)
  IndexHead   :: Shape sh
              => OpenExp env aenv (sh:.Int)
              -> OpenExp env aenv Int
  IndexTail   :: Shape sh
              => OpenExp env aenv (sh:.Int)
              -> OpenExp env aenv sh
  
  -- Conditional expression (non-strict in 2nd and 3rd argument)
  Cond        :: OpenExp env aenv Bool
              -> OpenExp env aenv t 
              -> OpenExp env aenv t 
              -> OpenExp env aenv t

  -- Primitive constants
  PrimConst   :: Elt t
              => PrimConst t -> OpenExp env aenv t

  -- Primitive scalar operations
  PrimApp     :: (Elt a, Elt r)
              => PrimFun (a -> r) 
              -> OpenExp env aenv a
              -> OpenExp env aenv r

  -- Project a single scalar from an array
  -- the array expression can not contain any free scalar variables
  IndexScalar :: OpenAcc aenv (Array dim t)
              -> OpenExp env aenv dim 
              -> OpenExp env aenv t

  -- Array shape
  -- the array expression can not contain any free scalar variables
  Shape       :: Elt dim
              => OpenAcc aenv (Array dim e) 
              -> OpenExp env aenv dim

  -- Number of elements of an array
  -- the array expression can not contain any free scalar variables
  Size        :: Elt dim
              => OpenAcc aenv (Array dim e)
              -> OpenExp env aenv Int

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
  PrimQuot     :: IntegralType a -> PrimFun ((a, a)   -> a)
  PrimRem      :: IntegralType a -> PrimFun ((a, a)   -> a)
  PrimIDiv     :: IntegralType a -> PrimFun ((a, a)   -> a)
  PrimMod      :: IntegralType a -> PrimFun ((a, a)   -> a)
  PrimBAnd     :: IntegralType a -> PrimFun ((a, a)   -> a)
  PrimBOr      :: IntegralType a -> PrimFun ((a, a)   -> a)
  PrimBXor     :: IntegralType a -> PrimFun ((a, a)   -> a)
  PrimBNot     :: IntegralType a -> PrimFun (a        -> a)
  PrimBShiftL  :: IntegralType a -> PrimFun ((a, Int) -> a)
  PrimBShiftR  :: IntegralType a -> PrimFun ((a, Int) -> a)
  PrimBRotateL :: IntegralType a -> PrimFun ((a, Int) -> a)
  PrimBRotateR :: IntegralType a -> PrimFun ((a, Int) -> a)

  -- operators from Fractional, Floating, RealFrac & RealFloat
  
  PrimFDiv        :: FloatingType a -> PrimFun ((a, a) -> a)
  PrimRecip       :: FloatingType a -> PrimFun (a      -> a)
  PrimSin         :: FloatingType a -> PrimFun (a      -> a)
  PrimCos         :: FloatingType a -> PrimFun (a      -> a)
  PrimTan         :: FloatingType a -> PrimFun (a      -> a)
  PrimAsin        :: FloatingType a -> PrimFun (a      -> a)
  PrimAcos        :: FloatingType a -> PrimFun (a      -> a)
  PrimAtan        :: FloatingType a -> PrimFun (a      -> a)
  PrimAsinh       :: FloatingType a -> PrimFun (a      -> a)
  PrimAcosh       :: FloatingType a -> PrimFun (a      -> a)
  PrimAtanh       :: FloatingType a -> PrimFun (a      -> a)
  PrimExpFloating :: FloatingType a -> PrimFun (a      -> a)
  PrimSqrt        :: FloatingType a -> PrimFun (a      -> a)
  PrimLog         :: FloatingType a -> PrimFun (a      -> a)
  PrimFPow        :: FloatingType a -> PrimFun ((a,a)  -> a)
  PrimLogBase     :: FloatingType a -> PrimFun ((a,a)  -> a)
  PrimAtan2       :: FloatingType a -> PrimFun ((a,a)  -> a)
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
