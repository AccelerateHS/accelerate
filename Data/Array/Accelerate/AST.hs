{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.AST
-- Copyright   : [2008..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
--               [2009..2012] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
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
--

module Data.Array.Accelerate.AST (

  -- * Typed de Bruijn indices
  Idx(..), idxToInt, tupleIdxToInt,

  -- * Valuation environment
  Val(..), ValElt(..), prj, prjElt,

  -- * Accelerated array expressions
  PreOpenAfun(..), OpenAfun, PreAfun, Afun, PreOpenAcc(..), OpenAcc(..), Acc,
  Stencil(..), StencilR(..),

  -- * Scalar expressions
  PreOpenFun(..), OpenFun, PreFun, Fun, PreOpenExp(..), OpenExp, PreExp, Exp, PrimConst(..),
  PrimFun(..),

  -- debugging
  showPreAccOp, showPreExpOp,

) where

--standard library
import Data.List
import Data.Typeable

-- friends
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Array.Representation       ( SliceIndex )
import Data.Array.Accelerate.Array.Sugar                as Sugar

#include "accelerate.h"


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

deriving instance Typeable1 Val

-- Valuation for an environment of array elements
--
data ValElt env where
  EmptyElt :: ValElt ()
  PushElt  :: Elt t
           => ValElt env -> EltRepr t -> ValElt (env, t)

-- Projection of a value from a valuation using a de Bruijn index
--
prj :: Idx env t -> Val env -> t
prj ZeroIdx       (Push _   v) = v
prj (SuccIdx idx) (Push val _) = prj idx val
prj _             _            = INTERNAL_ERROR(error) "prj" "inconsistent valuation"

-- Projection of a value from a valuation of array elements using a de Bruijn index
--
prjElt :: Idx env t -> ValElt env -> t
prjElt ZeroIdx       (PushElt _   v) = Sugar.toElt v
prjElt (SuccIdx idx) (PushElt val _) = prjElt idx val
prjElt _             _               = INTERNAL_ERROR(error) "prjElt" "inconsistent valuation"


-- Array expressions
-- -----------------

-- |Function abstraction over parametrised array computations
--
data PreOpenAfun acc aenv t where
  Abody :: Arrays t => acc             aenv      t -> PreOpenAfun acc aenv t
  Alam  :: Arrays a => PreOpenAfun acc (aenv, a) t -> PreOpenAfun acc aenv (a -> t)

-- Function abstraction over vanilla open array computations
--
type OpenAfun = PreOpenAfun OpenAcc

-- |Parametrised array-computation function without free array variables
--
type PreAfun acc = PreOpenAfun acc ()

-- |Vanilla array-computation function without free array variables
--
type Afun = OpenAfun ()

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
-- We use a non-recursive variant parametrised over the recursive closure, to facilitate attribute
-- calculation in the backend.
--
data PreOpenAcc acc aenv a where

  -- Local binding to represent sharing and demand explicitly; this is an
  -- eager(!) binding
  Alet        :: (Arrays bndArrs, Arrays bodyArrs)
              => acc            aenv            bndArrs         -- bound expression
              -> acc            (aenv, bndArrs) bodyArrs        -- the bound expression scope
              -> PreOpenAcc acc aenv            bodyArrs

  -- Variable bound by a 'Let', represented by a de Bruijn index
  Avar        :: Arrays arrs
              => Idx            aenv arrs
              -> PreOpenAcc acc aenv arrs

  -- Tuples of arrays
  Atuple      :: (Arrays arrs, IsTuple arrs)
              => Atuple    (acc aenv) (TupleRepr arrs)
              -> PreOpenAcc acc aenv  arrs

  Aprj        :: (Arrays arrs, IsTuple arrs, Arrays a)
              => TupleIdx (TupleRepr arrs) a
              ->            acc aenv arrs
              -> PreOpenAcc acc aenv a

  -- Array-function application.
  --
  -- The array function is not closed at the core level because we need access
  -- to free variables introduced by 'run1' style evaluators. See Issue#95.
  --
  Apply       :: (Arrays arrs1, Arrays arrs2)
              => PreOpenAfun acc aenv (arrs1 -> arrs2)
              -> acc             aenv arrs1
              -> PreOpenAcc  acc aenv arrs2

  -- Apply a backend-specific foreign function to an array, with a pure
  -- Accelerate version for use with other backends. The functions must be
  -- closed.
  Aforeign    :: (Arrays arrs, Arrays a, Foreign f)
              => f arrs a                                       -- The foreign function for a given backend
              -> PreAfun      acc      (arrs -> a)              -- A pure accelerate version
              -> acc              aenv arrs                     -- Arguments to the function
              -> PreOpenAcc   acc aenv a

  -- If-then-else for array-level computations
  Acond       :: Arrays arrs
              => PreExp     acc aenv Bool
              -> acc            aenv arrs
              -> acc            aenv arrs
              -> PreOpenAcc acc aenv arrs

  -- Value-recursion for array-level computations
  Awhile      :: Arrays arrs
              => PreOpenAfun acc aenv (arrs -> Scalar Bool)     -- continue iteration while true
              -> PreOpenAfun acc aenv (arrs -> arrs)            -- function to iterate
              -> acc             aenv arrs                      -- initial value
              -> PreOpenAcc  acc aenv arrs


  -- Array inlet (triggers async host->device transfer if necessary)
  Use         :: Arrays arrs
              => ArrRepr arrs
              -> PreOpenAcc acc aenv arrs

  -- Capture a scalar (or a tuple of scalars) in a singleton array
  Unit        :: Elt e
              => PreExp     acc aenv e
              -> PreOpenAcc acc aenv (Scalar e)

  -- Change the shape of an array without altering its contents
  -- > precondition: size dim == size dim'
  Reshape     :: (Shape sh, Shape sh', Elt e)
              => PreExp     acc aenv sh                         -- new shape
              -> acc            aenv (Array sh' e)              -- array to be reshaped
              -> PreOpenAcc acc aenv (Array sh e)

  -- Construct a new array by applying a function to each index.
  Generate    :: (Shape sh, Elt e)
              => PreExp     acc aenv sh                         -- output shape
              -> PreFun     acc aenv (sh -> e)                  -- representation function
              -> PreOpenAcc acc aenv (Array sh e)

  -- Hybrid map/backpermute, where we separate the index and value
  -- transformations.
  Transform   :: (Elt a, Elt b, Shape sh, Shape sh')
              => PreExp     acc aenv sh'                        -- dimension of the result
              -> PreFun     acc aenv (sh' -> sh)                -- index permutation function
              -> PreFun     acc aenv (a   -> b)                 -- function to apply at each element
              ->            acc aenv (Array sh  a)              -- source array
              -> PreOpenAcc acc aenv (Array sh' b)

  -- Replicate an array across one or more dimensions as given by the first
  -- argument
  Replicate   :: (Shape sh, Shape sl, Elt slix, Elt e)
              => SliceIndex (EltRepr slix)                      -- slice type specification
                            (EltRepr sl)
                            co
                            (EltRepr sh)
              -> PreExp     acc aenv slix                       -- slice value specification
              -> acc            aenv (Array sl e)               -- data to be replicated
              -> PreOpenAcc acc aenv (Array sh e)

  -- Index a sub-array out of an array; i.e., the dimensions not indexed are
  -- returned whole
  Slice       :: (Shape sh, Shape sl, Elt slix, Elt e)
              => SliceIndex (EltRepr slix)                      -- slice type specification
                            (EltRepr sl)
                            co
                            (EltRepr sh)
              -> acc            aenv (Array sh e)               -- array to be indexed
              -> PreExp     acc aenv slix                       -- slice value specification
              -> PreOpenAcc acc aenv (Array sl e)

  -- Apply the given unary function to all elements of the given array
  Map         :: (Shape sh, Elt e, Elt e')
              => PreFun     acc aenv (e -> e')
              -> acc            aenv (Array sh e)
              -> PreOpenAcc acc aenv (Array sh e')

  -- Apply a given binary function pairwise to all elements of the given arrays.
  -- The length of the result is the length of the shorter of the two argument
  -- arrays.
  ZipWith     :: (Shape sh, Elt e1, Elt e2, Elt e3)
              => PreFun     acc aenv (e1 -> e2 -> e3)
              -> acc            aenv (Array sh e1)
              -> acc            aenv (Array sh e2)
              -> PreOpenAcc acc aenv (Array sh e3)

  -- Fold along the innermost dimension of an array with a given /associative/ function.
  Fold        :: (Shape sh, Elt e)
              => PreFun     acc aenv (e -> e -> e)              -- combination function
              -> PreExp     acc aenv e                          -- default value
              -> acc            aenv (Array (sh:.Int) e)        -- folded array
              -> PreOpenAcc acc aenv (Array sh e)

  -- 'Fold' without a default value
  Fold1       :: (Shape sh, Elt e)
              => PreFun     acc aenv (e -> e -> e)              -- combination function
              -> acc            aenv (Array (sh:.Int) e)        -- folded array
              -> PreOpenAcc acc aenv (Array sh e)

  -- Segmented fold along the innermost dimension of an array with a given /associative/ function
  FoldSeg     :: (Shape sh, Elt e, Elt i, IsIntegral i)
              => PreFun     acc aenv (e -> e -> e)              -- combination function
              -> PreExp     acc aenv e                          -- default value
              -> acc            aenv (Array (sh:.Int) e)        -- folded array
              -> acc            aenv (Segments i)               -- segment descriptor
              -> PreOpenAcc acc aenv (Array (sh:.Int) e)

  -- 'FoldSeg' without a default value
  Fold1Seg    :: (Shape sh, Elt e, Elt i, IsIntegral i)
              => PreFun     acc aenv (e -> e -> e)              -- combination function
              -> acc            aenv (Array (sh:.Int) e)        -- folded array
              -> acc            aenv (Segments i)               -- segment descriptor
              -> PreOpenAcc acc aenv (Array (sh:.Int) e)

  -- Left-to-right Haskell-style scan of a linear array with a given *associative*
  -- function and an initial element (which does not need to be the neutral of the
  -- associative operations)
  Scanl       :: Elt e
              => PreFun     acc aenv (e -> e -> e)              -- combination function
              -> PreExp     acc aenv e                          -- initial value
              -> acc            aenv (Vector e)                 -- linear array
              -> PreOpenAcc acc aenv (Vector e)
    -- FIXME: Make the scans rank-polymorphic?

  -- Like 'Scan', but produces a rightmost fold value and an array with the same length as the input
  -- array (the fold value would be the rightmost element in a Haskell-style scan)
  Scanl'      :: Elt e
              => PreFun     acc aenv (e -> e -> e)              -- combination function
              -> PreExp     acc aenv e                          -- initial value
              -> acc            aenv (Vector e)                 -- linear array
              -> PreOpenAcc acc aenv (Vector e, Scalar e)

  -- Haskell-style scan without an initial value
  Scanl1      :: Elt e
              => PreFun     acc aenv (e -> e -> e)              -- combination function
              -> acc            aenv (Vector e)                 -- linear array
              -> PreOpenAcc acc aenv (Vector e)

  -- Right-to-left version of 'Scanl'
  Scanr       :: Elt e
              => PreFun     acc aenv (e -> e -> e)              -- combination function
              -> PreExp     acc aenv e                          -- initial value
              -> acc            aenv (Vector e)                 -- linear array
              -> PreOpenAcc acc aenv (Vector e)

  -- Right-to-left version of 'Scanl\''
  Scanr'      :: Elt e
              => PreFun     acc aenv (e -> e -> e)              -- combination function
              -> PreExp     acc aenv e                          -- initial value
              -> acc            aenv (Vector e)                 -- linear array
              -> PreOpenAcc acc aenv (Vector e, Scalar e)

  -- Right-to-left version of 'Scanl1'
  Scanr1      :: Elt e
              => PreFun     acc aenv (e -> e -> e)              -- combination function
              -> acc            aenv (Vector e)                 -- linear array
              -> PreOpenAcc acc aenv (Vector e)

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
  Permute     :: (Shape sh, Shape sh', Elt e)
              => PreFun     acc aenv (e -> e -> e)              -- combination function
              -> acc            aenv (Array sh' e)              -- default values
              -> PreFun     acc aenv (sh -> sh')                -- permutation function
              -> acc            aenv (Array sh e)               -- source array
              -> PreOpenAcc acc aenv (Array sh' e)

  -- Generalised multi-dimensional backwards permutation; the permutation can
  -- be between arrays of varying shape; the permutation function must be total
  Backpermute :: (Shape sh, Shape sh', Elt e)
              => PreExp     acc aenv sh'                        -- dimensions of the result
              -> PreFun     acc aenv (sh' -> sh)                -- permutation function
              -> acc            aenv (Array sh e)               -- source array
              -> PreOpenAcc acc aenv (Array sh' e)

  -- Map a stencil over an array.  In contrast to 'map', the domain of a stencil function is an
  -- entire /neighbourhood/ of each array element.
  Stencil     :: (Elt e, Elt e', Stencil sh e stencil)
              => PreFun     acc aenv (stencil -> e')            -- stencil function
              -> Boundary            (EltRepr e)                -- boundary condition
              -> acc            aenv (Array sh e)               -- source array
              -> PreOpenAcc acc aenv (Array sh e')

  -- Map a binary stencil over an array.
  Stencil2    :: (Elt e1, Elt e2, Elt e',
                  Stencil sh e1 stencil1,
                  Stencil sh e2 stencil2)
              => PreFun     acc aenv (stencil1 ->
                                      stencil2 -> e')           -- stencil function
              -> Boundary            (EltRepr e1)               -- boundary condition #1
              -> acc            aenv (Array sh e1)              -- source array #1
              -> Boundary            (EltRepr e2)               -- boundary condition #2
              -> acc            aenv (Array sh e2)              -- source array #2
              -> PreOpenAcc acc aenv (Array sh e')


-- Vanilla open array computations
--
newtype OpenAcc aenv t = OpenAcc (PreOpenAcc OpenAcc aenv t)

-- deriving instance Typeable3 PreOpenAcc
deriving instance Typeable2 OpenAcc

-- |Closed array expression aka an array program
--
type Acc = OpenAcc ()

-- |Operations on stencils.
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
  stencilAccess rf xi = (stencilAccess (rf' (i - 1)) ix,
                         stencilAccess (rf'  i     ) ix,
                         stencilAccess (rf' (i + 1)) ix)

    where
      -- Invert then re-invert to ensure each recursive step gets a shape in the
      -- standard scoc (right-recursive) ordering
      --
      ix' :. i  = invertShape xi
      ix        = invertShape ix'

      -- Inject this dimension innermost
      --
      rf' d ds  = rf $ invertShape (invertShape ds :. d)


instance (Stencil (sh:.Int) a row1,
          Stencil (sh:.Int) a row2,
          Stencil (sh:.Int) a row3,
          Stencil (sh:.Int) a row4,
          Stencil (sh:.Int) a row5) => Stencil (sh:.Int:.Int) a (row1, row2, row3, row4, row5) where
  stencil = StencilRtup5 stencil stencil stencil stencil stencil
  stencilAccess rf xi = (stencilAccess (rf' (i - 2)) ix,
                         stencilAccess (rf' (i - 1)) ix,
                         stencilAccess (rf'  i     ) ix,
                         stencilAccess (rf' (i + 1)) ix,
                         stencilAccess (rf' (i + 2)) ix)
    where
      ix' :. i  = invertShape xi
      ix        = invertShape ix'
      rf' d ds  = rf $ invertShape (invertShape ds :. d)

instance (Stencil (sh:.Int) a row1,
          Stencil (sh:.Int) a row2,
          Stencil (sh:.Int) a row3,
          Stencil (sh:.Int) a row4,
          Stencil (sh:.Int) a row5,
          Stencil (sh:.Int) a row6,
          Stencil (sh:.Int) a row7)
  => Stencil (sh:.Int:.Int) a (row1, row2, row3, row4, row5, row6, row7) where
  stencil = StencilRtup7 stencil stencil stencil stencil stencil stencil stencil
  stencilAccess rf xi = (stencilAccess (rf' (i - 3)) ix,
                         stencilAccess (rf' (i - 2)) ix,
                         stencilAccess (rf' (i - 1)) ix,
                         stencilAccess (rf'  i     ) ix,
                         stencilAccess (rf' (i + 1)) ix,
                         stencilAccess (rf' (i + 2)) ix,
                         stencilAccess (rf' (i + 3)) ix)
    where
      ix' :. i  = invertShape xi
      ix        = invertShape ix'
      rf' d ds  = rf $ invertShape (invertShape ds :. d)

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
  stencilAccess rf xi = (stencilAccess (rf' (i - 4)) ix,
                         stencilAccess (rf' (i - 3)) ix,
                         stencilAccess (rf' (i - 2)) ix,
                         stencilAccess (rf' (i - 1)) ix,
                         stencilAccess (rf'  i     ) ix,
                         stencilAccess (rf' (i + 1)) ix,
                         stencilAccess (rf' (i + 2)) ix,
                         stencilAccess (rf' (i + 3)) ix,
                         stencilAccess (rf' (i + 4)) ix)
    where
      ix' :. i  = invertShape xi
      ix        = invertShape ix'
      rf' d ds  = rf $ invertShape (invertShape ds :. d)


-- For stencilAccess to match how the user draws the stencil in code as a series
-- of nested tuples, we need to recurse from the left. That is, we desire the
-- following 2D stencil to represent elements to the top, bottom, left, and
-- right of the focus as follows:
--
-- stencil2D ( (_, t, _)
--           , (l, _, r)
--           , (_, b, _) ) = ...
--
-- This function is used to reverse all components of a shape so that the
-- innermost component, now the head, can be picked off.
--
-- ...but needing to go via lists is unfortunate.
--
invertShape :: Shape sh => sh -> sh
invertShape =  listToShape . reverse . shapeToList


-- Embedded expressions
-- --------------------

-- |Parametrised open function abstraction
--
data PreOpenFun (acc :: * -> * -> *) env aenv t where
  Body :: Elt t => PreOpenExp acc env      aenv t -> PreOpenFun acc env aenv t
  Lam  :: Elt a => PreOpenFun acc (env, a) aenv t -> PreOpenFun acc env aenv (a -> t)

-- |Vanilla open function abstraction
--
type OpenFun = PreOpenFun OpenAcc

-- |Parametrised function without free scalar variables
--
type PreFun acc = PreOpenFun acc ()

-- |Vanilla function without free scalar variables
--
type Fun = OpenFun ()

-- |Parametrised open expressions using de Bruijn indices for variables ranging over tuples
-- of scalars and arrays of tuples.  All code, except Cond, is evaluated eagerly.  N-tuples are
-- represented as nested pairs.
--
-- The data type is parametrised over the surface types (not the representation type).
--
data PreOpenExp (acc :: * -> * -> *) env aenv t where

  -- Local binding of a scalar expression
  Let           :: (Elt bnd_t, Elt body_t)
                => PreOpenExp acc env          aenv bnd_t
                -> PreOpenExp acc (env, bnd_t) aenv body_t
                -> PreOpenExp acc env          aenv body_t

  -- Variable index, ranging only over tuples or scalars
  Var           :: Elt t
                => Idx env t
                -> PreOpenExp acc env aenv t

  -- Apply a backend-specific foreign function
  Foreign       :: (Foreign f, Elt x, Elt y)
                => f x y
                -> PreFun acc () (x -> y)
                -> PreOpenExp acc env aenv x
                -> PreOpenExp acc env aenv y

  -- Constant values
  Const         :: Elt t
                => EltRepr t
                -> PreOpenExp acc env aenv t

  -- Tuples
  Tuple         :: (Elt t, IsTuple t)
                => Tuple (PreOpenExp acc env aenv) (TupleRepr t)
                -> PreOpenExp acc env aenv t

  Prj           :: (Elt t, IsTuple t, Elt e)
                => TupleIdx (TupleRepr t) e
                -> PreOpenExp acc env aenv t
                -> PreOpenExp acc env aenv e

  -- Array indices & shapes
  IndexNil      :: PreOpenExp acc env aenv Z

  IndexCons     :: (Slice sl, Elt a)
                => PreOpenExp acc env aenv sl
                -> PreOpenExp acc env aenv a
                -> PreOpenExp acc env aenv (sl:.a)

  IndexHead     :: (Slice sl, Elt a)
                => PreOpenExp acc env aenv (sl:.a)
                -> PreOpenExp acc env aenv a

  IndexTail     :: (Slice sl, Elt a)
                => PreOpenExp acc env aenv (sl:.a)
                -> PreOpenExp acc env aenv sl

  IndexAny      :: Shape sh
                => PreOpenExp acc env aenv (Any sh)

  IndexSlice    :: (Shape sh, Shape sl, Elt slix)
                => SliceIndex (EltRepr slix) (EltRepr sl) co (EltRepr sh)
                -> PreOpenExp acc env aenv slix
                -> PreOpenExp acc env aenv sh
                -> PreOpenExp acc env aenv sl

  IndexFull     :: (Shape sh, Shape sl, Elt slix)
                => SliceIndex (EltRepr slix) (EltRepr sl) co (EltRepr sh)
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
  Cond          :: Elt t
                => PreOpenExp acc env aenv Bool
                -> PreOpenExp acc env aenv t
                -> PreOpenExp acc env aenv t
                -> PreOpenExp acc env aenv t

  -- Value recursion
  While         :: Elt a
                => PreOpenFun acc env aenv (a -> Bool)  -- continue while true
                -> PreOpenFun acc env aenv (a -> a)     -- function to iterate
                -> PreOpenExp acc env aenv a            -- initial value
                -> PreOpenExp acc env aenv a

  -- Primitive constants
  PrimConst     :: Elt t
                => PrimConst t
                -> PreOpenExp acc env aenv t

  -- Primitive scalar operations
  PrimApp       :: (Elt a, Elt r)
                => PrimFun (a -> r)
                -> PreOpenExp acc env aenv a
                -> PreOpenExp acc env aenv r

  -- Project a single scalar from an array.
  -- The array expression can not contain any free scalar variables.
  Index         :: (Shape dim, Elt t)
                => acc                aenv (Array dim t)
                -> PreOpenExp acc env aenv dim
                -> PreOpenExp acc env aenv t

  LinearIndex   :: (Shape dim, Elt t)
                => acc                aenv (Array dim t)
                -> PreOpenExp acc env aenv Int
                -> PreOpenExp acc env aenv t

  -- Array shape.
  -- The array expression can not contain any free scalar variables.
  Shape         :: (Shape dim, Elt e)
                => acc                aenv (Array dim e)
                -> PreOpenExp acc env aenv dim

  -- Number of elements of an array given its shape
  ShapeSize     :: Shape dim
                => PreOpenExp acc env aenv dim
                -> PreOpenExp acc env aenv Int

  -- Intersection of two shapes
  Intersect     :: Shape dim
                => PreOpenExp acc env aenv dim
                -> PreOpenExp acc env aenv dim
                -> PreOpenExp acc env aenv dim


-- |Vanilla open expression
--
type OpenExp = PreOpenExp OpenAcc

-- |Parametrised expression without free scalar variables
--
type PreExp acc = PreOpenExp acc ()

-- |Vanilla expression without free scalar variables
--
type Exp = OpenExp ()

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
  PrimFPow        :: FloatingType a -> PrimFun ((a, a) -> a)
  PrimLogBase     :: FloatingType a -> PrimFun ((a, a) -> a)
  PrimAtan2       :: FloatingType a -> PrimFun ((a, a) -> a)
  PrimTruncate    :: FloatingType a -> IntegralType b -> PrimFun (a -> b)
  PrimRound       :: FloatingType a -> IntegralType b -> PrimFun (a -> b)
  PrimFloor       :: FloatingType a -> IntegralType b -> PrimFun (a -> b)
  PrimCeiling     :: FloatingType a -> IntegralType b -> PrimFun (a -> b)
  -- FIXME: add missing operations from RealFrac & RealFloat

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

  -- FIXME: conversions between various integer types
  --        should we have an overloaded functions like 'toInt'?
  --        (or 'fromEnum' for enums?)
  PrimBoolToInt    :: PrimFun (Bool -> Int)
  PrimFromIntegral :: IntegralType a -> NumType b -> PrimFun (a -> b)

  -- FIXME: what do we want to do about Enum?  succ and pred are only
  --   moderatly useful without user-defined enumerations, but we want
  --   the range constructs for arrays (but that's not scalar primitives)


-- Debugging
-- ---------

showPreAccOp :: forall acc aenv arrs. PreOpenAcc acc aenv arrs -> String
showPreAccOp Alet{}             = "Alet"
showPreAccOp (Avar ix)          = "Avar a" ++ show (idxToInt ix)
showPreAccOp (Use a)            = "Use "  ++ showArrays (toArr a :: arrs)
showPreAccOp Apply{}            = "Apply"
showPreAccOp Aforeign{}         = "Aforeign"
showPreAccOp Acond{}            = "Acond"
showPreAccOp Awhile{}           = "Awhile"
showPreAccOp Atuple{}           = "Atuple"
showPreAccOp Aprj{}             = "Aprj"
showPreAccOp Unit{}             = "Unit"
showPreAccOp Generate{}         = "Generate"
showPreAccOp Transform{}        = "Transform"
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
    l      = Sugar.toList arr
    cutoff = 5


showPreExpOp :: forall acc env aenv t. PreOpenExp acc env aenv t -> String
showPreExpOp Let{}              = "Let"
showPreExpOp (Var ix)           = "Var x" ++ show (idxToInt ix)
showPreExpOp (Const c)          = "Const " ++ show (toElt c :: t)
showPreExpOp Foreign{}          = "Foreign"
showPreExpOp Tuple{}            = "Tuple"
showPreExpOp Prj{}              = "Prj"
showPreExpOp IndexNil           = "IndexNil"
showPreExpOp IndexCons{}        = "IndexCons"
showPreExpOp IndexHead{}        = "IndexHead"
showPreExpOp IndexTail{}        = "IndexTail"
showPreExpOp IndexAny           = "IndexAny"
showPreExpOp IndexSlice{}       = "IndexSlice"
showPreExpOp IndexFull{}        = "IndexFull"
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

