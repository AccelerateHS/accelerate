{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.AST
-- Copyright   : [2008..2014] Manuel M T Chakravarty, Gabriele Keller
--               [2008..2009] Sean Lee
--               [2009..2014] Trevor L. McDonell
--               [2010..2011] Ben Lever
--               [2014..2014] Frederik M. Madsen
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

  -- * Accelerated sequences
  PreOpenSeq(..), PreOpenNaturalSeq, PreOpenChunkedSeq, OpenNaturalSeq, OpenChunkedSeq, OpenSeq,
  Producer(..), Consumer(..), NaturalProducer, ChunkedProducer, NaturalConsumer, ChunkedConsumer, Seq,
  Source(..), SeqIndex(..),

  -- * Scalar expressions
  PreOpenFun(..), OpenFun, PreFun, Fun, PreOpenExp(..), OpenExp, PreExp, Exp, PrimConst(..),
  PrimFun(..),

  -- NFData
  NFDataAcc,
  rnfOpenAcc, rnfPreOpenAfun, rnfPreOpenAcc, rnfPreOpenSeq, rnfPreOpenFun, rnfPreOpenExp,

  -- debugging
  showPreAccOp, showPreExpOp,

) where

--standard library
import Data.List
import Data.Typeable
import Control.DeepSeq

-- friends
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Array.Representation       ( SliceIndex(..) )
import Data.Array.Accelerate.Array.Sugar                as Sugar hiding ( tuple )
import Data.Array.Accelerate.Array.Lifted               ( LiftedType(..), LiftedTupleType(..) )
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
#if __GLASGOW_HASKELL__ < 800
prj _             _            = $internalError "prj" "inconsistent valuation"
#endif

-- Projection of a value from a valuation of array elements using a de Bruijn index
--
prjElt :: Idx env t -> ValElt env -> t
prjElt ZeroIdx       (PushElt _   v) = Sugar.toElt v
prjElt (SuccIdx idx) (PushElt val _) = prjElt idx val
#if __GLASGOW_HASKELL__ < 800
prjElt _             _               = $internalError "prjElt" "inconsistent valuation"
#endif

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

-- Vanilla open array computations
--
newtype OpenAcc aenv t = OpenAcc (PreOpenAcc OpenAcc aenv t)

-- |Closed array expression aka an array program
--
type Acc = OpenAcc ()

deriving instance Typeable PreOpenAcc
deriving instance Typeable OpenAcc


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
  Atuple      :: (Arrays arrs, IsAtuple arrs)
              => Atuple    (acc aenv) (TupleRepr arrs)
              -> PreOpenAcc acc aenv  arrs

  Aprj        :: (Arrays arrs, IsAtuple arrs, Arrays a)
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
  Aforeign    :: (Arrays as, Arrays bs, Foreign asm)
              => asm                   (as -> bs)               -- The foreign function for a given backend
              -> PreAfun      acc      (as -> bs)               -- Fallback implementation(s)
              -> acc              aenv as                       -- Arguments to the function
              -> PreOpenAcc   acc aenv bs

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

  -- Subarray inlet by strided copy
  Subarray    :: (Shape sh, Elt e, sh :<= DIM2)
              => PreExp acc aenv sh                             -- index of subarray
              -> PreExp acc aenv sh                             -- extent of subarray
              -> Array sh e
              -> PreOpenAcc acc aenv (Array sh e)

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
  Replicate   :: (Shape sh, Shape sl, Slice slix, Elt e)
              => SliceIndex (EltRepr slix)                      -- slice type specification
                            (EltRepr sl)
                            co
                            (EltRepr sh)
              -> PreExp     acc aenv slix                       -- slice value specification
              -> acc            aenv (Array sl e)               -- data to be replicated
              -> PreOpenAcc acc aenv (Array sh e)

  -- Index a sub-array out of an array; i.e., the dimensions not indexed are
  -- returned whole
  Slice       :: (Shape sh, Shape sl, Slice slix, Elt e)
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
  Scanl       :: (Shape sh, Elt e)
              => PreFun     acc aenv (e -> e -> e)              -- combination function
              -> PreExp     acc aenv e                          -- initial value
              -> acc            aenv (Array (sh:.Int) e)
              -> PreOpenAcc acc aenv (Array (sh:.Int) e)
    -- FIXME: Make the scans rank-polymorphic?

  -- Like 'Scan', but produces a rightmost fold value and an array with the same length as the input
  -- array (the fold value would be the rightmost element in a Haskell-style scan)
  Scanl'      :: (Shape sh, Elt e)
              => PreFun     acc aenv (e -> e -> e)              -- combination function
              -> PreExp     acc aenv e                          -- initial value
              -> acc            aenv (Array (sh:.Int) e)
              -> PreOpenAcc acc aenv (Array (sh:.Int) e, Array sh e)

  -- Haskell-style scan without an initial value
  Scanl1      :: (Shape sh, Elt e)
              => PreFun     acc aenv (e -> e -> e)              -- combination function
              -> acc            aenv (Array (sh:.Int) e)
              -> PreOpenAcc acc aenv (Array (sh:.Int) e)

  -- Right-to-left version of 'Scanl'
  Scanr       :: (Shape sh, Elt e)
              => PreFun     acc aenv (e -> e -> e)              -- combination function
              -> PreExp     acc aenv e                          -- initial value
              -> acc            aenv (Array (sh:.Int) e)
              -> PreOpenAcc acc aenv (Array (sh:.Int) e)

  -- Right-to-left version of 'Scanl\''
  Scanr'      :: (Shape sh, Elt e)
              => PreFun     acc aenv (e -> e -> e)              -- combination function
              -> PreExp     acc aenv e                          -- initial value
              -> acc            aenv (Array (sh:.Int) e)
              -> PreOpenAcc acc aenv (Array (sh:.Int) e, Array sh e)

  -- Right-to-left version of 'Scanl1'
  Scanr1      :: (Shape sh, Elt e)
              => PreFun     acc aenv (e -> e -> e)              -- combination function
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

  -- Force a sequence computation. The computation is iterated for as many times
  -- as is required to consume all input sequences or until the maximum number
  -- of iterations is reached. If the sequence has a chunked equivalent,
  -- backends will initially use the supplied minimum chunk size and may
  -- increase it for scheduling reasons, but never go above the provided
  -- maximum.
  --
  Collect     :: (SeqIndex index, Arrays arrs)
              => PreExp acc aenv Int                -- min number of elements per iteration
              -> Maybe (PreExp acc aenv Int)        -- max number per iteration
              -> Maybe (PreExp acc aenv Int)        -- max number of iterations
              -> PreOpenSeq index acc aenv arrs
              -> PreOpenAcc acc aenv arrs


-- | Computations over sequences.
--
-- By parameterising over the index type, we can encode sequences that compute
-- an element at a time as well as sequences that compute elements in chunks.
--
data PreOpenSeq index acc aenv arrs where

  -- Bind a producer.
  Producer :: Arrays a
           => Producer index acc aenv a
           -> PreOpenSeq index acc (aenv, a) arrs
           -> PreOpenSeq index acc aenv      arrs

  -- Consume previously bound producers.
  Consumer :: Arrays arrs
           => Consumer   index acc aenv arrs
           -> PreOpenSeq index acc aenv arrs

  -- Make a sequence manifest.
  --
  Reify    :: (Arrays arrs, Arrays arrs')
           => LiftedType arrs arrs'
           -> acc aenv arrs'
           -> PreOpenSeq index acc aenv [arrs]

-- | External sources of sequences for sequence computations.
--
data Source a where

  -- Lazily pull elements from a list to create a sequence.
  List      :: (Shape sh, Elt e)
            => [Array sh e]
            -> Source (Array sh e)

  -- Similar to above but all arrays are of a statically known shape.
  RegularList :: (Shape sh, Elt e)
              => sh
              -> [Array sh e]
              -> Source (Array sh e)

  Function    :: Arrays a
              => (Int -> s -> (Bool,a,s))
              -> s
              -> Source a

-- | A sequence producer.
--
data Producer index acc aenv a where

  -- Pull from the given source
  --
  -- Occurs in all stages of the pipeline.
  Pull         :: Arrays a
               => Source a
               -> Producer index acc aenv a

  -- Split an array up into subarrays along the outermost dimension.
  --
  -- Turned into 'ProduceAccum' and 'Subarray' by vectorisation.
  Subarrays    :: (Shape sh, Elt e, sh :<= DIM2)
               => PreExp acc aenv sh             -- The size of each subarray
               -> Array sh e                     -- The array to extract from
               -> Producer index acc aenv (Array sh e)

  -- Generate a sequence from segment descriptors and the flattened values
  -- vector.
  --
  -- Turned into 'ProduceAccum' by vectorisation
  FromSegs     :: (Shape sh, Elt e)
               => acc aenv (Segments (Int,sh))
               -> PreExp acc aenv Int            -- Number of segments
               -> acc aenv (Vector e)
               -> Producer index acc aenv (Array sh e)

  -- Generate a sequence from a function until limit is reached.
  --
  -- Converted to ProduceAccum by vectorisation (with () as the accumulator.)
  Produce      :: Arrays a
               => Maybe (PreExp acc aenv Int)        -- The "limit" of the sequence.
               -> PreOpenAfun acc aenv (Scalar index -> a)
               -> Producer index acc aenv a

  -- Perform a batched map.
  -- TODO: Make map subsequence
  -- MapBatch     :: (Arrays a, Arrays b, Arrays c, Arrays s)
  --              => PreOpenAfun acc aenv (s -> a -> b)
  --              -> PreOpenAfun acc aenv (s -> Regular b -> (s, Regular c))
  --              -> PreOpenAfun acc aenv (s -> Irregular b -> (s, Irregular c))
  --              -> acc aenv s
  --              -> acc aenv a
  --              -> Producer index acc aenv (s,c)

  -- Generate a sequence with some accumulator.
  --
  -- Generated by vectorisation. Not present in prior stages.
  ProduceAccum :: (Arrays a, Arrays b, Elt index)
               => Maybe (PreExp acc aenv Int)
               -> PreOpenAfun acc aenv (Scalar index -> b -> (a, b))
               -> acc aenv b
               -> Producer index acc aenv a

-- | A sequence consumer.
--
data Consumer index acc aenv a where

  -- A batched fold
  --
  -- Converted to ProduceAccum and Last by vectorisation.
  --
  FoldBatch      ::(Arrays a, Arrays s)
                 => PreOpenAfun acc aenv (s -> a -> s)
                 -> acc aenv s
                 -> acc aenv a
                 -> Consumer index acc aenv s

  -- Simply yield a result.
  --
  -- Exists through all stages of the pipeline.
  --
  Last           :: Arrays a
                 => acc aenv a                 -- Yields current value of sequence
                 -> acc aenv a                 -- The default value if any producers are empty
                 -> Consumer index acc aenv a

  -- Build a tuple of sequences.
  --
  -- Exists throughout all stages of the pipeline.
  --
  Stuple         :: (Arrays a, IsAtuple a)
                 => Atuple (PreOpenSeq index acc aenv) (TupleRepr a)
                 -> Consumer index acc aenv a

  -- Concatenate all elements of subarrays into one large vector.
  --
  -- Removed by vectorisation.
  --
  Elements       :: (Shape sh, Elt e)
                 => acc aenv (Array sh e)
                 -> Consumer index acc aenv (Vector e)

  -- Join all arrays along the outermost dimension. This has intersection
  -- semantics.
  --
  -- Removed by vectorisation.
  --
  Tabulate       :: (Shape sh, Elt e)
                 => acc aenv (Array sh e)
                 -> Consumer index acc aenv (Array (sh:.Int) e)

deriving instance Typeable PreOpenSeq

-- |A natural sequence computation is one where elements are computed one at a
-- time.
--
type PreOpenNaturalSeq = PreOpenSeq Int

-- |In a chunked sequence computation, elements are computed in chunks.
--
type PreOpenChunkedSeq = PreOpenSeq (Int, Int)

type NaturalProducer = Producer Int
type ChunkedProducer = Producer (Int, Int)
type NaturalConsumer = Consumer Int
type ChunkedConsumer = Consumer (Int, Int)

type OpenNaturalSeq = PreOpenNaturalSeq OpenAcc
type OpenChunkedSeq = PreOpenChunkedSeq OpenAcc

type OpenSeq index = PreOpenSeq index OpenAcc

-- |Closed sequence computations
--
type Seq index  = OpenSeq index ()

-- Sequence indexing
-- -----------------

class Elt index => SeqIndex index where
  initialIndex :: PreExp acc aenv Int -> PreExp acc aenv index
  limit        :: PreExp acc aenv index -> PreExp acc aenv Int -> PreExp acc aenv index
  contains     :: PreExp acc aenv Int -> PreExp acc aenv index -> PreExp acc aenv Bool
  contains'    :: index -> Int -> Bool
  nextIndex    :: index -> index
  modifySize   :: (Int -> Int) -> index -> index
  indexSize    :: index -> Int

instance SeqIndex Int where
  initialIndex _ = Const 0
  limit    _ l = l
  contains l i = PrimApp (PrimLt scalarType) (Tuple (NilTup `SnocTup` l `SnocTup` i))
  contains' = (<)
  nextIndex = (+1)
  modifySize = ($)
  indexSize _ = 1

instance SeqIndex (Int, Int) where
  initialIndex n = tuple (Const 0) n
  limit ix l
    = let
        i = Prj (SuccTupIdx ZeroTupIdx) ix
        n = Prj ZeroTupIdx ix
        j  = PrimApp (PrimAdd numType) (tuple i n)
      in Cond (PrimApp (PrimLt scalarType) (tuple j l))
              ix
              (tuple i (PrimApp (PrimSub numType) (tuple l i)))
  contains l i = PrimApp (PrimLt scalarType) (tuple (Prj (SuccTupIdx ZeroTupIdx) i) l)
  contains' (i,_) = (i <)
  nextIndex (i,n) = (i+n,n)
  modifySize = fmap
  indexSize = snd

-- |Operations on stencils.
--
class (Shape sh, Elt e, IsTuple stencil, Elt stencil) => Stencil sh e stencil where
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
  Foreign       :: (Foreign asm, Elt x, Elt y)
                => asm           (x -> y)
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

  IndexTrans    :: Shape sl
                => PreOpenExp acc env aenv sl
                -> PreOpenExp acc env aenv sl

  IndexAny      :: Shape sh
                => PreOpenExp acc env aenv (Any sh)

  IndexSlice    :: (Shape sh, Shape sl, Slice slix)
                => SliceIndex (EltRepr slix) (EltRepr sl) co (EltRepr sh)
                -> proxy slix
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

  ToSlice       :: Slice slix
                => SliceIndex (EltRepr slix) sl co (EltRepr (FullShape slix))
                -> PreOpenExp acc env aenv (FullShape slix)
                -> PreOpenExp acc env aenv Int
                -> PreOpenExp acc env aenv slix

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

  -- Union of two shapes
  Union         :: Shape dim
                => PreOpenExp acc env aenv dim
                -> PreOpenExp acc env aenv dim
                -> PreOpenExp acc env aenv dim


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
  PrimIsNaN :: FloatingType a -> PrimFun (a -> Bool)
  PrimAtan2 :: FloatingType a -> PrimFun ((a, a) -> a)
  -- PrimFloatRadix     :: FloatingType a -> PrimFun (a -> Int)         -- Integer?
  -- PrimFloatDigits    :: FloatingType a -> PrimFun (a -> Int)
  -- PrimFloatRange     :: FloatingType a -> PrimFun (a -> (Int, Int))
  -- PrimDecodeFloat    :: FloatingType a -> PrimFun (a -> (Int, Int))  -- Integer?
  -- PrimEncodeFloat    :: FloatingType a -> PrimFun ((Int, Int) -> a)  -- Integer?
  -- PrimExponent       :: FloatingType a -> PrimFun (a -> Int)
  -- PrimSignificand    :: FloatingType a -> PrimFun (a -> a)
  -- PrimScaleFloat     :: FloatingType a -> PrimFun ((Int, a) -> a)
  -- PrimIsInfinite     :: FloatingType a -> PrimFun (a -> Bool)
  -- PrimIsDenormalized :: FloatingType a -> PrimFun (a -> Bool)
  -- PrimIsNegativeZero :: FloatingType a -> PrimFun (a -> Bool)
  -- PrimIsIEEE         :: FloatingType a -> PrimFun (a -> Bool)

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
  -- FIXME: use IntegralType?
  PrimOrd  :: PrimFun (Char -> Int)
  PrimChr  :: PrimFun (Int  -> Char)

  -- boolean conversion
  PrimBoolToInt :: PrimFun (Bool -> Int)

  -- general conversion between types
  PrimFromIntegral :: IntegralType a -> NumType b -> PrimFun (a -> b)
  PrimToFloating   :: NumType a -> FloatingType b -> PrimFun (a -> b)

  -- reinterpret the bits of a value as a different type
  -- (the two types must have the same bit size)
  PrimCoerce :: ScalarType a -> ScalarType b -> PrimFun (a -> b)

  -- FIXME: Conversions between various integer types: should we have overloaded
  -- functions like 'toInt'? (or 'fromEnum' for enums?)

  -- FIXME: What do we want to do about Enum? 'succ' and 'pred' are only
  -- moderately useful without user-defined enumerations, but we want the range
  -- constructs for arrays (but that's not scalar primitives)


-- NFData instances
-- ================

instance NFData (OpenAfun aenv f) where
  rnf = rnfOpenAfun

instance NFData (OpenAcc aenv t) where
  rnf = rnfOpenAcc

instance NFData (OpenSeq index aenv t) where
  rnf = rnfPreOpenSeq rnfOpenAcc

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
rnfPreOpenAfun rnfA (Alam f)  = rnfPreOpenAfun rnfA f

rnfPreOpenAcc :: forall acc aenv t. NFDataAcc acc -> PreOpenAcc acc aenv t -> ()
rnfPreOpenAcc rnfA pacc =
  let
      rnfAF :: PreOpenAfun acc aenv' t' -> ()
      rnfAF = rnfPreOpenAfun rnfA

      rnfE :: PreOpenExp acc env' aenv' t' -> ()
      rnfE = rnfPreOpenExp rnfA

      rnfF :: PreOpenFun acc env' aenv' t' -> ()
      rnfF = rnfPreOpenFun rnfA

      rnfS :: PreOpenSeq index acc aenv' t' -> ()
      rnfS = rnfPreOpenSeq rnfA

      rnfL :: Maybe (PreOpenExp acc env' aenv' t') -> ()
      rnfL (Just e) = rnfE e
      rnfL Nothing  = ()

      rnfB :: forall aenv' sh e. Elt e => acc aenv' (Array sh e) -> Boundary (EltRepr e) -> ()
      rnfB _ = rnfBoundary (eltType (undefined::e))
  in
  case pacc of
    Alet bnd body             -> rnfA bnd `seq` rnfA body
    Avar ix                   -> rnfIdx ix
    Atuple atup               -> rnfAtuple rnfA atup
    Aprj tix a                -> rnfTupleIdx tix `seq` rnfA a
    Apply afun acc            -> rnfAF afun `seq` rnfA acc
    Aforeign asm afun a       -> rnf (strForeign asm) `seq` rnfAF afun `seq` rnfA a
    Acond p a1 a2             -> rnfE p `seq` rnfA a1 `seq` rnfA a2
    Awhile p f a              -> rnfAF p `seq` rnfAF f `seq` rnfA a
    Use arrs                  -> rnfArrays (arrays (undefined::t)) arrs
    Subarray sh ix arrs       -> rnfE sh `seq` rnfE ix `seq` rnfArrays (arrays (undefined::t)) arrs
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
    Stencil f b a             -> rnfF f `seq` rnfB a b `seq` rnfA a
    Stencil2 f b1 a1 b2 a2    -> rnfF f `seq` rnfB a1 b1 `seq` rnfB a2 b2 `seq` rnfA a1 `seq` rnfA a2
    Collect min max i s       -> rnfE min `seq` rnfL max `seq` rnfL i `seq` rnfS s


rnfAtuple :: NFDataAcc acc -> Atuple (acc aenv) t -> ()
rnfAtuple _    NilAtup          = ()
rnfAtuple rnfA (SnocAtup tup a) = rnfAtuple rnfA tup `seq` rnfA a

rnfArrays :: ArraysR arrs -> arrs -> ()
rnfArrays ArraysRunit           ()      = ()
rnfArrays ArraysRarray          arr     = rnf arr
rnfArrays (ArraysRpair ar1 ar2) (a1,a2) = rnfArrays ar1 a1 `seq` rnfArrays ar2 a2

rnfBoundary :: TupleType t -> Boundary t -> ()
rnfBoundary _ Clamp        = ()
rnfBoundary _ Mirror       = ()
rnfBoundary _ Wrap         = ()
rnfBoundary t (Constant c) = rnfConst t c


-- Sequence expressions
-- --------------------

rnfPreOpenSeq :: forall index acc aenv t. NFDataAcc acc -> PreOpenSeq index acc aenv t -> ()
rnfPreOpenSeq rnfA topSeq =
  let
      rnfS :: PreOpenSeq index acc aenv' t' -> ()
      rnfS = rnfPreOpenSeq rnfA

      rnfP :: Producer index acc aenv' t' -> ()
      rnfP = rnfSeqProducer rnfA

      rnfC :: Consumer index acc aenv' t' -> ()
      rnfC = rnfSeqConsumer rnfA

      rnfLiftedType :: LiftedType a a' -> ()
      rnfLiftedType UnitT       = ()
      rnfLiftedType LiftedUnitT = ()
      rnfLiftedType AvoidedT    = ()
      rnfLiftedType RegularT    = ()
      rnfLiftedType IrregularT  = ()
      rnfLiftedType (TupleT t)  = rnfLiftedTupleType t

      rnfLiftedTupleType :: LiftedTupleType tup tup' -> ()
      rnfLiftedTupleType NilLtup        = ()
      rnfLiftedTupleType (SnocLtup t a) = rnfLiftedTupleType t `seq` rnfLiftedType a
  in
  case topSeq of
    Producer p s              -> rnfP p `seq` rnfS s
    Consumer c                -> rnfC c
    Reify ty ix               -> rnfLiftedType ty `seq` rnfA ix

rnfSeqProducer :: forall index acc aenv t. NFDataAcc acc -> Producer index acc aenv t -> ()
rnfSeqProducer rnfA topSeq =
  let
      -- RCE: Should probably reconsider this. If we're streaming in from a list
      -- we don't want to force the entire list when we force the AST.
      -- rnfArrs :: forall a. Arrays a => [a] -> ()
      -- rnfArrs []     = ()
      -- rnfArrs (a:as) = rnfArrays (arrays (undefined::a)) (fromArr a) `seq` rnfArrs as

      rnfSource :: forall a. Arrays a => Source a -> ()
      rnfSource (RegularList sh _) = rnfConst (eltType sh) (fromElt sh)
      rnfSource (List _)           = ()
      rnfSource (Function _ _)     = ()

      rnfAF :: PreOpenAfun acc aenv' t' -> ()
      rnfAF = rnfPreOpenAfun rnfA

      rnfL :: Maybe (PreOpenExp acc env' aenv' t') -> ()
      rnfL (Just e) = rnfE e
      rnfL Nothing  = ()

      rnfE :: PreOpenExp acc env' aenv' t' -> ()
      rnfE = rnfPreOpenExp rnfA
  in
  case topSeq of
    Pull as            -> rnfSource as
    Subarrays sh a     -> rnfE sh `seq` rnfArrays (arrays a) (fromArr a)
    FromSegs s n v     -> rnfA s `seq` rnfE n `seq` rnfA v
    Produce l f        -> rnfL l `seq` rnfAF f
    ProduceAccum l f a -> rnfL l `seq` rnfAF f `seq` rnfA a

rnfSeqConsumer :: forall index acc aenv t. NFDataAcc acc -> Consumer index acc aenv t -> ()
rnfSeqConsumer rnfA topSeq =
  let
      rnfAF :: PreOpenAfun acc aenv' t' -> ()
      rnfAF = rnfPreOpenAfun rnfA
  in
  case topSeq of
    FoldBatch f a x -> rnfAF f `seq` rnfA a `seq` rnfA x
    Last a d        -> rnfA a `seq` rnfA d
    Stuple stup     -> rnfStuple rnfA stup
    Elements a      -> rnfA a
    Tabulate a      -> rnfA a

rnfStuple :: NFDataAcc acc -> Atuple (PreOpenSeq index acc aenv) t -> ()
rnfStuple _    NilAtup          = ()
rnfStuple rnfA (SnocAtup tup c) = rnfStuple rnfA tup `seq` rnfPreOpenSeq rnfA c


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
    Const t                   -> rnfConst (eltType (undefined::t)) t
    Tuple t                   -> rnfTuple rnfA t
    Prj ix e                  -> rnfTupleIdx ix `seq` rnfE e
    IndexNil                  -> ()
    IndexCons sh sz           -> rnfE sh `seq` rnfE sz
    IndexHead sh              -> rnfE sh
    IndexTail sh              -> rnfE sh
    IndexAny                  -> ()
    IndexSlice slice _ sh     -> rnfSliceIndex slice `seq` rnfE sh
    IndexFull slice slix sl   -> rnfSliceIndex slice `seq` rnfE slix `seq` rnfE sl
    ToIndex sh ix             -> rnfE sh `seq` rnfE ix
    FromIndex sh ix           -> rnfE sh `seq` rnfE ix
    IndexTrans sh             -> rnfE sh
    ToSlice slice slix i      -> rnfSliceIndex slice `seq` rnfE slix `seq` rnfE i
    Cond p e1 e2              -> rnfE p `seq` rnfE e1 `seq` rnfE e2
    While p f x               -> rnfF p `seq` rnfF f `seq` rnfE x
    PrimConst c               -> rnfPrimConst c
    PrimApp f x               -> rnfPrimFun f `seq` rnfE x
    Index a ix                -> rnfA a `seq` rnfE ix
    LinearIndex a ix          -> rnfA a `seq` rnfE ix
    Shape a                   -> rnfA a
    ShapeSize sh              -> rnfE sh
    Intersect sh1 sh2         -> rnfE sh1 `seq` rnfE sh2
    Union sh1 sh2             -> rnfE sh1 `seq` rnfE sh2

rnfTuple :: NFDataAcc acc -> Tuple (PreOpenExp acc env aenv) t -> ()
rnfTuple _    NilTup        = ()
rnfTuple rnfA (SnocTup t e) = rnfTuple rnfA t `seq` rnfPreOpenExp rnfA e

rnfConst :: TupleType t -> t -> ()
rnfConst UnitTuple          ()    = ()
rnfConst (SingleTuple t)    !_    = rnfScalarType t  -- scalars should have (nf == whnf)
rnfConst (PairTuple ta tb)  (a,b) = rnfConst ta a `seq` rnfConst tb b

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
rnfPrimFun (PrimAtan2 t)              = rnfFloatingType t
rnfPrimFun (PrimLt t)                 = rnfScalarType t
rnfPrimFun (PrimGt t)                 = rnfScalarType t
rnfPrimFun (PrimLtEq t)               = rnfScalarType t
rnfPrimFun (PrimGtEq t)               = rnfScalarType t
rnfPrimFun (PrimEq t)                 = rnfScalarType t
rnfPrimFun (PrimNEq t)                = rnfScalarType t
rnfPrimFun (PrimMax t)                = rnfScalarType t
rnfPrimFun (PrimMin t)                = rnfScalarType t
rnfPrimFun PrimLAnd                   = ()
rnfPrimFun PrimLOr                    = ()
rnfPrimFun PrimLNot                   = ()
rnfPrimFun PrimOrd                    = ()
rnfPrimFun PrimChr                    = ()
rnfPrimFun PrimBoolToInt              = ()
rnfPrimFun (PrimFromIntegral i n)     = rnfIntegralType i `seq` rnfNumType n
rnfPrimFun (PrimToFloating n f)       = rnfNumType n `seq` rnfFloatingType f
rnfPrimFun (PrimCoerce a b)           = rnfScalarType a `seq` rnfScalarType b

rnfSliceIndex :: SliceIndex ix slice co sh -> ()
rnfSliceIndex SliceNil        = ()
rnfSliceIndex (SliceAll sh)   = rnfSliceIndex sh
rnfSliceIndex (SliceFixed sh) = rnfSliceIndex sh

rnfScalarType :: ScalarType t -> ()
rnfScalarType (NumScalarType t)    = rnfNumType t
rnfScalarType (NonNumScalarType t) = rnfNonNumType t

rnfBoundedType :: BoundedType t -> ()
rnfBoundedType (IntegralBoundedType t) = rnfIntegralType t
rnfBoundedType (NonNumBoundedType t)   = rnfNonNumType t

rnfNumType :: NumType t -> ()
rnfNumType (IntegralNumType t) = rnfIntegralType t
rnfNumType (FloatingNumType t) = rnfFloatingType t

rnfNonNumType :: NonNumType t -> ()
rnfNonNumType (TypeBool   NonNumDict) = ()
rnfNonNumType (TypeChar   NonNumDict) = ()
rnfNonNumType (TypeCChar  NonNumDict) = ()
rnfNonNumType (TypeCSChar NonNumDict) = ()
rnfNonNumType (TypeCUChar NonNumDict) = ()

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
rnfIntegralType (TypeCShort  IntegralDict) = ()
rnfIntegralType (TypeCUShort IntegralDict) = ()
rnfIntegralType (TypeCInt    IntegralDict) = ()
rnfIntegralType (TypeCUInt   IntegralDict) = ()
rnfIntegralType (TypeCLong   IntegralDict) = ()
rnfIntegralType (TypeCULong  IntegralDict) = ()
rnfIntegralType (TypeCLLong  IntegralDict) = ()
rnfIntegralType (TypeCULLong IntegralDict) = ()

rnfFloatingType :: FloatingType t -> ()
rnfFloatingType (TypeFloat   FloatingDict) = ()
rnfFloatingType (TypeDouble  FloatingDict) = ()
rnfFloatingType (TypeCFloat  FloatingDict) = ()
rnfFloatingType (TypeCDouble FloatingDict) = ()

-- Utility
-- -------

tuple :: (Elt a, Elt b) => PreExp acc aenv a -> PreExp acc aenv b -> PreExp acc aenv (a,b)
tuple a b = Tuple (NilTup `SnocTup` a `SnocTup` b)


-- Debugging
-- ---------

showPreAccOp :: forall acc aenv arrs. PreOpenAcc acc aenv arrs -> String
showPreAccOp Alet{}             = "Alet"
showPreAccOp (Avar ix)          = "Avar a" ++ show (idxToInt ix)
showPreAccOp (Use a)            = "Use "  ++ showArrays (toArr a :: arrs)
showPreAccOp Subarray{}         = "Subarray"
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
showPreAccOp Collect{}          = "Collect"

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
showPreExpOp IndexTrans{}       = "IndexTrans"
showPreExpOp IndexAny           = "IndexAny"
showPreExpOp IndexSlice{}       = "IndexSlice"
showPreExpOp IndexFull{}        = "IndexFull"
showPreExpOp ToIndex{}          = "ToIndex"
showPreExpOp FromIndex{}        = "FromIndex"
showPreExpOp ToSlice{}          = "ToSlice"
showPreExpOp Cond{}             = "Cond"
showPreExpOp While{}            = "While"
showPreExpOp PrimConst{}        = "PrimConst"
showPreExpOp PrimApp{}          = "PrimApp"
showPreExpOp Index{}            = "Index"
showPreExpOp LinearIndex{}      = "LinearIndex"
showPreExpOp Shape{}            = "Shape"
showPreExpOp ShapeSize{}        = "ShapeSize"
showPreExpOp Intersect{}        = "Intersect"
showPreExpOp Union{}            = "Union"
