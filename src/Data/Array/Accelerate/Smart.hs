{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Smart
-- Copyright   : [2008..2019] The Accelerate Team
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
  Acc(..), SmartAcc(..), PreSmartAcc(..), PairIdx(..), Exp(..), SmartExp(..), PreSmartExp(..),
  Boundary(..), PreBoundary(..), Stencil(..), Level, Direction(..),

  -- * Smart constructors for literals
  constant, undef,

  -- * Smart destructors for shapes
  indexHead, indexTail,

  -- * Smart constructors for constants
  mkMinBound, mkMaxBound, mkPi,
  mkSin, mkCos, mkTan,
  mkAsin, mkAcos, mkAtan,
  mkSinh, mkCosh, mkTanh,
  mkAsinh, mkAcosh, mkAtanh,
  mkExpFloating, mkSqrt, mkLog,
  mkFPow, mkLogBase,
  mkTruncate, mkRound, mkFloor, mkCeiling,
  mkAtan2,

  -- * Smart constructors for primitive functions
  mkAdd, mkSub, mkMul, mkNeg, mkAbs, mkSig, mkQuot, mkRem, mkQuotRem, mkIDiv, mkMod, mkDivMod,
  mkBAnd, mkBOr, mkBXor, mkBNot, mkBShiftL, mkBShiftR, mkBRotateL, mkBRotateR, mkPopCount, mkCountLeadingZeros, mkCountTrailingZeros,
  mkFDiv, mkRecip, mkLt, mkGt, mkLtEq, mkGtEq, mkEq, mkNEq, mkMax, mkMin,
  mkLAnd, mkLOr, mkLNot, mkIsNaN, mkIsInfinite,

  -- * Smart constructors for type coercion functions
  mkOrd, mkChr, mkBoolToInt, mkFromIntegral, mkToFloating, mkBitcast, mkCoerce, Coerce,

  -- * Auxiliary functions
  ($$), ($$$), ($$$$), ($$$$$),
  ApplyAcc(..), HasExpType(..), HasArraysRepr(..),
  unAcc, unAccFunction, mkExp, unExp, unExpFunction, unExpBinaryFunction, unPair, mkPairToTuple,

  -- Debugging
  showPreAccOp, showPreExpOp,

) where

-- standard library
import Prelude                                     hiding ( exp )
import Data.Kind

-- friends
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Sugar           (Elt, Arrays, EltRepr, ArrRepr, (:.), Foreign, eltType, fromElt, DIM1)
import qualified Data.Array.Accelerate.Array.Sugar as Sugar
import Data.Array.Accelerate.Array.Representation  hiding (DIM1)
import Data.Array.Accelerate.AST                   hiding ( PreOpenAcc(..), OpenAcc(..), Acc
                                                          , OpenExp(..), Exp
                                                          , Boundary(..), HasArraysRepr(..), arrayRepr, expType
                                                          , showPreAccOp, showPreExpOp )
import GHC.TypeNats

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
-- >   fold (+) 0 ( zipWith (*) xs' ys' )
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
-- > dotp xs ys = fold (+) 0 ( zipWith (*) xs ys )
--
-- We might then be inclined to lift our dot-product program to the following
-- (incorrect) matrix-vector product, by applying @dotp@ to each row of the
-- input matrix:
--
-- > mvm_ndp :: Num a => Acc (Matrix a) -> Acc (Vector a) -> Acc (Vector a)
-- > mvm_ndp mat vec =
-- >   let Z :. rows :. cols  = unlift (shape mat)  :: Z :. Exp Int :. Exp Int
-- >   in  generate (index1 rows)
-- >                (\row -> the $ dotp vec (slice mat (lift (row :. All))))
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
-- > mvm :: A.Num a => Acc (Matrix a) -> Acc (Vector a) -> Acc (Vector a)
-- > mvm mat vec =
-- >   let Z :. rows :. cols = unlift (shape mat) :: Z :. Exp Int :. Exp Int
-- >       vec'              = A.replicate (lift (Z :. rows :. All)) vec
-- >   in
-- >   A.fold (+) 0 ( A.zipWith (*) mat vec' )
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
newtype Acc a = Acc (SmartAcc (ArrRepr a))

newtype SmartAcc a = SmartAcc (PreSmartAcc SmartAcc SmartExp a)


-- The level of lambda-bound variables. The root has level 0; then it increases with each bound
-- variable — i.e., it is the same as the size of the environment at the defining occurrence.
--
type Level = Int

-- | Array-valued collective computations without a recursive knot
--
data PreSmartAcc acc exp as where
    -- Needed for conversion to de Bruijn form
  Atag          :: ArraysR as
                -> Level                        -- environment size at defining occurrence
                -> PreSmartAcc acc exp as

  Pipe          :: ArraysR as
                -> ArraysR bs
                -> ArraysR cs
                -> (SmartAcc as -> acc bs)
                -> (SmartAcc bs -> acc cs)
                -> acc as
                -> PreSmartAcc acc exp cs

  Aforeign      :: Foreign asm
                => ArraysR bs
                -> asm (as -> bs)
                -> (SmartAcc as -> SmartAcc bs)
                -> acc as
                -> PreSmartAcc acc exp bs

  Acond         :: exp Bool
                -> acc as
                -> acc as
                -> PreSmartAcc acc exp as

  Awhile        :: ArraysR arrs
                -> (SmartAcc arrs -> acc (Scalar Bool))
                -> (SmartAcc arrs -> acc arrs)
                -> acc arrs
                -> PreSmartAcc acc exp arrs

  Anil          :: PreSmartAcc acc exp ()

  Apair         :: acc arrs1
                -> acc arrs2
                -> PreSmartAcc acc exp (arrs1, arrs2)

  Aprj          :: PairIdx (arrs1, arrs2) arrs
                -> acc (arrs1, arrs2)
                -> PreSmartAcc acc exp arrs

  Use           :: ArrayR (Array sh e)
                -> Array sh e
                -> PreSmartAcc acc exp (Array sh e)

  Unit          :: TupleType e
                -> exp e
                -> PreSmartAcc acc exp (Scalar e)

  Generate      :: ArrayR (Array sh e)
                -> exp sh
                -> (SmartExp sh -> exp e)
                -> PreSmartAcc acc exp (Array sh e)

  Reshape       :: ShapeR sh
                -> exp sh
                -> acc (Array sh' e)
                -> PreSmartAcc acc exp (Array sh e)

  Replicate     :: SliceIndex slix sl co sh
                -> exp slix
                -> acc                 (Array sl e)
                -> PreSmartAcc acc exp (Array sh e)

  Slice         :: SliceIndex slix sl co sh
                -> acc                 (Array sh e)
                -> exp slix
                -> PreSmartAcc acc exp (Array sl e)

  Map           :: TupleType e
                -> TupleType e'
                -> (SmartExp e -> exp e')
                -> acc (Array sh e)
                -> PreSmartAcc acc exp (Array sh e')

  ZipWith       :: TupleType e1
                -> TupleType e2
                -> TupleType e3
                -> (SmartExp e1 -> SmartExp e2 -> exp e3)
                -> acc (Array sh e1)
                -> acc (Array sh e2)
                -> PreSmartAcc acc exp (Array sh e3)

  Fold          :: TupleType e
                -> (SmartExp e -> SmartExp e -> exp e)
                -> Maybe (exp e)
                -> acc (Array (sh, Int) e)
                -> PreSmartAcc acc exp (Array sh e)

  FoldSeg       :: IntegralType i
                -> TupleType e
                -> (SmartExp e -> SmartExp e -> exp e)
                -> Maybe (exp e)
                -> acc (Array (sh, Int) e)
                -> acc (Segments i)
                -> PreSmartAcc acc exp (Array (sh, Int) e)

  Scan          :: Direction
                -> TupleType e
                -> (SmartExp e -> SmartExp e -> exp e)
                -> Maybe (exp e)
                -> acc (Array (sh, Int) e)
                -> PreSmartAcc acc exp (Array (sh, Int) e)

  Scan'         :: Direction
                -> TupleType e
                -> (SmartExp e -> SmartExp e -> exp e)
                -> exp e
                -> acc (Array (sh, Int) e)
                -> PreSmartAcc acc exp (Array (sh, Int) e, Array sh e)

  Permute       :: ArrayR (Array sh e)
                -> (SmartExp e -> SmartExp e -> exp e)
                -> acc (Array sh' e)
                -> (SmartExp sh -> exp sh')
                -> acc (Array sh e)
                -> PreSmartAcc acc exp (Array sh' e)

  Backpermute   :: ShapeR sh'
                -> exp sh'
                -> (SmartExp sh' -> exp sh)
                -> acc (Array sh e)
                -> PreSmartAcc acc exp (Array sh' e)

  Stencil       :: StencilR sh a stencil
                -> TupleType b
                -> (SmartExp stencil -> exp b)
                -> PreBoundary acc exp (Array sh a)
                -> acc (Array sh a)
                -> PreSmartAcc acc exp (Array sh b)

  Stencil2      :: StencilR sh a stencil1
                -> StencilR sh b stencil2
                -> TupleType c
                -> (SmartExp stencil1 -> SmartExp stencil2 -> exp c)
                -> PreBoundary acc exp (Array sh a)
                -> acc (Array sh a)
                -> PreBoundary acc exp (Array sh b)
                -> acc (Array sh b)
                -> PreSmartAcc acc exp (Array sh c)

  -- Collect       :: Arrays arrs
  --               => seq arrs
  --               -> PreSmartAcc acc seq exp arrs

class HasArraysRepr f where
  arraysRepr :: f a -> ArraysR a

arrayRepr :: HasArraysRepr f => f (Array sh e) -> ArrayR (Array sh e)
arrayRepr acc = case arraysRepr acc of
  TupRsingle repr -> repr

instance HasArraysRepr acc => HasArraysRepr (PreSmartAcc acc exp) where
  arraysRepr acc = case acc of
    Atag repr _               -> repr
    Pipe _ _ repr  _ _ _      -> repr
    Aforeign repr _ _ _       -> repr
    Acond _ a _               -> arraysRepr a
    Awhile _ _ _ a            -> arraysRepr a
    Anil                      -> TupRunit
    Apair a1 a2               -> arraysRepr a1 `TupRpair` arraysRepr a2
    Aprj idx a | TupRpair t1 t2 <- arraysRepr a
                              -> case idx of
                                   PairIdxLeft  -> t1
                                   PairIdxRight -> t2
    Aprj _ _                  -> error "Ejector seat? You're joking!"
    Use repr _                -> TupRsingle repr
    Unit tp _                 -> TupRsingle $ ArrayR ShapeRz $ tp
    Generate repr _ _         -> TupRsingle repr
    Reshape shr _ a           -> let ArrayR _ tp = arrayRepr a
                                 in  TupRsingle $ ArrayR shr tp
    Replicate si _ a          -> let ArrayR _ tp = arrayRepr a
                                 in  TupRsingle $ ArrayR (sliceDomainR si) tp
    Slice si a _              -> let ArrayR _ tp = arrayRepr a
                                 in  TupRsingle $ ArrayR (sliceShapeR si) tp
    Map _ tp _ a              -> let ArrayR shr _ = arrayRepr a
                                 in  TupRsingle $ ArrayR shr tp
    ZipWith _ _ tp _ a _      -> let ArrayR shr _ = arrayRepr a
                                 in  TupRsingle $ ArrayR shr tp
    Fold _ _ _ a              -> let ArrayR (ShapeRsnoc shr) tp = arrayRepr a
                                 in  TupRsingle (ArrayR shr tp)
    FoldSeg _ _ _ _ a _       -> arraysRepr a
    Scan _ _ _ _ a            -> arraysRepr a
    Scan' _ _ _ _ a           -> let repr@(ArrayR (ShapeRsnoc shr) tp) = arrayRepr a
                                 in  TupRsingle repr `TupRpair` TupRsingle (ArrayR shr tp)
    Permute _ _ a _ _         -> arraysRepr a
    Backpermute shr _ _ a     -> let ArrayR _ tp = arrayRepr a
                                 in  TupRsingle (ArrayR shr tp)
    Stencil s tp _ _ _        -> TupRsingle $ ArrayR (stencilShape s) tp
    Stencil2 s _ tp _ _ _ _ _ -> TupRsingle $ ArrayR (stencilShape s) tp

instance HasArraysRepr SmartAcc where
  arraysRepr (SmartAcc e) = arraysRepr e


{--
data PreSeq acc seq exp arrs where
  -- Convert the given Haskell-list of arrays to a sequence.
  StreamIn :: Arrays a
           => [a]
           -> PreSeq acc seq exp [a]

  -- Convert the given array to a sequence.
  -- Example:
  -- slix = Z :. All :. Split :. All :. All :. Split
  --              ^       ^       ^      ^      ^
  --              |        \     /      /       |
  --              |         \___/______/_______ Iteration space.
  --              |            /      /
  --           Element________/______/
  --            shape.
  --
  ToSeq :: ( Elt e
           , Slice slix
           , Division slsix
           , DivisionSlice slsix ~ slix
           , Typeable (FullShape slix)
           , Typeable (SliceShape slix)
           )
        => slsix
        -> acc (Array (FullShape slix) e)
        -> PreSeq acc seq exp [Array (SliceShape slix) e]

  -- Apply the given the given function to all elements of the given sequence.
  MapSeq :: (Arrays a, Arrays b)
         => (Acc a -> acc b)
         -> seq [a]
         -> PreSeq acc seq exp [b]

  -- Apply a given binary function pairwise to all elements of the given sequences.
  -- The length of the result is the length of the shorter of the two argument
  -- arrays.
  ZipWithSeq :: (Arrays a, Arrays b, Arrays c)
             => (Acc a -> Acc b -> acc c)
             -> seq [a]
             -> seq [b]
             -> PreSeq acc seq exp [c]

  -- ScanSeq (+) a0 x. Scan a sequence x by combining each element
  -- using the given binary operation (+). (+) must be associative:
  --
  --   Forall a b c. (a + b) + c = a + (b + c),
  --
  -- and a0 must be the identity element for (+):
  --
  --   Forall a. a0 + a = a = a + a0.
  --
  ScanSeq :: Elt a
          => (Exp a -> Exp a -> exp a)
          -> exp a
          -> seq [Scalar a]
          -> PreSeq acc seq exp [Scalar a]

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
          => (Exp a -> Exp a -> exp a)
          -> exp a
          -> seq [Scalar a]
          -> PreSeq acc seq exp (Scalar a)

  -- FoldSeqFlatten f a0 x. A specialized version of FoldSeqAct
  -- where reduction with the companion operator corresponds to
  -- flattening. f must be semi-associative, with vecotor append (++)
  -- as the companion operator:
  --
  --   Forall b s1 a2 sh2 a2.
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
                 => (Acc a -> Acc (Vector sh) -> Acc (Vector e) -> acc a)
                 -> acc a
                 -> seq [Array sh e]
                 -> PreSeq acc seq exp a

  -- Tuple up the results of a sequence computation. Note that the Arrays
  -- constraint requires that the elements of the tuple are Arrays, not
  -- streams ([]).
  Stuple :: (Arrays arrs, IsAtuple arrs)
         => Atuple (seq) (TupleRepr arrs)
         -> PreSeq acc seq exp arrs

-- |Array-valued sequence computations
--
newtype Seq a = Seq (PreSeq Acc Seq Exp a)

deriving instance Typeable Seq
--}


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
newtype Exp t = Exp (SmartExp (EltRepr t))
newtype SmartExp t = SmartExp (PreSmartExp SmartAcc SmartExp t)

-- | Scalar expressions to parametrise collective array operations, themselves parameterised over
-- the type of collective array operations.
--
data PreSmartExp acc exp t where
    -- Needed for conversion to de Bruijn form
  Tag           :: TupleType t
                -> Level                        -- environment size at defining occurrence
                -> PreSmartExp acc exp t

  -- All the same constructors as 'AST.Exp', plus projection
  Const         :: ScalarType t
                -> t
                -> PreSmartExp acc exp t

  Nil           :: PreSmartExp acc exp ()

  Pair          :: exp t1
                -> exp t2
                -> PreSmartExp acc exp (t1, t2)

  Prj           :: PairIdx (t1, t2) t
                -> exp (t1, t2)
                -> PreSmartExp acc exp t

  -- SIMD vectors
  VecPack       :: KnownNat n
                => VecR n s tup
                -> exp tup
                -> PreSmartExp acc exp (Vec n s)

  VecUnpack     :: KnownNat n
                => VecR n s tup
                -> exp (Vec n s)
                -> PreSmartExp acc exp tup

  ToIndex       :: ShapeR sh
                -> exp sh
                -> exp sh
                -> PreSmartExp acc exp Int

  FromIndex     :: ShapeR sh
                -> exp sh
                -> exp Int
                -> PreSmartExp acc exp sh

  Cond          :: exp Bool
                -> exp t
                -> exp t
                -> PreSmartExp acc exp t

  While         :: TupleType t
                -> (SmartExp t -> exp Bool)
                -> (SmartExp t -> exp t)
                -> exp t
                -> PreSmartExp acc exp t

  PrimConst     :: PrimConst t
                -> PreSmartExp acc exp t

  PrimApp       :: PrimFun (a -> r)
                -> exp a
                -> PreSmartExp acc exp r

  Index         :: TupleType t
                -> acc (Array sh t)
                -> exp sh
                -> PreSmartExp acc exp t

  LinearIndex   :: TupleType t
                -> acc (Array sh t)
                -> exp Int
                -> PreSmartExp acc exp t

  Shape         :: ShapeR sh
                -> acc (Array sh e)
                -> PreSmartExp acc exp sh

  ShapeSize     :: ShapeR sh
                -> exp sh
                -> PreSmartExp acc exp Int

  Foreign       :: Foreign asm
                => TupleType y
                -> asm (x -> y)
                -> (SmartExp x -> SmartExp y) -- RCE: Using SmartExp instead of exp to aid in sharing recovery.
                -> exp x
                -> PreSmartExp acc exp y

  Undef         :: ScalarType t
                -> PreSmartExp acc exp t

  Coerce        :: BitSizeEq a b
                => ScalarType a
                -> ScalarType b
                -> exp a
                -> PreSmartExp acc exp b

class HasExpType f where
  expType :: f t -> TupleType t

instance HasExpType exp => HasExpType (PreSmartExp acc exp) where
  expType expr = case expr of
    Tag tp _                        -> tp
    Const tp _                      -> TupRsingle tp
    Nil                             -> TupRunit
    Pair e1 e2                      -> expType e1 `TupRpair` expType e2
    Prj idx e | TupRpair t1 t2 <- expType e
                                    -> case idx of
                                         PairIdxLeft  -> t1
                                         PairIdxRight -> t2
    Prj _ _                         -> error "I never joke about my work"
    VecPack   vecR _                -> TupRsingle $ VectorScalarType $ vecRvector vecR
    VecUnpack vecR _                -> vecRtuple vecR
    ToIndex _ _ _                   -> TupRsingle $ scalarTypeInt
    FromIndex shr _ _               -> shapeType shr
    Cond _ e _                      -> expType e
    While t _ _ _                   -> t
    PrimConst c                     -> TupRsingle $ SingleScalarType $ primConstType c
    PrimApp f _                     -> snd $ primFunType f
    Index tp _ _                    -> tp
    LinearIndex tp _ _              -> tp
    Shape shr _                     -> shapeType shr
    ShapeSize _ _                   -> TupRsingle $ scalarTypeInt
    Foreign tp _ _ _                -> tp
    Undef tp                        -> TupRsingle tp
    Coerce _ tp _                   -> TupRsingle tp

instance HasExpType SmartExp where
  expType (SmartExp e) = expType e

-- Smart constructors for stencils
-- -------------------------------

-- | Boundary condition specification for stencil operations
--
data Boundary t where
  Boundary  :: !(PreBoundary SmartAcc SmartExp (Array (EltRepr sh) (EltRepr e)))
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
  type StencilRepr sh stencil :: Type

  stencilR :: StencilR (EltRepr sh) (EltRepr e) (StencilRepr sh stencil)
  stencilPrj :: SmartExp (StencilRepr sh stencil) -> stencil

-- DIM1
instance Elt e => Stencil DIM1 e (Exp e, Exp e, Exp e) where
  type StencilRepr DIM1 (Exp e, Exp e, Exp e)
    = EltRepr (e, e, e)
  stencilR = StencilRunit3 @(EltRepr e) $ eltType @e
  stencilPrj s = (Exp $ prj2 s,
                  Exp $ prj1 s,
                  Exp $ prj0 s)

instance Elt e => Stencil DIM1 e (Exp e, Exp e, Exp e, Exp e, Exp e) where
  type StencilRepr DIM1 (Exp e, Exp e, Exp e, Exp e, Exp e)
    = EltRepr (e, e, e, e, e)
  stencilR = StencilRunit5 $ eltType @e
  stencilPrj s = (Exp $ prj4 s,
                  Exp $ prj3 s,
                  Exp $ prj2 s,
                  Exp $ prj1 s,
                  Exp $ prj0 s)

instance Elt e => Stencil DIM1 e (Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e) where
  type StencilRepr DIM1 (Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e)
    = EltRepr (e, e, e, e, e, e, e)
  stencilR = StencilRunit7 $ eltType @e
  stencilPrj s = (Exp $ prj6 s,
                  Exp $ prj5 s,
                  Exp $ prj4 s,
                  Exp $ prj3 s,
                  Exp $ prj2 s,
                  Exp $ prj1 s,
                  Exp $ prj0 s)

instance Elt e => Stencil DIM1 e (Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e)
  where
  type StencilRepr DIM1 (Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e)
    = EltRepr (e, e, e, e, e, e, e, e, e)
  stencilR = StencilRunit9 $ eltType @e
  stencilPrj s = (Exp $ prj8 s,
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
  type StencilRepr (sh:.Int:.Int) (row2, row1, row0)
    = Tup3 (StencilRepr (sh:.Int) row2) (StencilRepr (sh:.Int) row1) (StencilRepr (sh:.Int) row0)
  stencilR = StencilRtup3 (stencilR @(sh:.Int) @a @row2) (stencilR @(sh:.Int) @a @row1) (stencilR @(sh:.Int) @a @row0)
  stencilPrj s = (stencilPrj @(sh:.Int) @a $ prj2 s,
                  stencilPrj @(sh:.Int) @a $ prj1 s,
                  stencilPrj @(sh:.Int) @a $ prj0 s)

instance (Stencil (sh:.Int) a row4,
          Stencil (sh:.Int) a row3,
          Stencil (sh:.Int) a row2,
          Stencil (sh:.Int) a row1,
          Stencil (sh:.Int) a row0) => Stencil (sh:.Int:.Int) a (row4, row3, row2, row1, row0) where
  type StencilRepr (sh:.Int:.Int) (row4, row3, row2, row1, row0)
    = Tup5 (StencilRepr (sh:.Int) row4) (StencilRepr (sh:.Int) row3) (StencilRepr (sh:.Int) row2)
       (StencilRepr (sh:.Int) row1) (StencilRepr (sh:.Int) row0)
  stencilR = StencilRtup5 (stencilR @(sh:.Int) @a @row4) (stencilR @(sh:.Int) @a @row3)
                  (stencilR @(sh:.Int) @a @row2) (stencilR @(sh:.Int) @a @row1) (stencilR @(sh:.Int) @a @row0)
  stencilPrj s = (stencilPrj @(sh:.Int) @a $ prj4 s,
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
  type StencilRepr (sh:.Int:.Int) (row6, row5, row4, row3, row2, row1, row0)
    = Tup7 (StencilRepr (sh:.Int) row6) (StencilRepr (sh:.Int) row5) (StencilRepr (sh:.Int) row4)
       (StencilRepr (sh:.Int) row3) (StencilRepr (sh:.Int) row2) (StencilRepr (sh:.Int) row1)
       (StencilRepr (sh:.Int) row0)
  stencilR = StencilRtup7 (stencilR @(sh:.Int) @a @row6)
                  (stencilR @(sh:.Int) @a @row5) (stencilR @(sh:.Int) @a @row4) (stencilR @(sh:.Int) @a @row3)
                  (stencilR @(sh:.Int) @a @row2) (stencilR @(sh:.Int) @a @row1) (stencilR @(sh:.Int) @a @row0)
  stencilPrj s = (stencilPrj @(sh:.Int) @a $ prj6 s,
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
  type StencilRepr (sh:.Int:.Int) (row8, row7, row6, row5, row4, row3, row2, row1, row0)
    = Tup9 (StencilRepr (sh:.Int) row8) (StencilRepr (sh:.Int) row7) (StencilRepr (sh:.Int) row6)
       (StencilRepr (sh:.Int) row5) (StencilRepr (sh:.Int) row4) (StencilRepr (sh:.Int) row3)
       (StencilRepr (sh:.Int) row2) (StencilRepr (sh:.Int) row1) (StencilRepr (sh:.Int) row0)
  stencilR = StencilRtup9
                  (stencilR @(sh:.Int) @a @row8) (stencilR @(sh:.Int) @a @row7) (stencilR @(sh:.Int) @a @row6)
                  (stencilR @(sh:.Int) @a @row5) (stencilR @(sh:.Int) @a @row4) (stencilR @(sh:.Int) @a @row3)
                  (stencilR @(sh:.Int) @a @row2) (stencilR @(sh:.Int) @a @row1) (stencilR @(sh:.Int) @a @row0)
  stencilPrj s = (stencilPrj @(sh:.Int) @a $ prj8 s,
                  stencilPrj @(sh:.Int) @a $ prj7 s,
                  stencilPrj @(sh:.Int) @a $ prj6 s,
                  stencilPrj @(sh:.Int) @a $ prj5 s,
                  stencilPrj @(sh:.Int) @a $ prj4 s,
                  stencilPrj @(sh:.Int) @a $ prj3 s,
                  stencilPrj @(sh:.Int) @a $ prj2 s,
                  stencilPrj @(sh:.Int) @a $ prj1 s,
                  stencilPrj @(sh:.Int) @a $ prj0 s)

prjTail :: SmartExp (t, a) -> SmartExp t
prjTail = SmartExp . Prj PairIdxLeft

prj0 :: SmartExp (t, a) -> SmartExp a
prj0 = SmartExp . Prj PairIdxRight

prj1 :: SmartExp ((t, a), s0) -> SmartExp a
prj1 = prj0 . prjTail

prj2 :: SmartExp (((t, a), s1), s0) -> SmartExp a
prj2 = prj1 . prjTail

prj3 :: SmartExp ((((t, a), s2), s1), s0) -> SmartExp a
prj3 = prj2 . prjTail

prj4 :: SmartExp (((((t, a), s3), s2), s1), s0) -> SmartExp a
prj4 = prj3 . prjTail

prj5 :: SmartExp ((((((t, a), s4), s3), s2), s1), s0) -> SmartExp a
prj5 = prj4 . prjTail

prj6 :: SmartExp (((((((t, a), s5), s4), s3), s2), s1), s0) -> SmartExp a
prj6 = prj5 . prjTail

prj7 :: SmartExp ((((((((t, a), s6), s5), s4), s3), s2), s1), s0) -> SmartExp a
prj7 = prj6 . prjTail

prj8 :: SmartExp (((((((((t, a), s7), s6), s5), s4), s3), s2), s1), s0) -> SmartExp a
prj8 = prj7 . prjTail


-- Smart constructor for literals
--

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
constant :: forall e. Elt e => e -> Exp e
constant = Exp . go (eltType @e) . fromElt
  where
    go :: TupleType t -> t -> SmartExp t
    go TupRunit         ()       = SmartExp $ Nil
    go (TupRsingle tp)  c        = SmartExp $ Const tp c
    go (TupRpair t1 t2) (c1, c2) = SmartExp $ go t1 c1 `Pair` go t2 c2

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
undef :: forall e. Elt e => Exp e
undef = Exp $ go $ eltType @e
  where
    go :: TupleType t -> SmartExp t
    go TupRunit         = SmartExp $ Nil
    go (TupRsingle t)   = SmartExp $ Undef t
    go (TupRpair t1 t2) = SmartExp $ go t1 `Pair` go t2

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
indexHead :: (Elt sh, Elt a) => Exp (sh :. a) -> Exp a
indexHead (Exp x) = mkExp $ Prj PairIdxRight x

-- | Get all but the innermost element of a shape
--
indexTail :: (Elt sh, Elt a) => Exp (sh :. a) -> Exp sh
indexTail (Exp x) = mkExp $ Prj PairIdxLeft x


-- Smart constructor for constants
--

mkMinBound :: (Elt t, IsBounded (EltRepr t)) => Exp t
mkMinBound = mkExp $ PrimConst (PrimMinBound boundedType)

mkMaxBound :: (Elt t, IsBounded (EltRepr t)) => Exp t
mkMaxBound = mkExp $ PrimConst (PrimMaxBound boundedType)

mkPi :: (Elt r, IsFloating (EltRepr r)) => Exp r
mkPi = mkExp $ PrimConst (PrimPi floatingType)


-- Smart constructors for primitive applications
--

-- Operators from Floating

mkSin :: (Elt t, IsFloating (EltRepr t)) => Exp t -> Exp t
mkSin = mkPrimUnary $ PrimSin floatingType

mkCos :: (Elt t, IsFloating (EltRepr t)) => Exp t -> Exp t
mkCos = mkPrimUnary $ PrimCos floatingType

mkTan :: (Elt t, IsFloating (EltRepr t)) => Exp t -> Exp t
mkTan = mkPrimUnary $ PrimTan floatingType

mkAsin :: (Elt t, IsFloating (EltRepr t)) => Exp t -> Exp t
mkAsin = mkPrimUnary $ PrimAsin floatingType

mkAcos :: (Elt t, IsFloating (EltRepr t)) => Exp t -> Exp t
mkAcos = mkPrimUnary $ PrimAcos floatingType

mkAtan :: (Elt t, IsFloating (EltRepr t)) => Exp t -> Exp t
mkAtan = mkPrimUnary $ PrimAtan floatingType

mkSinh :: (Elt t, IsFloating (EltRepr t)) => Exp t -> Exp t
mkSinh = mkPrimUnary $ PrimSinh floatingType

mkCosh :: (Elt t, IsFloating (EltRepr t)) => Exp t -> Exp t
mkCosh = mkPrimUnary $ PrimCosh floatingType

mkTanh :: (Elt t, IsFloating (EltRepr t)) => Exp t -> Exp t
mkTanh = mkPrimUnary $ PrimTanh floatingType

mkAsinh :: (Elt t, IsFloating (EltRepr t)) => Exp t -> Exp t
mkAsinh = mkPrimUnary $ PrimAsinh floatingType

mkAcosh :: (Elt t, IsFloating (EltRepr t)) => Exp t -> Exp t
mkAcosh = mkPrimUnary $ PrimAcosh floatingType

mkAtanh :: (Elt t, IsFloating (EltRepr t)) => Exp t -> Exp t
mkAtanh = mkPrimUnary $ PrimAtanh floatingType

mkExpFloating :: (Elt t, IsFloating (EltRepr t)) => Exp t -> Exp t
mkExpFloating = mkPrimUnary $ PrimExpFloating floatingType

mkSqrt :: (Elt t, IsFloating (EltRepr t)) => Exp t -> Exp t
mkSqrt = mkPrimUnary $ PrimSqrt floatingType

mkLog :: (Elt t, IsFloating (EltRepr t)) => Exp t -> Exp t
mkLog = mkPrimUnary $ PrimLog floatingType

mkFPow :: (Elt t, IsFloating (EltRepr t)) => Exp t -> Exp t -> Exp t
mkFPow = mkPrimBinary $ PrimFPow floatingType

mkLogBase :: (Elt t, IsFloating (EltRepr t)) => Exp t -> Exp t -> Exp t
mkLogBase = mkPrimBinary $ PrimLogBase floatingType

-- Operators from Num

mkAdd :: (Elt t, IsNum (EltRepr t)) => Exp t -> Exp t -> Exp t
mkAdd = mkPrimBinary $ PrimAdd numType

mkSub :: (Elt t, IsNum (EltRepr t)) => Exp t -> Exp t -> Exp t
mkSub = mkPrimBinary $ PrimSub numType

mkMul :: (Elt t, IsNum (EltRepr t)) => Exp t -> Exp t -> Exp t
mkMul = mkPrimBinary $ PrimMul numType

mkNeg :: (Elt t, IsNum (EltRepr t)) => Exp t -> Exp t
mkNeg = mkPrimUnary $ PrimNeg numType

mkAbs :: (Elt t, IsNum (EltRepr t)) => Exp t -> Exp t
mkAbs = mkPrimUnary $ PrimAbs numType

mkSig :: (Elt t, IsNum (EltRepr t)) => Exp t -> Exp t
mkSig = mkPrimUnary $ PrimSig numType

-- Operators from Integral

mkQuot :: (Elt t, IsIntegral (EltRepr t)) => Exp t -> Exp t -> Exp t
mkQuot = mkPrimBinary $ PrimQuot integralType

mkRem :: (Elt t, IsIntegral (EltRepr t)) => Exp t -> Exp t -> Exp t
mkRem = mkPrimBinary $ PrimRem integralType

mkQuotRem :: (Elt t, IsIntegral (EltRepr t)) => Exp t -> Exp t -> (Exp t, Exp t)
mkQuotRem (Exp x) (Exp y) =
  let pair = SmartExp $ PrimQuotRem integralType `PrimApp` SmartExp (Pair x y)
  in  (mkExp $ Prj PairIdxLeft pair, mkExp $ Prj PairIdxRight pair)

mkIDiv :: (Elt t, IsIntegral (EltRepr t)) => Exp t -> Exp t -> Exp t
mkIDiv = mkPrimBinary $ PrimIDiv integralType

mkMod :: (Elt t, IsIntegral (EltRepr t)) => Exp t -> Exp t -> Exp t
mkMod = mkPrimBinary $ PrimMod integralType

mkDivMod :: (Elt t, IsIntegral (EltRepr t)) => Exp t -> Exp t -> (Exp t, Exp t)
mkDivMod (Exp x) (Exp y) =
  let pair = SmartExp $ PrimDivMod integralType `PrimApp` SmartExp (Pair x y)
  in  (mkExp $ Prj PairIdxLeft pair, mkExp $ Prj PairIdxRight pair)

-- Operators from Bits and FiniteBits

mkBAnd :: (Elt t, IsIntegral (EltRepr t)) => Exp t -> Exp t -> Exp t
mkBAnd = mkPrimBinary $ PrimBAnd integralType

mkBOr :: (Elt t, IsIntegral (EltRepr t)) => Exp t -> Exp t -> Exp t
mkBOr = mkPrimBinary $ PrimBOr integralType

mkBXor :: (Elt t, IsIntegral (EltRepr t)) => Exp t -> Exp t -> Exp t
mkBXor = mkPrimBinary $ PrimBXor integralType

mkBNot :: (Elt t, IsIntegral (EltRepr t)) => Exp t -> Exp t
mkBNot = mkPrimUnary $ PrimBNot integralType

mkBShiftL :: (Elt t, IsIntegral (EltRepr t)) => Exp t -> Exp Int -> Exp t
mkBShiftL = mkPrimBinary $ PrimBShiftL integralType

mkBShiftR :: (Elt t, IsIntegral (EltRepr t)) => Exp t -> Exp Int -> Exp t
mkBShiftR = mkPrimBinary $ PrimBShiftR integralType

mkBRotateL :: (Elt t, IsIntegral (EltRepr t)) => Exp t -> Exp Int -> Exp t
mkBRotateL = mkPrimBinary $ PrimBRotateL integralType

mkBRotateR :: (Elt t, IsIntegral (EltRepr t)) => Exp t -> Exp Int -> Exp t
mkBRotateR = mkPrimBinary $ PrimBRotateR integralType

mkPopCount :: (Elt t, IsIntegral (EltRepr t)) => Exp t -> Exp Int
mkPopCount = mkPrimUnary $ PrimPopCount integralType

mkCountLeadingZeros :: (Elt t, IsIntegral (EltRepr t)) => Exp t -> Exp Int
mkCountLeadingZeros = mkPrimUnary $ PrimCountLeadingZeros integralType

mkCountTrailingZeros :: (Elt t, IsIntegral (EltRepr t)) => Exp t -> Exp Int
mkCountTrailingZeros = mkPrimUnary $ PrimCountTrailingZeros integralType


-- Operators from Fractional

mkFDiv :: (Elt t, IsFloating (EltRepr t)) => Exp t -> Exp t -> Exp t
mkFDiv = mkPrimBinary $ PrimFDiv floatingType

mkRecip :: (Elt t, IsFloating (EltRepr t)) => Exp t -> Exp t
mkRecip = mkPrimUnary $ PrimRecip floatingType

-- Operators from RealFrac

mkTruncate :: (Elt a, Elt b, IsFloating (EltRepr a), IsIntegral (EltRepr b)) => Exp a -> Exp b
mkTruncate = mkPrimUnary $ PrimTruncate floatingType integralType

mkRound :: (Elt a, Elt b, IsFloating (EltRepr a), IsIntegral (EltRepr b)) => Exp a -> Exp b
mkRound = mkPrimUnary $ PrimRound floatingType integralType

mkFloor :: (Elt a, Elt b, IsFloating (EltRepr a), IsIntegral (EltRepr b)) => Exp a -> Exp b
mkFloor = mkPrimUnary $ PrimFloor floatingType integralType

mkCeiling :: (Elt a, Elt b, IsFloating (EltRepr a), IsIntegral (EltRepr b)) => Exp a -> Exp b
mkCeiling = mkPrimUnary $ PrimCeiling floatingType integralType

-- Operators from RealFloat

mkAtan2 :: (Elt t, IsFloating (EltRepr t)) => Exp t -> Exp t -> Exp t
mkAtan2 = mkPrimBinary $ PrimAtan2 floatingType

mkIsNaN :: (Elt t, IsFloating (EltRepr t)) => Exp t -> Exp Bool
mkIsNaN = mkPrimUnary $ PrimIsNaN floatingType

mkIsInfinite :: (Elt t, IsFloating (EltRepr t)) => Exp t -> Exp Bool
mkIsInfinite = mkPrimUnary $ PrimIsInfinite floatingType

-- FIXME: add missing operations from Floating, RealFrac & RealFloat

-- Relational and equality operators

mkLt :: (Elt t, IsSingle (EltRepr t)) => Exp t -> Exp t -> Exp Bool
mkLt = mkPrimBinary $ PrimLt singleType

mkGt :: (Elt t, IsSingle (EltRepr t)) => Exp t -> Exp t -> Exp Bool
mkGt = mkPrimBinary $ PrimGt singleType

mkLtEq :: (Elt t, IsSingle (EltRepr t)) => Exp t -> Exp t -> Exp Bool
mkLtEq = mkPrimBinary $ PrimLtEq singleType

mkGtEq :: (Elt t, IsSingle (EltRepr t)) => Exp t -> Exp t -> Exp Bool
mkGtEq = mkPrimBinary $ PrimGtEq singleType

mkEq :: (Elt t, IsSingle (EltRepr t)) => Exp t -> Exp t -> Exp Bool
mkEq = mkPrimBinary $ PrimEq singleType

mkNEq :: (Elt t, IsSingle (EltRepr t)) => Exp t -> Exp t -> Exp Bool
mkNEq = mkPrimBinary $ PrimNEq singleType

mkMax :: (Elt t, IsSingle (EltRepr t)) => Exp t -> Exp t -> Exp t
mkMax = mkPrimBinary $ PrimMax singleType

mkMin :: (Elt t, IsSingle (EltRepr t)) => Exp t -> Exp t -> Exp t
mkMin = mkPrimBinary $ PrimMin singleType

-- Logical operators

mkLAnd :: Exp Bool -> Exp Bool -> Exp Bool
mkLAnd = mkPrimBinary PrimLAnd

mkLOr :: Exp Bool -> Exp Bool -> Exp Bool
mkLOr = mkPrimBinary PrimLOr

mkLNot :: Exp Bool -> Exp Bool
mkLNot = mkPrimUnary PrimLNot

-- Character conversions

mkOrd :: Exp Char -> Exp Int
mkOrd = mkPrimUnary PrimOrd

mkChr :: Exp Int -> Exp Char
mkChr = mkPrimUnary PrimChr

-- Numeric conversions

mkFromIntegral :: (Elt a, Elt b, IsIntegral (EltRepr a), IsNum (EltRepr b)) => Exp a -> Exp b
mkFromIntegral = mkPrimUnary $ PrimFromIntegral integralType numType

mkToFloating :: (Elt a, Elt b, IsNum (EltRepr a), IsFloating (EltRepr b)) => Exp a -> Exp b
mkToFloating = mkPrimUnary $ PrimToFloating numType floatingType

-- Other conversions

mkBoolToInt :: Exp Bool -> Exp Int
mkBoolToInt (Exp b) = mkExp $ PrimBoolToInt `PrimApp` b

-- NOTE: Restricted to scalar types with a type-level BitSizeEq constraint to
-- make this version "safe"
mkBitcast :: forall b a. (Elt a, Elt b, IsScalar (EltRepr a), IsScalar (EltRepr b), BitSizeEq (EltRepr a) (EltRepr b)) => Exp a -> Exp b
mkBitcast (Exp a) = mkExp $ Coerce (scalarType @(EltRepr a)) (scalarType @(EltRepr b)) a

mkCoerce :: Coerce (EltRepr a) (EltRepr b) => Exp a -> Exp b
mkCoerce (Exp a) = Exp $ mkCoerce' a

class Coerce a b where
  mkCoerce' :: SmartExp a -> SmartExp b

instance (IsScalar a, IsScalar b, BitSizeEq a b) => Coerce a b where
  mkCoerce' = SmartExp . Coerce (scalarType @a) (scalarType @b)

instance (Coerce a1 b1, Coerce a2 b2) => Coerce (a1, a2) (b1, b2) where
  mkCoerce' a = SmartExp $ Pair (mkCoerce' $ SmartExp $ Prj PairIdxLeft a) (mkCoerce' $ SmartExp $ Prj PairIdxRight a)

instance Coerce () () where
  mkCoerce' _ = SmartExp Nil

instance Coerce ((), a) a where
  mkCoerce' a = SmartExp $ Prj PairIdxRight a

instance Coerce a ((), a) where
  mkCoerce' = SmartExp . Pair (SmartExp Nil)



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

unAcc :: Arrays a => Acc a -> SmartAcc (ArrRepr a)
unAcc (Acc a) = a

unAccFunction :: (Arrays a, Arrays b) => (Acc a -> Acc b) -> SmartAcc (ArrRepr a) -> SmartAcc (ArrRepr b)
unAccFunction f = unAcc . f . Acc

mkExp :: PreSmartExp SmartAcc SmartExp (EltRepr t) -> Exp t
mkExp = Exp . SmartExp

unExp :: Elt e => Exp e -> SmartExp (EltRepr e)
unExp (Exp e) = e

unExpFunction :: (Elt a, Elt b) => (Exp a -> Exp b) -> SmartExp (EltRepr a) -> SmartExp (EltRepr b)
unExpFunction f = unExp . f . Exp

unExpBinaryFunction :: (Elt a, Elt b, Elt c) => (Exp a -> Exp b -> Exp c) -> SmartExp (EltRepr a) -> SmartExp (EltRepr b) -> SmartExp (EltRepr c)
unExpBinaryFunction f a b = unExp $ f (Exp a) (Exp b)

mkPrimUnary :: (Elt a, Elt b) => PrimFun (EltRepr a -> EltRepr b) -> Exp a -> Exp b
mkPrimUnary prim (Exp a) = mkExp $ PrimApp prim a

mkPrimBinary :: (Elt a, Elt b, Elt c) => PrimFun ((EltRepr a, EltRepr b) -> EltRepr c) -> Exp a -> Exp b -> Exp c
mkPrimBinary prim (Exp a) (Exp b) = mkExp $ PrimApp prim (SmartExp $ Pair a b)

unPair :: SmartExp (a, b) -> (SmartExp a, SmartExp b)
unPair e = (SmartExp $ Prj PairIdxLeft e, SmartExp $ Prj PairIdxRight e)

mkPairToTuple :: SmartAcc (a, b) -> SmartAcc (((), a), b)
mkPairToTuple e = SmartAcc Anil `pair` a `pair` b
  where
    a = SmartAcc $ Aprj PairIdxLeft e
    b = SmartAcc $ Aprj PairIdxRight e
    pair x y = SmartAcc $ Apair x y

class ApplyAcc a where
  type FromApplyAcc a
  applyAcc :: FromApplyAcc a -> a

instance ApplyAcc (SmartAcc a) where
  type FromApplyAcc (SmartAcc a) = PreSmartAcc SmartAcc SmartExp a
  applyAcc = SmartAcc

instance (Arrays a, ApplyAcc t) => ApplyAcc (Acc a -> t) where
  type FromApplyAcc (Acc a -> t) = SmartAcc (ArrRepr a) -> FromApplyAcc t
  applyAcc f a = applyAcc $ f (unAcc a)

instance (Elt a, ApplyAcc t) => ApplyAcc (Exp a -> t) where
  type FromApplyAcc (Exp a -> t) = SmartExp (EltRepr a) -> FromApplyAcc t
  applyAcc f a = applyAcc $ f (unExp a)

instance (Elt a, Elt b, ApplyAcc t) => ApplyAcc ((Exp a -> Exp b) -> t) where
  type FromApplyAcc ((Exp a -> Exp b) -> t) = (SmartExp (EltRepr a) -> SmartExp (EltRepr b)) -> FromApplyAcc t
  applyAcc f a = applyAcc $ f (unExpFunction a)

instance (Elt a, Elt b, Elt c, ApplyAcc t) => ApplyAcc ((Exp a -> Exp b -> Exp c) -> t) where
  type FromApplyAcc ((Exp a -> Exp b -> Exp c) -> t) = (SmartExp (EltRepr a) -> SmartExp (EltRepr b) -> SmartExp (EltRepr c)) -> FromApplyAcc t
  applyAcc f a = applyAcc $ f (unExpBinaryFunction a)

instance (Arrays a, Arrays b, ApplyAcc t) => ApplyAcc ((Acc a -> Acc b) -> t) where
  type FromApplyAcc ((Acc a -> Acc b) -> t) = (SmartAcc (ArrRepr a) -> SmartAcc (ArrRepr b)) -> FromApplyAcc t
  applyAcc f a = applyAcc $ f (unAccFunction a)

-- Debugging
-- ---------

showPreAccOp :: forall acc exp arrs. PreSmartAcc acc exp arrs -> String
showPreAccOp (Atag _ i)         = "Atag " ++ show i
showPreAccOp (Use repr a)       = "Use "  ++ showShortendArr repr a
showPreAccOp Pipe{}             = "Pipe"
showPreAccOp Acond{}            = "Acond"
showPreAccOp Awhile{}           = "Awhile"
showPreAccOp Apair{}            = "Apair"
showPreAccOp Anil{}             = "Anil"
showPreAccOp Aprj{}             = "Aprj"
showPreAccOp Unit{}             = "Unit"
showPreAccOp Generate{}         = "Generate"
showPreAccOp Reshape{}          = "Reshape"
showPreAccOp Replicate{}        = "Replicate"
showPreAccOp Slice{}            = "Slice"
showPreAccOp Map{}              = "Map"
showPreAccOp ZipWith{}          = "ZipWith"
showPreAccOp (Fold _ _ Just{} _) = "Fold"
showPreAccOp Fold{}             = "Fold1"
showPreAccOp (FoldSeg _ _ _ Just{} _ _) = "FoldSeg"
showPreAccOp FoldSeg{}          = "Fold1Seg"
showPreAccOp (Scan d _ _ z _)   = "Scan" ++ show d ++ maybe "1" (const "") z -- Scanl, Scanl1, Scanr, Scanr1
showPreAccOp (Scan' d _ _ _ _)  = "Scan" ++ show d ++ "'"
showPreAccOp Permute{}          = "Permute"
showPreAccOp Backpermute{}      = "Backpermute"
showPreAccOp Stencil{}          = "Stencil"
showPreAccOp Stencil2{}         = "Stencil2"
showPreAccOp Aforeign{}         = "Aforeign"
-- showPreAccOp Collect{}          = "Collect"

{--
showPreSeqOp :: PreSeq acc seq exp arrs -> String
showPreSeqOp (StreamIn{})       = "StreamIn"
showPreSeqOp (ToSeq{})          = "ToSeq"
showPreSeqOp (MapSeq{})         = "MapSeq"
showPreSeqOp (ZipWithSeq{})     = "ZipWithSeq"
showPreSeqOp (ScanSeq{})        = "ScanSeq"
showPreSeqOp (FoldSeq{})        = "FoldSeq"
showPreSeqOp (FoldSeqFlatten{}) = "FoldSeqFlatten"
showPreSeqOp (Stuple{})         = "Stuple"
--}


showPreExpOp :: PreSmartExp acc exp t -> String
showPreExpOp (Tag _ i)          = "Tag" ++ show i
showPreExpOp (Const tp c)       = "Const " ++ showElement (TupRsingle tp) c
showPreExpOp (Undef _)          = "Undef"
showPreExpOp Nil{}              = "Nil"
showPreExpOp Pair{}             = "Pair"
showPreExpOp Prj{}              = "Prj"
showPreExpOp VecPack{}          = "VecPack"
showPreExpOp VecUnpack{}        = "VecUnpack"
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
showPreExpOp Foreign{}          = "Foreign"
showPreExpOp Coerce{}           = "Coerce"

