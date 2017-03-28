{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Smart
-- Copyright   : [2008..2014] Manuel M T Chakravarty, Gabriele Keller
--               [2008..2009] Sean Lee
--               [2009..2014] Trevor L. McDonell
--               [2013..2014] Robert Clifton-Everest
--               [2014..2014] Frederik M. Madsen
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This modules defines the AST of the user-visible embedded language using more
-- convenient higher-order abstract syntax (instead of de Bruijn indices).
-- Moreover, it defines smart constructors to construct programs.
--

module Data.Array.Accelerate.Smart (

  -- * HOAS AST
  Acc(..), PreAcc(..), Exp(..), PreExp(..), Boundary(..), Stencil(..), Level,
  PreSeq(..), Seq(..),

  -- * Smart constructors for literals
  constant,

  -- * Smart constructors and destructors for tuples
  tup2, tup3, tup4, tup5, tup6, tup7, tup8, tup9, tup10, tup11, tup12, tup13, tup14, tup15,
  untup2, untup3, untup4, untup5, untup6, untup7, untup8, untup9, untup10, untup11, untup12, untup13, untup14, untup15,

  atup2, atup3, atup4, atup5, atup6, atup7, atup8, atup9, atup10, atup11, atup12, atup13, atup14, atup15,
  unatup2, unatup3, unatup4, unatup5, unatup6, unatup7, unatup8, unatup9, unatup10, unatup11, unatup12, unatup13, unatup14, unatup15,

  stup2, stup3, stup4, stup5, stup6, stup7, stup8, stup9, stup10, stup11, stup12, stup13, stup14, stup15,

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
  mkLAnd, mkLOr, mkLNot, mkIsNaN,

  -- * Smart constructors for type coercion functions
  mkOrd, mkChr, mkBoolToInt, mkFromIntegral, mkToFloating, mkBitcast, mkUnsafeCoerce,

  -- * Auxiliary functions
  ($$), ($$$), ($$$$), ($$$$$),

  -- Debugging
  showPreAccOp, showPreExpOp, showPreSeqOp

) where

-- standard library
import Prelude                                  hiding ( exp )
import Data.List
import Data.Typeable

-- friends
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.AST                hiding ( PreOpenAcc(..), OpenAcc(..), Acc
                                                       , PreOpenExp(..), OpenExp, PreExp, Exp
                                                       , Stencil(..), PreOpenSeq(..), Seq
                                                       , Consumer(..), Producer(..)
                                                       , showPreAccOp, showPreExpOp )
import qualified Data.Array.Accelerate.AST      as AST

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
-- For example, to compute a vector dot product we could write:
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
-- [/Fusion:/]
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
-- > map -> reverse -> reshape -> map -> map
--
-- > map -> backpermute ->
-- >                       zipWith -> map
-- >           generate ->
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
-- >      use ->
-- >             zipWith -> fold |-> map
-- > generate ->
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
-- [/Tips:/]
--
--  * Since 'Acc' represents embedded computations that will only be executed
--    when evaluated by a backend, we can programatically generate these
--    computations using the meta language Haskell; for example, unrolling loops
--    or embedding input values into the generated code.
--
--  * It is usually best to keep all intermediate computations in 'Acc', and
--    only 'run' the computation at the very end to produce the final result.
--    This enables optimisations between intermediate results (e.g. array
--    fusion) and, if the target architecture has a separate memory space as is
--    the case of GPUs, to prevent excessive data transfers.
--
newtype Acc a = Acc (PreAcc Acc Seq Exp a)
deriving instance Typeable Acc


-- The level of lambda-bound variables. The root has level 0; then it increases with each bound
-- variable — i.e., it is the same as the size of the environment at the defining occurrence.
--
type Level = Int

-- | Array-valued collective computations without a recursive knot
--
data PreAcc acc seq exp as where
    -- Needed for conversion to de Bruijn form
  Atag          :: Arrays as
                => Level                        -- environment size at defining occurrence
                -> PreAcc acc seq exp as

  Pipe          :: (Arrays as, Arrays bs, Arrays cs)
                => (Acc as -> acc bs)
                -> (Acc bs -> acc cs)
                -> acc as
                -> PreAcc acc seq exp cs

  Aforeign      :: (Arrays as, Arrays bs, Foreign asm)
                => asm (as -> bs)
                -> (Acc as -> Acc bs)
                -> acc as
                -> PreAcc acc seq exp bs

  Acond         :: Arrays as
                => exp Bool
                -> acc as
                -> acc as
                -> PreAcc acc seq exp as

  Awhile        :: Arrays arrs
                => (Acc arrs -> acc (Scalar Bool))
                -> (Acc arrs -> acc arrs)
                -> acc arrs
                -> PreAcc acc seq exp arrs

  Atuple        :: (Arrays arrs, IsAtuple arrs)
                => Atuple acc (TupleRepr arrs)
                -> PreAcc acc seq exp arrs

  Aprj          :: (Arrays arrs, IsAtuple arrs, Arrays a)
                => TupleIdx (TupleRepr arrs) a
                ->        acc     arrs
                -> PreAcc acc seq exp a

  Use           :: Arrays arrs
                => arrs
                -> PreAcc acc seq exp arrs

  Unit          :: Elt e
                => exp e
                -> PreAcc acc seq exp (Scalar e)

  Generate      :: (Shape sh, Elt e)
                => exp sh
                -> (Exp sh -> exp e)
                -> PreAcc acc seq exp (Array sh e)

  Reshape       :: (Shape sh, Shape sh', Elt e)
                => exp sh
                -> acc (Array sh' e)
                -> PreAcc acc seq exp (Array sh e)

  Replicate     :: (Slice slix, Elt e)
                => exp slix
                -> acc                (Array (SliceShape slix) e)
                -> PreAcc acc seq exp (Array (FullShape  slix) e)

  Slice         :: (Slice slix, Elt e)
                => acc                (Array (FullShape  slix) e)
                -> exp slix
                -> PreAcc acc seq exp (Array (SliceShape slix) e)

  Map           :: (Shape sh, Elt e, Elt e')
                => (Exp e -> exp e')
                -> acc (Array sh e)
                -> PreAcc acc seq exp (Array sh e')

  ZipWith       :: (Shape sh, Elt e1, Elt e2, Elt e3)
                => (Exp e1 -> Exp e2 -> exp e3)
                -> acc (Array sh e1)
                -> acc (Array sh e2)
                -> PreAcc acc seq exp (Array sh e3)

  Fold          :: (Shape sh, Elt e)
                => (Exp e -> Exp e -> exp e)
                -> exp e
                -> acc (Array (sh:.Int) e)
                -> PreAcc acc seq exp (Array sh e)

  Fold1         :: (Shape sh, Elt e)
                => (Exp e -> Exp e -> exp e)
                -> acc (Array (sh:.Int) e)
                -> PreAcc acc seq exp (Array sh e)

  FoldSeg       :: (Shape sh, Elt e, Elt i, IsIntegral i)
                => (Exp e -> Exp e -> exp e)
                -> exp e
                -> acc (Array (sh:.Int) e)
                -> acc (Segments i)
                -> PreAcc acc seq exp (Array (sh:.Int) e)

  Fold1Seg      :: (Shape sh, Elt e, Elt i, IsIntegral i)
                => (Exp e -> Exp e -> exp e)
                -> acc (Array (sh:.Int) e)
                -> acc (Segments i)
                -> PreAcc acc seq exp (Array (sh:.Int) e)

  Scanl         :: (Shape sh, Elt e)
                => (Exp e -> Exp e -> exp e)
                -> exp e
                -> acc (Array (sh :. Int) e)
                -> PreAcc acc seq exp (Array (sh :. Int) e)

  Scanl'        :: (Shape sh, Elt e)
                => (Exp e -> Exp e -> exp e)
                -> exp e
                -> acc (Array (sh :. Int) e)
                -> PreAcc acc seq exp (Array (sh :. Int) e, Array sh e)

  Scanl1        :: (Shape sh, Elt e)
                => (Exp e -> Exp e -> exp e)
                -> acc (Array (sh :. Int) e)
                -> PreAcc acc seq exp (Array (sh :. Int) e)

  Scanr         :: (Shape sh, Elt e)
                => (Exp e -> Exp e -> exp e)
                -> exp e
                -> acc (Array (sh :. Int) e)
                -> PreAcc acc seq exp (Array (sh :. Int) e)

  Scanr'        :: (Shape sh, Elt e)
                => (Exp e -> Exp e -> exp e)
                -> exp e
                -> acc (Array (sh :. Int) e)
                -> PreAcc acc seq exp (Array (sh :. Int) e, Array sh e)

  Scanr1        :: (Shape sh, Elt e)
                => (Exp e -> Exp e -> exp e)
                -> acc (Array (sh :. Int) e)
                -> PreAcc acc seq exp (Array (sh :. Int) e)

  Permute       :: (Shape sh, Shape sh', Elt e)
                => (Exp e -> Exp e -> exp e)
                -> acc (Array sh' e)
                -> (Exp sh -> exp sh')
                -> acc (Array sh e)
                -> PreAcc acc seq exp (Array sh' e)

  Backpermute   :: (Shape sh, Shape sh', Elt e)
                => exp sh'
                -> (Exp sh' -> exp sh)
                -> acc (Array sh e)
                -> PreAcc acc seq exp (Array sh' e)

  Stencil       :: (Shape sh, Elt a, Elt b, Stencil sh a stencil)
                => (stencil -> exp b)
                -> Boundary a
                -> acc (Array sh a)
                -> PreAcc acc seq exp (Array sh b)

  Stencil2      :: (Shape sh, Elt a, Elt b, Elt c,
                   Stencil sh a stencil1, Stencil sh b stencil2)
                => (stencil1 -> stencil2 -> exp c)
                -> Boundary a
                -> acc (Array sh a)
                -> Boundary b
                -> acc (Array sh b)
                -> PreAcc acc seq exp (Array sh c)

  Collect       :: Arrays arrs
                => seq arrs
                -> PreAcc acc seq exp arrs

data PreSeq acc seq exp arrs where
  -- Needed for conversion to de Bruijn form
  Stag           :: Arrays t
                 => Level                        -- environment size at defining occurrence
                 -> PreSeq acc seq exp [t]

  StreamIn       :: (Shape sh, Elt e)
                 => [Array sh e]
                 -> PreSeq acc seq exp [Array sh e]

  Subarrays      :: (Shape sh, Elt e, sh :<= DIM2)
                 => exp sh
                 -> Array sh e
                 -> PreSeq acc seq exp [Array sh e]

  FromSegs       :: (Shape sh, Elt e)
                 => acc (Segments (Int,sh))
                 -> exp Int                      -- Number of segments
                 -> acc (Vector e)
                 -> PreSeq acc seq exp [Array sh e]

  Produce        :: Arrays a
                 => exp Int
                 -> (Acc (Scalar Int) -> acc a)
                 -> PreSeq acc seq exp [a]

  MapSeq         :: (Arrays a, Arrays b)
                 => (Acc a -> acc b)
                 -> seq [a]
                 -> PreSeq acc seq exp [b]

  ZipWithSeq     :: (Arrays a, Arrays b, Arrays c)
                 => (Acc a -> Acc b -> acc c)
                 -> seq [a]
                 -> seq [b]
                 -> PreSeq acc seq exp [c]

  -- MapBatch       :: (Arrays a, Arrays b, Arrays c, Arrays s)
  --                => (Acc s -> Acc a -> acc b)
  --                -> (Acc s -> Acc (Regular b) -> acc (s, Regular c))
  --                -> (Acc s -> Acc (Irregular b) -> acc (s, Irregular c))
  --                -> acc s
  --                -> seq [a]
  --                -> PreSeq acc seq exp [(s,c)]

  -- FIXME: The Seq argument to the reduction function is only that because
  -- sequences are currently the only nested structure we have. It should really
  -- be a nested array.
  --
  FoldBatch      :: (Arrays a, Arrays s, Arrays b)
                 => (Acc s -> Seq [a] -> seq b)
                 -> (Acc b -> acc s)
                 -> acc s
                 -> seq [a]
                 -> PreSeq acc seq exp s

  Elements       :: (Shape sh, Elt e)
                 => seq [Array sh e]
                 -> PreSeq acc seq exp (Vector e)

  Tabulate       :: (Shape sh, Elt e)
                 => seq [Array sh e]
                 -> PreSeq acc seq exp (Array (sh:.Int) e)

  AsSeq          :: (Arrays a)
                 => acc a
                 -> PreSeq acc seq exp a

  -- Tuple up the results of a sequence computation. Note that the Arrays
  -- constraint requires that the elements of the tuple are Arrays, not
  -- streams ([]).
  Stuple         :: (Arrays arrs, IsAtuple arrs)
                 => Atuple seq (TupleRepr arrs)
                 -> PreSeq acc seq exp arrs

-- |Array-valued sequence computations
--
newtype Seq a = Seq (PreSeq Acc Seq Exp a)

deriving instance Typeable Seq


-- Embedded expressions of the surface language
-- --------------------------------------------

-- HOAS expressions mirror the constructors of 'AST.OpenExp', but with the 'Tag'
-- constructor instead of variables in the form of de Bruijn indices. Moreover,
-- HOAS expression use n-tuples and the type class 'Elt' to constrain element
-- types, whereas 'AST.OpenExp' uses nested pairs and the GADT 'TupleType'.
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
newtype Exp t = Exp (PreExp Acc Seq Exp t)

deriving instance Typeable Exp

-- | Scalar expressions to parametrise collective array operations, themselves parameterised over
-- the type of collective array operations.
--
data PreExp acc seq exp t where
    -- Needed for conversion to de Bruijn form
  Tag           :: Elt t
                => Level                        -- environment size at defining occurrence
                -> PreExp acc seq exp t

  -- All the same constructors as 'AST.Exp'
  Const         :: Elt t
                => t
                -> PreExp acc seq exp t

  Tuple         :: (Elt t, IsTuple t)
                => Tuple exp (TupleRepr t)
                -> PreExp acc seq exp t

  Prj           :: (Elt t, IsTuple t, Elt e)
                => TupleIdx (TupleRepr t) e
                -> exp t
                -> PreExp acc seq exp e

  IndexNil      :: PreExp acc seq exp Z

  IndexCons     :: (Slice sl, Elt a)
                => exp sl
                -> exp a
                -> PreExp acc seq exp (sl:.a)

  IndexHead     :: (Slice sl, Elt a)
                => exp (sl:.a)
                -> PreExp acc seq exp a

  IndexTail     :: (Slice sl, Elt a)
                => exp (sl:.a)
                -> PreExp acc seq exp sl

  IndexTrans    :: Shape sh
                => exp sh
                -> PreExp acc seq exp sh

  IndexAny      :: Shape sh
                => PreExp acc seq exp (Any sh)

  IndexSlice    :: Slice slix
                => exp slix
                -> exp (FullShape slix)
                -> PreExp acc seq exp (SliceShape slix)

  IndexFull     :: Slice slix
                => exp slix
                -> exp (SliceShape slix)
                -> PreExp acc seq exp (FullShape slix)

  ToIndex       :: Shape sh
                => exp sh
                -> exp sh
                -> PreExp acc seq exp Int

  FromIndex     :: Shape sh
                => exp sh
                -> exp Int
                -> PreExp acc seq exp sh

  ToSlice       :: Slice slix
                => exp slix
                -> exp (FullShape slix)
                -> exp Int
                -> PreExp acc seq exp slix

  Cond          :: Elt t
                => exp Bool
                -> exp t
                -> exp t
                -> PreExp acc seq exp t

  While         :: Elt t
                => (Exp t -> exp Bool)
                -> (Exp t -> exp t)
                -> exp t
                -> PreExp acc seq exp t

  PrimConst     :: Elt t
                => PrimConst t
                -> PreExp acc seq exp t

  PrimApp       :: (Elt a, Elt r)
                => PrimFun (a -> r)
                -> exp a
                -> PreExp acc seq exp r

  Index         :: (Shape sh, Elt t)
                => acc (Array sh t)
                -> exp sh
                -> PreExp acc seq exp t

  LinearIndex   :: (Shape sh, Elt t)
                => acc (Array sh t)
                -> exp Int
                -> PreExp acc seq exp t

  Shape         :: (Shape sh, Elt e)
                => acc (Array sh e)
                -> PreExp acc seq exp sh

  ShapeSize     :: Shape sh
                => exp sh
                -> PreExp acc seq exp Int

  Intersect     :: Shape sh
                => exp sh
                -> exp sh
                -> PreExp acc seq exp sh

  Union         :: Shape sh
                => exp sh
                -> exp sh
                -> PreExp acc seq exp sh

  Foreign       :: (Elt x, Elt y, Foreign asm)
                => asm (x -> y)
                -> (Exp x -> Exp y) -- RCE: Using Exp instead of exp to aid in sharing recovery.
                -> exp x
                -> PreExp acc seq exp y


-- Smart constructors and destructors for array tuples
-- ---------------------------------------------------

atup2 :: (Arrays a, Arrays b) => (Acc a, Acc b) -> Acc (a, b)
atup2 (a, b) = Acc $ Atuple (NilAtup `SnocAtup` a `SnocAtup` b)

atup3 :: (Arrays a, Arrays b, Arrays c) => (Acc a, Acc b, Acc c) -> Acc (a, b, c)
atup3 (a, b, c) = Acc $ Atuple (NilAtup `SnocAtup` a `SnocAtup` b `SnocAtup` c)

atup4 :: (Arrays a, Arrays b, Arrays c, Arrays d)
      => (Acc a, Acc b, Acc c, Acc d) -> Acc (a, b, c, d)
atup4 (a, b, c, d)
  = Acc $ Atuple (NilAtup `SnocAtup` a `SnocAtup` b `SnocAtup` c `SnocAtup` d)

atup5 :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e)
      => (Acc a, Acc b, Acc c, Acc d, Acc e) -> Acc (a, b, c, d, e)
atup5 (a, b, c, d, e)
  = Acc $ Atuple $
      NilAtup `SnocAtup` a `SnocAtup` b `SnocAtup` c `SnocAtup` d `SnocAtup` e

atup6 :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f)
      => (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f) -> Acc (a, b, c, d, e, f)
atup6 (a, b, c, d, e, f)
  = Acc $ Atuple $
      NilAtup `SnocAtup` a `SnocAtup` b `SnocAtup` c
              `SnocAtup` d `SnocAtup` e `SnocAtup` f

atup7 :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g)
      => (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g)
      -> Acc (a, b, c, d, e, f, g)
atup7 (a, b, c, d, e, f, g)
  = Acc $ Atuple $
      NilAtup `SnocAtup` a `SnocAtup` b `SnocAtup` c
              `SnocAtup` d `SnocAtup` e `SnocAtup` f `SnocAtup` g

atup8 :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h)
      => (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h)
      -> Acc (a, b, c, d, e, f, g, h)
atup8 (a, b, c, d, e, f, g, h)
  = Acc $ Atuple $
      NilAtup `SnocAtup` a `SnocAtup` b `SnocAtup` c `SnocAtup` d
              `SnocAtup` e `SnocAtup` f `SnocAtup` g `SnocAtup` h

atup9 :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i)
      => (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h, Acc i)
      -> Acc (a, b, c, d, e, f, g, h, i)
atup9 (a, b, c, d, e, f, g, h, i)
  = Acc $ Atuple $
      NilAtup `SnocAtup` a `SnocAtup` b `SnocAtup` c `SnocAtup` d
              `SnocAtup` e `SnocAtup` f `SnocAtup` g `SnocAtup` h `SnocAtup` i

atup10 :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i, Arrays j)
       => (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h, Acc i, Acc j)
       -> Acc (a, b, c, d, e, f, g, h, i, j)
atup10 (a, b, c, d, e, f, g, h, i, j)
  = Acc $ Atuple $
      NilAtup `SnocAtup` a `SnocAtup` b `SnocAtup` c `SnocAtup` d `SnocAtup` e
              `SnocAtup` f `SnocAtup` g `SnocAtup` h `SnocAtup` i `SnocAtup` j

atup11 :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i, Arrays j, Arrays k)
       => (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h, Acc i, Acc j, Acc k)
       -> Acc (a, b, c, d, e, f, g, h, i, j, k)
atup11 (a, b, c, d, e, f, g, h, i, j, k)
  = Acc $ Atuple $
      NilAtup `SnocAtup` a `SnocAtup` b `SnocAtup` c `SnocAtup` d `SnocAtup` e
              `SnocAtup` f `SnocAtup` g `SnocAtup` h `SnocAtup` i `SnocAtup` j `SnocAtup` k

atup12 :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i, Arrays j, Arrays k, Arrays l)
       => (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h, Acc i, Acc j, Acc k, Acc l)
       -> Acc (a, b, c, d, e, f, g, h, i, j, k, l)
atup12 (a, b, c, d, e, f, g, h, i, j, k, l)
  = Acc $ Atuple $
      NilAtup `SnocAtup` a `SnocAtup` b `SnocAtup` c `SnocAtup` d `SnocAtup` e `SnocAtup` f
              `SnocAtup` g `SnocAtup` h `SnocAtup` i `SnocAtup` j `SnocAtup` k `SnocAtup` l

atup13 :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i, Arrays j, Arrays k, Arrays l, Arrays m)
       => (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h, Acc i, Acc j, Acc k, Acc l, Acc m)
       -> Acc (a, b, c, d, e, f, g, h, i, j, k, l, m)
atup13 (a, b, c, d, e, f, g, h, i, j, k, l, m)
  = Acc $ Atuple $
      NilAtup `SnocAtup` a `SnocAtup` b `SnocAtup` c `SnocAtup` d `SnocAtup` e `SnocAtup` f
              `SnocAtup` g `SnocAtup` h `SnocAtup` i `SnocAtup` j `SnocAtup` k `SnocAtup` l `SnocAtup` m

atup14 :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i, Arrays j, Arrays k, Arrays l, Arrays m, Arrays n)
       => (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h, Acc i, Acc j, Acc k, Acc l, Acc m, Acc n)
       -> Acc (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
atup14 (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
  = Acc $ Atuple $
      NilAtup `SnocAtup` a `SnocAtup` b `SnocAtup` c `SnocAtup` d `SnocAtup` e `SnocAtup` f `SnocAtup` g
              `SnocAtup` h `SnocAtup` i `SnocAtup` j `SnocAtup` k `SnocAtup` l `SnocAtup` m `SnocAtup` n

atup15 :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i, Arrays j, Arrays k, Arrays l, Arrays m, Arrays n, Arrays o)
       => (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h, Acc i, Acc j, Acc k, Acc l, Acc m, Acc n, Acc o)
       -> Acc (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
atup15 (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
  = Acc $ Atuple $
      NilAtup `SnocAtup` a `SnocAtup` b `SnocAtup` c `SnocAtup` d `SnocAtup` e `SnocAtup` f `SnocAtup` g
              `SnocAtup` h `SnocAtup` i `SnocAtup` j `SnocAtup` k `SnocAtup` l `SnocAtup` m `SnocAtup` n `SnocAtup` o

unatup2 :: (Arrays a, Arrays b) => Acc (a, b) -> (Acc a, Acc b)
unatup2 e =
  ( Acc $ SuccTupIdx ZeroTupIdx `Aprj` e
  , Acc $ ZeroTupIdx `Aprj` e )

unatup3 :: (Arrays a, Arrays b, Arrays c) => Acc (a, b, c) -> (Acc a, Acc b, Acc c)
unatup3 e =
  ( Acc $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Aprj` e
  , Acc $ SuccTupIdx ZeroTupIdx `Aprj` e
  , Acc $ ZeroTupIdx `Aprj` e )

unatup4
    :: (Arrays a, Arrays b, Arrays c, Arrays d)
    => Acc (a, b, c, d) -> (Acc a, Acc b, Acc c, Acc d)
unatup4 e =
  ( Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Aprj` e
  , Acc $ SuccTupIdx ZeroTupIdx `Aprj` e
  , Acc $ ZeroTupIdx `Aprj` e )

unatup5
    :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e)
    => Acc (a, b, c, d, e) -> (Acc a, Acc b, Acc c, Acc d, Acc e)
unatup5 e =
  ( Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Aprj` e
  , Acc $ SuccTupIdx ZeroTupIdx `Aprj` e
  , Acc $ ZeroTupIdx `Aprj` e )

unatup6
    :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f)
    => Acc (a, b, c, d, e, f) -> (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f)
unatup6 e =
  ( Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Aprj` e
  , Acc $ SuccTupIdx ZeroTupIdx `Aprj` e
  , Acc $ ZeroTupIdx `Aprj` e )

unatup7
    :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g)
    => Acc (a, b, c, d, e, f, g) -> (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g)
unatup7 e =
  ( Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Aprj` e
  , Acc $ SuccTupIdx ZeroTupIdx `Aprj` e
  , Acc $ ZeroTupIdx `Aprj` e )

unatup8
    :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h)
    => Acc (a, b, c, d, e, f, g, h) -> (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h)
unatup8 e =
  ( Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Aprj` e
  , Acc $ SuccTupIdx ZeroTupIdx `Aprj` e
  , Acc $ ZeroTupIdx `Aprj` e )

unatup9
    :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i)
    => Acc (a, b, c, d, e, f, g, h, i) -> (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h, Acc i)
unatup9 e =
  ( Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Aprj` e
  , Acc $ SuccTupIdx ZeroTupIdx `Aprj` e
  , Acc $ ZeroTupIdx `Aprj` e )

unatup10 :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i, Arrays j)
         => Acc (a, b, c, d, e, f, g, h, i, j) -> (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h, Acc i, Acc j)
unatup10 e =
  ( Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Aprj` e
  , Acc $ SuccTupIdx ZeroTupIdx `Aprj` e
  , Acc $ ZeroTupIdx `Aprj` e)

unatup11 :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i, Arrays j, Arrays k)
         => Acc (a, b, c, d, e, f, g, h, i, j, k) -> (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h, Acc i, Acc j, Acc k)
unatup11 e =
  ( Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Aprj` e
  , Acc $ SuccTupIdx ZeroTupIdx `Aprj` e
  , Acc $ ZeroTupIdx `Aprj` e)

unatup12 :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i, Arrays j, Arrays k, Arrays l)
         => Acc (a, b, c, d, e, f, g, h, i, j, k, l) -> (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h, Acc i, Acc j, Acc k, Acc l)
unatup12 e =
  ( Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Aprj` e
  , Acc $ SuccTupIdx ZeroTupIdx `Aprj` e
  , Acc $ ZeroTupIdx `Aprj` e)

unatup13 :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i, Arrays j, Arrays k, Arrays l, Arrays m)
         => Acc (a, b, c, d, e, f, g, h, i, j, k, l, m) -> (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h, Acc i, Acc j, Acc k, Acc l, Acc m)
unatup13 e =
  ( Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))))))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Aprj` e
  , Acc $ SuccTupIdx ZeroTupIdx `Aprj` e
  , Acc $ ZeroTupIdx `Aprj` e)

unatup14 :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i, Arrays j, Arrays k, Arrays l, Arrays m, Arrays n)
         => Acc (a, b, c, d, e, f, g, h, i, j, k, l, m, n) -> (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h, Acc i, Acc j, Acc k, Acc l, Acc m, Acc n)
unatup14 e =
  ( Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))))))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))))))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Aprj` e
  , Acc $ SuccTupIdx ZeroTupIdx `Aprj` e
  , Acc $ ZeroTupIdx `Aprj` e)

unatup15 :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i, Arrays j, Arrays k, Arrays l, Arrays m, Arrays n, Arrays o)
         => Acc (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) -> (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h, Acc i, Acc j, Acc k, Acc l, Acc m, Acc n, Acc o)
unatup15 e =
  ( Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))))))))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))))))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))))))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Aprj` e
  , Acc $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Aprj` e
  , Acc $ SuccTupIdx ZeroTupIdx `Aprj` e
  , Acc $ ZeroTupIdx `Aprj` e)



-- Smart constructors for stencil reification
-- ------------------------------------------

-- Stencil reification
--
-- In the AST representation, we turn the stencil type from nested tuples of Accelerate expressions
-- into an Accelerate expression whose type is a tuple nested in the same manner.  This enables us
-- to represent the stencil function as a unary function (which also only needs one de Bruijn
-- index). The various positions in the stencil are accessed via tuple indices (i.e., projections).

class (Elt (StencilRepr sh stencil), AST.Stencil sh a (StencilRepr sh stencil))
  => Stencil sh a stencil where
  type StencilRepr sh stencil :: *
  stencilPrj :: sh{-dummy-} -> a{-dummy-} -> Exp (StencilRepr sh stencil) -> stencil

-- DIM1
instance Elt e => Stencil DIM1 e (Exp e, Exp e, Exp e) where
  type StencilRepr DIM1 (Exp e, Exp e, Exp e)
    = (e, e, e)
  stencilPrj _ _ s = (Exp $ Prj tix2 s,
                      Exp $ Prj tix1 s,
                      Exp $ Prj tix0 s)
instance Elt e => Stencil DIM1 e (Exp e, Exp e, Exp e, Exp e, Exp e) where
  type StencilRepr DIM1 (Exp e, Exp e, Exp e, Exp e, Exp e)
    = (e, e, e, e, e)
  stencilPrj _ _ s = (Exp $ Prj tix4 s,
                      Exp $ Prj tix3 s,
                      Exp $ Prj tix2 s,
                      Exp $ Prj tix1 s,
                      Exp $ Prj tix0 s)
instance Elt e => Stencil DIM1 e (Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e) where
  type StencilRepr DIM1 (Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e)
    = (e, e, e, e, e, e, e)
  stencilPrj _ _ s = (Exp $ Prj tix6 s,
                      Exp $ Prj tix5 s,
                      Exp $ Prj tix4 s,
                      Exp $ Prj tix3 s,
                      Exp $ Prj tix2 s,
                      Exp $ Prj tix1 s,
                      Exp $ Prj tix0 s)
instance Elt e => Stencil DIM1 e (Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e)
  where
  type StencilRepr DIM1 (Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e)
    = (e, e, e, e, e, e, e, e, e)
  stencilPrj _ _ s = (Exp $ Prj tix8 s,
                      Exp $ Prj tix7 s,
                      Exp $ Prj tix6 s,
                      Exp $ Prj tix5 s,
                      Exp $ Prj tix4 s,
                      Exp $ Prj tix3 s,
                      Exp $ Prj tix2 s,
                      Exp $ Prj tix1 s,
                      Exp $ Prj tix0 s)

-- DIM(n+1)
instance (Stencil (sh:.Int) a row2,
          Stencil (sh:.Int) a row1,
          Stencil (sh:.Int) a row0) => Stencil (sh:.Int:.Int) a (row2, row1, row0) where
  type StencilRepr (sh:.Int:.Int) (row2, row1, row0)
    = (StencilRepr (sh:.Int) row2, StencilRepr (sh:.Int) row1, StencilRepr (sh:.Int) row0)
  stencilPrj _ a s = (stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix2 s),
                      stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix1 s),
                      stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix0 s))
instance (Stencil (sh:.Int) a row1,
          Stencil (sh:.Int) a row2,
          Stencil (sh:.Int) a row3,
          Stencil (sh:.Int) a row4,
          Stencil (sh:.Int) a row5) => Stencil (sh:.Int:.Int) a (row1, row2, row3, row4, row5) where
  type StencilRepr (sh:.Int:.Int) (row1, row2, row3, row4, row5)
    = (StencilRepr (sh:.Int) row1, StencilRepr (sh:.Int) row2, StencilRepr (sh:.Int) row3,
       StencilRepr (sh:.Int) row4, StencilRepr (sh:.Int) row5)
  stencilPrj _ a s = (stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix4 s),
                      stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix3 s),
                      stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix2 s),
                      stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix1 s),
                      stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix0 s))
instance (Stencil (sh:.Int) a row1,
          Stencil (sh:.Int) a row2,
          Stencil (sh:.Int) a row3,
          Stencil (sh:.Int) a row4,
          Stencil (sh:.Int) a row5,
          Stencil (sh:.Int) a row6,
          Stencil (sh:.Int) a row7)
  => Stencil (sh:.Int:.Int) a (row1, row2, row3, row4, row5, row6, row7) where
  type StencilRepr (sh:.Int:.Int) (row1, row2, row3, row4, row5, row6, row7)
    = (StencilRepr (sh:.Int) row1, StencilRepr (sh:.Int) row2, StencilRepr (sh:.Int) row3,
       StencilRepr (sh:.Int) row4, StencilRepr (sh:.Int) row5, StencilRepr (sh:.Int) row6,
       StencilRepr (sh:.Int) row7)
  stencilPrj _ a s = (stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix6 s),
                      stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix5 s),
                      stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix4 s),
                      stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix3 s),
                      stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix2 s),
                      stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix1 s),
                      stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix0 s))
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
  type StencilRepr (sh:.Int:.Int) (row1, row2, row3, row4, row5, row6, row7, row8, row9)
    = (StencilRepr (sh:.Int) row1, StencilRepr (sh:.Int) row2, StencilRepr (sh:.Int) row3,
       StencilRepr (sh:.Int) row4, StencilRepr (sh:.Int) row5, StencilRepr (sh:.Int) row6,
       StencilRepr (sh:.Int) row7, StencilRepr (sh:.Int) row8, StencilRepr (sh:.Int) row9)
  stencilPrj _ a s = (stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix8 s),
                      stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix7 s),
                      stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix6 s),
                      stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix5 s),
                      stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix4 s),
                      stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix3 s),
                      stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix2 s),
                      stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix1 s),
                      stencilPrj (undefined::(sh:.Int)) a (Exp $ Prj tix0 s))

-- Auxiliary tuple index constants
--
tix0 :: TupleIdx (t, s0) s0
tix0 = ZeroTupIdx

tix1 :: TupleIdx ((t, s1), s0) s1
tix1 = SuccTupIdx tix0

tix2 :: TupleIdx (((t, s2), s1), s0) s2
tix2 = SuccTupIdx tix1

tix3 :: TupleIdx ((((t, s3), s2), s1), s0) s3
tix3 = SuccTupIdx tix2

tix4 :: TupleIdx (((((t, s4), s3), s2), s1), s0) s4
tix4 = SuccTupIdx tix3

tix5 :: TupleIdx ((((((t, s5), s4), s3), s2), s1), s0) s5
tix5 = SuccTupIdx tix4

tix6 :: TupleIdx (((((((t, s6), s5), s4), s3), s2), s1), s0) s6
tix6 = SuccTupIdx tix5

tix7 :: TupleIdx ((((((((t, s7), s6), s5), s4), s3), s2), s1), s0) s7
tix7 = SuccTupIdx tix6

tix8 :: TupleIdx (((((((((t, s8), s7), s6), s5), s4), s3), s2), s1), s0) s8
tix8 = SuccTupIdx tix7

-- Smart constructors for array tuples in sequence computations
-- ---------------------------------------------------

stup2 :: (Arrays a, Arrays b) => (Seq a, Seq b) -> Seq (a, b)
stup2 (a, b) = Seq $ Stuple (NilAtup `SnocAtup` a `SnocAtup` b)

stup3 :: (Arrays a, Arrays b, Arrays c) => (Seq a, Seq b, Seq c) -> Seq (a, b, c)
stup3 (a, b, c) = Seq $ Stuple (NilAtup `SnocAtup` a `SnocAtup` b `SnocAtup` c)

stup4 :: (Arrays a, Arrays b, Arrays c, Arrays d)
      => (Seq a, Seq b, Seq c, Seq d) -> Seq (a, b, c, d)
stup4 (a, b, c, d)
  = Seq $ Stuple (NilAtup `SnocAtup` a `SnocAtup` b `SnocAtup` c `SnocAtup` d)

stup5 :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e)
      => (Seq a, Seq b, Seq c, Seq d, Seq e) -> Seq (a, b, c, d, e)
stup5 (a, b, c, d, e)
  = Seq $ Stuple $
      NilAtup `SnocAtup` a `SnocAtup` b `SnocAtup` c `SnocAtup` d `SnocAtup` e

stup6 :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f)
      => (Seq a, Seq b, Seq c, Seq d, Seq e, Seq f) -> Seq (a, b, c, d, e, f)
stup6 (a, b, c, d, e, f)
  = Seq $ Stuple $
      NilAtup `SnocAtup` a `SnocAtup` b `SnocAtup` c
              `SnocAtup` d `SnocAtup` e `SnocAtup` f

stup7 :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g)
      => (Seq a, Seq b, Seq c, Seq d, Seq e, Seq f, Seq g)
      -> Seq (a, b, c, d, e, f, g)
stup7 (a, b, c, d, e, f, g)
  = Seq $ Stuple $
      NilAtup `SnocAtup` a `SnocAtup` b `SnocAtup` c
              `SnocAtup` d `SnocAtup` e `SnocAtup` f `SnocAtup` g

stup8 :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h)
      => (Seq a, Seq b, Seq c, Seq d, Seq e, Seq f, Seq g, Seq h)
      -> Seq (a, b, c, d, e, f, g, h)
stup8 (a, b, c, d, e, f, g, h)
  = Seq $ Stuple $
      NilAtup `SnocAtup` a `SnocAtup` b `SnocAtup` c `SnocAtup` d
              `SnocAtup` e `SnocAtup` f `SnocAtup` g `SnocAtup` h

stup9 :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i)
      => (Seq a, Seq b, Seq c, Seq d, Seq e, Seq f, Seq g, Seq h, Seq i)
      -> Seq (a, b, c, d, e, f, g, h, i)
stup9 (a, b, c, d, e, f, g, h, i)
  = Seq $ Stuple $
      NilAtup `SnocAtup` a `SnocAtup` b `SnocAtup` c `SnocAtup` d
              `SnocAtup` e `SnocAtup` f `SnocAtup` g `SnocAtup` h `SnocAtup` i

stup10 :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i, Arrays j)
       => (Seq a, Seq b, Seq c, Seq d, Seq e, Seq f, Seq g, Seq h, Seq i, Seq j)
       -> Seq (a, b, c, d, e, f, g, h, i, j)
stup10 (a, b, c, d, e, f, g, h, i, j)
  = Seq $ Stuple $
      NilAtup `SnocAtup` a `SnocAtup` b `SnocAtup` c `SnocAtup` d `SnocAtup` e
              `SnocAtup` f `SnocAtup` g `SnocAtup` h `SnocAtup` i `SnocAtup` j

stup11 :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i, Arrays j, Arrays k)
       => (Seq a, Seq b, Seq c, Seq d, Seq e, Seq f, Seq g, Seq h, Seq i, Seq j, Seq k)
       -> Seq (a, b, c, d, e, f, g, h, i, j, k)
stup11 (a, b, c, d, e, f, g, h, i, j, k)
  = Seq $ Stuple $
      NilAtup `SnocAtup` a `SnocAtup` b `SnocAtup` c `SnocAtup` d `SnocAtup` e
              `SnocAtup` f `SnocAtup` g `SnocAtup` h `SnocAtup` i `SnocAtup` j `SnocAtup` k

stup12 :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i, Arrays j, Arrays k, Arrays l)
       => (Seq a, Seq b, Seq c, Seq d, Seq e, Seq f, Seq g, Seq h, Seq i, Seq j, Seq k, Seq l)
       -> Seq (a, b, c, d, e, f, g, h, i, j, k, l)
stup12 (a, b, c, d, e, f, g, h, i, j, k, l)
  = Seq $ Stuple $
      NilAtup `SnocAtup` a `SnocAtup` b `SnocAtup` c `SnocAtup` d `SnocAtup` e `SnocAtup` f
              `SnocAtup` g `SnocAtup` h `SnocAtup` i `SnocAtup` j `SnocAtup` k `SnocAtup` l

stup13 :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i, Arrays j, Arrays k, Arrays l, Arrays m)
       => (Seq a, Seq b, Seq c, Seq d, Seq e, Seq f, Seq g, Seq h, Seq i, Seq j, Seq k, Seq l, Seq m)
       -> Seq (a, b, c, d, e, f, g, h, i, j, k, l, m)
stup13 (a, b, c, d, e, f, g, h, i, j, k, l, m)
  = Seq $ Stuple $
      NilAtup `SnocAtup` a `SnocAtup` b `SnocAtup` c `SnocAtup` d `SnocAtup` e `SnocAtup` f
              `SnocAtup` g `SnocAtup` h `SnocAtup` i `SnocAtup` j `SnocAtup` k `SnocAtup` l `SnocAtup` m

stup14 :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i, Arrays j, Arrays k, Arrays l, Arrays m, Arrays n)
       => (Seq a, Seq b, Seq c, Seq d, Seq e, Seq f, Seq g, Seq h, Seq i, Seq j, Seq k, Seq l, Seq m, Seq n)
       -> Seq (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
stup14 (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
  = Seq $ Stuple $
      NilAtup `SnocAtup` a `SnocAtup` b `SnocAtup` c `SnocAtup` d `SnocAtup` e `SnocAtup` f `SnocAtup` g
              `SnocAtup` h `SnocAtup` i `SnocAtup` j `SnocAtup` k `SnocAtup` l `SnocAtup` m `SnocAtup` n

stup15 :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i, Arrays j, Arrays k, Arrays l, Arrays m, Arrays n, Arrays o)
       => (Seq a, Seq b, Seq c, Seq d, Seq e, Seq f, Seq g, Seq h, Seq i, Seq j, Seq k, Seq l, Seq m, Seq n, Seq o)
       -> Seq (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
stup15 (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
  = Seq $ Stuple $
      NilAtup `SnocAtup` a `SnocAtup` b `SnocAtup` c `SnocAtup` d `SnocAtup` e `SnocAtup` f `SnocAtup` g
              `SnocAtup` h `SnocAtup` i `SnocAtup` j `SnocAtup` k `SnocAtup` l `SnocAtup` m `SnocAtup` n `SnocAtup` o

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
constant :: Elt t => t -> Exp t
constant = Exp . Const

-- Smart constructor and destructors for scalar tuples
--
tup2 :: (Elt a, Elt b) => (Exp a, Exp b) -> Exp (a, b)
tup2 (a, b) = Exp $ Tuple (NilTup `SnocTup` a `SnocTup` b)

tup3 :: (Elt a, Elt b, Elt c) => (Exp a, Exp b, Exp c) -> Exp (a, b, c)
tup3 (a, b, c) = Exp $ Tuple (NilTup `SnocTup` a `SnocTup` b `SnocTup` c)

tup4 :: (Elt a, Elt b, Elt c, Elt d)
     => (Exp a, Exp b, Exp c, Exp d) -> Exp (a, b, c, d)
tup4 (a, b, c, d)
  = Exp $ Tuple (NilTup `SnocTup` a `SnocTup` b `SnocTup` c `SnocTup` d)

tup5 :: (Elt a, Elt b, Elt c, Elt d, Elt e)
     => (Exp a, Exp b, Exp c, Exp d, Exp e) -> Exp (a, b, c, d, e)
tup5 (a, b, c, d, e)
  = Exp $ Tuple $
      NilTup `SnocTup` a `SnocTup` b `SnocTup` c `SnocTup` d `SnocTup` e

tup6 :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f)
     => (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f) -> Exp (a, b, c, d, e, f)
tup6 (a, b, c, d, e, f)
  = Exp $ Tuple $
      NilTup `SnocTup` a `SnocTup` b `SnocTup` c `SnocTup` d `SnocTup` e `SnocTup` f

tup7 :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g)
     => (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g)
     -> Exp (a, b, c, d, e, f, g)
tup7 (a, b, c, d, e, f, g)
  = Exp $ Tuple $
      NilTup `SnocTup` a `SnocTup` b `SnocTup` c
             `SnocTup` d `SnocTup` e `SnocTup` f `SnocTup` g

tup8 :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h)
     => (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h)
     -> Exp (a, b, c, d, e, f, g, h)
tup8 (a, b, c, d, e, f, g, h)
  = Exp $ Tuple $
      NilTup `SnocTup` a `SnocTup` b `SnocTup` c `SnocTup` d
             `SnocTup` e `SnocTup` f `SnocTup` g `SnocTup` h

tup9 :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i)
     => (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i)
     -> Exp (a, b, c, d, e, f, g, h, i)
tup9 (a, b, c, d, e, f, g, h, i)
  = Exp $ Tuple $
      NilTup `SnocTup` a `SnocTup` b `SnocTup` c `SnocTup` d
             `SnocTup` e `SnocTup` f `SnocTup` g `SnocTup` h `SnocTup` i

tup10 :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j)
      => (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i, Exp j)
      -> Exp (a, b, c, d, e, f, g, h, i, j)
tup10 (a, b, c, d, e, f, g, h, i, j)
  = Exp $ Tuple $
      NilTup `SnocTup` a `SnocTup` b `SnocTup` c `SnocTup` d `SnocTup` e
             `SnocTup` f `SnocTup` g `SnocTup` h `SnocTup` i `SnocTup` j

tup11 :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j, Elt k)
      => (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i, Exp j, Exp k)
      -> Exp (a, b, c, d, e, f, g, h, i, j, k)
tup11 (a, b, c, d, e, f, g, h, i, j, k)
  = Exp $ Tuple $
      NilTup `SnocTup` a `SnocTup` b `SnocTup` c `SnocTup` d `SnocTup` e
             `SnocTup` f `SnocTup` g `SnocTup` h `SnocTup` i `SnocTup` j `SnocTup` k

tup12 :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j, Elt k, Elt l)
      => (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i, Exp j, Exp k, Exp l)
      -> Exp (a, b, c, d, e, f, g, h, i, j, k, l)
tup12 (a, b, c, d, e, f, g, h, i, j, k, l)
  = Exp $ Tuple $
      NilTup `SnocTup` a `SnocTup` b `SnocTup` c `SnocTup` d `SnocTup` e `SnocTup` f
             `SnocTup` g `SnocTup` h `SnocTup` i `SnocTup` j `SnocTup` k `SnocTup` l

tup13 :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j, Elt k, Elt l, Elt m)
      => (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i, Exp j, Exp k, Exp l, Exp m)
      -> Exp (a, b, c, d, e, f, g, h, i, j, k, l, m)
tup13 (a, b, c, d, e, f, g, h, i, j, k, l, m)
  = Exp $ Tuple $
      NilTup `SnocTup` a `SnocTup` b `SnocTup` c `SnocTup` d `SnocTup` e `SnocTup` f
             `SnocTup` g `SnocTup` h `SnocTup` i `SnocTup` j `SnocTup` k `SnocTup` l `SnocTup` m

tup14 :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j, Elt k, Elt l, Elt m, Elt n)
      => (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i, Exp j, Exp k, Exp l, Exp m, Exp n)
      -> Exp (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
tup14 (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
  = Exp $ Tuple $
      NilTup `SnocTup` a `SnocTup` b `SnocTup` c `SnocTup` d `SnocTup` e `SnocTup` f `SnocTup` g
             `SnocTup` h `SnocTup` i `SnocTup` j `SnocTup` k `SnocTup` l `SnocTup` m `SnocTup` n

tup15 :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j, Elt k, Elt l, Elt m, Elt n, Elt o)
      => (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i, Exp j, Exp k, Exp l, Exp m, Exp n, Exp o)
      -> Exp (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
tup15 (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
  = Exp $ Tuple $
      NilTup `SnocTup` a `SnocTup` b `SnocTup` c `SnocTup` d `SnocTup` e `SnocTup` f `SnocTup` g
             `SnocTup` h `SnocTup` i `SnocTup` j `SnocTup` k `SnocTup` l `SnocTup` m `SnocTup` n `SnocTup` o

untup2 :: (Elt a, Elt b) => Exp (a, b) -> (Exp a, Exp b)
untup2 e =
  ( Exp $ SuccTupIdx ZeroTupIdx `Prj` e
  , Exp $ ZeroTupIdx `Prj` e )

untup3 :: (Elt a, Elt b, Elt c) => Exp (a, b, c) -> (Exp a, Exp b, Exp c)
untup3 e =
  ( Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` e
  , Exp $ SuccTupIdx ZeroTupIdx `Prj` e
  , Exp $ ZeroTupIdx `Prj` e)

untup4 :: (Elt a, Elt b, Elt c, Elt d)
       => Exp (a, b, c, d) -> (Exp a, Exp b, Exp c, Exp d)
untup4 e =
  ( Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` e
  , Exp $ SuccTupIdx ZeroTupIdx `Prj` e
  , Exp $ ZeroTupIdx `Prj` e)

untup5 :: (Elt a, Elt b, Elt c, Elt d, Elt e)
       => Exp (a, b, c, d, e) -> (Exp a, Exp b, Exp c, Exp d, Exp e)
untup5 e =
  ( Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` e
  , Exp $ SuccTupIdx ZeroTupIdx `Prj` e
  , Exp $ ZeroTupIdx `Prj` e)

untup6 :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f)
       => Exp (a, b, c, d, e, f) -> (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f)
untup6 e =
  ( Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` e
  , Exp $ SuccTupIdx ZeroTupIdx `Prj` e
  , Exp $ ZeroTupIdx `Prj` e)

untup7 :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g)
       => Exp (a, b, c, d, e, f, g) -> (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g)
untup7 e =
  ( Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` e
  , Exp $ SuccTupIdx ZeroTupIdx `Prj` e
  , Exp $ ZeroTupIdx `Prj` e)

untup8 :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h)
       => Exp (a, b, c, d, e, f, g, h) -> (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h)
untup8 e =
  ( Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` e
  , Exp $ SuccTupIdx ZeroTupIdx `Prj` e
  , Exp $ ZeroTupIdx `Prj` e)

untup9 :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i)
       => Exp (a, b, c, d, e, f, g, h, i) -> (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i)
untup9 e =
  ( Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` e
  , Exp $ SuccTupIdx ZeroTupIdx `Prj` e
  , Exp $ ZeroTupIdx `Prj` e)

untup10 :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j)
        => Exp (a, b, c, d, e, f, g, h, i, j) -> (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i, Exp j)
untup10 e =
  ( Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` e
  , Exp $ SuccTupIdx ZeroTupIdx `Prj` e
  , Exp $ ZeroTupIdx `Prj` e)

untup11 :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j, Elt k)
        => Exp (a, b, c, d, e, f, g, h, i, j, k) -> (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i, Exp j, Exp k)
untup11 e =
  ( Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` e
  , Exp $ SuccTupIdx ZeroTupIdx `Prj` e
  , Exp $ ZeroTupIdx `Prj` e)

untup12 :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j, Elt k, Elt l)
        => Exp (a, b, c, d, e, f, g, h, i, j, k, l) -> (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i, Exp j, Exp k, Exp l)
untup12 e =
  ( Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` e
  , Exp $ SuccTupIdx ZeroTupIdx `Prj` e
  , Exp $ ZeroTupIdx `Prj` e)

untup13 :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j, Elt k, Elt l, Elt m)
        => Exp (a, b, c, d, e, f, g, h, i, j, k, l, m) -> (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i, Exp j, Exp k, Exp l, Exp m)
untup13 e =
  ( Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))))))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` e
  , Exp $ SuccTupIdx ZeroTupIdx `Prj` e
  , Exp $ ZeroTupIdx `Prj` e)

untup14 :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j, Elt k, Elt l, Elt m, Elt n)
        => Exp (a, b, c, d, e, f, g, h, i, j, k, l, m, n) -> (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i, Exp j, Exp k, Exp l, Exp m, Exp n)
untup14 e =
  ( Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))))))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))))))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` e
  , Exp $ SuccTupIdx ZeroTupIdx `Prj` e
  , Exp $ ZeroTupIdx `Prj` e)

untup15 :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j, Elt k, Elt l, Elt m, Elt n, Elt o)
        => Exp (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) -> (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i, Exp j, Exp k, Exp l, Exp m, Exp n, Exp o)
untup15 e =
  ( Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))))))))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))))))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))))))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` e
  , Exp $ SuccTupIdx ZeroTupIdx `Prj` e
  , Exp $ ZeroTupIdx `Prj` e)

-- Smart constructor for constants
--

mkMinBound :: (Elt t, IsBounded t) => Exp t
mkMinBound = Exp $ PrimConst (PrimMinBound boundedType)

mkMaxBound :: (Elt t, IsBounded t) => Exp t
mkMaxBound = Exp $ PrimConst (PrimMaxBound boundedType)

mkPi :: (Elt r, IsFloating r) => Exp r
mkPi = Exp $ PrimConst (PrimPi floatingType)


-- Smart constructors for primitive applications
--

-- Operators from Floating

mkSin :: (Elt t, IsFloating t) => Exp t -> Exp t
mkSin x = Exp $ PrimSin floatingType `PrimApp` x

mkCos :: (Elt t, IsFloating t) => Exp t -> Exp t
mkCos x = Exp $ PrimCos floatingType `PrimApp` x

mkTan :: (Elt t, IsFloating t) => Exp t -> Exp t
mkTan x = Exp $ PrimTan floatingType `PrimApp` x

mkAsin :: (Elt t, IsFloating t) => Exp t -> Exp t
mkAsin x = Exp $ PrimAsin floatingType `PrimApp` x

mkAcos :: (Elt t, IsFloating t) => Exp t -> Exp t
mkAcos x = Exp $ PrimAcos floatingType `PrimApp` x

mkAtan :: (Elt t, IsFloating t) => Exp t -> Exp t
mkAtan x = Exp $ PrimAtan floatingType `PrimApp` x

mkSinh :: (Elt t, IsFloating t) => Exp t -> Exp t
mkSinh x = Exp $ PrimSinh floatingType `PrimApp` x

mkCosh :: (Elt t, IsFloating t) => Exp t -> Exp t
mkCosh x = Exp $ PrimCosh floatingType `PrimApp` x

mkTanh :: (Elt t, IsFloating t) => Exp t -> Exp t
mkTanh x = Exp $ PrimTanh floatingType `PrimApp` x

mkAsinh :: (Elt t, IsFloating t) => Exp t -> Exp t
mkAsinh x = Exp $ PrimAsinh floatingType `PrimApp` x

mkAcosh :: (Elt t, IsFloating t) => Exp t -> Exp t
mkAcosh x = Exp $ PrimAcosh floatingType `PrimApp` x

mkAtanh :: (Elt t, IsFloating t) => Exp t -> Exp t
mkAtanh x = Exp $ PrimAtanh floatingType `PrimApp` x

mkExpFloating :: (Elt t, IsFloating t) => Exp t -> Exp t
mkExpFloating x = Exp $ PrimExpFloating floatingType `PrimApp` x

mkSqrt :: (Elt t, IsFloating t) => Exp t -> Exp t
mkSqrt x = Exp $ PrimSqrt floatingType `PrimApp` x

mkLog :: (Elt t, IsFloating t) => Exp t -> Exp t
mkLog x = Exp $ PrimLog floatingType `PrimApp` x

mkFPow :: (Elt t, IsFloating t) => Exp t -> Exp t -> Exp t
mkFPow x y = Exp $ PrimFPow floatingType `PrimApp` tup2 (x, y)

mkLogBase :: (Elt t, IsFloating t) => Exp t -> Exp t -> Exp t
mkLogBase x y = Exp $ PrimLogBase floatingType `PrimApp` tup2 (x, y)

-- Operators from Num

mkAdd :: (Elt t, IsNum t) => Exp t -> Exp t -> Exp t
mkAdd x y = Exp $ PrimAdd numType `PrimApp` tup2 (x, y)

mkSub :: (Elt t, IsNum t) => Exp t -> Exp t -> Exp t
mkSub x y = Exp $ PrimSub numType `PrimApp` tup2 (x, y)

mkMul :: (Elt t, IsNum t) => Exp t -> Exp t -> Exp t
mkMul x y = Exp $ PrimMul numType `PrimApp` tup2 (x, y)

mkNeg :: (Elt t, IsNum t) => Exp t -> Exp t
mkNeg x = Exp $ PrimNeg numType `PrimApp` x

mkAbs :: (Elt t, IsNum t) => Exp t -> Exp t
mkAbs x = Exp $ PrimAbs numType `PrimApp` x

mkSig :: (Elt t, IsNum t) => Exp t -> Exp t
mkSig x = Exp $ PrimSig numType `PrimApp` x

-- Operators from Integral

mkQuot :: (Elt t, IsIntegral t) => Exp t -> Exp t -> Exp t
mkQuot x y = Exp $ PrimQuot integralType `PrimApp` tup2 (x, y)

mkRem :: (Elt t, IsIntegral t) => Exp t -> Exp t -> Exp t
mkRem x y = Exp $ PrimRem integralType `PrimApp` tup2 (x, y)

mkQuotRem :: (Elt t, IsIntegral t) => Exp t -> Exp t -> (Exp t, Exp t)
mkQuotRem x y = untup2 $ Exp $ PrimQuotRem integralType `PrimApp` tup2 (x ,y)

mkIDiv :: (Elt t, IsIntegral t) => Exp t -> Exp t -> Exp t
mkIDiv x y = Exp $ PrimIDiv integralType `PrimApp` tup2 (x, y)

mkMod :: (Elt t, IsIntegral t) => Exp t -> Exp t -> Exp t
mkMod x y = Exp $ PrimMod integralType `PrimApp` tup2 (x, y)

mkDivMod :: (Elt t, IsIntegral t) => Exp t -> Exp t -> (Exp t, Exp t)
mkDivMod x y = untup2 $ Exp $ PrimDivMod integralType `PrimApp` tup2 (x ,y)


-- Operators from Bits and FiniteBits

mkBAnd :: (Elt t, IsIntegral t) => Exp t -> Exp t -> Exp t
mkBAnd x y = Exp $ PrimBAnd integralType `PrimApp` tup2 (x, y)

mkBOr :: (Elt t, IsIntegral t) => Exp t -> Exp t -> Exp t
mkBOr x y = Exp $ PrimBOr integralType `PrimApp` tup2 (x, y)

mkBXor :: (Elt t, IsIntegral t) => Exp t -> Exp t -> Exp t
mkBXor x y = Exp $ PrimBXor integralType `PrimApp` tup2 (x, y)

mkBNot :: (Elt t, IsIntegral t) => Exp t -> Exp t
mkBNot x = Exp $ PrimBNot integralType `PrimApp` x

mkBShiftL :: (Elt t, IsIntegral t) => Exp t -> Exp Int -> Exp t
mkBShiftL x i = Exp $ PrimBShiftL integralType `PrimApp` tup2 (x, i)

mkBShiftR :: (Elt t, IsIntegral t) => Exp t -> Exp Int -> Exp t
mkBShiftR x i = Exp $ PrimBShiftR integralType `PrimApp` tup2 (x, i)

mkBRotateL :: (Elt t, IsIntegral t) => Exp t -> Exp Int -> Exp t
mkBRotateL x i = Exp $ PrimBRotateL integralType `PrimApp` tup2 (x, i)

mkBRotateR :: (Elt t, IsIntegral t) => Exp t -> Exp Int -> Exp t
mkBRotateR x i = Exp $ PrimBRotateR integralType `PrimApp` tup2 (x, i)

mkPopCount :: (Elt t, IsIntegral t) => Exp t -> Exp Int
mkPopCount x = Exp $ PrimPopCount integralType `PrimApp` x

mkCountLeadingZeros :: (Elt t, IsIntegral t) => Exp t -> Exp Int
mkCountLeadingZeros x = Exp $ PrimCountLeadingZeros integralType `PrimApp` x

mkCountTrailingZeros :: (Elt t, IsIntegral t) => Exp t -> Exp Int
mkCountTrailingZeros x = Exp $ PrimCountTrailingZeros integralType `PrimApp` x


-- Operators from Fractional

mkFDiv :: (Elt t, IsFloating t) => Exp t -> Exp t -> Exp t
mkFDiv x y = Exp $ PrimFDiv floatingType `PrimApp` tup2 (x, y)

mkRecip :: (Elt t, IsFloating t) => Exp t -> Exp t
mkRecip x = Exp $ PrimRecip floatingType `PrimApp` x

-- Operators from RealFrac

mkTruncate :: (Elt a, Elt b, IsFloating a, IsIntegral b) => Exp a -> Exp b
mkTruncate x = Exp $ PrimTruncate floatingType integralType `PrimApp` x

mkRound :: (Elt a, Elt b, IsFloating a, IsIntegral b) => Exp a -> Exp b
mkRound x = Exp $ PrimRound floatingType integralType `PrimApp` x

mkFloor :: (Elt a, Elt b, IsFloating a, IsIntegral b) => Exp a -> Exp b
mkFloor x = Exp $ PrimFloor floatingType integralType `PrimApp` x

mkCeiling :: (Elt a, Elt b, IsFloating a, IsIntegral b) => Exp a -> Exp b
mkCeiling x = Exp $ PrimCeiling floatingType integralType `PrimApp` x

-- Operators from RealFloat

mkAtan2 :: (Elt t, IsFloating t) => Exp t -> Exp t -> Exp t
mkAtan2 x y = Exp $ PrimAtan2 floatingType `PrimApp` tup2 (x, y)

mkIsNaN :: (Elt t, IsFloating t) => Exp t -> Exp Bool
mkIsNaN x = Exp $ PrimIsNaN floatingType `PrimApp` x

-- FIXME: add missing operations from Floating, RealFrac & RealFloat

-- Relational and equality operators

mkLt :: (Elt t, IsScalar t) => Exp t -> Exp t -> Exp Bool
mkLt x y = Exp $ PrimLt scalarType `PrimApp` tup2 (x, y)

mkGt :: (Elt t, IsScalar t) => Exp t -> Exp t -> Exp Bool
mkGt x y = Exp $ PrimGt scalarType `PrimApp` tup2 (x, y)

mkLtEq :: (Elt t, IsScalar t) => Exp t -> Exp t -> Exp Bool
mkLtEq x y = Exp $ PrimLtEq scalarType `PrimApp` tup2 (x, y)

mkGtEq :: (Elt t, IsScalar t) => Exp t -> Exp t -> Exp Bool
mkGtEq x y = Exp $ PrimGtEq scalarType `PrimApp` tup2 (x, y)

mkEq :: (Elt t, IsScalar t) => Exp t -> Exp t -> Exp Bool
mkEq x y = Exp $ PrimEq scalarType `PrimApp` tup2 (x, y)

mkNEq :: (Elt t, IsScalar t) => Exp t -> Exp t -> Exp Bool
mkNEq x y = Exp $ PrimNEq scalarType `PrimApp` tup2 (x, y)

mkMax :: (Elt t, IsScalar t) => Exp t -> Exp t -> Exp t
mkMax x y = Exp $ PrimMax scalarType `PrimApp` tup2 (x, y)

mkMin :: (Elt t, IsScalar t) => Exp t -> Exp t -> Exp t
mkMin x y = Exp $ PrimMin scalarType `PrimApp` tup2 (x, y)

-- Logical operators

mkLAnd :: Exp Bool -> Exp Bool -> Exp Bool
mkLAnd x y = Exp $ PrimLAnd `PrimApp` tup2 (x, y)

mkLOr :: Exp Bool -> Exp Bool -> Exp Bool
mkLOr x y = Exp $ PrimLOr `PrimApp` tup2 (x, y)

mkLNot :: Exp Bool -> Exp Bool
mkLNot x = Exp $ PrimLNot `PrimApp` x

-- Character conversions

mkOrd :: Exp Char -> Exp Int
mkOrd x = Exp $ PrimOrd `PrimApp` x

mkChr :: Exp Int -> Exp Char
mkChr x = Exp $ PrimChr `PrimApp` x

-- Numeric conversions

mkFromIntegral :: (Elt a, Elt b, IsIntegral a, IsNum b) => Exp a -> Exp b
mkFromIntegral x = Exp $ PrimFromIntegral integralType numType `PrimApp` x

mkToFloating :: (Elt a, Elt b, IsNum a, IsFloating b) => Exp a -> Exp b
mkToFloating x = Exp $ PrimToFloating numType floatingType `PrimApp` x

-- Other conversions

mkBoolToInt :: Exp Bool -> Exp Int
mkBoolToInt b = Exp $ PrimBoolToInt `PrimApp` b

-- NOTE: BitSizeEq constraint is used to make this version "safe"
mkBitcast :: (Elt a, Elt b, IsScalar a, IsScalar b, BitSizeEq a b) => Exp a -> Exp b
mkBitcast = mkUnsafeCoerce

mkUnsafeCoerce :: (Elt a, Elt b, IsScalar a, IsScalar b) => Exp a -> Exp b
mkUnsafeCoerce x = Exp $ PrimCoerce scalarType scalarType `PrimApp` x


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


-- Debugging
-- ---------

showPreAccOp :: forall acc seq exp arrs. PreAcc acc seq exp arrs -> String
showPreAccOp (Atag i)           = "Atag " ++ show i
showPreAccOp (Use a)            = "Use "  ++ showArrays a
showPreAccOp Pipe{}             = "Pipe"
showPreAccOp Acond{}            = "Acond"
showPreAccOp Awhile{}           = "Awhile"
showPreAccOp Atuple{}           = "Atuple"
showPreAccOp Aprj{}             = "Aprj"
showPreAccOp Unit{}             = "Unit"
showPreAccOp Generate{}         = "Generate"
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
showPreAccOp Aforeign{}         = "Aforeign"
showPreAccOp Collect{}          = "Collect"

showPreSeqOp :: PreSeq acc seq exp arrs -> String
showPreSeqOp Stag{}           = "Stag"
showPreSeqOp StreamIn{}       = "StreamIn"
showPreSeqOp Subarrays{}      = "Subarrays"
showPreSeqOp FromSegs{}       = "FromSegs"
showPreSeqOp Produce{}        = "Produce"
showPreSeqOp MapSeq{}         = "MapSeq"
showPreSeqOp ZipWithSeq{}     = "ZipWithSeq"
-- showPreSeqOp MapBatch{}       = "MapBatch"
showPreSeqOp FoldBatch{}      = "FoldBatch"
showPreSeqOp Stuple{}         = "Stuple"
showPreSeqOp Elements{}       = "Elements"
showPreSeqOp Tabulate{}       = "Tabulate"
showPreSeqOp AsSeq{}          = "AsSeq"

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
    l      = toList arr
    cutoff = 5


showPreExpOp :: PreExp acc seq exp t -> String
showPreExpOp (Const c)          = "Const " ++ show c
showPreExpOp (Tag i)            = "Tag" ++ show i
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
showPreExpOp Foreign{}          = "Foreign"
