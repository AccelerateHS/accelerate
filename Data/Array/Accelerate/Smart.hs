{-# LANGUAGE CPP, GADTs, TypeOperators, TypeFamilies, ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, PatternGuards #-}
-- |
-- Module      : Data.Array.Accelerate.Smart
-- Copyright   : [2008..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
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
  Acc(..), PreAcc(..), Exp, PreExp(..), Boundary(..), Stencil(..),

  -- * HOAS -> de Bruijn conversion
  convertAcc, convertAccFun1,

  -- * Smart constructors for pairing and unpairing
  pair, unpair,

  -- * Smart constructors for literals
  constant,

  -- * Smart constructors and destructors for tuples
  tup2, tup3, tup4, tup5, tup6, tup7, tup8, tup9,
  untup2, untup3, untup4, untup5, untup6, untup7, untup8, untup9,

  -- * Smart constructors for constants
  mkMinBound, mkMaxBound, mkPi,
  mkSin, mkCos, mkTan,
  mkAsin, mkAcos, mkAtan,
  mkAsinh, mkAcosh, mkAtanh,
  mkExpFloating, mkSqrt, mkLog,
  mkFPow, mkLogBase,
  mkTruncate, mkRound, mkFloor, mkCeiling,
  mkAtan2,

  -- * Smart constructors for primitive functions
  mkAdd, mkSub, mkMul, mkNeg, mkAbs, mkSig, mkQuot, mkRem, mkIDiv, mkMod,
  mkBAnd, mkBOr, mkBXor, mkBNot, mkBShiftL, mkBShiftR, mkBRotateL, mkBRotateR,
  mkFDiv, mkRecip, mkLt, mkGt, mkLtEq, mkGtEq, mkEq, mkNEq, mkMax, mkMin,
  mkLAnd, mkLOr, mkLNot,

  -- * Smart constructors for type coercion functions
  mkBoolToInt, mkFromIntegral,

  -- * Auxiliary functions
  ($$), ($$$), ($$$$), ($$$$$)

) where

-- standard library
import Control.Applicative                      hiding (Const)
import Control.Monad
import Data.HashTable                           as Hash
import Data.List
import Data.Maybe
import qualified Data.IntMap                    as IntMap
import Data.Typeable
import System.Mem.StableName
import System.IO.Unsafe                         (unsafePerformIO)
import Prelude                                  hiding (exp)

-- friends
import Data.Array.Accelerate.Debug
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Tuple              hiding (Tuple)
import Data.Array.Accelerate.AST                hiding (
  PreOpenAcc(..), OpenAcc(..), Acc, Stencil(..), PreOpenExp(..), OpenExp, PreExp, Exp)
import qualified Data.Array.Accelerate.Tuple    as Tuple
import qualified Data.Array.Accelerate.AST      as AST
import Data.Array.Accelerate.Pretty ()

#include "accelerate.h"


-- Configuration
-- -------------

-- Are array computations floated out of expressions irrespective of whether they are shared or 
-- not?  'True' implies floating them out.
--
floatOutAccFromExp :: Bool
floatOutAccFromExp = True


-- Layouts
-- -------

-- A layout of an environment has an entry for each entry of the environment.
-- Each entry in the layout holds the deBruijn index that refers to the
-- corresponding entry in the environment.
--
data Layout env env' where
  EmptyLayout :: Layout env ()
  PushLayout  :: Typeable t
              => Layout env env' -> Idx env t -> Layout env (env', t)

-- Project the nth index out of an environment layout.
--
prjIdx :: Typeable t => Int -> Layout env env' -> Idx env t
prjIdx 0 (PushLayout _ ix) = case gcast ix of
                               Just ix' -> ix'
                               Nothing  -> INTERNAL_ERROR(error) "prjIdx" "type mismatch"
prjIdx n (PushLayout l _)  = prjIdx (n - 1) l
prjIdx _ EmptyLayout       = INTERNAL_ERROR(error) "prjIdx" "inconsistent valuation"

-- Add an entry to a layout, incrementing all indices
--
incLayout :: Layout env env' -> Layout (env, t) env'
incLayout EmptyLayout         = EmptyLayout
incLayout (PushLayout lyt ix) = PushLayout (incLayout lyt) (SuccIdx ix)


-- Array computations
-- ------------------

-- |Array-valued collective computations without a recursive knot
--
-- Note [Pipe and sharing recovery]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The 'Pipe' constructor is special.  It is the only form that contains functions over array
-- computations and these functions are fixed to be over vanilla 'Acc' types.  This enables us to
-- perform sharing recovery independently from the context for them.
--
data PreAcc acc a where  
    -- Needed for conversion to de Bruijn form
  Atag        :: Arrays as
              => Int                        -- environment size at defining occurrence
              -> PreAcc acc as

  Pipe        :: (Arrays as, Arrays bs, Arrays cs) 
              => (Acc as -> Acc bs)         -- see comment above on why 'Acc' and not 'acc'
              -> (Acc bs -> Acc cs) 
              -> acc as 
              -> PreAcc acc cs
  Acond       :: (Arrays as)
              => PreExp acc Bool
              -> acc as
              -> acc as
              -> PreAcc acc as
  FstArray    :: (Shape sh1, Shape sh2, Elt e1, Elt e2)
              => acc (Array sh1 e1, Array sh2 e2)
              -> PreAcc acc (Array sh1 e1)
  SndArray    :: (Shape sh1, Shape sh2, Elt e1, Elt e2)
              => acc (Array sh1 e1, Array sh2 e2)
              -> PreAcc acc (Array sh2 e2)
  PairArrays  :: (Shape sh1, Shape sh2, Elt e1, Elt e2)
              => acc (Array sh1 e1)
              -> acc (Array sh2 e2)
              -> PreAcc acc (Array sh1 e1, Array sh2 e2)

  Use         :: (Shape sh, Elt e)
              => Array sh e -> PreAcc acc (Array sh e)
  Unit        :: Elt e
              => PreExp acc e 
              -> PreAcc acc (Scalar e)
  Generate    :: (Shape sh, Elt e)
              => PreExp acc sh
              -> (Exp sh -> PreExp acc e)
              -> PreAcc acc (Array sh e)
  Reshape     :: (Shape sh, Shape sh', Elt e)
              => PreExp acc sh
              -> acc (Array sh' e)
              -> PreAcc acc (Array sh e)
  Replicate   :: (Slice slix, Elt e,
                  Typeable (SliceShape slix), Typeable (FullShape slix))
                  -- the Typeable constraints shouldn't be necessary as they are implied by 
                  -- 'SliceIx slix' — unfortunately, the (old) type checker doesn't grok that
              => PreExp acc slix
              -> acc (Array (SliceShape slix)    e)
              -> PreAcc acc (Array (FullShape slix) e)
  Index       :: (Slice slix, Elt e, 
                  Typeable (SliceShape slix), Typeable (FullShape slix))
                  -- the Typeable constraints shouldn't be necessary as they are implied by 
                  -- 'SliceIx slix' — unfortunately, the (old) type checker doesn't grok that
              => acc (Array (FullShape slix) e)
              -> PreExp acc slix
              -> PreAcc acc (Array (SliceShape slix) e)
  Map         :: (Shape sh, Elt e, Elt e')
              => (Exp e -> PreExp acc e') 
              -> acc (Array sh e)
              -> PreAcc acc (Array sh e')
  ZipWith     :: (Shape sh, Elt e1, Elt e2, Elt e3)
              => (Exp e1 -> Exp e2 -> PreExp acc e3) 
              -> acc (Array sh e1)
              -> acc (Array sh e2)
              -> PreAcc acc (Array sh e3)
  Fold        :: (Shape sh, Elt e)
              => (Exp e -> Exp e -> PreExp acc e)
              -> PreExp acc e
              -> acc (Array (sh:.Int) e)
              -> PreAcc acc (Array sh e)
  Fold1       :: (Shape sh, Elt e)
              => (Exp e -> Exp e -> PreExp acc e)
              -> acc (Array (sh:.Int) e)
              -> PreAcc acc (Array sh e)
  FoldSeg     :: (Shape sh, Elt e)
              => (Exp e -> Exp e -> PreExp acc e)
              -> PreExp acc e
              -> acc (Array (sh:.Int) e)
              -> acc Segments
              -> PreAcc acc (Array (sh:.Int) e)
  Fold1Seg    :: (Shape sh, Elt e)
              => (Exp e -> Exp e -> PreExp acc e)
              -> acc (Array (sh:.Int) e)
              -> acc Segments
              -> PreAcc acc (Array (sh:.Int) e)
  Scanl       :: Elt e
              => (Exp e -> Exp e -> PreExp acc e)
              -> PreExp acc e
              -> acc (Vector e)
              -> PreAcc acc (Vector e)
  Scanl'      :: Elt e
              => (Exp e -> Exp e -> PreExp acc e)
              -> PreExp acc e
              -> acc (Vector e)
              -> PreAcc acc (Vector e, Scalar e)
  Scanl1      :: Elt e
              => (Exp e -> Exp e -> PreExp acc e)
              -> acc (Vector e)
              -> PreAcc acc (Vector e)
  Scanr       :: Elt e
              => (Exp e -> Exp e -> PreExp acc e)
              -> PreExp acc e
              -> acc (Vector e)
              -> PreAcc acc (Vector e)
  Scanr'      :: Elt e
              => (Exp e -> Exp e -> PreExp acc e)
              -> PreExp acc e
              -> acc (Vector e)
              -> PreAcc acc (Vector e, Scalar e)
  Scanr1      :: Elt e
              => (Exp e -> Exp e -> PreExp acc e)
              -> acc (Vector e)
              -> PreAcc acc (Vector e)
  Permute     :: (Shape sh, Shape sh', Elt e)
              => (Exp e -> Exp e -> PreExp acc e)
              -> acc (Array sh' e)
              -> (Exp sh -> PreExp acc sh')
              -> acc (Array sh e)
              -> PreAcc acc (Array sh' e)
  Backpermute :: (Shape sh, Shape sh', Elt e)
              => PreExp acc sh'
              -> (Exp sh' -> PreExp acc sh)
              -> acc (Array sh e)
              -> PreAcc acc (Array sh' e)
  Stencil     :: (Shape sh, Elt a, Elt b, Stencil sh a stencil)
              => (stencil -> PreExp acc b)
              -> Boundary a
              -> acc (Array sh a)
              -> PreAcc acc (Array sh b)
  Stencil2    :: (Shape sh, Elt a, Elt b, Elt c,
                 Stencil sh a stencil1, Stencil sh b stencil2)
              => (stencil1 -> stencil2 -> PreExp acc c)
              -> Boundary a
              -> acc (Array sh a)
              -> Boundary b
              -> acc (Array sh b)
              -> PreAcc acc (Array sh c)

-- |Array-valued collective computations
--
newtype Acc a = Acc (PreAcc Acc a)

deriving instance Typeable1 Acc

-- |Conversion from HOAS to de Bruijn computation AST
-- -

-- |Convert a closed array expression to de Bruijn form while also incorporating sharing
-- information.
--
convertAcc :: Arrays arrs => Acc arrs -> AST.Acc arrs
convertAcc = convertOpenAcc EmptyLayout

-- |Convert a closed array expression to de Bruijn form while also incorporating sharing
-- information.
--
convertOpenAcc :: Arrays arrs => Layout aenv aenv -> Acc arrs -> AST.OpenAcc aenv arrs
convertOpenAcc alyt = convertSharingAcc alyt [] . recoverSharing floatOutAccFromExp

-- |Convert a unary function over array computations
--
convertAccFun1 :: forall a b. (Arrays a, Arrays b)
               => (Acc a -> Acc b) 
               -> AST.Afun (a -> b)
convertAccFun1 f = Alam (Abody openF)
  where
    a     = Atag 0
    alyt  = EmptyLayout 
            `PushLayout` 
            (ZeroIdx :: Idx ((), a) a)
    openF = convertOpenAcc alyt (f (Acc a))

-- |Convert an array expression with given array environment layout and sharing information into
-- de Bruijn form while recovering sharing at the same time (by introducing appropriate let
-- bindings).  The latter implements the third phase of sharing recovery.
--
-- The sharing environment 'env' keeps track of all currently bound sharing variables, keeping them
-- in reverse chronological order (outermost variable is at the end of the list)
--
convertSharingAcc :: forall a aenv. Arrays a
                  => Layout aenv aenv
                  -> [StableSharingAcc]
                  -> SharingAcc a
                  -> AST.OpenAcc aenv a
convertSharingAcc alyt env (VarSharing sa)
  | Just i <- findIndex (matchStableAcc sa) env 
  = AST.OpenAcc $ AST.Avar (prjIdx i alyt)
  | otherwise                                   
  = INTERNAL_ERROR(error) "convertSharingAcc (prjIdx)" err
  where
    err = "inconsistent valuation; sa = " ++ show (hashStableName sa) ++ "; env = " ++ show env
convertSharingAcc alyt env (LetSharing sa@(StableSharingAcc _ boundAcc) bodyAcc)
  = AST.OpenAcc
  $ let alyt' = incLayout alyt `PushLayout` ZeroIdx
    in
    AST.Let (convertSharingAcc alyt env boundAcc) (convertSharingAcc alyt' (sa:env) bodyAcc)
convertSharingAcc alyt env (AccSharing _ preAcc)
  = AST.OpenAcc
  $ (case preAcc of
      Atag i
        -> AST.Avar (prjIdx i alyt)
      Pipe afun1 afun2 acc
        -> let boundAcc = convertAccFun1 afun1 `AST.Apply` convertSharingAcc alyt env acc
               bodyAcc  = convertAccFun1 afun2 `AST.Apply` AST.OpenAcc (AST.Avar AST.ZeroIdx)
           in
           AST.Let (AST.OpenAcc boundAcc) (AST.OpenAcc bodyAcc)
      Acond b acc1 acc2
        -> AST.Acond (convertExp alyt env b) (convertSharingAcc alyt env acc1)
                     (convertSharingAcc alyt env acc2)
      FstArray acc
        -> AST.Let2 (convertSharingAcc alyt env acc) 
                    (AST.OpenAcc $ AST.Avar (AST.SuccIdx AST.ZeroIdx))
      SndArray acc
        -> AST.Let2 (convertSharingAcc alyt env acc) 
                    (AST.OpenAcc $ AST.Avar AST.ZeroIdx)
      PairArrays acc1 acc2
        -> AST.PairArrays (convertSharingAcc alyt env acc1)
                          (convertSharingAcc alyt env acc2)
      Use array
        -> AST.Use array
      Unit e
        -> AST.Unit (convertExp alyt env e)
      Generate sh f
        -> AST.Generate (convertExp alyt env sh) (convertFun1 alyt env f)
      Reshape e acc
        -> AST.Reshape (convertExp alyt env e) (convertSharingAcc alyt env acc)
      Replicate ix acc
        -> mkReplicate (convertExp alyt env ix) (convertSharingAcc alyt env acc)
      Index acc ix
        -> mkIndex (convertSharingAcc alyt env acc) (convertExp alyt env ix)
      Map f acc 
        -> AST.Map (convertFun1 alyt env f) (convertSharingAcc alyt env acc)
      ZipWith f acc1 acc2
        -> AST.ZipWith (convertFun2 alyt env f) 
                       (convertSharingAcc alyt env acc1)
                       (convertSharingAcc alyt env acc2)
      Fold f e acc
        -> AST.Fold (convertFun2 alyt env f) (convertExp alyt env e) 
                    (convertSharingAcc alyt env acc)
      Fold1 f acc
        -> AST.Fold1 (convertFun2 alyt env f) (convertSharingAcc alyt env acc)
      FoldSeg f e acc1 acc2
        -> AST.FoldSeg (convertFun2 alyt env f) (convertExp alyt env e) 
                       (convertSharingAcc alyt env acc1) (convertSharingAcc alyt env acc2)
      Fold1Seg f acc1 acc2
        -> AST.Fold1Seg (convertFun2 alyt env f)
                        (convertSharingAcc alyt env acc1)
                        (convertSharingAcc alyt env acc2)
      Scanl f e acc
        -> AST.Scanl (convertFun2 alyt env f) (convertExp alyt env e) 
                     (convertSharingAcc alyt env acc)
      Scanl' f e acc
        -> AST.Scanl' (convertFun2 alyt env f)
                      (convertExp alyt env e)
                      (convertSharingAcc alyt env acc)
      Scanl1 f acc
        -> AST.Scanl1 (convertFun2 alyt env f) (convertSharingAcc alyt env acc)
      Scanr f e acc
        -> AST.Scanr (convertFun2 alyt env f) (convertExp alyt env e)
                     (convertSharingAcc alyt env acc)
      Scanr' f e acc
        -> AST.Scanr' (convertFun2 alyt env f)
                      (convertExp alyt env e)
                      (convertSharingAcc alyt env acc)
      Scanr1 f acc
        -> AST.Scanr1 (convertFun2 alyt env f) (convertSharingAcc alyt env acc)
      Permute f dftAcc perm acc
        -> AST.Permute (convertFun2 alyt env f) 
                       (convertSharingAcc alyt env dftAcc)
                       (convertFun1 alyt env perm) 
                       (convertSharingAcc alyt env acc)
      Backpermute newDim perm acc
        -> AST.Backpermute (convertExp alyt env newDim)
                           (convertFun1 alyt env perm) 
                           (convertSharingAcc alyt env acc)
      Stencil stencil boundary acc
        -> AST.Stencil (convertStencilFun acc alyt env stencil) 
                       (convertBoundary boundary) 
                       (convertSharingAcc alyt env acc)
      Stencil2 stencil bndy1 acc1 bndy2 acc2
        -> AST.Stencil2 (convertStencilFun2 acc1 acc2 alyt env stencil) 
                        (convertBoundary bndy1) 
                        (convertSharingAcc alyt env acc1)
                        (convertBoundary bndy2) 
                        (convertSharingAcc alyt env acc2)
    :: AST.PreOpenAcc AST.OpenAcc aenv a)

-- |Convert a boundary condition
--
convertBoundary :: Elt e => Boundary e -> Boundary (EltRepr e)
convertBoundary Clamp        = Clamp
convertBoundary Mirror       = Mirror
convertBoundary Wrap         = Wrap
convertBoundary (Constant e) = Constant (fromElt e)


-- Sharing recovery
-- ----------------

-- Sharing recovery proceeds in two phases:
--
-- /Phase One: build the occurence map/
--
-- This is a top-down traversal of the AST that computes a map from AST nodes to the number of
-- occurences of that AST node in the overall Accelerate program.  An occurrences count of two or
-- more indicates sharing.
--
-- IMPORTANT: To avoid unfolding the sharing, we do not descent into subtrees that we have
--   previously encountered.  Hence, the complexity is proprtional to the number of nodes in the
--   tree /with/ sharing.  Consequently, the occurence count is that in the tree with sharing
--   as well.
--
-- During computation of the occurences, the tree is annotated with stable names on every node
-- using 'AccSharing' constructors and all but the first occurence of shared subtrees are pruned
-- using 'VarSharing' constructors (see 'SharingAcc' below).  This phase is impure as it is based
-- on stable names.
--
-- We use a hash table (instead of 'Data.Map') as computing stable names forces us to live in IO
-- anyway.  Once, the computation of occurence counts is complete, we freeze the hash table into
-- a 'Data.Map'.
--
-- (Implemented by 'makeOccMap'.)
--
-- /Phase Two: determine scopes and inject sharing information/
--
-- This is a bottom-up traversal that determines the scope for every binding to be introduced
-- to share a subterm.  It uses the occurence map to determine, for every shared subtree, the
-- lowest AST node at which the binding for that shared subtree can be placed (using a 'LetSharing'
-- constructor)— it's the meet of all the shared subtree occurences.
--
-- The second phase is also replacing the first occurence of each shared subtree with a
-- 'VarSharing' node and floats the shared subtree up to its binding point.
--
--  (Implemented by 'determineScopes'.)

-- Opaque stable name for an array computation — used to key the occurence map.
--
data StableAccName where
  StableAccName :: Typeable arrs => StableName (Acc arrs) -> StableAccName

instance Show StableAccName where
  show (StableAccName sn) = show $ hashStableName sn

instance Eq StableAccName where
  StableAccName sn1 == StableAccName sn2
    | Just sn1' <- gcast sn1 = sn1' == sn2
    | otherwise              = False

makeStableAcc :: Acc arrs -> IO (StableName (Acc arrs))
makeStableAcc acc = acc `seq` makeStableName acc

-- Interleave sharing annotations into an array computation AST.  Subtrees can be marked as being
-- represented by variable (binding a shared subtree) using 'VarSharing' and as being prefixed by
-- a let binding (for a shared subtree) using 'LetSharing'.
--
data SharingAcc arrs where
  VarSharing :: Arrays arrs => StableName (Acc arrs)                           -> SharingAcc arrs
  LetSharing ::                StableSharingAcc -> SharingAcc arrs             -> SharingAcc arrs
  AccSharing :: Arrays arrs => StableName (Acc arrs) -> PreAcc SharingAcc arrs -> SharingAcc arrs

-- Stable name for an array computation associated with its sharing-annotated version.
--
data StableSharingAcc where
  StableSharingAcc :: Arrays arrs => StableName (Acc arrs) -> SharingAcc arrs -> StableSharingAcc

instance Show StableSharingAcc where
  show (StableSharingAcc sn _) = show $ hashStableName sn

instance Eq StableSharingAcc where
  StableSharingAcc sn1 _ == StableSharingAcc sn2 _
    | Just sn1' <- gcast sn1 = sn1' == sn2
    | otherwise              = False

-- Test whether the given stable names matches an array computation with sharing.
--
matchStableAcc :: Typeable arrs => StableName (Acc arrs) -> StableSharingAcc -> Bool
matchStableAcc sn1 (StableSharingAcc sn2 _)
  | Just sn1' <- gcast sn1 = sn1' == sn2
  | otherwise              = False

-- Hash table keyed on the stable names of array computations.
--    
type AccHashTable v = Hash.HashTable StableAccName v

-- Mutable version of the occurrence map, which associates each AST node with an occurence count.
--
type OccMapHash = AccHashTable Int

-- Create a new hash table keyed by array computations.
--
newAccHashTable :: IO (AccHashTable v)
newAccHashTable = Hash.new (==) hashStableAcc
  where
    hashStableAcc (StableAccName sn) = fromIntegral (hashStableName sn)

-- Immutable version of the occurence map.  We use the 'StableName' hash to index an 'IntMap' and
-- disambiguate 'StableName's with identical hashes explicitly, storing them in a list in the
-- 'IntMap'.
--
type OccMap = IntMap.IntMap [(StableAccName, Int)]

-- Turn a mutable into an immutable occurence map.
--
freezeOccMap :: OccMapHash -> IO OccMap
freezeOccMap oc
  = do
      kvs <- Hash.toList oc
      return . IntMap.fromList . map (\kvs -> (key (head kvs), kvs)). groupBy sameKey $ kvs
  where
    key (StableAccName sn, _) = hashStableName sn
    sameKey kv1 kv2           = key kv1 == key kv2

-- Look up the occurence map keyed by array computations using a stable name.  If a the key does
-- not exist in the map, return an occurence count of '1'.
--
lookupWithAccName :: OccMap -> StableAccName -> Int
lookupWithAccName oc sa@(StableAccName sn) 
  = fromMaybe 1 $ IntMap.lookup (hashStableName sn) oc >>= Prelude.lookup sa
    
-- Look up the occurence map keyed by array computations using a sharing array computation.  If an
-- the key does not exist in the map, return an occurence count of '1'.
--
lookupWithSharingAcc :: OccMap -> StableSharingAcc -> Int
lookupWithSharingAcc oc (StableSharingAcc sn _) = lookupWithAccName oc (StableAccName sn)

-- Compute the occurence map, marks all nodes with stable names, and drop repeated occurences
-- of shared subtrees (Phase One).
--
-- Note [Traversing functions and side effects]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- We need to descent into function bodies to build the 'OccMap' with all occurences in the
-- function bodies.  Due to the side effects in the construction of the occurence map and, more
-- importantly, the dependence of the second phase on /global/ occurence information, we may not
-- delay the body traversals by putting them under a lambda.  Hence, we apply the each function, to
-- traverse its body and use a /dummy abstraction/ of the result.
--
-- For example, given a function 'f', we traverse 'f (Tag 0)', which yields a transformed body 'e'.
-- As the result of the traversal of the overall function, we use 'const e'.  Hence, it is crucial
-- that the 'Tag' supplied during the initial traversal is already the one required by the HOAS to
-- de Bruijn conversion in 'convertSharingAcc' — any subsequent application of 'const e' will only
-- yield 'e' with the embedded 'Tag 0' of the original application.
--
makeOccMap :: Typeable arrs => Acc arrs -> IO (SharingAcc arrs, OccMapHash)
makeOccMap rootAcc
  = do
      occMap <- newAccHashTable
      rootAcc' <- traverseAcc True (enterOcc occMap) rootAcc
      return (rootAcc', occMap)
  where
    -- Enter one AST node occurrence into an occurrence map.  Returns 'True' if this is a repeated
    -- occurence.
    --
    -- The first argument determines whether the 'OccMap' will be modified - see Note [Traversing
    -- functions and side effects].
    --
    enterOcc :: OccMapHash -> Bool -> StableAccName -> IO Bool
    enterOcc occMap updateMap sa 
      = do
          entry <- Hash.lookup occMap sa
          case entry of
            Nothing -> when updateMap (       Hash.insert occMap sa 1      ) >> return False
            Just n  -> when updateMap (void $ Hash.update occMap sa (n + 1)) >> return True
              where
                void = (>> return ())

    traverseAcc :: forall arrs. Typeable arrs
                => Bool -> (Bool -> StableAccName -> IO Bool) -> Acc arrs -> IO (SharingAcc arrs)
    traverseAcc updateMap enter acc'@(Acc pacc)
      = do
            -- Compute stable name and enter it into the occurence map
          sn <- makeStableAcc acc'
          isRepeatedOccurence <- enter updateMap $ StableAccName sn
          
          traceLine (showPreAccOp pacc) $
            if isRepeatedOccurence 
              then "REPEATED occurence"
              else "first occurence (" ++ show (hashStableName sn) ++ ")"

            -- Reconstruct the computation in shared form
            --
            -- NB: This function can only be used in the case alternatives below; outside of the
            --     case we cannot discharge the 'Arrays arrs' constraint.
          let reconstruct :: Arrays arrs 
                          => IO (PreAcc SharingAcc arrs)
                          -> IO (SharingAcc arrs)
              reconstruct newAcc | isRepeatedOccurence = pure $ VarSharing sn
                                 | otherwise           = AccSharing sn <$> newAcc

          case pacc of
            Atag i                   -> reconstruct $ return (Atag i)
            Pipe afun1 afun2 acc     -> reconstruct $ travA (Pipe afun1 afun2) acc
            Acond e acc1 acc2        -> reconstruct $ do
                                          e'    <- traverseExp updateMap enter e
                                          acc1' <- traverseAcc updateMap enter acc1
                                          acc2' <- traverseAcc updateMap enter acc2
                                          return (Acond e' acc1' acc2')
            FstArray acc             -> reconstruct $ travA FstArray acc
            SndArray acc             -> reconstruct $ travA SndArray acc
            PairArrays acc1 acc2     -> reconstruct $ do
                                          acc1' <- traverseAcc updateMap enter acc1
                                          acc2' <- traverseAcc updateMap enter acc2
                                          return (PairArrays acc1' acc2')
            Use arr                  -> reconstruct $ return (Use arr)
            Unit e                   -> reconstruct $ do
                                          e' <- traverseExp updateMap enter e
                                          return (Unit e')
            Generate e f             -> reconstruct $ do
                                          e' <- traverseExp  updateMap enter e
                                          f' <- traverseFun1 updateMap enter f
                                          return (Generate e' f')
            Reshape e acc            -> reconstruct $ travEA Reshape e acc
            Replicate e acc          -> reconstruct $ travEA Replicate e acc
            Index acc e              -> reconstruct $ travEA (flip Index) e acc
            Map f acc                -> reconstruct $ do
                                          f'   <- traverseFun1 updateMap enter f
                                          acc' <- traverseAcc  updateMap enter acc
                                          return (Map f' acc')
            ZipWith f acc1 acc2      -> reconstruct $ travF2A2 ZipWith f acc1 acc2
            Fold f e acc             -> reconstruct $ travF2EA Fold f e acc
            Fold1 f acc              -> reconstruct $ travF2A Fold1 f acc
            FoldSeg f e acc1 acc2    -> reconstruct $ do
                                          f'    <- traverseFun2 updateMap enter f
                                          e'    <- traverseExp  updateMap enter e
                                          acc1' <- traverseAcc  updateMap enter acc1
                                          acc2' <- traverseAcc  updateMap enter acc2
                                          return (FoldSeg f' e' acc1' acc2')
            Fold1Seg f acc1 acc2     -> reconstruct $ travF2A2 Fold1Seg f acc1 acc2
            Scanl f e acc            -> reconstruct $ travF2EA Scanl f e acc
            Scanl' f e acc           -> reconstruct $ travF2EA Scanl' f e acc
            Scanl1 f acc             -> reconstruct $ travF2A Scanl1 f acc
            Scanr f e acc            -> reconstruct $ travF2EA Scanr f e acc
            Scanr' f e acc           -> reconstruct $ travF2EA Scanr' f e acc
            Scanr1 f acc             -> reconstruct $ travF2A Scanr1 f acc
            Permute c acc1 p acc2    -> reconstruct $ do
                                          c'    <- traverseFun2 updateMap enter c
                                          p'    <- traverseFun1 updateMap enter p
                                          acc1' <- traverseAcc  updateMap enter acc1
                                          acc2' <- traverseAcc  updateMap enter acc2
                                          return (Permute c' acc1' p' acc2')
            Backpermute e p acc      -> reconstruct $ do
                                          e'   <- traverseExp  updateMap enter e
                                          p'   <- traverseFun1 updateMap enter p
                                          acc' <- traverseAcc  updateMap enter acc
                                          return (Backpermute e' p' acc')
            Stencil s bnd acc        -> reconstruct $ do
                                          s'   <- traverseStencil1 acc updateMap enter s
                                          acc' <- traverseAcc  updateMap enter acc
                                          return (Stencil s' bnd acc')
            Stencil2 s bnd1 acc1 
                       bnd2 acc2     -> reconstruct $ do
                                          s'    <- traverseStencil2 acc1 acc2 updateMap enter s
                                          acc1' <- traverseAcc  updateMap enter acc1
                                          acc2' <- traverseAcc  updateMap enter acc2
                                          return (Stencil2 s' bnd1 acc1' bnd2 acc2')
      where
        travA :: Arrays arrs'
              => (SharingAcc arrs' -> PreAcc SharingAcc arrs) 
              -> Acc arrs' -> IO (PreAcc SharingAcc arrs)
        travA c acc
          = do
              acc' <- traverseAcc updateMap enter acc
              return $ c acc'

        travEA :: (Typeable b, Arrays arrs')
               => (SharingExp b -> SharingAcc arrs' -> PreAcc SharingAcc arrs) 
               -> Exp b -> Acc arrs' -> IO (PreAcc SharingAcc arrs)
        travEA c exp acc
          = do
              exp' <- traverseExp updateMap enter exp
              acc' <- traverseAcc updateMap enter acc
              return $ c exp' acc'

        travF2A :: (Elt b, Elt c, Typeable d, Arrays arrs')
                => ((Exp b -> Exp c -> SharingExp d) -> SharingAcc arrs' -> PreAcc SharingAcc arrs) 
                -> (Exp b -> Exp c -> Exp d) -> Acc arrs' -> IO (PreAcc SharingAcc arrs)
        travF2A c fun acc
          = do
              fun' <- traverseFun2 updateMap enter fun
              acc' <- traverseAcc updateMap enter acc
              return $ c fun' acc'

        travF2EA :: (Elt b, Elt c, Typeable d, Typeable e, Arrays arrs')
                 => ((Exp b -> Exp c -> SharingExp d) -> SharingExp e
                       -> SharingAcc arrs' -> PreAcc SharingAcc arrs) 
                 -> (Exp b -> Exp c -> Exp d) -> Exp e -> Acc arrs' -> IO (PreAcc SharingAcc arrs)
        travF2EA c fun exp acc
          = do
              fun' <- traverseFun2 updateMap enter fun
              exp' <- traverseExp updateMap enter exp
              acc' <- traverseAcc updateMap enter acc
              return $ c fun' exp' acc'

        travF2A2 :: (Elt b, Elt c, Typeable d, Arrays arrs1, Arrays arrs2)
                 => ((Exp b -> Exp c -> SharingExp d) -> SharingAcc arrs1
                       -> SharingAcc arrs2 -> PreAcc SharingAcc arrs) 
                 -> (Exp b -> Exp c -> Exp d) -> Acc arrs1 -> Acc arrs2 
                 -> IO (PreAcc SharingAcc arrs)
        travF2A2 c fun acc1 acc2
          = do
              fun' <- traverseFun2 updateMap enter fun
              acc1' <- traverseAcc updateMap enter acc1
              acc2' <- traverseAcc updateMap enter acc2
              return $ c fun' acc1' acc2'

    traverseFun1 :: (Elt b, Typeable c) 
                  => Bool -> (Bool -> StableAccName -> IO Bool) -> (Exp b -> Exp c) 
                  -> IO (Exp b -> SharingExp c)
    traverseFun1 updateMap enter f
      = do
            -- see Note [Traversing functions and side effects]
          body <- traverseExp updateMap enter $ f (Tag 0)
          return $ const body

    traverseFun2 :: (Elt b, Elt c, Typeable d) 
                  => Bool -> (Bool -> StableAccName -> IO Bool) -> (Exp b -> Exp c -> Exp d) 
                  -> IO (Exp b -> Exp c -> SharingExp d)
    traverseFun2 updateMap enter f
      = do
            -- see Note [Traversing functions and side effects]
          body <- traverseExp updateMap enter $ f (Tag 1) (Tag 0)
          return $ \_ _ -> body

    traverseStencil1 :: forall sh b c stencil. (Stencil sh b stencil, Typeable c) 
                     => Acc (Array sh b){-dummy-}
                     -> Bool -> (Bool -> StableAccName -> IO Bool) -> (stencil -> Exp c) 
                     -> IO (stencil -> SharingExp c)
    traverseStencil1 _ updateMap enter stencilFun 
      = do
            -- see Note [Traversing functions and side effects]
          body <- traverseExp updateMap enter $ 
                    stencilFun (stencilPrj (undefined::sh) (undefined::b) (Tag 0))
          return $ const body
        
    traverseStencil2 :: forall sh b c d stencil1 stencil2. 
                        (Stencil sh b stencil1, Stencil sh c stencil2, Typeable d) 
                     => Acc (Array sh b){-dummy-}
                     -> Acc (Array sh c){-dummy-}
                     -> Bool -> (Bool -> StableAccName -> IO Bool) 
                     -> (stencil1 -> stencil2 -> Exp d) 
                     -> IO (stencil1 -> stencil2 -> SharingExp d)
    traverseStencil2 _ _ updateMap enter stencilFun 
      = do
            -- see Note [Traversing functions and side effects]
          body <- traverseExp updateMap enter $ 
                    stencilFun (stencilPrj (undefined::sh) (undefined::b) (Tag 1))
                               (stencilPrj (undefined::sh) (undefined::c) (Tag 0))
          return $ \_ _ -> body
        
    traverseExp :: Typeable a 
                => Bool -> (Bool -> StableAccName -> IO Bool) -> Exp a -> IO (SharingExp a)
    traverseExp updateMap enter exp  -- @(Exp pexp)
      = -- FIXME: recover sharing of scalar expressions as well
          case exp of
            Tag i           -> return $ Tag i
            Const c         -> return $ Const c
            Tuple tup       -> Tuple <$> travTup tup
            Prj i e         -> travE1 (Prj i) e
            IndexNil        -> return IndexNil
            IndexCons ix i  -> travE2 IndexCons ix i
            IndexHead i     -> travE1 IndexHead i
            IndexTail ix    -> travE1 IndexTail ix
            IndexAny        -> return $ IndexAny
            Cond e1 e2 e3   -> travE3 Cond e1 e2 e3
            PrimConst c     -> return $ PrimConst c
            PrimApp p e     -> travE1 (PrimApp p) e
            IndexScalar a e -> travAE IndexScalar a e
            Shape a         -> travA Shape a
            Size a          -> travA Size a
      where
        travE1 :: Typeable b => (SharingExp b -> SharingExp c) -> Exp b -> IO (SharingExp c)
        travE1 c e
          = do
              e' <- traverseExp updateMap enter e
              return $ c e'
      
        travE2 :: (Typeable b, Typeable c) 
               => (SharingExp b -> SharingExp c -> SharingExp d) -> Exp b -> Exp c 
               -> IO (SharingExp d)
        travE2 c e1 e2
          = do
              e1' <- traverseExp updateMap enter e1
              e2' <- traverseExp updateMap enter e2
              return $ c e1' e2'
      
        travE3 :: (Typeable b, Typeable c, Typeable d) 
               => (SharingExp b -> SharingExp c -> SharingExp d -> SharingExp e) 
               -> Exp b -> Exp c -> Exp d
               -> IO (SharingExp e)
        travE3 c e1 e2 e3
          = do
              e1' <- traverseExp updateMap enter e1
              e2' <- traverseExp updateMap enter e2
              e3' <- traverseExp updateMap enter e3
              return $ c e1' e2' e3'
      
        travA :: Typeable b => (SharingAcc b -> SharingExp c) -> Acc b -> IO (SharingExp c)
        travA c acc
          = do
              acc' <- traverseAcc updateMap enter acc
              return $ c acc'

        travAE :: (Typeable b, Typeable c) 
               => (SharingAcc b -> SharingExp c -> SharingExp d) -> Acc b -> Exp c 
               -> IO (SharingExp d)
        travAE c acc e
          = do
              acc' <- traverseAcc updateMap enter acc
              e' <- traverseExp updateMap enter e
              return $ c acc' e'

        travTup :: Tuple.Tuple (PreExp Acc) tup -> IO (Tuple.Tuple (PreExp SharingAcc) tup)
        travTup NilTup          = return NilTup
        travTup (SnocTup tup e) = pure SnocTup <*> travTup tup <*> traverseExp updateMap enter e

-- Type used to maintain how often each shared subterm occured.
--
--   Invariant: If one shared term 's' is itself a subterm of another shared term 't', then 's' 
--              must occur *after* 't' in the 'NodeCounts'.  Moreover, no shared term occur twice.
--
-- To ensure the invariant is preserved over merging node counts from sibling subterms, the
-- function '(+++)' must be used.
--
newtype NodeCounts = NodeCounts [(StableSharingAcc, Int)]
  deriving Show

-- Empty node counts
--
noNodeCounts :: NodeCounts
noNodeCounts = NodeCounts []

-- Singleton node counts
--
nodeCount :: (StableSharingAcc, Int) -> NodeCounts
nodeCount nc = NodeCounts [nc]

-- Combine node counts that belong to the same node.
--
-- * We assume that the node counts invariant —subterms follow their parents— holds for both
--   arguments and guarantee that it still holds for the result.
--
-- * This function has quadratic complexity.  This could be improved by labelling nodes with their
--   nesting depth, but doesn't seem worthwhile as the arguments are expected to be fairly short.
--   Change if profiling suggests that this function is a bottleneck.
--
(+++) :: NodeCounts -> NodeCounts -> NodeCounts
NodeCounts us +++ NodeCounts vs = NodeCounts $ merge us vs
  where
    merge []                         ys                         = ys
    merge xs                         []                         = xs
    merge xs@(x@(sa1, count1) : xs') ys@(y@(sa2, count2) : ys') 
      | sa1 == sa2                = (sa1 `pickNoneVar` sa2, count1 + count2) : merge xs' ys'
      | sa1 `notElem` map fst ys' = x : merge xs' ys
      | sa2 `notElem` map fst xs' = y : merge xs  ys'
      | otherwise                 = INTERNAL_ERROR(error) "(+++)" "Precondition violated"

    (StableSharingAcc _ (VarSharing _)) `pickNoneVar` sa2                                 = sa2
    sa1                                 `pickNoneVar` _sa2                                = sa1

-- Determine the scopes of all variables representing shared subterms (Phase Two) in a bottom-up
-- sweep.  The first argument determines whether array computations are floated out of expressions
-- irrespective of whether they are shared or not — 'True' implies floating them out.
--
-- Precondition: there are only 'VarSharing' and 'AccSharing' nodes in the argument.
--
determineScopes :: Typeable a => Bool -> OccMap -> SharingAcc a -> SharingAcc a
determineScopes floatOutAcc occMap rootAcc = fst $ scopesAcc rootAcc
  where
    scopesAcc :: forall arrs. SharingAcc arrs -> (SharingAcc arrs, NodeCounts)
    scopesAcc (LetSharing _ _)
      = INTERNAL_ERROR(error) "determineScopes: scopes" "unexpected 'LetSharing'"
    scopesAcc sharingAcc@(VarSharing sn)
      = (VarSharing sn, nodeCount (StableSharingAcc sn sharingAcc, 1))
    scopesAcc (AccSharing sn pacc)
      = case pacc of
          Atag i                  -> reconstruct (Atag i) noNodeCounts
          Pipe afun1 afun2 acc    -> travA (Pipe afun1 afun2) acc
            -- we are not traversing 'afun1' & 'afun2' — see Note [Pipe and sharing recovery]
          Acond e acc1 acc2       -> let
                                       (e'   , accCount1) = scopesExp e
                                       (acc1', accCount2) = scopesAcc acc1
                                       (acc2', accCount3) = scopesAcc acc2
                                     in
                                     reconstruct (Acond e' acc1' acc2')
                                                 (accCount1 +++ accCount2 +++ accCount3)
          FstArray acc            -> travA FstArray acc
          SndArray acc            -> travA SndArray acc
          PairArrays acc1 acc2    -> let
                                       (acc1', accCount1) = scopesAcc acc1
                                       (acc2', accCount2) = scopesAcc acc2
                                     in
                                     reconstruct (PairArrays acc1' acc2') (accCount1 +++ accCount2)
          Use arr                 -> reconstruct (Use arr) noNodeCounts
          Unit e                  -> let
                                       (e', accCount) = scopesExp e
                                     in
                                     reconstruct (Unit e') accCount
          Generate sh f           -> let
                                       (sh', accCount1) = scopesExp sh
                                       (f' , accCount2) = scopesFun1 f
                                     in
                                     reconstruct (Generate sh' f') (accCount1 +++ accCount2)
          Reshape sh acc          -> travEA Reshape sh acc
          Replicate n acc         -> travEA Replicate n acc
          Index acc i             -> travEA (flip Index) i acc
          Map f acc               -> let
                                       (f'  , accCount1) = scopesFun1 f
                                       (acc', accCount2) = scopesAcc  acc
                                     in
                                     reconstruct (Map f' acc') (accCount1 +++ accCount2)
          ZipWith f acc1 acc2     -> travF2A2 ZipWith f acc1 acc2
          Fold f z acc            -> travF2EA Fold f z acc
          Fold1 f acc             -> travF2A Fold1 f acc
          FoldSeg f z acc1 acc2   -> let
                                       (f'   , accCount1)  = scopesFun2 f
                                       (z'   , accCount2)  = scopesExp  z
                                       (acc1', accCount3)  = scopesAcc  acc1
                                       (acc2', accCount4)  = scopesAcc  acc2
                                     in
                                     reconstruct (FoldSeg f' z' acc1' acc2') 
                                       (accCount1 +++ accCount2 +++ accCount3 +++ accCount4)
          Fold1Seg f acc1 acc2    -> travF2A2 Fold1Seg f acc1 acc2
          Scanl f z acc           -> travF2EA Scanl f z acc
          Scanl' f z acc          -> travF2EA Scanl' f z acc
          Scanl1 f acc            -> travF2A Scanl1 f acc
          Scanr f z acc           -> travF2EA Scanr f z acc
          Scanr' f z acc          -> travF2EA Scanr' f z acc
          Scanr1 f acc            -> travF2A Scanr1 f acc
          Permute fc acc1 fp acc2 -> let
                                       (fc'  , accCount1) = scopesFun2 fc
                                       (acc1', accCount2) = scopesAcc  acc1
                                       (fp'  , accCount3) = scopesFun1 fp
                                       (acc2', accCount4) = scopesAcc  acc2
                                     in
                                     reconstruct (Permute fc' acc1' fp' acc2')
                                       (accCount1 +++ accCount2 +++ accCount3 +++ accCount4)
          Backpermute sh fp acc   -> let
                                       (sh' , accCount1) = scopesExp  sh
                                       (fp' , accCount2) = scopesFun1 fp
                                       (acc', accCount3) = scopesAcc  acc
                                     in
                                     reconstruct (Backpermute sh' fp' acc')
                                       (accCount1 +++ accCount2 +++ accCount3)
          Stencil st bnd acc      -> let
                                       (st' , accCount1) = scopesStencil1 acc st
                                       (acc', accCount2) = scopesAcc      acc
                                     in
                                     reconstruct (Stencil st' bnd acc') (accCount1 +++ accCount2)
          Stencil2 st bnd1 acc1 bnd2 acc2 
                                  -> let
                                       (st'  , accCount1) = scopesStencil2 acc1 acc2 st
                                       (acc1', accCount2) = scopesAcc acc1
                                       (acc2', accCount3) = scopesAcc acc2
                                     in
                                     reconstruct (Stencil2 st' bnd1 acc1' bnd2 acc2')
                                       (accCount1 +++ accCount2 +++ accCount3)
      where
        travEA :: Arrays arrs 
               => (SharingExp e -> SharingAcc arrs' -> PreAcc SharingAcc arrs) 
               -> SharingExp e
               -> SharingAcc arrs' 
               -> (SharingAcc arrs, NodeCounts)
        travEA c e acc = reconstruct (c e' acc') (accCount1 +++ accCount2)
          where
            (e'  , accCount1) = scopesExp e
            (acc', accCount2) = scopesAcc acc

        travF2A :: (Elt a, Elt b, Arrays arrs)
                => ((Exp a -> Exp b -> SharingExp c) -> SharingAcc arrs' -> PreAcc SharingAcc arrs) 
                -> (Exp a -> Exp b -> SharingExp c)
                -> SharingAcc arrs'
                -> (SharingAcc arrs, NodeCounts)
        travF2A c f acc = reconstruct (c f' acc') (accCount1 +++ accCount2)
          where
            (f'  , accCount1) = scopesFun2 f
            (acc', accCount2) = scopesAcc  acc              

        travF2EA :: (Elt a, Elt b, Arrays arrs)
                 => ((Exp a -> Exp b -> SharingExp c) -> SharingExp e 
                     -> SharingAcc arrs' -> PreAcc SharingAcc arrs) 
                 -> (Exp a -> Exp b -> SharingExp c)
                 -> SharingExp e 
                 -> SharingAcc arrs'
                 -> (SharingAcc arrs, NodeCounts)
        travF2EA c f e acc = reconstruct (c f' e' acc') (accCount1 +++ accCount2 +++ accCount3)
          where
            (f'  , accCount1) = scopesFun2 f
            (e'  , accCount2) = scopesExp  e
            (acc', accCount3) = scopesAcc  acc

        travF2A2 :: (Elt a, Elt b, Arrays arrs)
                 => ((Exp a -> Exp b -> SharingExp c) -> SharingAcc arrs1 
                     -> SharingAcc arrs2 -> PreAcc SharingAcc arrs) 
                 -> (Exp a -> Exp b -> SharingExp c)
                 -> SharingAcc arrs1 
                 -> SharingAcc arrs2 
                 -> (SharingAcc arrs, NodeCounts)
        travF2A2 c f acc1 acc2 = reconstruct (c f' acc1' acc2') 
                                             (accCount1 +++ accCount2 +++ accCount3)
          where
            (f'   , accCount1) = scopesFun2 f
            (acc1', accCount2) = scopesAcc  acc1
            (acc2', accCount3) = scopesAcc  acc2

        travA :: Arrays arrs 
              => (SharingAcc arrs' -> PreAcc SharingAcc arrs) 
              -> SharingAcc arrs' 
              -> (SharingAcc arrs, NodeCounts)
        travA c acc = reconstruct (c acc') accCount
          where
            (acc', accCount) = scopesAcc acc

          -- Occurence count of the currently processed node
        occCount = lookupWithAccName occMap (StableAccName sn)

        -- Reconstruct the current tree node.
        --
        -- * If the current node is being shared ('occCount > 1'), replace it by a 'VarSharing'
        --   node and float the shared subtree out wrapped in a 'NodeCounts' value.
        -- * If the current node is not shared, reconstruct it in place.
        --
        -- In either case, any completed 'NodeCounts' are injected as bindings using 'LetSharing'
        -- node.
        -- 
        reconstruct :: Arrays arrs 
                    => PreAcc SharingAcc arrs -> NodeCounts -> (SharingAcc arrs, NodeCounts)
        reconstruct newAcc subCount
          | occCount > 1 = ( VarSharing sn
                           , nodeCount (StableSharingAcc sn sharingAcc, 1) +++ newCount)
          | otherwise    = (sharingAcc, newCount)
          where
              -- Determine the bindings that need to be attached to the current node...
            (newCount, bindHere) = filterCompleted subCount

              -- ...and wrap them in 'LetSharing' constructors
            lets       = foldl (flip (.)) id . map LetSharing $ bindHere
            sharingAcc = lets $ AccSharing sn newAcc

        -- Extract nodes that have a complete node count (i.e., their node count is equal to the
        -- number of occurences of that node in the overall expression) => nodes with a completed
        -- node count should be let bound at the currently processed node.
        --
        filterCompleted :: NodeCounts -> (NodeCounts, [StableSharingAcc])
        filterCompleted (NodeCounts counts) 
          = let (counts', completed) = fc counts
            in (NodeCounts counts', completed)
          where
            fc []                 = ([], [])
            fc (sub@(sa, n):subs)
                -- current node is the binding point for the shared node 'sa'
              | occCount == n     = (subs', sa:bindHere)
                -- not a binding point
              | otherwise         = (sub:subs', bindHere)
              where
                occCount          = lookupWithSharingAcc occMap sa
                (subs', bindHere) = fc subs

    scopesExp :: forall arrs. SharingExp arrs -> (SharingExp arrs, NodeCounts)
    scopesExp pacc
      = case pacc of
          Tag i           -> (Tag i, noNodeCounts)
          Const c         -> (Const c, noNodeCounts)
          Tuple tup       -> let (tup', accCount) = travTup tup in (Tuple tup', accCount)
          Prj i e         -> travE1 (Prj i) e
          IndexNil        -> (IndexNil, noNodeCounts)
          IndexCons ix i  -> travE2 IndexCons ix i
          IndexHead i     -> travE1 IndexHead i
          IndexTail ix    -> travE1 IndexTail ix
          IndexAny        -> (IndexAny, noNodeCounts)
          Cond e1 e2 e3   -> travE3 Cond e1 e2 e3
          PrimConst c     -> (PrimConst c, noNodeCounts)
          PrimApp p e     -> travE1 (PrimApp p) e
          IndexScalar a e -> travAE IndexScalar a e
          Shape a         -> travA Shape a
          Size a          -> travA Size a
     where
        travTup :: Tuple.Tuple (PreExp SharingAcc) tup 
                -> (Tuple.Tuple (PreExp SharingAcc) tup, NodeCounts)
        travTup NilTup          = (NilTup, noNodeCounts)
        travTup (SnocTup tup e) = let
                                    (tup', accCountT) = travTup tup
                                    (e'  , accCountE) = scopesExp e
                                  in
                                  (SnocTup tup' e', accCountT +++ accCountE)

        travE1 :: (SharingExp a -> SharingExp b) -> SharingExp a -> (SharingExp b, NodeCounts)
        travE1 c e = (c e', accCount)
          where
            (e', accCount) = scopesExp e

        travE2 :: (SharingExp a -> SharingExp b -> SharingExp c) -> SharingExp a -> SharingExp b 
               -> (SharingExp c, NodeCounts)
        travE2 c e1 e2 = (c e1' e2', accCount1 +++ accCount2)
          where
            (e1', accCount1) = scopesExp e1
            (e2', accCount2) = scopesExp e2

        travE3 :: (SharingExp a -> SharingExp b -> SharingExp c -> SharingExp d) 
               -> SharingExp a -> SharingExp b -> SharingExp c 
               -> (SharingExp d, NodeCounts)
        travE3 c e1 e2 e3 = (c e1' e2' e3', accCount1 +++ accCount2 +++ accCount3)
          where
            (e1', accCount1) = scopesExp e1
            (e2', accCount2) = scopesExp e2
            (e3', accCount3) = scopesExp e3

        travA :: (SharingAcc a -> SharingExp b) -> SharingAcc a -> (SharingExp b, NodeCounts)
        travA c acc = maybeFloatOutAcc c acc' accCount
          where
            (acc', accCount)  = scopesAcc acc
        
        travAE :: (SharingAcc a -> SharingExp b -> SharingExp c) -> SharingAcc a -> SharingExp b 
               -> (SharingExp c, NodeCounts)
        travAE c acc e = maybeFloatOutAcc (flip c e') acc' (accCountA +++ accCountE)
          where
            (acc', accCountA) = scopesAcc acc
            (e'  , accCountE) = scopesExp e
        
        maybeFloatOutAcc :: (SharingAcc a -> SharingExp b) -> SharingAcc a -> NodeCounts
                         -> (SharingExp b, NodeCounts)
        maybeFloatOutAcc c acc@(VarSharing _) accCount = (c acc, accCount)  -- nothing to float out
        maybeFloatOutAcc c acc                accCount
          | floatOutAcc = (c var, nodeCount (stableAcc, 1) +++ accCount)
          | otherwise   = (c acc, accCount)
          where
             (var, stableAcc) = abstract acc id

        abstract :: SharingAcc a -> (SharingAcc a -> SharingAcc a) 
                 -> (SharingAcc a, StableSharingAcc)
        abstract (VarSharing _)        _    = INTERNAL_ERROR(error) "sharingAccToVar" "VarSharing"
        abstract (LetSharing sa acc)   lets = abstract acc (lets . LetSharing sa)
        abstract acc@(AccSharing sn _) lets = (VarSharing sn, StableSharingAcc sn (lets acc))

    -- The lambda bound variable is at this point already irrelevant; for details, see
    -- Note [Traversing functions and side effects]
    --
    scopesFun1 :: Elt e1 => (Exp e1 -> SharingExp e2) -> (Exp e1 -> SharingExp e2, NodeCounts)
    scopesFun1 f = (const body, counts)
      where
        (body, counts) = scopesExp (f undefined)

    -- The lambda bound variable is at this point already irrelevant; for details, see
    -- Note [Traversing functions and side effects]
    --
    scopesFun2 :: (Elt e1, Elt e2) 
               => (Exp e1 -> Exp e2 -> SharingExp e3) 
               -> (Exp e1 -> Exp e2 -> SharingExp e3, NodeCounts)
    scopesFun2 f = (\_ _ -> body, counts)
      where
        (body, counts) = scopesExp (f undefined undefined)

    -- The lambda bound variable is at this point already irrelevant; for details, see
    -- Note [Traversing functions and side effects]
    --
    scopesStencil1 :: forall sh e1 e2 stencil. Stencil sh e1 stencil
                   => SharingAcc (Array sh e1){-dummy-}
                   -> (stencil -> SharingExp e2) 
                   -> (stencil -> SharingExp e2, NodeCounts)
    scopesStencil1 _ stencilFun = (const body, counts)
      where
        (body, counts) = scopesExp (stencilFun undefined)
          
    -- The lambda bound variable is at this point already irrelevant; for details, see
    -- Note [Traversing functions and side effects]
    --
    scopesStencil2 :: forall sh e1 e2 e3 stencil1 stencil2. 
                      (Stencil sh e1 stencil1, Stencil sh e2 stencil2)
                   => SharingAcc (Array sh e1){-dummy-}
                   -> SharingAcc (Array sh e2){-dummy-}
                   -> (stencil1 -> stencil2 -> SharingExp e3) 
                   -> (stencil1 -> stencil2 -> SharingExp e3, NodeCounts)
    scopesStencil2 _ _ stencilFun = (\_ _ -> body, counts)
      where
        (body, counts) = scopesExp (stencilFun undefined undefined)          
                  
-- |Recover sharing information and annotate the HOAS AST with variable and let binding
--  annotations.  The first argument determines whether array computations are floated out of
--  expressions irrespective of whether they are shared or not — 'True' implies floating them out.
--
-- NB: Strictly speaking, this function is not deterministic, as it uses stable pointers to
--     determine the sharing of subterms.  The stable pointer API does not guarantee its
--     completeness; i.e., it may miss some equalities, which implies that we may fail to discover
--     some sharing.  However, sharing does not affect the denotational meaning of an array
--     computation; hence, we do not compromise denotational correctness.
--
recoverSharing :: Typeable a => Bool -> Acc a -> SharingAcc a
{-# NOINLINE recoverSharing #-}
recoverSharing floatOutAcc acc 
  = let (acc', occMap) =   -- as we need to use stable pointers; it's safe as explained above
          unsafePerformIO $ do
            (acc', occMap) <- makeOccMap acc
 
            occMapList <- Hash.toList occMap
            traceChunk "OccMap" $
              show occMapList
 
            frozenOccMap <- freezeOccMap occMap
            return (acc', frozenOccMap)
    in 
    determineScopes floatOutAcc occMap acc'


-- Embedded expressions of the surface language
-- --------------------------------------------

-- HOAS expressions mirror the constructors of `AST.OpenExp', but with the
-- `Tag' constructor instead of variables in the form of de Bruijn indices.
-- Moreover, HOAS expression use n-tuples and the type class 'Elt' to
-- constrain element types, whereas `AST.OpenExp' uses nested pairs and the 
-- GADT 'TupleType'.
--

-- |Scalar expressions to parametrise collective array operations, themselves parameterised over
-- the type of collective array operations.
--
data PreExp acc t where
    -- Needed for conversion to de Bruijn form
  Tag         :: Elt t
              => Int                           -> PreExp acc t
                 -- environment size at defining occurrence

    -- All the same constructors as 'AST.Exp'
  Const       :: Elt t 
              => t                                               -> PreExp acc t

  Tuple       :: (Elt t, IsTuple t)
              => Tuple.Tuple (PreExp acc) (TupleRepr t)          -> PreExp acc t
  Prj         :: (Elt t, IsTuple t)
              => TupleIdx (TupleRepr t) e     
              -> PreExp acc t                                    -> PreExp acc e
  IndexNil    ::                                                    PreExp acc Z
  IndexCons   :: (Slice sl, Elt a)
              => PreExp acc sl -> PreExp acc a                   -> PreExp acc (sl:.a)
  IndexHead   :: (Slice sl, Elt a)
              => PreExp acc (sl:.a)                              -> PreExp acc a
  IndexTail   :: (Slice sl, Elt a)
              => PreExp acc (sl:.a)                              -> PreExp acc sl
  IndexAny    :: Shape sh
              =>                                                    PreExp acc (Any sh)
  Cond        :: PreExp acc Bool -> PreExp acc t -> PreExp acc t -> PreExp acc t
  PrimConst   :: Elt t                       
              => PrimConst t                                     -> PreExp acc t
  PrimApp     :: (Elt a, Elt r)             
              => PrimFun (a -> r) -> PreExp acc a                -> PreExp acc r
  IndexScalar :: (Shape sh, Elt t)
              => acc (Array sh t) -> PreExp acc sh               -> PreExp acc t
  Shape       :: (Shape sh, Elt e)
              => acc (Array sh e)                                -> PreExp acc sh
  Size        :: (Shape sh, Elt e)
              => acc (Array sh e)                                -> PreExp acc Int

-- |Scalar expressions for plain array computations.
--
type Exp t = PreExp Acc t

-- |Scalar expressions for array computations with sharing annotations.
--
type SharingExp t = PreExp SharingAcc t

-- |Conversion from HOAS to de Bruijn expression AST
-- -

-- |Convert an open expression with given environment layouts.
--
convertOpenExp :: forall t env aenv. 
                  Layout env  env       -- scalar environment
               -> Layout aenv aenv      -- array environment
               -> [StableSharingAcc]    -- currently bound sharing variables
               -> SharingExp t          -- expression to be converted
               -> AST.OpenExp env aenv t
convertOpenExp lyt alyt env = cvt
  where
    cvt :: SharingExp t' -> AST.OpenExp env aenv t'
    cvt (Tag i)             = AST.Var (prjIdx i lyt)
    cvt (Const v)           = AST.Const (fromElt v)
    cvt (Tuple tup)         = AST.Tuple (convertTuple lyt alyt env tup)
    cvt (Prj idx e)         = AST.Prj idx (cvt e)
    cvt IndexNil            = AST.IndexNil
    cvt (IndexCons ix i)    = AST.IndexCons (cvt ix) (cvt i)
    cvt (IndexHead i)       = AST.IndexHead (cvt i)
    cvt (IndexTail ix)      = AST.IndexTail (cvt ix)
    cvt (IndexAny)          = AST.IndexAny
    cvt (Cond e1 e2 e3)     = AST.Cond (cvt e1) (cvt e2) (cvt e3)
    cvt (PrimConst c)       = AST.PrimConst c
    cvt (PrimApp p e)       = AST.PrimApp p (cvt e)
    cvt (IndexScalar a e)   = AST.IndexScalar (convertSharingAcc alyt env a) (cvt e)
    cvt (Shape a)           = AST.Shape (convertSharingAcc alyt env a)
    cvt (Size a)            = AST.Size (convertSharingAcc alyt env a)

-- |Convert a tuple expression
--
convertTuple :: Layout env env 
             -> Layout aenv aenv 
             -> [StableSharingAcc]                 -- currently bound sharing variables
             -> Tuple.Tuple (PreExp SharingAcc) t 
             -> Tuple.Tuple (AST.OpenExp env aenv) t
convertTuple _lyt _alyt _env NilTup           = NilTup
convertTuple lyt  alyt  env  (es `SnocTup` e) 
  = convertTuple lyt alyt env es `SnocTup` convertOpenExp lyt alyt env e

-- |Convert an expression closed wrt to scalar variables
--
convertExp :: Layout aenv aenv      -- array environment
           -> [StableSharingAcc]    -- currently bound sharing variables
           -> SharingExp t          -- expression to be converted
           -> AST.Exp aenv t
convertExp alyt env = convertOpenExp EmptyLayout alyt env

-- |Convert a unary functions
--
convertFun1 :: forall a b aenv. Elt a
            => Layout aenv aenv 
            -> [StableSharingAcc]               -- currently bound sharing variables
            -> (Exp a -> SharingExp b) 
            -> AST.Fun aenv (a -> b)
convertFun1 alyt env f = Lam (Body openF)
  where
    a     = Tag 0
    lyt   = EmptyLayout 
            `PushLayout` 
            (ZeroIdx :: Idx ((), EltRepr a) (EltRepr a))
    openF = convertOpenExp lyt alyt env (f a)

-- |Convert a binary functions
--
convertFun2 :: forall a b c aenv. (Elt a, Elt b) 
            => Layout aenv aenv 
            -> [StableSharingAcc]               -- currently bound sharing variables
            -> (Exp a -> Exp b -> SharingExp c) 
            -> AST.Fun aenv (a -> b -> c)
convertFun2 alyt env f = Lam (Lam (Body openF))
  where
    a     = Tag 1
    b     = Tag 0
    lyt   = EmptyLayout 
            `PushLayout`
            (SuccIdx ZeroIdx :: Idx (((), EltRepr a), EltRepr b) (EltRepr a))
            `PushLayout`
            (ZeroIdx         :: Idx (((), EltRepr a), EltRepr b) (EltRepr b))
    openF = convertOpenExp lyt alyt env (f a b)

-- Convert a unary stencil function
--
convertStencilFun :: forall sh a stencil b aenv. (Elt a, Stencil sh a stencil)
                  => SharingAcc (Array sh a)            -- just passed to fix the type variables
                  -> Layout aenv aenv 
                  -> [StableSharingAcc]                 -- currently bound sharing variables
                  -> (stencil -> SharingExp b)
                  -> AST.Fun aenv (StencilRepr sh stencil -> b)
convertStencilFun _ alyt env stencilFun = Lam (Body openStencilFun)
  where
    stencil = Tag 0 :: Exp (StencilRepr sh stencil)
    lyt     = EmptyLayout 
              `PushLayout` 
              (ZeroIdx :: Idx ((), EltRepr (StencilRepr sh stencil)) 
                              (EltRepr (StencilRepr sh stencil)))
    openStencilFun = convertOpenExp lyt alyt env $
                       stencilFun (stencilPrj (undefined::sh) (undefined::a) stencil)

-- Convert a binary stencil function
--
convertStencilFun2 :: forall sh a b stencil1 stencil2 c aenv. 
                      (Elt a, Stencil sh a stencil1,
                       Elt b, Stencil sh b stencil2)
                   => SharingAcc (Array sh a)           -- just passed to fix the type variables
                   -> SharingAcc (Array sh b)           -- just passed to fix the type variables
                   -> Layout aenv aenv 
                   -> [StableSharingAcc]                 -- currently bound sharing variables
                   -> (stencil1 -> stencil2 -> SharingExp c)
                   -> AST.Fun aenv (StencilRepr sh stencil1 ->
                                    StencilRepr sh stencil2 -> c)
convertStencilFun2 _ _ alyt env stencilFun = Lam (Lam (Body openStencilFun))
  where
    stencil1 = Tag 1 :: Exp (StencilRepr sh stencil1)
    stencil2 = Tag 0 :: Exp (StencilRepr sh stencil2)
    lyt     = EmptyLayout 
              `PushLayout` 
              (SuccIdx ZeroIdx :: Idx (((), EltRepr (StencilRepr sh stencil1)),
                                            EltRepr (StencilRepr sh stencil2)) 
                                       (EltRepr (StencilRepr sh stencil1)))
              `PushLayout` 
              (ZeroIdx         :: Idx (((), EltRepr (StencilRepr sh stencil1)),
                                            EltRepr (StencilRepr sh stencil2)) 
                                       (EltRepr (StencilRepr sh stencil2)))
    openStencilFun = convertOpenExp lyt alyt env $
                       stencilFun (stencilPrj (undefined::sh) (undefined::a) stencil1)
                                  (stencilPrj (undefined::sh) (undefined::b) stencil2)


-- Pretty printing
--

instance Arrays arrs => Show (Acc arrs) where
  show = show . convertAcc
  
instance Show (Exp a) where
  show = show . convertExp EmptyLayout [] . toSharingExp
    where
      toSharingExp :: Exp b -> SharingExp b
      toSharingExp (Tag i)             = Tag i
      toSharingExp (Const v)           = Const v
      toSharingExp (Tuple tup)         = Tuple (toSharingTup tup)
      toSharingExp (Prj idx e)         = Prj idx (toSharingExp e)
      toSharingExp IndexNil            = IndexNil
      toSharingExp (IndexCons ix i)    = IndexCons (toSharingExp ix) (toSharingExp i)
      toSharingExp (IndexHead ix)      = IndexHead (toSharingExp ix)
      toSharingExp (IndexTail ix)      = IndexTail (toSharingExp ix)
      toSharingExp (IndexAny)          = IndexAny
      toSharingExp (Cond e1 e2 e3)     = Cond (toSharingExp e1) (toSharingExp e2) (toSharingExp e3)
      toSharingExp (PrimConst c)       = PrimConst c
      toSharingExp (PrimApp p e)       = PrimApp p (toSharingExp e)
      toSharingExp (IndexScalar a e)   = IndexScalar (recoverSharing False a) (toSharingExp e)
      toSharingExp (Shape a)           = Shape (recoverSharing False a)
      toSharingExp (Size a)            = Size (recoverSharing False a)

      toSharingTup :: Tuple.Tuple (PreExp Acc) tup -> Tuple.Tuple (PreExp SharingAcc) tup
      toSharingTup NilTup          = NilTup
      toSharingTup (SnocTup tup e) = SnocTup (toSharingTup tup) (toSharingExp e)

-- for debugging
showPreAccOp :: PreAcc acc arrs -> String
showPreAccOp (Atag _)             = "Atag"                   
showPreAccOp (Pipe _ _ _)         = "Pipe"
showPreAccOp (Acond _ _ _)        = "Acond"
showPreAccOp (FstArray _)         = "FstArray"
showPreAccOp (SndArray _)         = "SndArray"
showPreAccOp (PairArrays _ _)     = "PairArrays"
showPreAccOp (Use _)              = "Use"
showPreAccOp (Unit _)             = "Unit"
showPreAccOp (Generate _ _)       = "Generate"
showPreAccOp (Reshape _ _)        = "Reshape"
showPreAccOp (Replicate _ _)      = "Replicate"
showPreAccOp (Index _ _)          = "Index"
showPreAccOp (Map _ _)            = "Map"
showPreAccOp (ZipWith _ _ _)      = "ZipWith"
showPreAccOp (Fold _ _ _)         = "Fold"
showPreAccOp (Fold1 _ _)          = "Fold1"
showPreAccOp (FoldSeg _ _ _ _)    = "FoldSeg"
showPreAccOp (Fold1Seg _ _ _)     = "Fold1Seg"
showPreAccOp (Scanl _ _ _)        = "Scanl"
showPreAccOp (Scanl' _ _ _)       = "Scanl'"
showPreAccOp (Scanl1 _ _)         = "Scanl1"
showPreAccOp (Scanr _ _ _)        = "Scanr"
showPreAccOp (Scanr' _ _ _)       = "Scanr'"
showPreAccOp (Scanr1 _ _)         = "Scanr1"
showPreAccOp (Permute _ _ _ _)    = "Permute"
showPreAccOp (Backpermute _ _ _)  = "Backpermute"
showPreAccOp (Stencil _ _ _)      = "Stencil"
showPreAccOp (Stencil2 _ _ _ _ _) = "Stencil2"

_showSharingAccOp :: SharingAcc arrs -> String
_showSharingAccOp (VarSharing sn)    = "VAR " ++ show (hashStableName sn)
_showSharingAccOp (LetSharing _ acc) = "LET " ++ _showSharingAccOp acc
_showSharingAccOp (AccSharing _ acc) = showPreAccOp acc


-- |Smart constructors to construct representation AST forms
-- ---------------------------------------------------------

mkIndex :: forall slix e aenv. (Slice slix, Elt e)
        => AST.OpenAcc                aenv (Array (FullShape slix) e)
        -> AST.Exp                    aenv slix
        -> AST.PreOpenAcc AST.OpenAcc aenv (Array (SliceShape slix) e)
mkIndex arr e
  = AST.Index (sliceIndex slix) arr e
  where
    slix = undefined :: slix

mkReplicate :: forall slix e aenv. (Slice slix, Elt e)
        => AST.Exp                    aenv slix
        -> AST.OpenAcc                aenv (Array (SliceShape slix) e)
        -> AST.PreOpenAcc AST.OpenAcc aenv (Array (FullShape slix) e)
mkReplicate e arr
  = AST.Replicate (sliceIndex slix) e arr
  where
    slix = undefined :: slix


-- |Smart constructors for stencil reification
-- -------------------------------------------

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
  stencilPrj _ _ s = (Prj tix2 s, 
                      Prj tix1 s, 
                      Prj tix0 s)
instance Elt e => Stencil DIM1 e (Exp e, Exp e, Exp e, Exp e, Exp e) where
  type StencilRepr DIM1 (Exp e, Exp e, Exp e, Exp e, Exp e)
    = (e, e, e, e, e)
  stencilPrj _ _ s = (Prj tix4 s, 
                      Prj tix3 s, 
                      Prj tix2 s, 
                      Prj tix1 s, 
                      Prj tix0 s)
instance Elt e => Stencil DIM1 e (Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e) where
  type StencilRepr DIM1 (Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e) 
    = (e, e, e, e, e, e, e)
  stencilPrj _ _ s = (Prj tix6 s, 
                      Prj tix5 s, 
                      Prj tix4 s, 
                      Prj tix3 s, 
                      Prj tix2 s, 
                      Prj tix1 s, 
                      Prj tix0 s)
instance Elt e => Stencil DIM1 e (Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e) where
  type StencilRepr DIM1 (Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e, Exp e)
    = (e, e, e, e, e, e, e, e, e)
  stencilPrj _ _ s = (Prj tix8 s, 
                      Prj tix7 s, 
                      Prj tix6 s, 
                      Prj tix5 s, 
                      Prj tix4 s, 
                      Prj tix3 s, 
                      Prj tix2 s, 
                      Prj tix1 s, 
                      Prj tix0 s)

-- DIM(n+1)
instance (Stencil (sh:.Int) a row2, 
          Stencil (sh:.Int) a row1,
          Stencil (sh:.Int) a row0) => Stencil (sh:.Int:.Int) a (row2, row1, row0) where
  type StencilRepr (sh:.Int:.Int) (row2, row1, row0) 
    = (StencilRepr (sh:.Int) row2, StencilRepr (sh:.Int) row1, StencilRepr (sh:.Int) row0)
  stencilPrj _ a s = (stencilPrj (undefined::(sh:.Int)) a (Prj tix2 s), 
                      stencilPrj (undefined::(sh:.Int)) a (Prj tix1 s), 
                      stencilPrj (undefined::(sh:.Int)) a (Prj tix0 s))
instance (Stencil (sh:.Int) a row1,
          Stencil (sh:.Int) a row2,
          Stencil (sh:.Int) a row3,
          Stencil (sh:.Int) a row4,
          Stencil (sh:.Int) a row5) => Stencil (sh:.Int:.Int) a (row1, row2, row3, row4, row5) where
  type StencilRepr (sh:.Int:.Int) (row1, row2, row3, row4, row5) 
    = (StencilRepr (sh:.Int) row1, StencilRepr (sh:.Int) row2, StencilRepr (sh:.Int) row3,
       StencilRepr (sh:.Int) row4, StencilRepr (sh:.Int) row5)
  stencilPrj _ a s = (stencilPrj (undefined::(sh:.Int)) a (Prj tix4 s), 
                      stencilPrj (undefined::(sh:.Int)) a (Prj tix3 s), 
                      stencilPrj (undefined::(sh:.Int)) a (Prj tix2 s), 
                      stencilPrj (undefined::(sh:.Int)) a (Prj tix1 s), 
                      stencilPrj (undefined::(sh:.Int)) a (Prj tix0 s))
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
  stencilPrj _ a s = (stencilPrj (undefined::(sh:.Int)) a (Prj tix6 s), 
                      stencilPrj (undefined::(sh:.Int)) a (Prj tix5 s), 
                      stencilPrj (undefined::(sh:.Int)) a (Prj tix4 s), 
                      stencilPrj (undefined::(sh:.Int)) a (Prj tix3 s), 
                      stencilPrj (undefined::(sh:.Int)) a (Prj tix2 s), 
                      stencilPrj (undefined::(sh:.Int)) a (Prj tix1 s), 
                      stencilPrj (undefined::(sh:.Int)) a (Prj tix0 s))
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
  stencilPrj _ a s = (stencilPrj (undefined::(sh:.Int)) a (Prj tix8 s), 
                      stencilPrj (undefined::(sh:.Int)) a (Prj tix7 s), 
                      stencilPrj (undefined::(sh:.Int)) a (Prj tix6 s), 
                      stencilPrj (undefined::(sh:.Int)) a (Prj tix5 s), 
                      stencilPrj (undefined::(sh:.Int)) a (Prj tix4 s), 
                      stencilPrj (undefined::(sh:.Int)) a (Prj tix3 s), 
                      stencilPrj (undefined::(sh:.Int)) a (Prj tix2 s), 
                      stencilPrj (undefined::(sh:.Int)) a (Prj tix1 s), 
                      stencilPrj (undefined::(sh:.Int)) a (Prj tix0 s))
  
-- Auxiliary tuple index constants
--
tix0 :: Elt s => TupleIdx (t, s) s
tix0 = ZeroTupIdx
tix1 :: Elt s => TupleIdx ((t, s), s1) s
tix1 = SuccTupIdx tix0
tix2 :: Elt s => TupleIdx (((t, s), s1), s2) s
tix2 = SuccTupIdx tix1
tix3 :: Elt s => TupleIdx ((((t, s), s1), s2), s3) s
tix3 = SuccTupIdx tix2
tix4 :: Elt s => TupleIdx (((((t, s), s1), s2), s3), s4) s
tix4 = SuccTupIdx tix3
tix5 :: Elt s => TupleIdx ((((((t, s), s1), s2), s3), s4), s5) s
tix5 = SuccTupIdx tix4
tix6 :: Elt s => TupleIdx (((((((t, s), s1), s2), s3), s4), s5), s6) s
tix6 = SuccTupIdx tix5
tix7 :: Elt s => TupleIdx ((((((((t, s), s1), s2), s3), s4), s5), s6), s7) s
tix7 = SuccTupIdx tix6
tix8 :: Elt s => TupleIdx (((((((((t, s), s1), s2), s3), s4), s5), s6), s7), s8) s
tix8 = SuccTupIdx tix7

-- Pushes the 'Acc' constructor through a pair
--
unpair :: (Shape sh1, Shape sh2, Elt e1, Elt e2)
       => Acc (Array sh1 e1, Array sh2 e2) 
       -> (Acc (Array sh1 e1), Acc (Array sh2 e2))
unpair acc = (Acc $ FstArray acc, Acc $ SndArray acc)

-- Creates an 'Acc' pair from two separate 'Acc's.
--
pair :: (Shape sh1, Shape sh2, Elt e1, Elt e2)
     => Acc (Array sh1 e1)
     -> Acc (Array sh2 e2)
     -> Acc (Array sh1 e1, Array sh2 e2)
pair acc1 acc2 = Acc $ PairArrays acc1 acc2


-- Smart constructor for literals
-- 

-- |Constant scalar expression
--
constant :: Elt t => t -> Exp t
constant = Const

-- Smart constructor and destructors for tuples
--

tup2 :: (Elt a, Elt b) => (Exp a, Exp b) -> Exp (a, b)
tup2 (x1, x2) = Tuple (NilTup `SnocTup` x1 `SnocTup` x2)

tup3 :: (Elt a, Elt b, Elt c) => (Exp a, Exp b, Exp c) -> Exp (a, b, c)
tup3 (x1, x2, x3) = Tuple (NilTup `SnocTup` x1 `SnocTup` x2 `SnocTup` x3)

tup4 :: (Elt a, Elt b, Elt c, Elt d) 
     => (Exp a, Exp b, Exp c, Exp d) -> Exp (a, b, c, d)
tup4 (x1, x2, x3, x4) 
  = Tuple (NilTup `SnocTup` x1 `SnocTup` x2 `SnocTup` x3 `SnocTup` x4)

tup5 :: (Elt a, Elt b, Elt c, Elt d, Elt e) 
     => (Exp a, Exp b, Exp c, Exp d, Exp e) -> Exp (a, b, c, d, e)
tup5 (x1, x2, x3, x4, x5)
  = Tuple $
      NilTup `SnocTup` x1 `SnocTup` x2 `SnocTup` x3 `SnocTup` x4 `SnocTup` x5

tup6 :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f)
     => (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f) -> Exp (a, b, c, d, e, f)
tup6 (x1, x2, x3, x4, x5, x6)
  = Tuple $
      NilTup `SnocTup` x1 `SnocTup` x2 `SnocTup` x3 `SnocTup` x4 `SnocTup` x5 `SnocTup` x6

tup7 :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g)
     => (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g)
     -> Exp (a, b, c, d, e, f, g)
tup7 (x1, x2, x3, x4, x5, x6, x7)
  = Tuple $
      NilTup `SnocTup` x1 `SnocTup` x2 `SnocTup` x3
	     `SnocTup` x4 `SnocTup` x5 `SnocTup` x6 `SnocTup` x7

tup8 :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h)
     => (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h)
     -> Exp (a, b, c, d, e, f, g, h)
tup8 (x1, x2, x3, x4, x5, x6, x7, x8)
  = Tuple $
      NilTup `SnocTup` x1 `SnocTup` x2 `SnocTup` x3 `SnocTup` x4
	     `SnocTup` x5 `SnocTup` x6 `SnocTup` x7 `SnocTup` x8

tup9 :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i)
     => (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i)
     -> Exp (a, b, c, d, e, f, g, h, i)
tup9 (x1, x2, x3, x4, x5, x6, x7, x8, x9)
  = Tuple $
      NilTup `SnocTup` x1 `SnocTup` x2 `SnocTup` x3 `SnocTup` x4
	     `SnocTup` x5 `SnocTup` x6 `SnocTup` x7 `SnocTup` x8 `SnocTup` x9

untup2 :: (Elt a, Elt b) => Exp (a, b) -> (Exp a, Exp b)
untup2 e = (SuccTupIdx ZeroTupIdx `Prj` e, ZeroTupIdx `Prj` e)

untup3 :: (Elt a, Elt b, Elt c) => Exp (a, b, c) -> (Exp a, Exp b, Exp c)
untup3 e = (SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` e, 
            SuccTupIdx ZeroTupIdx `Prj` e, 
            ZeroTupIdx `Prj` e)

untup4 :: (Elt a, Elt b, Elt c, Elt d) 
       => Exp (a, b, c, d) -> (Exp a, Exp b, Exp c, Exp d)
untup4 e = (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` e, 
            SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` e, 
            SuccTupIdx ZeroTupIdx `Prj` e, 
            ZeroTupIdx `Prj` e)

untup5 :: (Elt a, Elt b, Elt c, Elt d, Elt e) 
       => Exp (a, b, c, d, e) -> (Exp a, Exp b, Exp c, Exp d, Exp e)
untup5 e = (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))) 
            `Prj` e, 
            SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` e, 
            SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` e, 
            SuccTupIdx ZeroTupIdx `Prj` e, 
            ZeroTupIdx `Prj` e)

untup6 :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f)
       => Exp (a, b, c, d, e, f) -> (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f)
untup6 e = (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))) `Prj` e,
            SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))) `Prj` e,
            SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` e,
            SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` e,
            SuccTupIdx ZeroTupIdx `Prj` e,
            ZeroTupIdx `Prj` e)

untup7 :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g)
       => Exp (a, b, c, d, e, f, g) -> (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g)
untup7 e = (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))) `Prj` e,
            SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))) `Prj` e,
            SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))) `Prj` e,
            SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` e,
            SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` e,
            SuccTupIdx ZeroTupIdx `Prj` e,
            ZeroTupIdx `Prj` e)

untup8 :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h)
       => Exp (a, b, c, d, e, f, g, h) -> (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h)
untup8 e = (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))) `Prj` e,
            SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))) `Prj` e,
            SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))) `Prj` e,
            SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))) `Prj` e,
            SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` e,
            SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` e,
            SuccTupIdx ZeroTupIdx `Prj` e,
            ZeroTupIdx `Prj` e)

untup9 :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i)
       => Exp (a, b, c, d, e, f, g, h, i) -> (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i)
untup9 e = (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))))) `Prj` e,
            SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))))) `Prj` e,
            SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))))) `Prj` e,
            SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)))) `Prj` e,
            SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))) `Prj` e,
            SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` e,
            SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` e,
            SuccTupIdx ZeroTupIdx `Prj` e,
            ZeroTupIdx `Prj` e)

-- Smart constructor for constants
-- 

mkMinBound :: (Elt t, IsBounded t) => Exp t
mkMinBound = PrimConst (PrimMinBound boundedType)

mkMaxBound :: (Elt t, IsBounded t) => Exp t
mkMaxBound = PrimConst (PrimMaxBound boundedType)

mkPi :: (Elt r, IsFloating r) => Exp r
mkPi = PrimConst (PrimPi floatingType)


-- Smart constructors for primitive applications
--

-- Operators from Floating

mkSin :: (Elt t, IsFloating t) => Exp t -> Exp t
mkSin x = PrimSin floatingType `PrimApp` x

mkCos :: (Elt t, IsFloating t) => Exp t -> Exp t
mkCos x = PrimCos floatingType `PrimApp` x

mkTan :: (Elt t, IsFloating t) => Exp t -> Exp t
mkTan x = PrimTan floatingType `PrimApp` x

mkAsin :: (Elt t, IsFloating t) => Exp t -> Exp t
mkAsin x = PrimAsin floatingType `PrimApp` x

mkAcos :: (Elt t, IsFloating t) => Exp t -> Exp t
mkAcos x = PrimAcos floatingType `PrimApp` x

mkAtan :: (Elt t, IsFloating t) => Exp t -> Exp t
mkAtan x = PrimAtan floatingType `PrimApp` x

mkAsinh :: (Elt t, IsFloating t) => Exp t -> Exp t
mkAsinh x = PrimAsinh floatingType `PrimApp` x

mkAcosh :: (Elt t, IsFloating t) => Exp t -> Exp t
mkAcosh x = PrimAcosh floatingType `PrimApp` x

mkAtanh :: (Elt t, IsFloating t) => Exp t -> Exp t
mkAtanh x = PrimAtanh floatingType `PrimApp` x

mkExpFloating :: (Elt t, IsFloating t) => Exp t -> Exp t
mkExpFloating x = PrimExpFloating floatingType `PrimApp` x

mkSqrt :: (Elt t, IsFloating t) => Exp t -> Exp t
mkSqrt x = PrimSqrt floatingType `PrimApp` x

mkLog :: (Elt t, IsFloating t) => Exp t -> Exp t
mkLog x = PrimLog floatingType `PrimApp` x

mkFPow :: (Elt t, IsFloating t) => Exp t -> Exp t -> Exp t
mkFPow x y = PrimFPow floatingType `PrimApp` tup2 (x, y)

mkLogBase :: (Elt t, IsFloating t) => Exp t -> Exp t -> Exp t
mkLogBase x y = PrimLogBase floatingType `PrimApp` tup2 (x, y)

-- Operators from Num

mkAdd :: (Elt t, IsNum t) => Exp t -> Exp t -> Exp t
mkAdd x y = PrimAdd numType `PrimApp` tup2 (x, y)

mkSub :: (Elt t, IsNum t) => Exp t -> Exp t -> Exp t
mkSub x y = PrimSub numType `PrimApp` tup2 (x, y)

mkMul :: (Elt t, IsNum t) => Exp t -> Exp t -> Exp t
mkMul x y = PrimMul numType `PrimApp` tup2 (x, y)

mkNeg :: (Elt t, IsNum t) => Exp t -> Exp t
mkNeg x = PrimNeg numType `PrimApp` x

mkAbs :: (Elt t, IsNum t) => Exp t -> Exp t
mkAbs x = PrimAbs numType `PrimApp` x

mkSig :: (Elt t, IsNum t) => Exp t -> Exp t
mkSig x = PrimSig numType `PrimApp` x

-- Operators from Integral & Bits

mkQuot :: (Elt t, IsIntegral t) => Exp t -> Exp t -> Exp t
mkQuot x y = PrimQuot integralType `PrimApp` tup2 (x, y)

mkRem :: (Elt t, IsIntegral t) => Exp t -> Exp t -> Exp t
mkRem x y = PrimRem integralType `PrimApp` tup2 (x, y)

mkIDiv :: (Elt t, IsIntegral t) => Exp t -> Exp t -> Exp t
mkIDiv x y = PrimIDiv integralType `PrimApp` tup2 (x, y)

mkMod :: (Elt t, IsIntegral t) => Exp t -> Exp t -> Exp t
mkMod x y = PrimMod integralType `PrimApp` tup2 (x, y)

mkBAnd :: (Elt t, IsIntegral t) => Exp t -> Exp t -> Exp t
mkBAnd x y = PrimBAnd integralType `PrimApp` tup2 (x, y)

mkBOr :: (Elt t, IsIntegral t) => Exp t -> Exp t -> Exp t
mkBOr x y = PrimBOr integralType `PrimApp` tup2 (x, y)

mkBXor :: (Elt t, IsIntegral t) => Exp t -> Exp t -> Exp t
mkBXor x y = PrimBXor integralType `PrimApp` tup2 (x, y)

mkBNot :: (Elt t, IsIntegral t) => Exp t -> Exp t
mkBNot x = PrimBNot integralType `PrimApp` x

mkBShiftL :: (Elt t, IsIntegral t) => Exp t -> Exp Int -> Exp t
mkBShiftL x i = PrimBShiftL integralType `PrimApp` tup2 (x, i)

mkBShiftR :: (Elt t, IsIntegral t) => Exp t -> Exp Int -> Exp t
mkBShiftR x i = PrimBShiftR integralType `PrimApp` tup2 (x, i)

mkBRotateL :: (Elt t, IsIntegral t) => Exp t -> Exp Int -> Exp t
mkBRotateL x i = PrimBRotateL integralType `PrimApp` tup2 (x, i)

mkBRotateR :: (Elt t, IsIntegral t) => Exp t -> Exp Int -> Exp t
mkBRotateR x i = PrimBRotateR integralType `PrimApp` tup2 (x, i)

-- Operators from Fractional

mkFDiv :: (Elt t, IsFloating t) => Exp t -> Exp t -> Exp t
mkFDiv x y = PrimFDiv floatingType `PrimApp` tup2 (x, y)

mkRecip :: (Elt t, IsFloating t) => Exp t -> Exp t
mkRecip x = PrimRecip floatingType `PrimApp` x

-- Operators from RealFrac

mkTruncate :: (Elt a, Elt b, IsFloating a, IsIntegral b) => Exp a -> Exp b
mkTruncate x = PrimTruncate floatingType integralType `PrimApp` x

mkRound :: (Elt a, Elt b, IsFloating a, IsIntegral b) => Exp a -> Exp b
mkRound x = PrimRound floatingType integralType `PrimApp` x

mkFloor :: (Elt a, Elt b, IsFloating a, IsIntegral b) => Exp a -> Exp b
mkFloor x = PrimFloor floatingType integralType `PrimApp` x

mkCeiling :: (Elt a, Elt b, IsFloating a, IsIntegral b) => Exp a -> Exp b
mkCeiling x = PrimCeiling floatingType integralType `PrimApp` x

-- Operators from RealFloat

mkAtan2 :: (Elt t, IsFloating t) => Exp t -> Exp t -> Exp t
mkAtan2 x y = PrimAtan2 floatingType `PrimApp` tup2 (x, y)

-- FIXME: add missing operations from Floating, RealFrac & RealFloat

-- Relational and equality operators

mkLt :: (Elt t, IsScalar t) => Exp t -> Exp t -> Exp Bool
mkLt x y = PrimLt scalarType `PrimApp` tup2 (x, y)

mkGt :: (Elt t, IsScalar t) => Exp t -> Exp t -> Exp Bool
mkGt x y = PrimGt scalarType `PrimApp` tup2 (x, y)

mkLtEq :: (Elt t, IsScalar t) => Exp t -> Exp t -> Exp Bool
mkLtEq x y = PrimLtEq scalarType `PrimApp` tup2 (x, y)

mkGtEq :: (Elt t, IsScalar t) => Exp t -> Exp t -> Exp Bool
mkGtEq x y = PrimGtEq scalarType `PrimApp` tup2 (x, y)

mkEq :: (Elt t, IsScalar t) => Exp t -> Exp t -> Exp Bool
mkEq x y = PrimEq scalarType `PrimApp` tup2 (x, y)

mkNEq :: (Elt t, IsScalar t) => Exp t -> Exp t -> Exp Bool
mkNEq x y = PrimNEq scalarType `PrimApp` tup2 (x, y)

mkMax :: (Elt t, IsScalar t) => Exp t -> Exp t -> Exp t
mkMax x y = PrimMax scalarType `PrimApp` tup2 (x, y)

mkMin :: (Elt t, IsScalar t) => Exp t -> Exp t -> Exp t
mkMin x y = PrimMin scalarType `PrimApp` tup2 (x, y)

-- Logical operators

mkLAnd :: Exp Bool -> Exp Bool -> Exp Bool
mkLAnd x y = PrimLAnd `PrimApp` tup2 (x, y)

mkLOr :: Exp Bool -> Exp Bool -> Exp Bool
mkLOr x y = PrimLOr `PrimApp` tup2 (x, y)

mkLNot :: Exp Bool -> Exp Bool
mkLNot x = PrimLNot `PrimApp` x

-- FIXME: Character conversions

-- FIXME: Numeric conversions

mkFromIntegral :: (Elt a, Elt b, IsIntegral a, IsNum b) => Exp a -> Exp b
mkFromIntegral x = PrimFromIntegral integralType numType `PrimApp` x

-- FIXME: Other conversions

mkBoolToInt :: Exp Bool -> Exp Int
mkBoolToInt b = PrimBoolToInt `PrimApp` b


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
