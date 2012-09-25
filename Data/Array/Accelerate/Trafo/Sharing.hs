{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -fno-warn-orphans        #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
-- |
-- Module      : Data.Array.Accelerate.Trafo.Sharing
-- Copyright   : [2008..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
--               [2009..2012] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module implements HOAS to de Bruijn conversion of array expressions
-- while incorporating sharing information.
--

module Data.Array.Accelerate.Trafo.Sharing (

  -- * HOAS -> de Bruijn conversion
  convertAcc, convertAccFun1

) where

-- standard library
import Control.Applicative                              hiding ( Const )
import Control.Monad.Fix
import Data.List
import Data.Maybe
import Data.Hashable
import Data.Typeable
import qualified Data.HashTable.IO                      as Hash
import qualified Data.IntMap                            as IntMap
import System.IO.Unsafe                                 ( unsafePerformIO )
import System.Mem.StableName

-- friends
import Data.Array.Accelerate.Debug
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Array.Sugar                as Sugar
import Data.Array.Accelerate.Tuple                      hiding ( Tuple )
import Data.Array.Accelerate.AST                        hiding (
  PreOpenAcc(..), OpenAcc(..), Acc, Stencil(..), PreOpenExp(..), OpenExp, PreExp, Exp )
import qualified Data.Array.Accelerate.AST              as AST
import qualified Data.Array.Accelerate.Tuple            as Tuple

#include "accelerate.h"

type HashTable key val  = Hash.BasicHashTable key val


-- Configuration
-- -------------
--
-- This is mostly for debugging...
--

-- Recover the sharing of array computations?
--
recoverAccSharing :: Bool
recoverAccSharing = True

-- Are array computations floated out of expressions irrespective of whether they are shared or
-- not?  'True' implies floating them out.  (Requires 'recoverAccSharing' to be 'True' as well.)
--
floatOutAccFromExp :: Bool
floatOutAccFromExp = recoverAccSharing && True

-- Recover the sharing of scalar expressions?
--
recoverExpSharing :: Bool
recoverExpSharing = True


-- Layouts
-- -------

-- A layout of an environment has an entry for each entry of the environment.
-- Each entry in the layout holds the de Bruijn index that refers to the
-- corresponding entry in the environment.
--
data Layout env env' where
  EmptyLayout :: Layout env ()
  PushLayout  :: Typeable t
              => Layout env env' -> Idx env t -> Layout env (env', t)

-- Project the nth index out of an environment layout.
--
-- The first argument provides context information for error messages in the case of failure.
--
prjIdx :: forall t env env'. Typeable t => String -> Int -> Layout env env' -> Idx env t
prjIdx ctxt 0 (PushLayout _ (ix :: Idx env0 t0))
  = case gcast ix of
      Just ix' -> ix'
      Nothing  -> possiblyNestedErr ctxt $
                    "Couldn't match expected type `" ++ show (typeOf (undefined::t)) ++
                    "' with actual type `" ++ show (typeOf (undefined::t0)) ++ "'" ++
                    "\n  Type mismatch"
prjIdx ctxt n (PushLayout l _)  = prjIdx ctxt (n - 1) l
prjIdx ctxt _ EmptyLayout       = possiblyNestedErr ctxt "Environment doesn't contain index"

possiblyNestedErr :: String -> String -> a
possiblyNestedErr ctxt failreason
  = error $ "Fatal error in Sharing.prjIdx:"
      ++ "\n  " ++ failreason ++ " at " ++ ctxt
      ++ "\n  Possible reason: nested data parallelism — array computation that depends on a"
      ++ "\n    scalar variable of type 'Exp a'"

-- Add an entry to a layout, incrementing all indices
--
incLayout :: Layout env env' -> Layout (env, t) env'
incLayout EmptyLayout         = EmptyLayout
incLayout (PushLayout lyt ix) = PushLayout (incLayout lyt) (SuccIdx ix)



-- Conversion from HOAS to de Bruijn computation AST
-- =================================================

-- Array computations
-- ------------------

-- | Convert a closed array expression to de Bruijn form while also incorporating sharing
-- information.
--
convertAcc :: Arrays arrs => Acc arrs -> AST.Acc arrs
convertAcc = convertOpenAcc 0 [] EmptyLayout

-- | Convert an open array expression to de Bruijn form while also incorporating sharing
-- information.
--
convertOpenAcc :: Arrays arrs => Level -> [Level] -> Layout aenv aenv -> Acc arrs -> AST.OpenAcc aenv arrs
convertOpenAcc lvl fvs alyt acc
  = let (sharingAcc, initialEnv) = recoverSharingAcc floatOutAccFromExp lvl fvs acc
    in
    convertSharingAcc alyt initialEnv sharingAcc

-- |Convert a unary function over array computations
--
convertAccFun1 :: forall a b. (Arrays a, Arrays b)
               => (Acc a -> Acc b)
               -> AST.Afun (a -> b)
convertAccFun1 f = Alam (Abody openF)
  where
    lvl   = 0
    a     = Atag lvl
    alyt  = EmptyLayout
            `PushLayout`
            (ZeroIdx :: Idx ((), a) a)
    openF = convertOpenAcc (lvl + 1) [lvl] alyt (f (Acc a))

-- | Convert an array expression with given array environment layout and sharing information into
-- de Bruijn form while recovering sharing at the same time (by introducing appropriate let
-- bindings).  The latter implements the third phase of sharing recovery.
--
-- The sharing environment 'env' keeps track of all currently bound sharing variables, keeping them
-- in reverse chronological order (outermost variable is at the end of the list).
--
convertSharingAcc :: forall a aenv. Arrays a
                  => Layout aenv aenv
                  -> [StableSharingAcc]
                  -> SharingAcc a
                  -> AST.OpenAcc aenv a
convertSharingAcc alyt env (AvarSharing sa)
  | Just i <- findIndex (matchStableAcc sa) env
  = AST.OpenAcc $ AST.Avar (prjIdx (ctxt ++ "; i = " ++ show i) i alyt)
  | null env
  = error $ "Cyclic definition of a value of type 'Acc' (sa = " ++
            show (hashStableNameHeight sa) ++ ")"
  | otherwise
  = INTERNAL_ERROR(error) "convertSharingAcc" err
  where
    ctxt = "shared 'Acc' tree with stable name " ++ show (hashStableNameHeight sa)
    err  = "inconsistent valuation @ " ++ ctxt ++ ";\n  env = " ++ show env
convertSharingAcc alyt env (AletSharing sa@(StableSharingAcc _ boundAcc) bodyAcc)
  = AST.OpenAcc
  $ let alyt' = incLayout alyt `PushLayout` ZeroIdx
    in
    AST.Alet (convertSharingAcc alyt env boundAcc) (convertSharingAcc alyt' (sa:env) bodyAcc)
convertSharingAcc alyt env (AccSharing _ preAcc)
  = AST.OpenAcc
  $ case preAcc of
      Atag i
        -> AST.Avar (prjIdx ("de Bruijn conversion tag " ++ show i) i alyt)
      Pipe afun1 afun2 acc
        -> let boundAcc = convertAccFun1 afun1 `AST.Apply` convertSharingAcc alyt env acc
               bodyAcc  = convertAccFun1 afun2 `AST.Apply` AST.OpenAcc (AST.Avar AST.ZeroIdx)
           in
           AST.Alet (AST.OpenAcc boundAcc) (AST.OpenAcc bodyAcc)
      Acond b acc1 acc2
        -> AST.Acond (convertExp alyt env b) (convertSharingAcc alyt env acc1)
                     (convertSharingAcc alyt env acc2)
      Atuple arrs
        -> AST.Atuple (convertSharingAtuple alyt env arrs)
      Aprj ix a
        -> AST.Aprj ix (convertSharingAcc alyt env a)
      Use array
        -> AST.Use (fromArr array)
      Unit e
        -> AST.Unit (convertExp alyt env e)
      Generate sh f
        -> AST.Generate (convertExp alyt env sh) (convertFun1 alyt env f)
      Reshape e acc
        -> AST.Reshape (convertExp alyt env e) (convertSharingAcc alyt env acc)
      Replicate ix acc
        -> mkReplicate (convertExp alyt env ix) (convertSharingAcc alyt env acc)
      Slice acc ix
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

convertSharingAtuple
    :: forall aenv a.
       Layout aenv aenv
    -> [StableSharingAcc]
    -> Tuple.Atuple SharingAcc a
    -> Tuple.Atuple (AST.OpenAcc aenv) a
convertSharingAtuple alyt aenv = cvt
  where
    cvt :: Tuple.Atuple SharingAcc a' -> Tuple.Atuple (AST.OpenAcc aenv) a'
    cvt NilAtup         = NilAtup
    cvt (SnocAtup t a)  = cvt t `SnocAtup` convertSharingAcc alyt aenv a


-- | Convert a boundary condition
--
convertBoundary :: Elt e => Boundary e -> Boundary (EltRepr e)
convertBoundary Clamp        = Clamp
convertBoundary Mirror       = Mirror
convertBoundary Wrap         = Wrap
convertBoundary (Constant e) = Constant (fromElt e)


-- Smart constructors to represent AST forms
--
mkIndex :: forall slix e aenv. (Slice slix, Elt e)
        => AST.OpenAcc                aenv (Array (FullShape  slix) e)
        -> AST.Exp                    aenv slix
        -> AST.PreOpenAcc AST.OpenAcc aenv (Array (SliceShape slix) e)
mkIndex arr e = AST.Slice (sliceIndex slix) arr e
  where
    slix = undefined :: slix

mkReplicate :: forall slix e aenv. (Slice slix, Elt e)
        => AST.Exp                    aenv slix
        -> AST.OpenAcc                aenv (Array (SliceShape slix) e)
        -> AST.PreOpenAcc AST.OpenAcc aenv (Array (FullShape  slix) e)
mkReplicate e arr = AST.Replicate (sliceIndex slix) e arr
  where
    slix = undefined :: slix



-- Scalar expressions
-- ------------------

-- | Convert an open expression with given environment layouts and sharing information into
-- de Bruijn form while recovering sharing at the same time (by introducing appropriate let
-- bindings).  The latter implements the third phase of sharing recovery.
--
-- The sharing environments 'env' and 'aenv' keep track of all currently bound sharing variables,
-- keeping them in reverse chronological order (outermost variable is at the end of the list).
--
convertSharingExp :: forall t env aenv
                   . Elt t
                  => Layout env  env      -- scalar environment
                  -> Layout aenv aenv     -- array environment
                  -> [StableSharingExp]   -- currently bound sharing variables of expressions
                  -> [StableSharingAcc]   -- currently bound sharing variables of array computations
                  -> SharingExp t         -- expression to be converted
                  -> AST.OpenExp env aenv t
convertSharingExp lyt alyt env aenv = cvt
  where
    cvt :: Elt t' => SharingExp t' -> AST.OpenExp env aenv t'
    cvt (VarSharing se)
      | Just i <- findIndex (matchStableExp se) env
      = AST.Var (prjIdx (ctxt ++ "; i = " ++ show i) i lyt)
      | null env
      = error $ "Cyclic definition of a value of type 'Exp' (sa = " ++ show (hashStableNameHeight se) ++ ")"
      | otherwise
      = INTERNAL_ERROR(error) "convertSharingExp" err
      where
        ctxt = "shared 'Exp' tree with stable name " ++ show (hashStableNameHeight se)
        err  = "inconsistent valuation @ " ++ ctxt ++ ";\n  env = " ++ show env
    cvt (LetSharing se@(StableSharingExp _ boundExp) bodyExp)
      = let lyt' = incLayout lyt `PushLayout` ZeroIdx
        in
        AST.Let (cvt boundExp) (convertSharingExp lyt' alyt (se:env) aenv bodyExp)
    cvt (ExpSharing _ pexp)
      = case pexp of
          Tag i           -> AST.Var (prjIdx ("de Bruijn conversion tag " ++ show i) i lyt)
          Const v         -> AST.Const (fromElt v)
          Tuple tup       -> AST.Tuple (convertTuple lyt alyt env aenv tup)
          Prj idx e       -> AST.Prj idx (cvt e)
          IndexNil        -> AST.IndexNil
          IndexCons ix i  -> AST.IndexCons (cvt ix) (cvt i)
          IndexHead i     -> AST.IndexHead (cvt i)
          IndexTail ix    -> AST.IndexTail (cvt ix)
          IndexAny        -> AST.IndexAny
          Cond e1 e2 e3   -> AST.Cond (cvt e1) (cvt e2) (cvt e3)
          PrimConst c     -> AST.PrimConst c
          PrimApp f e     -> case cvt e of
            AST.Let bnd body -> AST.Let bnd (AST.PrimApp f body)
            xs               -> AST.PrimApp f xs
          IndexScalar a e -> AST.IndexScalar (convertSharingAcc alyt aenv a) (cvt e)
          Shape a         -> AST.Shape (convertSharingAcc alyt aenv a)
          ShapeSize e     -> AST.ShapeSize (cvt e)

-- | Convert a tuple expression
--
convertTuple :: Layout env env
             -> Layout aenv aenv
             -> [StableSharingExp]                 -- currently bound scalar sharing-variables
             -> [StableSharingAcc]                 -- currently bound array sharing-variables
             -> Tuple.Tuple SharingExp t
             -> Tuple.Tuple (AST.OpenExp env aenv) t
convertTuple _lyt _alyt _env _aenv NilTup           = NilTup
convertTuple lyt  alyt  env  aenv  (es `SnocTup` e)
  = convertTuple lyt alyt env aenv es `SnocTup` convertSharingExp lyt alyt env aenv e

-- | Convert a scalar expression, which is closed with respect to scalar variables
--
convertExp :: Elt t
           => Layout aenv aenv      -- array environment
           -> [StableSharingAcc]    -- currently bound array sharing-variables
           -> RootExp t             -- expression to be converted
           -> AST.Exp aenv t
convertExp alyt aenv (EnvExp env exp) = convertSharingExp EmptyLayout alyt env aenv exp
convertExp _    _    _                = INTERNAL_ERROR(error) "convertExp" "not an 'EnvExp'"

-- | Convert a unary functions
--
convertFun1 :: forall a b aenv. (Elt a, Elt b)
            => Layout aenv aenv
            -> [StableSharingAcc]               -- currently bound array sharing-variables
            -> (Exp a -> RootExp b)
            -> AST.Fun aenv (a -> b)
convertFun1 alyt aenv f = Lam (Body openF)
  where
    a               = Exp $ undefined           -- the 'tag' was already embedded in Phase 1
    lyt             = EmptyLayout
                      `PushLayout`
                      (ZeroIdx :: Idx ((), a) a)
    EnvExp env body = f a
    openF           = convertSharingExp lyt alyt env aenv body

-- | Convert a binary functions
--
convertFun2 :: forall a b c aenv. (Elt a, Elt b, Elt c)
            => Layout aenv aenv
            -> [StableSharingAcc]               -- currently bound array sharing-variables
            -> (Exp a -> Exp b -> RootExp c)
            -> AST.Fun aenv (a -> b -> c)
convertFun2 alyt aenv f = Lam (Lam (Body openF))
  where
    a               = Exp $ undefined
    b               = Exp $ undefined
    lyt             = EmptyLayout
                      `PushLayout`
                      (SuccIdx ZeroIdx :: Idx (((), a), b) a)
                      `PushLayout`
                      (ZeroIdx         :: Idx (((), a), b) b)
    EnvExp env body = f a b
    openF           = convertSharingExp lyt alyt env aenv body

-- | Convert a unary stencil function
--
convertStencilFun :: forall sh a stencil b aenv. (Elt a, Stencil sh a stencil, Elt b)
                  => SharingAcc (Array sh a)            -- just passed to fix the type variables
                  -> Layout aenv aenv
                  -> [StableSharingAcc]                 -- currently bound array sharing-variables
                  -> (stencil -> RootExp b)
                  -> AST.Fun aenv (StencilRepr sh stencil -> b)
convertStencilFun _ alyt aenv stencilFun = Lam (Body openStencilFun)
  where
    stencil = Exp $ undefined :: Exp (StencilRepr sh stencil)
    lyt     = EmptyLayout
              `PushLayout`
              (ZeroIdx :: Idx ((), StencilRepr sh stencil)
                              (StencilRepr sh stencil))

    EnvExp env body = stencilFun (stencilPrj (undefined::sh) (undefined::a) stencil)
    openStencilFun  = convertSharingExp lyt alyt env aenv body

-- | Convert a binary stencil function
--
convertStencilFun2 :: forall sh a b stencil1 stencil2 c aenv.
                      (Elt a, Stencil sh a stencil1,
                       Elt b, Stencil sh b stencil2,
                       Elt c)
                   => SharingAcc (Array sh a)           -- just passed to fix the type variables
                   -> SharingAcc (Array sh b)           -- just passed to fix the type variables
                   -> Layout aenv aenv
                   -> [StableSharingAcc]                 -- currently bound array sharing-variables
                   -> (stencil1 -> stencil2 -> RootExp c)
                   -> AST.Fun aenv (StencilRepr sh stencil1 ->
                                    StencilRepr sh stencil2 -> c)
convertStencilFun2 _ _ alyt aenv stencilFun = Lam (Lam (Body openStencilFun))
  where
    stencil1 = Exp $ undefined :: Exp (StencilRepr sh stencil1)
    stencil2 = Exp $ undefined :: Exp (StencilRepr sh stencil2)
    lyt     = EmptyLayout
              `PushLayout`
              (SuccIdx ZeroIdx :: Idx (((), StencilRepr sh stencil1),
                                            StencilRepr sh stencil2)
                                       (StencilRepr sh stencil1))
              `PushLayout`
              (ZeroIdx         :: Idx (((), StencilRepr sh stencil1),
                                            StencilRepr sh stencil2)
                                       (StencilRepr sh stencil2))

    EnvExp env body = stencilFun (stencilPrj (undefined::sh) (undefined::a) stencil1)
                                 (stencilPrj (undefined::sh) (undefined::b) stencil2)
    openStencilFun  = convertSharingExp lyt alyt env aenv body


-- Sharing recovery
-- ================

-- Sharing recovery proceeds in two phases:
--
-- /Phase One: build the occurrence map/
--
-- This is a top-down traversal of the AST that computes a map from AST nodes to the number of
-- occurrences of that AST node in the overall Accelerate program.  An occurrences count of two or
-- more indicates sharing.
--
-- IMPORTANT: To avoid unfolding the sharing, we do not descent into subtrees that we have
--   previously encountered.  Hence, the complexity is proportional to the number of nodes in the
--   tree /with/ sharing.  Consequently, the occurrence count is that in the tree with sharing
--   as well.
--
-- During computation of the occurrences, the tree is annotated with stable names on every node
-- using 'AccSharing' constructors and all but the first occurrence of shared subtrees are pruned
-- using 'AvarSharing' constructors (see 'SharingAcc' below).  This phase is impure as it is based
-- on stable names.
--
-- We use a hash table (instead of 'Data.Map') as computing stable names forces us to live in IO
-- anyway.  Once, the computation of occurrence counts is complete, we freeze the hash table into
-- a 'Data.Map'.
--
-- (Implemented by 'makeOccMap'.)
--
-- /Phase Two: determine scopes and inject sharing information/
--
-- This is a bottom-up traversal that determines the scope for every binding to be introduced
-- to share a subterm.  It uses the occurrence map to determine, for every shared subtree, the
-- lowest AST node at which the binding for that shared subtree can be placed (using a
-- 'AletSharing' constructor)— it's the meet of all the shared subtree occurrences.
--
-- The second phase is also replacing the first occurrence of each shared subtree with a
-- 'AvarSharing' node and floats the shared subtree up to its binding point.
--
--  (Implemented by 'determineScopes'.)
--
-- /Sharing recovery for expressions/
--
-- We recover sharing for each expression (including function bodies) independently of any other
-- expression — i.e., we cannot share scalar expressions across array computations.  Hence, during
-- Phase One, we mark all scalar expression nodes with a stable name and compute one occurrence map
-- for every scalar expression (including functions) that occurs in an array computation.  These
-- occurrence maps are added to the root of scalar expressions using 'RootExp'.
--
-- NB: We do not need to worry sharing recovery will try to float a shared subexpression past a
--     binder that occurs in that subexpression.  Why?  Otherwise, the binder would already occur
--     out of scope in the original source program.
--
-- /Lambda bound variables/
--
-- During sharing recovery, lambda bound variables appear in the form of 'Atag' and 'Tag' data
-- constructors.  The tag values are determined during Phase One of sharing recovery by computing
-- the /level/ of each variable at its binding occurrence.  The level at the root of the AST is 0
-- and increases by one with each lambda on each path through the AST.

-- Stable names

-- Opaque stable name for AST nodes — used to key the occurrence map.
--
data StableASTName c where
  StableASTName :: (Typeable1 c, Typeable t) => StableName (c t) -> StableASTName c

instance Show (StableASTName c) where
  show (StableASTName sn) = show $ hashStableName sn

instance Eq (StableASTName c) where
  StableASTName sn1 == StableASTName sn2
    | Just sn1' <- gcast sn1 = sn1' == sn2
    | otherwise              = False

instance Hashable (StableASTName c) where
  hash (StableASTName sn) = hashStableName sn

makeStableAST :: c t -> IO (StableName (c t))
makeStableAST e = e `seq` makeStableName e

-- Stable name for an AST node including the height of the AST representing the array computation.
--
data StableNameHeight t = StableNameHeight (StableName t) Int

instance Eq (StableNameHeight t) where
  (StableNameHeight sn1 _) == (StableNameHeight sn2 _) = sn1 == sn2

higherSNH :: StableNameHeight t1 -> StableNameHeight t2 -> Bool
StableNameHeight _ h1 `higherSNH` StableNameHeight _ h2 = h1 > h2

hashStableNameHeight :: StableNameHeight t -> Int
hashStableNameHeight (StableNameHeight sn _) = hashStableName sn

-- Mutable occurrence map

-- Hash table keyed on the stable names of array computations.
--
type ASTHashTable c v = HashTable (StableASTName c) v

-- Mutable hashtable version of the occurrence map, which associates each AST node with an
-- occurrence count and the height of the AST.
--
type OccMapHash c = ASTHashTable c (Int, Int)

-- Create a new hash table keyed on AST nodes.
--
newASTHashTable :: IO (ASTHashTable c v)
newASTHashTable = Hash.new

-- Enter one AST node occurrence into an occurrence map.  Returns 'Just h' if this is a repeated
-- occurrence and the height of the repeatedly occurring AST is 'h'.
--
-- If this is the first occurrence, the 'height' *argument* must provide the height of the AST;
-- otherwise, the height will be *extracted* from the occurrence map.  In the latter case, this
-- function yields the AST height.
--
enterOcc :: OccMapHash c -> StableASTName c -> Int -> IO (Maybe Int)
enterOcc occMap sa height
  = do
      entry <- Hash.lookup occMap sa
      case entry of
        Nothing           -> Hash.insert occMap sa (1    , height)  >> return Nothing
        Just (n, heightS) -> Hash.insert occMap sa (n + 1, heightS) >> return (Just heightS)

-- Immutable occurrence map

-- Immutable version of the occurrence map (storing the occurrence count only, not the height).  We
-- use the 'StableName' hash to index an 'IntMap' and disambiguate 'StableName's with identical
-- hashes explicitly, storing them in a list in the 'IntMap'.
--
type OccMap c = IntMap.IntMap [(StableASTName c, Int)]

-- Turn a mutable into an immutable occurrence map.
--
freezeOccMap :: OccMapHash c -> IO (OccMap c)
freezeOccMap oc
  = do
      kvs <- map dropHeight <$> Hash.toList oc
      return . IntMap.fromList . map (\kvs -> (key (head kvs), kvs)). groupBy sameKey $ kvs
  where
    key (StableASTName sn, _) = hashStableName sn
    sameKey kv1 kv2           = key kv1 == key kv2
    dropHeight (k, (cnt, _))  = (k, cnt)

-- Look up the occurrence map keyed by array computations using a stable name.  If the key does
-- not exist in the map, return an occurrence count of '1'.
--
lookupWithASTName :: OccMap c -> StableASTName c -> Int
lookupWithASTName oc sa@(StableASTName sn)
  = fromMaybe 1 $ IntMap.lookup (hashStableName sn) oc >>= Prelude.lookup sa

-- Look up the occurrence map keyed by array computations using a sharing array computation.  If an
-- the key does not exist in the map, return an occurrence count of '1'.
--
lookupWithSharingAcc :: OccMap Acc -> StableSharingAcc -> Int
lookupWithSharingAcc oc (StableSharingAcc (StableNameHeight sn _) _)
  = lookupWithASTName oc (StableASTName sn)

-- Look up the occurrence map keyed by scalar expressions using a sharing expression.  If an
-- the key does not exist in the map, return an occurrence count of '1'.
--
lookupWithSharingExp :: OccMap Exp -> StableSharingExp -> Int
lookupWithSharingExp oc (StableSharingExp (StableNameHeight sn _) _)
  = lookupWithASTName oc (StableASTName sn)

-- Stable 'Acc' nodes

-- Stable name for 'Acc' nodes including the height of the AST.
--
type StableAccName arrs = StableNameHeight (Acc arrs)

-- Interleave sharing annotations into an array computation AST.  Subtrees can be marked as being
-- represented by variable (binding a shared subtree) using 'AvarSharing' and as being prefixed by
-- a let binding (for a shared subtree) using 'AletSharing'.
--
data SharingAcc arrs where
  AvarSharing :: Arrays arrs
              => StableAccName arrs                                   -> SharingAcc arrs
  AletSharing :: StableSharingAcc -> SharingAcc arrs                  -> SharingAcc arrs
  AccSharing  :: Arrays arrs
              => StableAccName arrs -> PreAcc SharingAcc RootExp arrs -> SharingAcc arrs

-- Stable name for an array computation associated with its sharing-annotated version.
--
data StableSharingAcc where
  StableSharingAcc :: Arrays arrs
                   => StableAccName arrs
                   -> SharingAcc arrs
                   -> StableSharingAcc

instance Show StableSharingAcc where
  show (StableSharingAcc sn _) = show $ hashStableNameHeight sn

instance Eq StableSharingAcc where
  StableSharingAcc sn1 _ == StableSharingAcc sn2 _
    | Just sn1' <- gcast sn1 = sn1' == sn2
    | otherwise              = False

higherSSA :: StableSharingAcc -> StableSharingAcc -> Bool
StableSharingAcc sn1 _ `higherSSA` StableSharingAcc sn2 _ = sn1 `higherSNH` sn2

-- Test whether the given stable names matches an array computation with sharing.
--
matchStableAcc :: Typeable arrs => StableAccName arrs -> StableSharingAcc -> Bool
matchStableAcc sn1 (StableSharingAcc sn2 _)
  | Just sn1' <- gcast sn1 = sn1' == sn2
  | otherwise              = False

-- Dummy entry for environments to be used for unused variables.
--
noStableAccName :: StableAccName arrs
noStableAccName = unsafePerformIO $ StableNameHeight <$> makeStableName undefined <*> pure 0

-- Stable 'Exp' nodes

-- Stable name for 'Exp' nodes including the height of the AST.
--
type StableExpName t = StableNameHeight (Exp t)

-- Interleave sharing annotations into a scalar expressions AST in the same manner as 'SharingAcc'
-- do for array computations.
--
data SharingExp t where
  VarSharing :: Elt t
             => StableExpName t                                   -> SharingExp t
  LetSharing :: StableSharingExp -> SharingExp t                  -> SharingExp t
  ExpSharing :: Elt t
             => StableExpName t -> PreExp SharingAcc SharingExp t -> SharingExp t

-- Expressions rooted in 'Acc' computations.
--
-- * Between counting occurrences and determining scopes, the root of every expression embedded in an
--   'Acc' is annotated by (1) the tags of free scalar variables and (2) an occurrence map for that
--   one expression (excluding any subterms that are rooted in embedded 'Acc's.)
-- * After determining scopes, the root of every expression is annotated with a sorted environment of
--   the 'StableSharingExp's corresponding to its free expression-valued variables.
--
data RootExp t where
  OccMapExp :: [Int] -> OccMap Exp  -> SharingExp t -> RootExp t
  EnvExp    :: [StableSharingExp] -> SharingExp t -> RootExp t

-- Stable name for an expression associated with its sharing-annotated version.
--
data StableSharingExp where
  StableSharingExp :: Elt t => StableExpName t -> SharingExp t -> StableSharingExp

instance Show StableSharingExp where
  show (StableSharingExp sn _) = show $ hashStableNameHeight sn

instance Eq StableSharingExp where
  StableSharingExp sn1 _ == StableSharingExp sn2 _
    | Just sn1' <- gcast sn1 = sn1' == sn2
    | otherwise              = False

higherSSE :: StableSharingExp -> StableSharingExp -> Bool
StableSharingExp sn1 _ `higherSSE` StableSharingExp sn2 _ = sn1 `higherSNH` sn2

-- Test whether the given stable names matches an expression with sharing.
--
matchStableExp :: Typeable t => StableExpName t -> StableSharingExp -> Bool
matchStableExp sn1 (StableSharingExp sn2 _)
  | Just sn1' <- gcast sn1 = sn1' == sn2
  | otherwise              = False

-- Dummy entry for environments to be used for unused variables.
--
noStableExpName :: StableExpName t
noStableExpName = unsafePerformIO $ StableNameHeight <$> makeStableName undefined <*> pure 0


-- Compute the 'Acc' occurrence map, marks all nodes (both 'Acc' and 'Exp' nodes) with stable names,
-- and drop repeated occurrences of shared 'Acc' and 'Exp' subtrees (Phase One).
--
-- We compute a single 'Acc' occurrence map for the whole AST, but one 'Exp' occurrence map for each
-- sub-expression rooted in an 'Acc' operation.  This is as we cannot float 'Exp' subtrees across
-- 'Acc' operations, but we can float 'Acc' subtrees out of 'Exp' expressions.
--
-- Note [Traversing functions and side effects]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- We need to descent into function bodies to build the 'OccMap' with all occurrences in the
-- function bodies.  Due to the side effects in the construction of the occurrence map and, more
-- importantly, the dependence of the second phase on /global/ occurrence information, we may not
-- delay the body traversals by putting them under a lambda.  Hence, we apply each function, to
-- traverse its body and use a /dummy abstraction/ of the result.
--
-- For example, given a function 'f', we traverse 'f (Tag 0)', which yields a transformed body 'e'.
-- As the result of the traversal of the overall function, we use 'const e'.  Hence, it is crucial
-- that the 'Tag' supplied during the initial traversal is already the one required by the HOAS to
-- de Bruijn conversion in 'convertSharingAcc' — any subsequent application of 'const e' will only
-- yield 'e' with the embedded 'Tag 0' of the original application.  During sharing recovery, we
-- float /all/ free variables ('Atag' and 'Tag') out to construct the initial environment for
-- producing de Bruijn indices, which replaces them by 'AvarSharing' or 'VarSharing' nodes.  Hence,
-- the tag values only serve the purpose of determining the ordering in that initial environment.
-- They are /not/ directly used to compute the de Brujin indices.
--
makeOccMap :: Typeable arrs => Level -> Acc arrs -> IO (SharingAcc arrs, OccMapHash Acc)
makeOccMap lvl rootAcc
  = do
      traceLine "makeOccMap" "Enter"
      occMap <- newASTHashTable
      (rootAcc', _) <- traverseAcc lvl occMap rootAcc
      traceLine "makeOccMap" "Exit"
      return (rootAcc', occMap)
  where
    traverseAcc :: forall arrs. Typeable arrs
                => Level -> OccMapHash Acc -> Acc arrs -> IO (SharingAcc arrs, Int)
    traverseAcc lvl occMap acc@(Acc pacc)
      = mfix $ \ ~(_, height) -> do
        {   -- Compute stable name and enter it into the occurrence map
        ; sn                         <- makeStableAST acc
        ; heightIfRepeatedOccurrence <- enterOcc occMap (StableASTName sn) height

        ; traceLine (showPreAccOp pacc) $
            case heightIfRepeatedOccurrence of
              Just height -> "REPEATED occurrence (sn = " ++ show (hashStableName sn) ++
                             "; height = " ++ show height ++ ")"
              Nothing     -> "first occurrence (sn = " ++ show (hashStableName sn) ++ ")"

            -- Reconstruct the computation in shared form.
            --
            -- In case of a repeated occurrence, the height comes from the occurrence map; otherwise
            -- it is computed by the traversal function passed in 'newAcc'.  See also 'enterOcc'.
            --
            -- NB: This function can only be used in the case alternatives below; outside of the
            --     case we cannot discharge the 'Arrays arrs' constraint.
        ; let reconstruct :: Arrays arrs
                          => IO (PreAcc SharingAcc RootExp arrs, Int)
                          -> IO (SharingAcc arrs, Int)
              reconstruct newAcc
                = case heightIfRepeatedOccurrence of
                    Just height | recoverAccSharing
                      -> return (AvarSharing (StableNameHeight sn height), height)
                    _ -> do
                         { (acc, height) <- newAcc
                         ; return (AccSharing (StableNameHeight sn height) acc, height)
                         }

        ; case pacc of
            Atag i                   -> reconstruct $ return (Atag i, 0)           -- height is 0!
            Pipe afun1 afun2 acc     -> reconstruct $ travA (Pipe afun1 afun2) acc
            Acond e acc1 acc2        -> reconstruct $ do
                                          (e'   , h1) <- enterExp lvl occMap e
                                          (acc1', h2) <- traverseAcc lvl occMap acc1
                                          (acc2', h3) <- traverseAcc lvl occMap acc2
                                          return (Acond e' acc1' acc2', h1 `max` h2 `max` h3 + 1)

            Atuple tup               -> reconstruct $ do
                                          (tup', h) <- travAtup tup
                                          return (Atuple tup', h)
            Aprj ix a                -> reconstruct $ travA (Aprj ix) a

            Use arr                  -> reconstruct $ return (Use arr, 1)
            Unit e                   -> reconstruct $ do
                                          (e', h) <- enterExp lvl occMap e
                                          return (Unit e', h + 1)
            Generate e f             -> reconstruct $ do
                                          (e', h1) <- enterExp lvl  occMap e
                                          (f', h2) <- traverseFun1 lvl occMap f
                                          return (Generate e' f', h1 `max` h2 + 1)
            Reshape e acc            -> reconstruct $ travEA Reshape e acc
            Replicate e acc          -> reconstruct $ travEA Replicate e acc
            Slice acc e              -> reconstruct $ travEA (flip Slice) e acc
            Map f acc                -> reconstruct $ do
                                          (f'  , h1) <- traverseFun1 lvl occMap f
                                          (acc', h2) <- traverseAcc lvl  occMap acc
                                          return (Map f' acc', h1 `max` h2 + 1)
            ZipWith f acc1 acc2      -> reconstruct $ travF2A2 ZipWith f acc1 acc2
            Fold f e acc             -> reconstruct $ travF2EA Fold f e acc
            Fold1 f acc              -> reconstruct $ travF2A Fold1 f acc
            FoldSeg f e acc1 acc2    -> reconstruct $ do
                                          (f'   , h1) <- traverseFun2 lvl occMap f
                                          (e'   , h2) <- enterExp lvl  occMap e
                                          (acc1', h3) <- traverseAcc lvl occMap acc1
                                          (acc2', h4) <- traverseAcc lvl occMap acc2
                                          return (FoldSeg f' e' acc1' acc2',
                                                  h1 `max` h2 `max` h3 `max` h4 + 1)
            Fold1Seg f acc1 acc2     -> reconstruct $ travF2A2 Fold1Seg f acc1 acc2
            Scanl f e acc            -> reconstruct $ travF2EA Scanl f e acc
            Scanl' f e acc           -> reconstruct $ travF2EA Scanl' f e acc
            Scanl1 f acc             -> reconstruct $ travF2A Scanl1 f acc
            Scanr f e acc            -> reconstruct $ travF2EA Scanr f e acc
            Scanr' f e acc           -> reconstruct $ travF2EA Scanr' f e acc
            Scanr1 f acc             -> reconstruct $ travF2A Scanr1 f acc
            Permute c acc1 p acc2    -> reconstruct $ do
                                          (c'   , h1) <- traverseFun2 lvl occMap c
                                          (p'   , h2) <- traverseFun1 lvl occMap p
                                          (acc1', h3) <- traverseAcc lvl  occMap acc1
                                          (acc2', h4) <- traverseAcc lvl  occMap acc2
                                          return (Permute c' acc1' p' acc2',
                                                  h1 `max` h2 `max` h3 `max` h4 + 1)
            Backpermute e p acc      -> reconstruct $ do
                                          (e'  , h1) <- enterExp lvl  occMap e
                                          (p'  , h2) <- traverseFun1 lvl occMap p
                                          (acc', h3) <- traverseAcc lvl occMap acc
                                          return (Backpermute e' p' acc', h1 `max` h2 `max` h3 + 1)
            Stencil s bnd acc        -> reconstruct $ do
                                          (s'  , h1) <- traverseStencil1 acc lvl occMap s
                                          (acc', h2) <- traverseAcc lvl occMap acc
                                          return (Stencil s' bnd acc', h1 `max` h2 + 1)
            Stencil2 s bnd1 acc1
                       bnd2 acc2     -> reconstruct $ do
                                          (s'   , h1) <- traverseStencil2 acc1 acc2 lvl occMap s
                                          (acc1', h2) <- traverseAcc lvl occMap acc1
                                          (acc2', h3) <- traverseAcc lvl occMap acc2
                                          return (Stencil2 s' bnd1 acc1' bnd2 acc2',
                                                  h1 `max` h2 `max` h3 + 1)
        }
      where
        travA :: Arrays arrs'
              => (SharingAcc arrs' -> PreAcc SharingAcc RootExp arrs)
              -> Acc arrs' -> IO (PreAcc SharingAcc RootExp arrs, Int)
        travA c acc
          = do
              (acc', h) <- traverseAcc lvl occMap acc
              return (c acc', h + 1)

        travEA :: (Typeable b, Arrays arrs')
               => (RootExp b -> SharingAcc arrs' -> PreAcc SharingAcc RootExp arrs)
               -> Exp b -> Acc arrs' -> IO (PreAcc SharingAcc RootExp arrs, Int)
        travEA c exp acc
          = do
              (exp', h1) <- enterExp lvl occMap exp
              (acc', h2) <- traverseAcc lvl occMap acc
              return (c exp' acc', h1 `max` h2 + 1)

        travF2A :: (Elt b, Elt c, Typeable d, Arrays arrs')
                => ((Exp b -> Exp c -> RootExp d) -> SharingAcc arrs'
                    -> PreAcc SharingAcc RootExp arrs)
                -> (Exp b -> Exp c -> Exp d) -> Acc arrs'
                -> IO (PreAcc SharingAcc RootExp arrs, Int)
        travF2A c fun acc
          = do
              (fun', h1) <- traverseFun2 lvl occMap fun
              (acc', h2) <- traverseAcc lvl occMap acc
              return (c fun' acc', h1 `max` h2 + 1)

        travF2EA :: (Elt b, Elt c, Typeable d, Typeable e, Arrays arrs')
                 => ((Exp b -> Exp c -> RootExp d) -> RootExp e
                       -> SharingAcc arrs' -> PreAcc SharingAcc RootExp arrs)
                 -> (Exp b -> Exp c -> Exp d) -> Exp e -> Acc arrs'
                 -> IO (PreAcc SharingAcc RootExp arrs, Int)
        travF2EA c fun exp acc
          = do
              (fun', h1) <- traverseFun2 lvl occMap fun
              (exp', h2) <- enterExp lvl occMap exp
              (acc', h3) <- traverseAcc lvl occMap acc
              return (c fun' exp' acc', h1 `max` h2 `max` h3 + 1)

        travF2A2 :: (Elt b, Elt c, Typeable d, Arrays arrs1, Arrays arrs2)
                 => ((Exp b -> Exp c -> RootExp d) -> SharingAcc arrs1
                       -> SharingAcc arrs2 -> PreAcc SharingAcc RootExp arrs)
                 -> (Exp b -> Exp c -> Exp d) -> Acc arrs1 -> Acc arrs2
                 -> IO (PreAcc SharingAcc RootExp arrs, Int)
        travF2A2 c fun acc1 acc2
          = do
              (fun' , h1) <- traverseFun2 lvl occMap fun
              (acc1', h2) <- traverseAcc lvl occMap acc1
              (acc2', h3) <- traverseAcc lvl occMap acc2
              return (c fun' acc1' acc2', h1 `max` h2 `max` h3 + 1)

        travAtup :: Tuple.Atuple Acc a
                 -> IO (Tuple.Atuple SharingAcc a, Int)
        travAtup NilAtup          = return (NilAtup, 1)
        travAtup (SnocAtup tup a) = do
          (tup', h1) <- travAtup tup
          (a',   h2) <- traverseAcc lvl occMap a
          return (SnocAtup tup' a', h1 `max` h2 + 1)

    traverseFun1 :: (Elt b, Typeable c)
                  => Level -> OccMapHash Acc -> (Exp b -> Exp c) -> IO (Exp b -> RootExp c, Int)
    traverseFun1 lvl occMap f
      = do
            -- see Note [Traversing functions and side effects]
          (body, h) <- enterFun (lvl + 1) [lvl] occMap $ f (Exp $ Tag lvl)
          return (const body, h + 1)

    traverseFun2 :: (Elt b, Elt c, Typeable d)
                  => Level -> OccMapHash Acc -> (Exp b -> Exp c -> Exp d)
                  -> IO (Exp b -> Exp c -> RootExp d, Int)
    traverseFun2 lvl occMap f
      = do
            -- see Note [Traversing functions and side effects]
          (body, h) <- enterFun (lvl + 2) [lvl, lvl + 1] occMap $ f (Exp $ Tag (lvl + 1)) (Exp $ Tag lvl)
          return (\_ _ -> body, h + 2)

    traverseStencil1 :: forall sh b c stencil. (Stencil sh b stencil, Typeable c)
                     => Acc (Array sh b){-dummy-}
                     -> Level -> OccMapHash Acc -> (stencil -> Exp c)
                     -> IO (stencil -> RootExp c, Int)
    traverseStencil1 _ lvl occMap stencilFun
      = do
            -- see Note [Traversing functions and side effects]
          (body, h) <- enterFun (lvl + 1) [lvl] occMap $
                         stencilFun (stencilPrj (undefined::sh) (undefined::b) (Exp $ Tag lvl))
          return (const body, h + 1)

    traverseStencil2 :: forall sh b c d stencil1 stencil2.
                        (Stencil sh b stencil1, Stencil sh c stencil2, Typeable d)
                     => Acc (Array sh b){-dummy-}
                     -> Acc (Array sh c){-dummy-}
                     -> Level
                     -> OccMapHash Acc
                     -> (stencil1 -> stencil2 -> Exp d)
                     -> IO (stencil1 -> stencil2 -> RootExp d, Int)
    traverseStencil2 _ _ lvl occMap stencilFun
      = do
            -- see Note [Traversing functions and side effects]
          (body, h) <- enterFun (lvl + 2) [lvl, lvl + 1] occMap $
                         stencilFun (stencilPrj (undefined::sh) (undefined::b) (Exp $ Tag (lvl + 1)))
                                    (stencilPrj (undefined::sh) (undefined::c) (Exp $ Tag lvl))
          return (\_ _ -> body, h + 2)

    -- Enter an 'Exp' subtree from an 'Acc' tree => need a local 'Exp' occurrence map
    --
    -- First argument is the level (of bound variables) and the second are the tags of newly
    -- introduced free scalar variables in this expression.
    --
    enterFun :: forall a. Typeable a => Level -> [Int] -> OccMapHash Acc -> Exp a -> IO (RootExp a, Int)
    enterFun lvl fvs accOccMap exp
      = do
        { expOccMap       <- newASTHashTable
        ; (exp', h)       <- traverseExp lvl accOccMap expOccMap exp
        ; frozenExpOccMap <- freezeOccMap expOccMap
        ; return (OccMapExp fvs frozenExpOccMap exp', h)
        }

    -- 'enterFun' without any new free variables.
    --
    enterExp :: forall a. Typeable a => Level -> OccMapHash Acc -> Exp a -> IO (RootExp a, Int)
    enterExp lvl = enterFun lvl []

    traverseExp :: forall a. Typeable a => Level -> OccMapHash Acc -> OccMapHash Exp -> Exp a -> IO (SharingExp a, Int)
    traverseExp lvl accOccMap expOccMap exp@(Exp pexp)
      = mfix $ \ ~(_, height) -> do
        {   -- Compute stable name and enter it into the occurrence map
        ; sn                         <- makeStableAST exp
        ; heightIfRepeatedOccurrence <- enterOcc expOccMap (StableASTName sn) height

        ; traceLine (showPreExpOp pexp) $
            case heightIfRepeatedOccurrence of
              Just height -> "REPEATED occurrence (sn = " ++ show (hashStableName sn) ++
                             "; height = " ++ show height ++ ")"
              Nothing     -> "first occurrence (sn = " ++ show (hashStableName sn) ++ ")"


            -- Reconstruct the computation in shared form.
            --
            -- In case of a repeated occurrence, the height comes from the occurrence map; otherwise
            -- it is computed by the traversal function passed in 'newExp'.  See also 'enterOcc'.
            --
            -- NB: This function can only be used in the case alternatives below; outside of the
            --     case we cannot discharge the 'Elt a' constraint.
        ; let reconstruct :: Elt a
                          => IO (PreExp SharingAcc SharingExp a, Int)
                          -> IO (SharingExp a, Int)
              reconstruct newExp
                = case heightIfRepeatedOccurrence of
                    Just height | recoverExpSharing
                      -> return (VarSharing (StableNameHeight sn height), height)
                    _ -> do
                         { (exp, height) <- newExp
                         ; return (ExpSharing (StableNameHeight sn height) exp, height)
                         }

        ; case pexp of
            Tag i           -> reconstruct $ return (Tag i, 0)              -- height is 0!
            Const c         -> reconstruct $ return (Const c, 1)
            Tuple tup       -> reconstruct $ do
                                 (tup', h) <- travTup tup
                                 return (Tuple tup', h)
            Prj i e         -> reconstruct $ travE1 (Prj i) e
            IndexNil        -> reconstruct $ return (IndexNil, 1)
            IndexCons ix i  -> reconstruct $ travE2 IndexCons ix i
            IndexHead i     -> reconstruct $ travE1 IndexHead i
            IndexTail ix    -> reconstruct $ travE1 IndexTail ix
            IndexAny        -> reconstruct $ return (IndexAny, 1)
            Cond e1 e2 e3   -> reconstruct $ travE3 Cond e1 e2 e3
            PrimConst c     -> reconstruct $ return (PrimConst c, 1)
            PrimApp p e     -> reconstruct $ travE1 (PrimApp p) e
            IndexScalar a e -> reconstruct $ travAE IndexScalar a e
            Shape a         -> reconstruct $ travA Shape a
            ShapeSize e     -> reconstruct $ travE1 ShapeSize e
          }
      where
        travE1 :: Typeable b => (SharingExp b -> PreExp SharingAcc SharingExp a) -> Exp b
               -> IO (PreExp SharingAcc SharingExp a, Int)
        travE1 c e
          = do
              (e', h) <- traverseExp lvl accOccMap expOccMap e
              return (c e', h + 1)

        travE2 :: (Typeable b, Typeable c)
               => (SharingExp b -> SharingExp c -> PreExp SharingAcc SharingExp a)
               -> Exp b -> Exp c
               -> IO (PreExp SharingAcc SharingExp a, Int)
        travE2 c e1 e2
          = do
              (e1', h1) <- traverseExp lvl accOccMap expOccMap e1
              (e2', h2) <- traverseExp lvl accOccMap expOccMap e2
              return (c e1' e2', h1 `max` h2 + 1)

        travE3 :: (Typeable b, Typeable c, Typeable d)
               => (SharingExp b -> SharingExp c -> SharingExp d -> PreExp SharingAcc SharingExp a)
               -> Exp b -> Exp c -> Exp d
               -> IO (PreExp SharingAcc SharingExp a, Int)
        travE3 c e1 e2 e3
          = do
              (e1', h1) <- traverseExp lvl accOccMap expOccMap e1
              (e2', h2) <- traverseExp lvl accOccMap expOccMap e2
              (e3', h3) <- traverseExp lvl accOccMap expOccMap e3
              return (c e1' e2' e3', h1 `max` h2 `max` h3 + 1)

        travA :: Typeable b => (SharingAcc b -> PreExp SharingAcc SharingExp a) -> Acc b
              -> IO (PreExp SharingAcc SharingExp a, Int)
        travA c acc
          = do
              (acc', h) <- traverseAcc lvl accOccMap acc
              return (c acc', h + 1)

        travAE :: (Typeable b, Typeable c)
               => (SharingAcc b -> SharingExp c -> PreExp SharingAcc SharingExp a)
               -> Acc b -> Exp c
               -> IO (PreExp SharingAcc SharingExp a, Int)
        travAE c acc e
          = do
              (acc', h1) <- traverseAcc lvl accOccMap acc
              (e'  , h2) <- traverseExp lvl accOccMap expOccMap e
              return (c acc' e', h1 `max` h2 + 1)

        travTup :: Tuple.Tuple Exp tup -> IO (Tuple.Tuple SharingExp tup, Int)
        travTup NilTup          = return (NilTup, 1)
        travTup (SnocTup tup e) = do
                                    (tup', h1) <- travTup tup
                                    (e'  , h2) <- traverseExp lvl accOccMap expOccMap e
                                    return (SnocTup tup' e', h1 `max` h2 + 1)

-- Type used to maintain how often each shared subterm, so far, occurred during a bottom-up sweep.
--
--   Invariants:
--   - If one shared term 's' is itself a subterm of another shared term 't', then 's' must occur
--     *after* 't' in the 'NodeCounts'.
--   - No shared term occurs twice.
--   - A term may have a final occurrence count of only 1 iff it is either a free variable ('Atag'
--     or 'Tag') or an array computation listed out of an expression.
--   - All 'Exp' node counts precede all 'Acc' node counts as we don't share 'Exp' nodes across 'Acc'
--     nodes.
--
-- We determine the subterm property by using the tree height in 'StableNameHeight'.  Trees get
-- smaller towards the end of a 'NodeCounts' list.  The height of free variables ('Atag' or 'Tag')
-- is 0, whereas other leaves have height 1.  This guarantees that all free variables are at the end
-- of the 'NodeCounts' list.
--
-- To ensure the invariant is preserved over merging node counts from sibling subterms, the
-- function '(+++)' must be used.
--
type NodeCounts = [NodeCount]

data NodeCount = AccNodeCount StableSharingAcc Int
               | ExpNodeCount StableSharingExp Int
               deriving Show

-- Empty node counts
--
noNodeCounts :: NodeCounts
noNodeCounts = []

-- Singleton node counts for 'Acc'
--
accNodeCount :: StableSharingAcc -> Int -> NodeCounts
accNodeCount ssa n = [AccNodeCount ssa n]

-- Singleton node counts for 'Exp'
--
expNodeCount :: StableSharingExp -> Int -> NodeCounts
expNodeCount sse n = [ExpNodeCount sse n]

-- Combine node counts that belong to the same node.
--
-- * We assume that the node counts invariant —subterms follow their parents— holds for both
--   arguments and guarantee that it still holds for the result.
-- * In the same manner, we assume that all 'Exp' node counts precede 'Acc' node counts and
--   guarantee that this also hold for the result.
--
(+++) :: NodeCounts -> NodeCounts -> NodeCounts
us +++ vs = foldr insert us vs
  where
    insert x               []                         = [x]
    insert x@(AccNodeCount sa1 count1) ys@(y@(AccNodeCount sa2 count2) : ys')
      | sa1 == sa2          = AccNodeCount (sa1 `pickNoneAvar` sa2) (count1 + count2) : ys'
      | sa1 `higherSSA` sa2 = x : ys
      | otherwise           = y : insert x ys'
    insert x@(ExpNodeCount se1 count1) ys@(y@(ExpNodeCount se2 count2) : ys')
      | se1 == se2          = ExpNodeCount (se1 `pickNoneVar` se2) (count1 + count2) : ys'
      | se1 `higherSSE` se2 = x : ys
      | otherwise           = y : insert x ys'
    insert x@(AccNodeCount _ _) (y@(ExpNodeCount _ _) : ys')
      = y : insert x ys'
    insert x@(ExpNodeCount _ _) (y@(AccNodeCount _ _) : ys')
      = x : insert y ys'

    (StableSharingAcc _ (AvarSharing _)) `pickNoneAvar` sa2  = sa2
    sa1                                  `pickNoneAvar` _sa2 = sa1

    (StableSharingExp _ (VarSharing _))  `pickNoneVar`  sa2  = sa2
    sa1                                  `pickNoneVar`  _sa2 = sa1

-- Build an initial environment for the tag values given in the first argument for traversing an
-- array expression.  The 'StableSharingAcc's for all tags /actually used/ in the expressions are
-- in the second argument. (Tags are not used if a bound variable has no usage occurrence.)
--
-- Bail out if any tag occurs multiple times as this indicates that the sharing of an argument
-- variable was not preserved and we cannot build an appropriate initial environment (c.f., comments
-- at 'determineScopes'.
--
buildInitialEnvAcc :: [Level] -> [StableSharingAcc] -> [StableSharingAcc]
buildInitialEnvAcc tags sas = map (lookupSA sas) tags
  where
    lookupSA sas tag1
      = case filter hasTag sas of
          []   -> noStableSharing    -- tag is not used in the analysed expression
          [sa] -> sa                 -- tag has a unique occurrence
          sas2 -> INTERNAL_ERROR(error) "buildInitialEnvAcc"
                    ("Encountered duplicate 'ATag's\n  " ++ concat (intersperse ", " (map showSA sas2)))
      where
        hasTag (StableSharingAcc _ (AccSharing _ (Atag tag2))) = tag1 == tag2
        hasTag sa
          = INTERNAL_ERROR(error) "buildInitialEnvAcc"
            ("Encountered a node that is not a plain 'Atag'\n  " ++ showSA sa)

        noStableSharing :: StableSharingAcc
        noStableSharing = StableSharingAcc noStableAccName (undefined :: SharingAcc ())

    showSA (StableSharingAcc _ (AccSharing  sn acc)) = show (hashStableNameHeight sn) ++ ": " ++
                                                       showPreAccOp acc
    showSA (StableSharingAcc _ (AvarSharing sn))     = "AvarSharing " ++ show (hashStableNameHeight sn)
    showSA (StableSharingAcc _ (AletSharing sa _ ))  = "AletSharing " ++ show sa ++ "..."

-- Build an initial environment for the tag values given in the first argument for traversing a
-- scalar expression.  The 'StableSharingExp's for all tags /actually used/ in the expressions are
-- in the second argument. (Tags are not used if a bound variable has no usage occurrence.)
--
-- Bail out if any tag occurs multiple times as this indicates that the sharing of an argument
-- variable was not preserved and we cannot build an appropriate initial environment (c.f., comments
-- at 'determineScopes'.
--
buildInitialEnvExp :: [Level] -> [StableSharingExp] -> [StableSharingExp]
buildInitialEnvExp tags ses = map (lookupSE ses) tags
  where
    lookupSE ses tag1
      = case filter hasTag ses of
          []   -> noStableSharing    -- tag is not used in the analysed expression
          [se] -> se                 -- tag has a unique occurrence
          ses2 -> INTERNAL_ERROR(error) "buildInitialEnvExp"
                    ("Encountered a duplicate 'Tag'\n  " ++ concat (intersperse ", " (map showSE ses2)))
      where
        hasTag (StableSharingExp _ (ExpSharing _ (Tag tag2))) = tag1 == tag2
        hasTag se
          = INTERNAL_ERROR(error) "buildInitialEnvExp"
              ("Encountered a node that is not a plain 'Tag'\n  " ++ showSE se)

        noStableSharing :: StableSharingExp
        noStableSharing = StableSharingExp noStableExpName (undefined :: SharingExp ())

    showSE (StableSharingExp _ (ExpSharing sn exp)) = show (hashStableNameHeight sn) ++ ": " ++
                                                      showPreExpOp exp
    showSE (StableSharingExp _ (VarSharing sn))     = "VarSharing " ++ show (hashStableNameHeight sn)
    showSE (StableSharingExp _ (LetSharing se _ ))  = "LetSharing " ++ show se ++ "..."

-- Determine whether a 'NodeCount' is for an 'Atag' or 'Tag', which represent free variables.
--
isFreeVar :: NodeCount -> Bool
isFreeVar (AccNodeCount (StableSharingAcc _ (AccSharing _ (Atag _))) _) = True
isFreeVar (ExpNodeCount (StableSharingExp _ (ExpSharing _ (Tag  _))) _) = True
isFreeVar _                                                             = False

-- Determine the scopes of all variables representing shared subterms (Phase Two) in a bottom-up
-- sweep.  The first argument determines whether array computations are floated out of expressions
-- irrespective of whether they are shared or not — 'True' implies floating them out.
--
-- In addition to the AST with sharing information, yield the 'StableSharingAcc's for all free
-- variables of 'rootAcc', which are represented by 'Atag' leaves in the tree. They are in order of
-- the tag values — i.e., in the same order that they need to appear in an environment to use the
-- tag for indexing into that environment.
--
-- Precondition: there are only 'AvarSharing' and 'AccSharing' nodes in the argument.
--
determineScopes :: Typeable a
                => Bool -> [Level] -> OccMap Acc -> SharingAcc a -> (SharingAcc a, [StableSharingAcc])
determineScopes floatOutAcc fvs accOccMap rootAcc
  = let
      (sharingAcc, counts) = scopesAcc rootAcc
      unboundTrees         = filter (not . isFreeVar) counts
    in
    if all isFreeVar counts
      then
        (sharingAcc, buildInitialEnvAcc fvs [sa | AccNodeCount sa _ <- counts])
      else
        INTERNAL_ERROR(error) "determineScopes" ("unbound shared subtrees" ++ show unboundTrees)
  where
    scopesAcc :: forall arrs. SharingAcc arrs -> (SharingAcc arrs, NodeCounts)
    scopesAcc (AletSharing _ _)
      = INTERNAL_ERROR(error) "determineScopes: scopesAcc" "unexpected 'AletSharing'"
    scopesAcc sharingAcc@(AvarSharing sn)
      = (sharingAcc, StableSharingAcc sn sharingAcc `accNodeCount` 1)
    scopesAcc (AccSharing sn pacc)
      = case pacc of
          Atag i                  -> reconstruct (Atag i) noNodeCounts
          Pipe afun1 afun2 acc    -> travA (Pipe afun1 afun2) acc
            -- we are not traversing 'afun1' & 'afun2' — see Note [Pipe and sharing recovery]
          Acond e acc1 acc2       -> let
                                       (e'   , accCount1) = scopesExpInit e
                                       (acc1', accCount2) = scopesAcc acc1
                                       (acc2', accCount3) = scopesAcc acc2
                                     in
                                     reconstruct (Acond e' acc1' acc2')
                                                 (accCount1 +++ accCount2 +++ accCount3)

          Atuple tup              -> let (tup', accCount) = travAtup tup
                                     in  reconstruct (Atuple tup') accCount
          Aprj ix a               -> travA (Aprj ix) a

          Use arr                 -> reconstruct (Use arr) noNodeCounts
          Unit e                  -> let
                                       (e', accCount) = scopesExpInit e
                                     in
                                     reconstruct (Unit e') accCount
          Generate sh f           -> let
                                       (sh', accCount1) = scopesExpInit sh
                                       (f' , accCount2) = scopesFun1 f
                                     in
                                     reconstruct (Generate sh' f') (accCount1 +++ accCount2)
          Reshape sh acc          -> travEA Reshape sh acc
          Replicate n acc         -> travEA Replicate n acc
          Slice acc i             -> travEA (flip Slice) i acc
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
                                       (z'   , accCount2)  = scopesExpInit  z
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
                                       (sh' , accCount1) = scopesExpInit  sh
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
               => (RootExp e -> SharingAcc arrs' -> PreAcc SharingAcc RootExp arrs)
               -> RootExp e
               -> SharingAcc arrs'
               -> (SharingAcc arrs, NodeCounts)
        travEA c e acc = reconstruct (c e' acc') (accCount1 +++ accCount2)
          where
            (e'  , accCount1) = scopesExpInit e
            (acc', accCount2) = scopesAcc acc

        travF2A :: (Elt a, Elt b, Arrays arrs)
                => ((Exp a -> Exp b -> RootExp c) -> SharingAcc arrs'
                    -> PreAcc SharingAcc RootExp arrs)
                -> (Exp a -> Exp b -> RootExp c)
                -> SharingAcc arrs'
                -> (SharingAcc arrs, NodeCounts)
        travF2A c f acc = reconstruct (c f' acc') (accCount1 +++ accCount2)
          where
            (f'  , accCount1) = scopesFun2 f
            (acc', accCount2) = scopesAcc  acc

        travF2EA :: (Elt a, Elt b, Arrays arrs)
                 => ((Exp a -> Exp b -> RootExp c) -> RootExp e
                     -> SharingAcc arrs' -> PreAcc SharingAcc RootExp arrs)
                 -> (Exp a -> Exp b -> RootExp c)
                 -> RootExp e
                 -> SharingAcc arrs'
                 -> (SharingAcc arrs, NodeCounts)
        travF2EA c f e acc = reconstruct (c f' e' acc') (accCount1 +++ accCount2 +++ accCount3)
          where
            (f'  , accCount1) = scopesFun2 f
            (e'  , accCount2) = scopesExpInit  e
            (acc', accCount3) = scopesAcc  acc

        travF2A2 :: (Elt a, Elt b, Arrays arrs)
                 => ((Exp a -> Exp b -> RootExp c) -> SharingAcc arrs1
                     -> SharingAcc arrs2 -> PreAcc SharingAcc RootExp arrs)
                 -> (Exp a -> Exp b -> RootExp c)
                 -> SharingAcc arrs1
                 -> SharingAcc arrs2
                 -> (SharingAcc arrs, NodeCounts)
        travF2A2 c f acc1 acc2 = reconstruct (c f' acc1' acc2')
                                             (accCount1 +++ accCount2 +++ accCount3)
          where
            (f'   , accCount1) = scopesFun2 f
            (acc1', accCount2) = scopesAcc  acc1
            (acc2', accCount3) = scopesAcc  acc2

        travAtup ::  Tuple.Atuple SharingAcc a
                 -> (Tuple.Atuple SharingAcc a, NodeCounts)
        travAtup NilAtup          = (NilAtup, noNodeCounts)
        travAtup (SnocAtup tup a) = let (tup', accCountT) = travAtup tup
                                        (a',   accCountA) = scopesAcc a
                                    in
                                    (SnocAtup tup' a', accCountT +++ accCountA)

        travA :: Arrays arrs
              => (SharingAcc arrs' -> PreAcc SharingAcc RootExp arrs)
              -> SharingAcc arrs'
              -> (SharingAcc arrs, NodeCounts)
        travA c acc = reconstruct (c acc') accCount
          where
            (acc', accCount) = scopesAcc acc

          -- Occurrence count of the currently processed node
        accOccCount = let StableNameHeight sn' _ = sn
                      in
                      lookupWithASTName accOccMap (StableASTName sn')

        -- Reconstruct the current tree node.
        --
        -- * If the current node is being shared ('accOccCount > 1'), replace it by a 'AvarSharing'
        --   node and float the shared subtree out wrapped in a 'NodeCounts' value.
        -- * If the current node is not shared, reconstruct it in place.
        -- * Special case for free variables ('Atag'): Replace the tree by a sharing variable and
        --   float the 'Atag' out in a 'NodeCounts' value.  This is independent of the number of
        --   occurrences.
        --
        -- In either case, any completed 'NodeCounts' are injected as bindings using 'AletSharing'
        -- node.
        --
        reconstruct :: Arrays arrs
                    => PreAcc SharingAcc RootExp arrs -> NodeCounts
                    -> (SharingAcc arrs, NodeCounts)
        reconstruct newAcc@(Atag _) _subCount
              -- free variable => replace by a sharing variable regardless of the number of
              -- occurrences
          = let thisCount = StableSharingAcc sn (AccSharing sn newAcc) `accNodeCount` 1
            in
            tracePure "FREE" (show thisCount) $
            (AvarSharing sn, thisCount)
        reconstruct newAcc subCount
              -- shared subtree => replace by a sharing variable (if 'recoverAccSharing' enabled)
          | accOccCount > 1 && recoverAccSharing
          = let allCount = (StableSharingAcc sn sharingAcc `accNodeCount` 1) +++ newCount
            in
            tracePure ("SHARED" ++ completed) (show allCount) $
            (AvarSharing sn, allCount)
              -- neither shared nor free variable => leave it as it is
          | otherwise
          = tracePure ("Normal" ++ completed) (show newCount) $
            (sharingAcc, newCount)
          where
              -- Determine the bindings that need to be attached to the current node...
            (newCount, bindHere) = filterCompleted subCount

              -- ...and wrap them in 'AletSharing' constructors
            lets       = foldl (flip (.)) id . map AletSharing $ bindHere
            sharingAcc = lets $ AccSharing sn newAcc

              -- trace support
            completed | null bindHere = ""
                      | otherwise     = "(" ++ show (length bindHere) ++ " lets)"

        -- Extract *leading* nodes that have a complete node count (i.e., their node count is equal
        -- to the number of occurrences of that node in the overall expression).
        --
        -- Nodes with a completed node count should be let bound at the currently processed node.
        --
        -- NB: Only extract leading nodes (i.e., the longest run at the *front* of the list that is
        --     complete).  Otherwise, we would let-bind subterms before their parents, which leads
        --     scope errors.
        --
        filterCompleted :: NodeCounts -> (NodeCounts, [StableSharingAcc])
        filterCompleted counts
          = let (completed, counts') = break notComplete counts
            in (counts', [sa | AccNodeCount sa _ <- completed])
          where
            -- a node is not yet complete while the node count 'n' is below the overall number
            -- of occurrences for that node in the whole program, with the exception that free
            -- variables are never complete
            notComplete nc@(AccNodeCount sa n) | not . isFreeVar $ nc = lookupWithSharingAcc accOccMap sa > n
            notComplete _                                             = True

    scopesExpInit :: RootExp t -> (RootExp t, NodeCounts)
    scopesExpInit (OccMapExp fvs expOccMap exp)
      = let
          (expWithScopes, nodeCounts) = scopesExp expOccMap exp
          (expCounts, accCounts)      = break isAccNodeCount nodeCounts
        in
        (EnvExp (buildInitialEnvExp fvs [se | ExpNodeCount se _ <- expCounts]) expWithScopes, accCounts)
      where
        isAccNodeCount (AccNodeCount {}) = True
        isAccNodeCount _                 = False
    scopesExpInit _ = INTERNAL_ERROR(error) "scopesExpInit" "not an 'OccMapExp'"

    scopesExp :: forall t. OccMap Exp -> SharingExp t -> (SharingExp t, NodeCounts)
    scopesExp _expOccMap (LetSharing _ _)
      = INTERNAL_ERROR(error) "determineScopes: scopesExp" "unexpected 'LetSharing'"
    scopesExp _expOccMap sharingExp@(VarSharing sn)
      = (sharingExp, StableSharingExp sn sharingExp `expNodeCount` 1)
    scopesExp expOccMap  (ExpSharing sn pexp)
      = case pexp of
          Tag i           -> reconstruct (Tag i) noNodeCounts
          Const c         -> reconstruct (Const c) noNodeCounts
          Tuple tup       -> let (tup', accCount) = travTup tup
                             in
                             reconstruct (Tuple tup') accCount
          Prj i e         -> travE1 (Prj i) e
          IndexNil        -> reconstruct IndexNil noNodeCounts
          IndexCons ix i  -> travE2 IndexCons ix i
          IndexHead i     -> travE1 IndexHead i
          IndexTail ix    -> travE1 IndexTail ix
          IndexAny        -> reconstruct IndexAny noNodeCounts
          Cond e1 e2 e3   -> travE3 Cond e1 e2 e3
          PrimConst c     -> reconstruct (PrimConst c) noNodeCounts
          PrimApp p e     -> travE1 (PrimApp p) e
          IndexScalar a e -> travAE IndexScalar a e
          Shape a         -> travA Shape a
          ShapeSize e     -> travE1 ShapeSize e
      where
        travTup :: Tuple.Tuple SharingExp tup -> (Tuple.Tuple SharingExp tup, NodeCounts)
        travTup NilTup          = (NilTup, noNodeCounts)
        travTup (SnocTup tup e) = let
                                    (tup', accCountT) = travTup tup
                                    (e'  , accCountE) = scopesExp expOccMap e
                                  in
                                  (SnocTup tup' e', accCountT +++ accCountE)

        travE1 :: (SharingExp a -> PreExp SharingAcc SharingExp t) -> SharingExp a
               -> (SharingExp t, NodeCounts)
        travE1 c e = reconstruct (c e') accCount
          where
            (e', accCount) = scopesExp expOccMap e

        travE2 :: (SharingExp a -> SharingExp b -> PreExp SharingAcc SharingExp t)
               -> SharingExp a
               -> SharingExp b
               -> (SharingExp t, NodeCounts)
        travE2 c e1 e2 = reconstruct (c e1' e2') (accCount1 +++ accCount2)
          where
            (e1', accCount1) = scopesExp expOccMap e1
            (e2', accCount2) = scopesExp expOccMap e2

        travE3 :: (SharingExp a -> SharingExp b -> SharingExp c -> PreExp SharingAcc SharingExp t)
               -> SharingExp a
               -> SharingExp b
               -> SharingExp c
               -> (SharingExp t, NodeCounts)
        travE3 c e1 e2 e3 = reconstruct (c e1' e2' e3') (accCount1 +++ accCount2 +++ accCount3)
          where
            (e1', accCount1) = scopesExp expOccMap e1
            (e2', accCount2) = scopesExp expOccMap e2
            (e3', accCount3) = scopesExp expOccMap e3

        travA :: (SharingAcc a -> PreExp SharingAcc SharingExp t) -> SharingAcc a
              -> (SharingExp t, NodeCounts)
        travA c acc = maybeFloatOutAcc c acc' accCount
          where
            (acc', accCount)  = scopesAcc acc

        travAE :: (SharingAcc a -> SharingExp b -> PreExp SharingAcc SharingExp t)
               -> SharingAcc a
               -> SharingExp b
               -> (SharingExp t, NodeCounts)
        travAE c acc e = maybeFloatOutAcc (flip c e') acc' (accCountA +++ accCountE)
          where
            (acc', accCountA) = scopesAcc acc
            (e'  , accCountE) = scopesExp expOccMap e

        maybeFloatOutAcc :: (SharingAcc a -> PreExp SharingAcc SharingExp t)
                         -> SharingAcc a
                         -> NodeCounts
                         -> (SharingExp t, NodeCounts)
        maybeFloatOutAcc c acc@(AvarSharing _) accCount        -- nothing to float out
          = reconstruct (c acc) accCount
        maybeFloatOutAcc c acc                 accCount
          | floatOutAcc = reconstruct (c var) ((stableAcc `accNodeCount` 1) +++ accCount)
          | otherwise   = reconstruct (c acc) accCount
          where
             (var, stableAcc) = abstract acc id

        abstract :: SharingAcc a -> (SharingAcc a -> SharingAcc a)
                 -> (SharingAcc a, StableSharingAcc)
        abstract (AvarSharing _)       _    = INTERNAL_ERROR(error) "sharingAccToVar" "AvarSharing"
        abstract (AletSharing sa acc)  lets = abstract acc (lets . AletSharing sa)
        abstract acc@(AccSharing sn _) lets = (AvarSharing sn, StableSharingAcc sn (lets acc))

        -- Occurrence count of the currently processed node
        expOccCount = let StableNameHeight sn' _ = sn
                      in
                      lookupWithASTName expOccMap (StableASTName sn')

        -- Reconstruct the current tree node.
        --
        -- * If the current node is being shared ('expOccCount > 1'), replace it by a 'VarSharing'
        --   node and float the shared subtree out wrapped in a 'NodeCounts' value.
        -- * If the current node is not shared, reconstruct it in place.
        -- * Special case for free variables ('Tag'): Replace the tree by a sharing variable and
        --   float the 'Tag' out in a 'NodeCounts' value.  This is independent of the number of
        --   occurrences.
        --
        -- In either case, any completed 'NodeCounts' are injected as bindings using 'LetSharing'
        -- node.
        --
        reconstruct :: PreExp SharingAcc SharingExp t -> NodeCounts
                    -> (SharingExp t, NodeCounts)
        reconstruct newExp@(Tag _) _subCount
              -- free variable => replace by a sharing variable regardless of the number of
              -- occurrences
          = let thisCount = StableSharingExp sn (ExpSharing sn newExp) `expNodeCount` 1
            in
            tracePure "FREE" (show thisCount) $
            (VarSharing sn, thisCount)
        reconstruct newExp subCount
              -- shared subtree => replace by a sharing variable (if 'recoverExpSharing' enabled)
          | expOccCount > 1 && recoverExpSharing
          = let allCount = (StableSharingExp sn sharingExp `expNodeCount` 1) +++ newCount
            in
            tracePure ("SHARED" ++ completed) (show allCount) $
            (VarSharing sn, allCount)
              -- neither shared nor free variable => leave it as it is
          | otherwise
          = tracePure ("Normal" ++ completed) (show newCount) $
            (sharingExp, newCount)
          where
              -- Determine the bindings that need to be attached to the current node...
            (newCount, bindHere) = filterCompleted subCount

              -- ...and wrap them in 'LetSharing' constructors
            lets       = foldl (flip (.)) id . map LetSharing $ bindHere
            sharingExp = lets $ ExpSharing sn newExp

              -- trace support
            completed | null bindHere = ""
                      | otherwise     = " (" ++ show (length bindHere) ++ " lets)"

        -- Extract *leading* nodes that have a complete node count (i.e., their node count is equal
        -- to the number of occurrences of that node in the overall expression).
        --
        -- Nodes with a completed node count should be let bound at the currently processed node.
        --
        -- NB: Only extract leading nodes (i.e., the longest run at the *front* of the list that is
        --     complete).  Otherwise, we would let-bind subterms before their parents, which leads
        --     scope errors.
        --
        filterCompleted :: NodeCounts -> (NodeCounts, [StableSharingExp])
        filterCompleted counts
          = let (completed, counts') = break notComplete counts
            in (counts', [sa | ExpNodeCount sa _ <- completed])
          where
            -- a node is not yet complete while the node count 'n' is below the overall number
            -- of occurrences for that node in the whole program, with the exception that free
            -- variables are never complete
            notComplete nc@(ExpNodeCount sa n) | not . isFreeVar $ nc = lookupWithSharingExp expOccMap sa > n
            notComplete _                                             = True

    -- The lambda bound variable is at this point already irrelevant; for details, see
    -- Note [Traversing functions and side effects]
    --
    scopesFun1 :: Elt e1 => (Exp e1 -> RootExp e2) -> (Exp e1 -> RootExp e2, NodeCounts)
    scopesFun1 f = (const body, counts)
      where
        (body, counts) = scopesExpInit (f undefined)

    -- The lambda bound variable is at this point already irrelevant; for details, see
    -- Note [Traversing functions and side effects]
    --
    scopesFun2 :: (Elt e1, Elt e2)
               => (Exp e1 -> Exp e2 -> RootExp e3)
               -> (Exp e1 -> Exp e2 -> RootExp e3, NodeCounts)
    scopesFun2 f = (\_ _ -> body, counts)
      where
        (body, counts) = scopesExpInit (f undefined undefined)

    -- The lambda bound variable is at this point already irrelevant; for details, see
    -- Note [Traversing functions and side effects]
    --
    scopesStencil1 :: forall sh e1 e2 stencil. Stencil sh e1 stencil
                   => SharingAcc (Array sh e1){-dummy-}
                   -> (stencil -> RootExp e2)
                   -> (stencil -> RootExp e2, NodeCounts)
    scopesStencil1 _ stencilFun = (const body, counts)
      where
        (body, counts) = scopesExpInit (stencilFun undefined)

    -- The lambda bound variable is at this point already irrelevant; for details, see
    -- Note [Traversing functions and side effects]
    --
    scopesStencil2 :: forall sh e1 e2 e3 stencil1 stencil2.
                      (Stencil sh e1 stencil1, Stencil sh e2 stencil2)
                   => SharingAcc (Array sh e1){-dummy-}
                   -> SharingAcc (Array sh e2){-dummy-}
                   -> (stencil1 -> stencil2 -> RootExp e3)
                   -> (stencil1 -> stencil2 -> RootExp e3, NodeCounts)
    scopesStencil2 _ _ stencilFun = (\_ _ -> body, counts)
      where
        (body, counts) = scopesExpInit (stencilFun undefined undefined)

-- |Recover sharing information and annotate the HOAS AST with variable and let binding
-- annotations.  The first argument determines whether array computations are floated out of
-- expressions irrespective of whether they are shared or not — 'True' implies floating them out.
--
-- Also returns the 'StableSharingAcc's of all 'Atag' leaves in environment order — they represent
-- the free variables of the AST.
--
-- NB: Strictly speaking, this function is not deterministic, as it uses stable pointers to
--     determine the sharing of subterms.  The stable pointer API does not guarantee its
--     completeness; i.e., it may miss some equalities, which implies that we may fail to discover
--     some sharing.  However, sharing does not affect the denotational meaning of an array
--     computation; hence, we do not compromise denotational correctness.
--
--     There is one caveat: We currently rely on the 'Atag' and 'Tag' leaves representing free
--       variables to be shared if any of them is used more than once.  If one is duplicated, the
--       environment for de Bruijn conversion will have a duplicate entry, and hence, be of the
--       wrong size, which is fatal. (The 'buildInitialEnv*' functions will already bail out.)
--
recoverSharingAcc :: Typeable a => Bool -> Level -> [Level] -> Acc a -> (SharingAcc a, [StableSharingAcc])
{-# NOINLINE recoverSharingAcc #-}
recoverSharingAcc floatOutAcc lvl fvs acc
  = let (acc', occMap) =
          unsafePerformIO $ do        -- to enable stable pointers; it's safe as explained above
            { (acc', occMap) <- makeOccMap lvl acc

            ; occMapList <- Hash.toList occMap
            ; traceChunk "OccMap" $
                show occMapList

            ; frozenOccMap <- freezeOccMap occMap
            ; return (acc', frozenOccMap)
            }
    in
    determineScopes floatOutAcc fvs occMap acc'


-- Pretty printing
-- ---------------

instance Elt a => Show (Exp a) where
  show = show . convertExp EmptyLayout [] . EnvExp undefined . toSharingExp
    where
      toSharingExp :: Exp b -> SharingExp b
      toSharingExp (Exp pexp)
        = case pexp of
            Tag i           -> ExpSharing undefined $ Tag i
            Const v         -> ExpSharing undefined $ Const v
            Tuple tup       -> ExpSharing undefined $ Tuple (toSharingTup tup)
            Prj idx e       -> ExpSharing undefined $ Prj idx (toSharingExp e)
            IndexNil        -> ExpSharing undefined $ IndexNil
            IndexCons ix i  -> ExpSharing undefined $ IndexCons (toSharingExp ix) (toSharingExp i)
            IndexHead ix    -> ExpSharing undefined $ IndexHead (toSharingExp ix)
            IndexTail ix    -> ExpSharing undefined $ IndexTail (toSharingExp ix)
            IndexAny        -> ExpSharing undefined $ IndexAny
            Cond e1 e2 e3   -> ExpSharing undefined $ Cond (toSharingExp e1) (toSharingExp e2)
                                                           (toSharingExp e3)
            PrimConst c     -> ExpSharing undefined $ PrimConst c
            PrimApp p e     -> ExpSharing undefined $ PrimApp p (toSharingExp e)
            IndexScalar a e -> ExpSharing undefined $ IndexScalar (fst $ recoverSharingAcc False 0 [] a)
                                                                  (toSharingExp e)
            Shape a         -> ExpSharing undefined $ Shape (fst $ recoverSharingAcc False 0 [] a)
            ShapeSize e     -> ExpSharing undefined $ ShapeSize (toSharingExp e)

      toSharingTup :: Tuple.Tuple Exp tup -> Tuple.Tuple SharingExp tup
      toSharingTup NilTup          = NilTup
      toSharingTup (SnocTup tup e) = SnocTup (toSharingTup tup) (toSharingExp e)

-- for debugging
showPreAccOp :: forall acc exp arrs. PreAcc acc exp arrs -> String
showPreAccOp (Atag i)             = "Atag " ++ show i
showPreAccOp (Pipe _ _ _)         = "Pipe"
showPreAccOp (Acond _ _ _)        = "Acond"
showPreAccOp (Atuple _)           = "Atuple"
showPreAccOp (Aprj _ _)           = "Aprj"
showPreAccOp (Use a)              = "Use " ++ showArrays a
showPreAccOp (Unit _)             = "Unit"
showPreAccOp (Generate _ _)       = "Generate"
showPreAccOp (Reshape _ _)        = "Reshape"
showPreAccOp (Replicate _ _)      = "Replicate"
showPreAccOp (Slice _ _)          = "Slice"
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
    display xs  = "(" ++ concat (intersperse ", " xs) ++ ")"


showShortendArr :: Elt e => Array sh e -> String
showShortendArr arr
  = show (take cutoff l) ++ if length l > cutoff then ".." else ""
  where
    l      = Sugar.toList arr
    cutoff = 5

_showSharingAccOp :: SharingAcc arrs -> String
_showSharingAccOp (AvarSharing sn)    = "AVAR " ++ show (hashStableNameHeight sn)
_showSharingAccOp (AletSharing _ acc) = "ALET " ++ _showSharingAccOp acc
_showSharingAccOp (AccSharing _ acc)  = showPreAccOp acc

-- for debugging
showPreExpOp :: PreExp acc exp t -> String
showPreExpOp (Tag _)           = "Tag"
showPreExpOp (Const c)         = "Const " ++ show c
showPreExpOp (Tuple _)         = "Tuple"
showPreExpOp (Prj _ _)         = "Prj"
showPreExpOp IndexNil          = "IndexNil"
showPreExpOp (IndexCons _ _)   = "IndexCons"
showPreExpOp (IndexHead _)     = "IndexHead"
showPreExpOp (IndexTail _)     = "IndexTail"
showPreExpOp IndexAny          = "IndexAny"
showPreExpOp (Cond _ _ _)      = "Cons"
showPreExpOp (PrimConst _)     = "PrimConst"
showPreExpOp (PrimApp _ _)     = "PrimApp"
showPreExpOp (IndexScalar _ _) = "IndexScalar"
showPreExpOp (Shape _)         = "Shape"
showPreExpOp (ShapeSize _)     = "ShapeSize"

