{-# LANGUAGE GADTs, ScopedTypeVariables, NoMonomorphismRestriction #-}
-- |
-- Module      : Data.Array.Accelerate.Pretty.Traverse
-- Copyright   : [2010..2011] Sean Seefried
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Data.Array.Accelerate.Pretty.Traverse
  where

-- friends
import Data.Array.Accelerate.Array.Sugar hiding ((!))
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Type


cat :: Show s => String -> s -> String
cat t s = t ++ show s

data Labels = Labels { accFormat :: String
                     , expFormat :: String
                     , funFormat :: String
                     , tupleFormat :: String
                     , arrayFormat :: String
                     , boundaryFormat :: String
                     , primFunFormat :: String
                     }

travAcc :: forall m b aenv a. Monad m => Labels -> (String -> String -> [m b] -> m b)
       -> (String -> String -> m b) -> OpenAcc aenv a -> m b
travAcc f c l (OpenAcc openAcc) = travAcc' openAcc
  where
    combine = c (accFormat f)
    leaf    = l (accFormat f)
    travAcc' :: PreOpenAcc OpenAcc aenv a -> m b
    travAcc' (Alet acc1 acc2)  = combine "Alet" [travAcc f c l acc1, travAcc f c l acc2]
    travAcc' (Alet2 acc1 acc2) = combine "Alet2" [ travAcc f c l acc1, travAcc f c l acc2 ]
    travAcc' (PairArrays acc1 acc2) = combine "PairArrays" [travAcc f c l acc1, travAcc f c l acc2]
    travAcc' (Avar idx) = leaf ("AVar " `cat` idxToInt idx)
    travAcc' (Apply afun acc) = combine "Apply" [travAfun f c l afun, travAcc f c l acc]
    travAcc' (Acond e acc1 acc2) = combine "Acond" [travExp f c l e, travAcc f c l acc1, travAcc f c l acc2]
    travAcc' (Use arr) = combine "Use" [ travArray f l arr ]
    travAcc' (Unit e) = combine "Unit" [ travExp f c l e ]
    travAcc' (Generate sh fun) = combine "Generate" [ travExp f c l  sh, travFun f c l fun]
    travAcc' (Reshape sh acc) = combine "Reshape" [ travExp f c l  sh, travAcc f c l acc ]
    travAcc' (Replicate _ ix acc) = combine "Replicate" [ travExp f c l  ix, travAcc f c l acc ]
    travAcc' (Index _ acc ix) = combine "Index" [ travAcc f c l acc, travExp f c l  ix ]
    travAcc' (Map fun acc) = combine "Map" [ travFun f c l fun, travAcc f c l acc ]
    travAcc' (ZipWith fun acc1 acc2) = combine "ZipWith" [ travFun f c l fun, travAcc f c l acc1, travAcc f c l acc2 ]
    travAcc' (Fold fun e acc) = combine "Fold" [ travFun f c l fun, travExp f c l  e, travAcc f c l acc]
    travAcc' (Fold1 fun acc) = combine "Fold1" [ travFun f c l fun, travAcc f c l acc]
    travAcc' (FoldSeg fun e acc1 acc2) = combine "FoldSeg" [ travFun f c l fun, travExp f c l  e,
                                                            travAcc f c l acc1, travAcc f c l acc2 ]
    travAcc' (Fold1Seg fun acc1 acc2) = combine "FoldSeg1" [ travFun f c l fun, travAcc f c l acc1, travAcc f c l acc2 ]
    travAcc' (Scanl fun e acc) = combine "Scanl" [ travFun f c l fun, travExp f c l  e, travAcc f c l acc ]
    travAcc' (Scanl' fun e acc) = combine "Scanl'" [ travFun f c l fun, travExp f c l  e, travAcc f c l acc ]
    travAcc' (Scanl1 fun acc) = combine "Scanl1" [ travFun f c l fun, travAcc f c l acc ]
    travAcc' (Scanr fun e acc) = combine "Scanr" [ travFun f c l fun, travExp f c l  e, travAcc f c l acc ]
    travAcc' (Scanr' fun e acc) = combine "Scanr'" [ travFun f c l fun, travExp f c l  e, travAcc f c l acc ]
    travAcc' (Scanr1 fun acc) = combine "Scanr1" [ travFun f c l fun, travAcc f c l acc ]
    travAcc' (Permute fun dfts p acc) = combine "Permute" [ travFun f c l fun, travAcc f c l dfts,
                                                           travFun f c l p, travAcc f c l acc]
    travAcc' (Backpermute sh p acc) = combine "Backpermute" [ travExp f c l  sh, travFun f c l p, travAcc f c l acc]
    travAcc' (Stencil sten bndy acc) = combine "Stencil" [ travFun f c l sten, travBoundary f l acc bndy
                                                        , travAcc f c l acc]
    travAcc' (Stencil2 sten bndy1 acc1 bndy2 acc2) = combine "Stencil2" [ travFun f c l sten, travBoundary f l acc1 bndy1,
                                                                   travAcc f c l acc1, travBoundary f l acc2 bndy2,
                                                                   travAcc f c l acc2]

travExp :: forall m env aenv a b . Monad m => Labels
       -> (String -> String -> [m b] -> m b)
       -> (String -> String -> m b)
       -> OpenExp env aenv a -> m b
travExp f c l expr = travExp' expr
  where
    combine = c (expFormat f)
    leaf    = l (expFormat f)
    travExp' :: OpenExp env aenv a -> m b
    travExp' (Let e1 e2)         = combine "Let" [travExp f c l e1, travExp f c l e2]
    travExp' (Var idx)           = leaf ("Var "   `cat` idxToInt idx)
    travExp' (Const v)           = leaf ("Const " `cat` (toElt v :: a))
    travExp' (Tuple tup)         = combine "Tuple" [ travTuple f c l tup ]
    travExp' (Prj idx e)         = combine ("Prj " `cat` tupleIdxToInt idx) [ travExp f c l e ]
    travExp' (IndexNil)          = leaf "IndexNil"
    travExp' (IndexCons t h)     = combine "IndexCons" [ travExp f c l t, travExp f c l h]
    travExp' (IndexHead ix)      = combine "IndexHead" [ travExp f c l ix ]
    travExp' (IndexTail ix)      = combine "IndexTail" [ travExp f c l ix ]
    travExp' (IndexAny)          = leaf "IndexAny"
    travExp' (Cond cond thn els) = combine "Cond" [travExp f c l cond, travExp f c l thn, travExp f c l els]
    travExp' (PrimConst a)       = leaf ("PrimConst " `cat` labelForConst a)
    travExp' (PrimApp p a)       = combine "PrimApp" [ l (primFunFormat f) (labelForPrimFun p), travExp f c l a ]
    travExp' (IndexScalar idx i) = combine "IndexScalar" [ travAcc f c l idx, travExp f c l i]
    travExp' (Shape idx)         = combine "Shape" [ travAcc f c l idx ]
    travExp' (Size idx)          = combine "Size" [ travAcc f c l idx ]


travAfun :: forall m b aenv fun. Monad m => Labels -> (String -> String -> [m b] -> m b)
        -> (String -> String -> m b) -> OpenAfun aenv fun -> m b
travAfun f c l openAfun = travAfun' openAfun
  where
    combine = c (funFormat f)
    travAfun' :: OpenAfun aenv fun -> m b
    travAfun' (Abody body) = combine "Abody" [ travAcc f c l body ]
    travAfun' (Alam fun)   = combine "Alam"  [ travAfun f c l fun ]

travFun :: forall m b env aenv fun.Monad m => Labels -> (String -> String -> [m b] -> m b)
        -> (String -> String -> m b) -> OpenFun env aenv fun -> m b
travFun f c l openFun = travFun' openFun
  where
    combine = c (funFormat f)
    travFun' :: OpenFun env aenv fun -> m b
    travFun' (Body body) = combine "Body" [ travExp f c l body ]
    travFun' (Lam fun)   = combine "Lam"  [ travFun f c l fun ]

travArray :: forall dim a m b. Monad m => Labels -> (String -> String -> m b) -> Array dim a -> m b
travArray f l (Array sh _) = l (arrayFormat f) ("Array" `cat` (toElt sh :: dim))

travBoundary :: forall aenv dim e m b. (Monad m, Elt e) => Labels -> (String -> String -> m b)
             -> {-dummy-}OpenAcc aenv (Array dim e)
             -> Boundary (EltRepr e) -> m b
travBoundary f l boundary = travBoundary' boundary
  where
    leaf = l (boundaryFormat f)
    travBoundary' :: {-dummy-}OpenAcc aenv (Array dim e) -> Boundary (EltRepr e) -> m b
    travBoundary' _ Clamp        = leaf "Clamp"
    travBoundary' _ Mirror       = leaf "Mirror"
    travBoundary' _ Wrap         = leaf "Wrap"
    travBoundary' _ (Constant e) = leaf ("Constant " `cat` (toElt e :: e))


travTuple :: forall m b env aenv t. Monad m => Labels -> (String -> String -> [m b] -> m b)
          -> (String -> String -> m b) -> Tuple (OpenExp env aenv) t -> m b
travTuple f c l tuple = travTuple' tuple
  where
    leaf    = l (tupleFormat f)
    combine = c (tupleFormat f)
    travTuple' :: Tuple (OpenExp env aenv) t -> m b
    travTuple' NilTup          = leaf     "NilTup"
    travTuple' (SnocTup tup e) = combine "SnocTup" [ travTuple f c l tup, travExp f c l e ]

labelForPrimFun :: PrimFun a -> String
labelForPrimFun (PrimAdd _)            = "PrimAdd"
labelForPrimFun (PrimSub _)            = "PrimSub"
labelForPrimFun (PrimMul _)            = "PrimMul"
labelForPrimFun (PrimNeg _)            = "PrimNeg"
labelForPrimFun (PrimAbs _)            = "PrimAbs"
labelForPrimFun (PrimSig _)            = "PrimSig"
labelForPrimFun (PrimQuot _)           = "PrimQuot"
labelForPrimFun (PrimRem _)            = "PrimRem"
labelForPrimFun (PrimIDiv _)           = "PrimIDiv"
labelForPrimFun (PrimMod _)            = "PrimMod"
labelForPrimFun (PrimBAnd _)           = "PrimBAnd"
labelForPrimFun (PrimBOr _)            = "PrimBOr"
labelForPrimFun (PrimBXor _)           = "PrimBXor"
labelForPrimFun (PrimBNot _)           = "PrimBNot"
labelForPrimFun (PrimBShiftL _)        = "PrimBShiftL"
labelForPrimFun (PrimBShiftR _)        = "PrimBShiftR"
labelForPrimFun (PrimBRotateL _)       = "PrimBRotateL"
labelForPrimFun (PrimBRotateR _)       = "PrimBRotateR"
labelForPrimFun (PrimFDiv _)           = "PrimFDiv"
labelForPrimFun (PrimRecip _)          = "PrimRecip"
labelForPrimFun (PrimSin _)            = "PrimSin"
labelForPrimFun (PrimCos _)            = "PrimCos"
labelForPrimFun (PrimTan _)            = "PrimTan"
labelForPrimFun (PrimAsin _)           = "PrimAsin"
labelForPrimFun (PrimAcos _)           = "PrimAcos"
labelForPrimFun (PrimAtan _)           = "PrimAtan"
labelForPrimFun (PrimAsinh _)          = "PrimAsinh"
labelForPrimFun (PrimAcosh _)          = "PrimAcosh"
labelForPrimFun (PrimAtanh _)          = "PrimAtanh"
labelForPrimFun (PrimExpFloating _)    = "PrimExpFloating"
labelForPrimFun (PrimSqrt _)           = "PrimSqrt"
labelForPrimFun (PrimLog _)            = "PrimLog"
labelForPrimFun (PrimFPow _)           = "PrimFPow"
labelForPrimFun (PrimLogBase _)        = "PrimLogBase"
labelForPrimFun (PrimTruncate _ _)     = "PrimTruncate"
labelForPrimFun (PrimRound _ _)        = "PrimRound"
labelForPrimFun (PrimFloor _ _)        = "PrimFloor"
labelForPrimFun (PrimCeiling _ _)      = "PrimCeiling"
labelForPrimFun (PrimAtan2 _)          = "PrimAtan2"
labelForPrimFun (PrimLt _)             = "PrimLt"
labelForPrimFun (PrimGt _)             = "PrimGt"
labelForPrimFun (PrimLtEq _)           = "PrimLtEq"
labelForPrimFun (PrimGtEq _)           = "PrimGtEq"
labelForPrimFun (PrimEq _)             = "PrimEq"
labelForPrimFun (PrimNEq _)            = "PrimNEq"
labelForPrimFun (PrimMax _)            = "PrimMax"
labelForPrimFun (PrimMin _)            = "PrimMin"
labelForPrimFun PrimLAnd               = "PrimLAnd"
labelForPrimFun PrimLOr                = "PrimLOr"
labelForPrimFun PrimLNot               = "PrimLNot"
labelForPrimFun PrimOrd                = "PrimOrd"
labelForPrimFun PrimChr                = "PrimChr"
labelForPrimFun PrimBoolToInt          = "PrimBoolToInt"
labelForPrimFun (PrimFromIntegral _ _) = "PrimFromIntegral"

labelForConst :: PrimConst a -> String
labelForConst (PrimMinBound _) = "PrimMinBound"
labelForConst (PrimMaxBound _) = "PrimMaxBound"
labelForConst (PrimPi       _) = "PrimPi"

tupleIdxToInt :: TupleIdx t e -> Int
tupleIdxToInt ZeroTupIdx     = 0
tupleIdxToInt (SuccTupIdx n) = 1 + tupleIdxToInt n

-- Auxiliary ops
--

