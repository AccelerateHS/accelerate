{-# LANGUAGE GADTs, FlexibleInstances, TypeOperators, ScopedTypeVariables, RankNTypes #-}
-- |
-- Module      : Data.Array.Accelerate.Pretty.Print
-- Copyright   : [2008..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Pretty.Print (

  -- * Pretty printing functions
  PrettyAcc,
  prettyPreAcc, prettyAcc, 
  prettyPreExp, prettyExp, 
  prettyPreAfun, prettyAfun, 
  prettyPreFun, prettyFun, 
  noParens

) where

-- standard libraries
import Text.PrettyPrint
import Prelude hiding (exp)

-- friends
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Type

-- Pretty printing
-- ---------------

-- The type of pretty printing functions for array computations.
--
type PrettyAcc acc = forall aenv t. Int -> (Doc -> Doc) -> acc aenv t -> Doc

-- Pretty print an array expression
--
prettyAcc :: PrettyAcc OpenAcc
prettyAcc alvl wrap (OpenAcc acc) = prettyPreAcc prettyAcc alvl wrap acc

prettyPreAcc :: PrettyAcc acc -> Int -> (Doc -> Doc) -> PreOpenAcc acc aenv a -> Doc
prettyPreAcc pp alvl wrap (Let acc1 acc2)
  = wrap 
  $ sep [ hang (text "let a" <> int alvl <+> char '=') 2 $
            pp alvl noParens acc1
        , text "in" <+> pp (alvl + 1) noParens acc2
        ]
prettyPreAcc pp alvl wrap (Let2 acc1 acc2)
  = wrap
  $ sep [ hang (text "let (a" <> int alvl <> text ", a" <> int (alvl + 1) <> char ')' <+>
                char '=') 2 $
            pp alvl noParens acc1
        , text "in" <+> pp (alvl + 2) noParens acc2
        ]
prettyPreAcc pp alvl wrap (PairArrays acc1 acc2)
  = wrap $ sep [pp alvl parens acc1, pp alvl parens acc2]
prettyPreAcc _  alvl _    (Avar idx)
  = text $ 'a' : show (alvl - idxToInt idx - 1)
prettyPreAcc pp alvl wrap (Apply afun acc)
  = wrap $ sep [parens (prettyPreAfun pp alvl afun), pp alvl parens acc]
prettyPreAcc pp alvl wrap (Acond e acc1 acc2)
  = wrap $ prettyArrOp "cond" [prettyPreExp pp 0 alvl parens e, pp alvl parens acc1, pp alvl parens acc2]
prettyPreAcc _  _    wrap (Use arr)
  = wrap $ prettyArrOp "use" [prettyArray arr]
prettyPreAcc pp alvl wrap (Unit e)
  = wrap $ prettyArrOp "unit" [prettyPreExp pp 0 alvl parens e]
prettyPreAcc pp alvl wrap (Generate sh f)
  = wrap 
  $ prettyArrOp "generate" [prettyPreExp pp 0 alvl parens sh, parens (prettyPreFun pp alvl f)]
prettyPreAcc pp alvl wrap (Reshape sh acc)
  = wrap $ prettyArrOp "reshape" [prettyPreExp pp 0 alvl parens sh, pp alvl parens acc]
prettyPreAcc pp alvl wrap (Replicate _ty ix acc)
  = wrap $ prettyArrOp "replicate" [prettyPreExp pp 0 alvl id ix, pp alvl parens acc]
prettyPreAcc pp alvl wrap (Index _ty acc ix)
  = wrap $ sep [pp alvl parens acc, char '!', prettyPreExp pp 0 alvl id ix]
prettyPreAcc pp alvl wrap (Map f acc)
  = wrap $ prettyArrOp "map" [parens (prettyPreFun pp alvl f), pp alvl parens acc]
prettyPreAcc pp alvl wrap (ZipWith f acc1 acc2)
  = wrap 
  $ prettyArrOp "zipWith"
      [parens (prettyPreFun pp alvl f), pp alvl parens acc1, pp alvl parens acc2]
prettyPreAcc pp alvl wrap (Fold f e acc)
  = wrap 
  $ prettyArrOp "fold" [parens (prettyPreFun pp alvl f), prettyPreExp pp 0 alvl parens e,
                        pp alvl parens acc]
prettyPreAcc pp alvl wrap (Fold1 f acc)
  = wrap $ prettyArrOp "fold1" [parens (prettyPreFun pp alvl f), pp alvl parens acc]
prettyPreAcc pp alvl wrap (FoldSeg f e acc1 acc2)
  = wrap 
  $ prettyArrOp "foldSeg" [parens (prettyPreFun pp alvl f), prettyPreExp pp 0 alvl parens e,
                           pp alvl parens acc1, pp alvl parens acc2]
prettyPreAcc pp alvl wrap (Fold1Seg f acc1 acc2)
  = wrap 
  $ prettyArrOp "fold1Seg" [parens (prettyPreFun pp alvl f), pp alvl parens acc1,
                            pp alvl parens acc2]
prettyPreAcc pp alvl wrap (Scanl f e acc)
  = wrap 
  $ prettyArrOp "scanl" [parens (prettyPreFun pp alvl f), prettyPreExp pp 0 alvl parens e,
                         pp alvl parens acc]
prettyPreAcc pp alvl wrap (Scanl' f e acc)
  = wrap 
  $ prettyArrOp "scanl'" [parens (prettyPreFun pp alvl f), prettyPreExp pp 0 alvl parens e,
                          pp alvl parens acc]
prettyPreAcc pp alvl wrap (Scanl1 f acc)
  = wrap 
  $ prettyArrOp "scanl1" [parens (prettyPreFun pp alvl f), pp alvl parens acc]
prettyPreAcc pp alvl wrap (Scanr f e acc)
  = wrap 
  $ prettyArrOp "scanr" [parens (prettyPreFun pp alvl f), prettyPreExp pp 0 alvl parens e,
                         pp alvl parens acc]
prettyPreAcc pp alvl wrap (Scanr' f e acc)
  = wrap 
  $ prettyArrOp "scanr'" [parens (prettyPreFun pp alvl f), prettyPreExp pp 0 alvl parens e,
                          pp alvl parens acc]
prettyPreAcc pp alvl wrap (Scanr1 f acc)
  = wrap 
  $ prettyArrOp "scanr1" [parens (prettyPreFun pp alvl f), pp alvl parens acc]
prettyPreAcc pp alvl wrap (Permute f dfts p acc)
  = wrap 
  $ prettyArrOp "permute" [parens (prettyPreFun pp alvl f), pp alvl parens dfts,
                           parens (prettyPreFun pp alvl p), pp alvl parens acc]
prettyPreAcc pp alvl wrap (Backpermute sh p acc)
  = wrap 
  $ prettyArrOp "backpermute" [prettyPreExp pp 0 alvl parens sh,
                               parens (prettyPreFun pp alvl p),
                               pp alvl parens acc]
prettyPreAcc pp alvl wrap (Stencil sten bndy acc)
  = wrap 
  $ prettyArrOp "stencil" [parens (prettyPreFun pp alvl sten),
                           prettyBoundary acc bndy,
                           pp alvl parens acc]
prettyPreAcc pp alvl wrap (Stencil2 sten bndy1 acc1 bndy2 acc2)
  = wrap 
  $ prettyArrOp "stencil2" [parens (prettyPreFun pp alvl sten),
                            prettyBoundary acc1 bndy1,
                            pp alvl parens acc1,
                            prettyBoundary acc2 bndy2,
                            pp alvl parens acc2]

prettyBoundary :: forall acc aenv dim e. Elt e
               => {-dummy-}acc aenv (Array dim e) -> Boundary (EltRepr e) -> Doc
prettyBoundary _ Clamp        = text "Clamp"
prettyBoundary _ Mirror       = text "Mirror"
prettyBoundary _ Wrap         = text "Wrap"
prettyBoundary _ (Constant e) = parens $ text "Constant" <+> text (show (toElt e :: e))

prettyArrOp :: String -> [Doc] -> Doc
prettyArrOp name docs = hang (text name) 2 $ sep docs

-- Pretty print a function over array computations.
--
-- At the moment restricted to /closed/ functions.
--
prettyAfun :: Int -> Afun fun -> Doc
prettyAfun = prettyPreAfun prettyAcc

prettyPreAfun :: forall acc fun. PrettyAcc acc -> Int -> PreAfun acc fun -> Doc
prettyPreAfun pp _alvl fun =
  let (n, bodyDoc) = count n fun
  in
  char '\\' <> hsep [text $ 'a' : show idx | idx <- [0..n]] <+>
  text "->" <+> bodyDoc
  where
     count :: Int -> PreOpenAfun acc aenv' fun' -> (Int, Doc)
     count lvl (Abody body) = (-1, pp (lvl + 1) noParens body) -- 'lvl+1' ok as functions is closed!
     count lvl (Alam  fun') = let (n, body) = count lvl fun' in (1 + n, body)

-- Pretty print a function over scalar expressions.
--
prettyFun :: Int -> OpenFun env aenv fun -> Doc
prettyFun = prettyPreFun prettyAcc

prettyPreFun :: forall acc env aenv fun. PrettyAcc acc -> Int -> PreOpenFun acc env aenv fun -> Doc
prettyPreFun pp alvl fun =
  let (n, bodyDoc) = count n fun
  in
  char '\\' <> hsep [text $ 'x' : show idx | idx <- [0..n]] <+>
  text "->" <+> bodyDoc
  where
     count :: Int -> PreOpenFun acc env' aenv' fun' -> (Int, Doc)
     count lvl (Body body) = (-1, prettyPreExp pp lvl alvl noParens body)
     count lvl (Lam  fun') = let (n, body) = count lvl fun' in (1 + n, body)

-- Pretty print an expression.
--
-- * Apply the wrapping combinator (3rd argument) to any compound expressions.
--
prettyExp :: Int -> Int -> (Doc -> Doc) -> OpenExp env aenv t -> Doc
prettyExp = prettyPreExp prettyAcc

prettyPreExp :: forall acc t env aenv.
                PrettyAcc acc -> Int -> Int -> (Doc -> Doc) -> PreOpenExp acc env aenv t -> Doc
prettyPreExp _pp lvl _ _ (Var idx)
  = text $ 'x' : show (lvl - idxToInt idx)
prettyPreExp _pp _ _ _ (Const v)
  = text $ show (toElt v :: t)
prettyPreExp pp lvl alvl _ (Tuple tup)
  = prettyTuple pp lvl alvl tup
prettyPreExp pp lvl alvl wrap (Prj idx e)
  = wrap $ prettyTupleIdx idx <+> prettyPreExp pp lvl alvl parens e
prettyPreExp _pp _lvl _alvl wrap IndexNil
  = wrap $ text "index Z"
prettyPreExp pp lvl alvl wrap (IndexCons t h)
  = wrap $
      text "index" <+>
      parens (prettyPreExp pp lvl alvl parens t <+> text ":." <+> prettyPreExp pp lvl alvl parens h)
prettyPreExp pp lvl alvl wrap (IndexHead ix)
  = wrap $ text "indexHead" <+> prettyPreExp pp lvl alvl parens ix
prettyPreExp pp lvl alvl wrap (IndexTail ix)
  = wrap $ text "indexTail" <+> prettyPreExp pp lvl alvl parens ix
prettyPreExp _ _ _ wrap (IndexAny)
  = wrap $ text "indexAny"
prettyPreExp pp lvl alvl wrap (Cond c t e)
  = wrap $ sep [prettyPreExp pp lvl alvl parens c <+> char '?',
                parens (prettyPreExp pp lvl alvl noParens t <> comma <+>
                        prettyPreExp pp lvl alvl noParens e)]
prettyPreExp _pp _ _ _ (PrimConst a)
 = prettyConst a
prettyPreExp pp lvl alvl wrap (PrimApp p a)
  = wrap $ prettyPrim p <+> prettyPreExp pp lvl alvl parens a
prettyPreExp pp lvl alvl wrap (IndexScalar idx i)
  = wrap $ cat [pp alvl parens idx, char '!', prettyPreExp pp lvl alvl parens i]
prettyPreExp pp _lvl alvl wrap (Shape idx)
  = wrap $ text "shape" <+> pp alvl parens idx
prettyPreExp pp _lvl alvl wrap (Size idx)
  = wrap $ text "size" <+> pp alvl parens idx

-- Pretty print nested pairs as a proper tuple.
--
prettyTuple :: forall acc env aenv t.
               PrettyAcc acc -> Int -> Int -> Tuple (PreOpenExp acc env aenv) t -> Doc
prettyTuple pp lvl alvl exp = parens $ sep (map (<> comma) (init es) ++ [last es])
  where
    es = collect exp
    --
    collect :: Tuple (PreOpenExp acc env aenv) t' -> [Doc]
    collect NilTup          = []
    collect (SnocTup tup e) = collect tup ++ [prettyPreExp pp lvl alvl noParens e]

-- Pretty print an index for a tuple projection
--
prettyTupleIdx :: TupleIdx t e -> Doc
prettyTupleIdx = int . toInt
  where
    toInt  :: TupleIdx t e -> Int
    toInt ZeroTupIdx       = 0
    toInt (SuccTupIdx tup) = toInt tup + 1

-- Pretty print a primitive constant
--
prettyConst :: PrimConst a -> Doc
prettyConst (PrimMinBound _) = text "minBound"
prettyConst (PrimMaxBound _) = text "maxBound"
prettyConst (PrimPi       _) = text "pi"

-- Pretty print a primitive operation
--
prettyPrim :: PrimFun a -> Doc
prettyPrim (PrimAdd _)            = text "(+)"
prettyPrim (PrimSub _)            = text "(-)"
prettyPrim (PrimMul _)            = text "(*)"
prettyPrim (PrimNeg _)            = text "negate"
prettyPrim (PrimAbs _)            = text "abs"
prettyPrim (PrimSig _)            = text "signum"
prettyPrim (PrimQuot _)           = text "quot"
prettyPrim (PrimRem _)            = text "rem"
prettyPrim (PrimIDiv _)           = text "div"
prettyPrim (PrimMod _)            = text "mod"
prettyPrim (PrimBAnd _)           = text "(.&.)"
prettyPrim (PrimBOr _)            = text "(.|.)"
prettyPrim (PrimBXor _)           = text "xor"
prettyPrim (PrimBNot _)           = text "complement"
prettyPrim (PrimBShiftL _)        = text "shiftL"
prettyPrim (PrimBShiftR _)        = text "shiftR"
prettyPrim (PrimBRotateL _)       = text "rotateL"
prettyPrim (PrimBRotateR _)       = text "rotateR"
prettyPrim (PrimFDiv _)           = text "(/)"
prettyPrim (PrimRecip _)          = text "recip"
prettyPrim (PrimSin _)            = text "sin"
prettyPrim (PrimCos _)            = text "cos"
prettyPrim (PrimTan _)            = text "tan"
prettyPrim (PrimAsin _)           = text "asin"
prettyPrim (PrimAcos _)           = text "acos"
prettyPrim (PrimAtan _)           = text "atan"
prettyPrim (PrimAsinh _)          = text "asinh"
prettyPrim (PrimAcosh _)          = text "acosh"
prettyPrim (PrimAtanh _)          = text "atanh"
prettyPrim (PrimExpFloating _)    = text "exp"
prettyPrim (PrimSqrt _)           = text "sqrt"
prettyPrim (PrimLog _)            = text "log"
prettyPrim (PrimFPow _)           = text "(**)"
prettyPrim (PrimLogBase _)        = text "logBase"
prettyPrim (PrimTruncate _ _)     = text "truncate"
prettyPrim (PrimRound _ _)        = text "round"
prettyPrim (PrimFloor _ _)        = text "floor"
prettyPrim (PrimCeiling _ _)      = text "ceiling"
prettyPrim (PrimAtan2 _)          = text "atan2"
prettyPrim (PrimLt _)             = text "(<*)"
prettyPrim (PrimGt _)             = text "(>*)"
prettyPrim (PrimLtEq _)           = text "(<=*)"
prettyPrim (PrimGtEq _)           = text "(>=*)"
prettyPrim (PrimEq _)             = text "(==*)"
prettyPrim (PrimNEq _)            = text "(/=*)"
prettyPrim (PrimMax _)            = text "max"
prettyPrim (PrimMin _)            = text "min"
prettyPrim PrimLAnd               = text "&&*"
prettyPrim PrimLOr                = text "||*"
prettyPrim PrimLNot               = text "not"
prettyPrim PrimOrd                = text "ord"
prettyPrim PrimChr                = text "chr"
prettyPrim PrimBoolToInt          = text "boolToInt"
prettyPrim (PrimFromIntegral _ _) = text "fromIntegral"

{-
-- Pretty print type
--
prettyAnyType :: ScalarType a -> Doc
prettyAnyType ty = text $ show ty
-}

prettyArray :: forall dim e. Array dim e -> Doc
prettyArray arr@(Array sh _)
  = parens $
      hang (text "Array") 2 $
        sep [showDoc (toElt sh :: dim), dataDoc]
  where
    showDoc :: forall a. Show a => a -> Doc
    showDoc = text . show
    l       = toList arr
    dataDoc | length l <= 1000 = showDoc l
            | otherwise        = showDoc (take 1000 l) <+>
                                 text "{truncated at 1000 elements}"


-- Auxiliary pretty printing combinators
--

noParens :: Doc -> Doc
noParens = id

-- Auxiliary ops
--

-- Auxiliary dictionary operations
--

{-
-- Show scalar values
--
runScalarShow :: ScalarType a -> (a -> String)
runScalarShow (NumScalarType (IntegralNumType ty))
  | IntegralDict <- integralDict ty = show
runScalarShow (NumScalarType (FloatingNumType ty))
  | FloatingDict <- floatingDict ty = show
runScalarShow (NonNumScalarType ty)
  | NonNumDict   <- nonNumDict ty   = show
-}
