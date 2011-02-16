{-# LANGUAGE GADTs, FlexibleInstances, TypeOperators, ScopedTypeVariables #-}

-- |Embedded array processing language: pretty printing
--
--  Copyright (c) 2009 Manuel M T Chakravarty, Gabriele Keller, Sean Lee
--
--  License: BSD3
--
--- Description ---------------------------------------------------------------
--

module Data.Array.Accelerate.Pretty.Print (

  -- * Pretty printing functions
  prettyAcc, prettyExp, prettyFun, noParens

) where

-- standard libraries
import Text.PrettyPrint

-- friends
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Type

-- Pretty printing
-- ---------------

-- Pretty print an array expression
--
prettyAcc :: Int -> OpenAcc aenv a -> Doc
prettyAcc alvl (Let acc1 acc2)
  = sep [ hang (text "let a" <> int alvl <+> char '=') 2 $
            prettyAcc alvl acc1
        , text "in" <+> prettyAcc (alvl + 1) acc2
        ]
prettyAcc alvl (Let2 acc1 acc2)
  = sep [ hang (text "let (a" <> int alvl <> text ", a" <> int (alvl + 1) <> char ')' <+> char '=') 2
          $
            prettyAcc alvl acc1
        , text "in" <+> prettyAcc (alvl + 2) acc2
        ]
prettyAcc alvl (Avar idx)
  = text $ 'a' : show (alvl - idxToInt idx - 1)
prettyAcc _   (Use arr)
  = prettyArrOp "use" [prettyArray arr]
prettyAcc alvl (Unit e)
  = prettyArrOp "unit" [prettyExp 0 alvl parens e]
prettyAcc alvl (Generate sh f)
  = prettyArrOp "generate" [prettyExp 0 alvl parens sh, parens (prettyFun alvl f)]
prettyAcc alvl (Reshape sh acc)
  = prettyArrOp "reshape" [prettyExp 0 alvl parens sh, prettyAccParens alvl acc]
prettyAcc alvl (Replicate _ty ix acc)
  = prettyArrOp "replicate" [prettyExp 0 alvl id ix, prettyAccParens alvl acc]
prettyAcc alvl (Index _ty acc ix)
  = sep [prettyAccParens alvl acc, char '!', prettyExp 0 alvl id ix]
prettyAcc alvl (Map f acc)
  = prettyArrOp "map" [parens (prettyFun alvl f), prettyAccParens alvl acc]
prettyAcc alvl (ZipWith f acc1 acc2)
  = prettyArrOp "zipWith"
      [parens (prettyFun alvl f), prettyAccParens alvl acc1,
       prettyAccParens alvl acc2]
prettyAcc alvl (Fold f e acc)
  = prettyArrOp "fold" [parens (prettyFun alvl f), prettyExp 0 alvl parens e,
                        prettyAccParens alvl acc]
prettyAcc alvl (Fold1 f acc)
  = prettyArrOp "fold1" [parens (prettyFun alvl f), prettyAccParens alvl acc]
prettyAcc alvl (FoldSeg f e acc1 acc2)
  = prettyArrOp "foldSeg" [parens (prettyFun alvl f), prettyExp 0 alvl parens e,
                           prettyAccParens alvl acc1, prettyAccParens alvl acc2]
prettyAcc alvl (Fold1Seg f acc1 acc2)
  = prettyArrOp "fold1Seg" [parens (prettyFun alvl f),
                            prettyAccParens alvl acc1, prettyAccParens alvl acc2]
prettyAcc alvl (Scanl f e acc)
  = prettyArrOp "scanl" [parens (prettyFun alvl f), prettyExp 0 alvl parens e,
                         prettyAccParens alvl acc]
prettyAcc alvl (Scanl' f e acc)
  = prettyArrOp "scanl'" [parens (prettyFun alvl f), prettyExp 0 alvl parens e,
                          prettyAccParens alvl acc]
prettyAcc alvl (Scanl1 f acc)
  = prettyArrOp "scanl1" [parens (prettyFun alvl f), prettyAccParens alvl acc]
prettyAcc alvl (Scanr f e acc)
  = prettyArrOp "scanr" [parens (prettyFun alvl f), prettyExp 0 alvl parens e,
                         prettyAccParens alvl acc]
prettyAcc alvl (Scanr' f e acc)
  = prettyArrOp "scanr'" [parens (prettyFun alvl f), prettyExp 0 alvl parens e,
                          prettyAccParens alvl acc]
prettyAcc alvl (Scanr1 f acc)
  = prettyArrOp "scanr1" [parens (prettyFun alvl f), prettyAccParens alvl acc]
prettyAcc alvl (Permute f dfts p acc)
  = prettyArrOp "permute" [parens (prettyFun alvl f), prettyAccParens alvl dfts,
                           parens (prettyFun alvl p), prettyAccParens alvl acc]
prettyAcc alvl (Backpermute sh p acc)
  = prettyArrOp "backpermute" [prettyExp 0 alvl parens sh,
                               parens (prettyFun alvl p),
                               prettyAccParens alvl acc]
prettyAcc alvl (Stencil sten bndy acc)
  = prettyArrOp "stencil" [parens (prettyFun alvl sten),
                           prettyBoundary acc bndy,
                           prettyAccParens alvl acc]
prettyAcc alvl (Stencil2 sten bndy1 acc1 bndy2 acc2)
  = prettyArrOp "stencil2" [parens (prettyFun alvl sten),
                            prettyBoundary acc1 bndy1,
                            prettyAccParens alvl acc1,
                            prettyBoundary acc2 bndy2,
                            prettyAccParens alvl acc2]

prettyBoundary :: forall aenv dim e. Elt e
               => {-dummy-}OpenAcc aenv (Array dim e) -> Boundary (EltRepr e) -> Doc
prettyBoundary _ Clamp        = text "Clamp"
prettyBoundary _ Mirror       = text "Mirror"
prettyBoundary _ Wrap         = text "Wrap"
prettyBoundary _ (Constant e) = parens $ text "Constant" <+> text (show (toElt e :: e))

prettyArrOp :: String -> [Doc] -> Doc
prettyArrOp name docs = hang (text name) 2 $ sep docs

-- Wrap into parenthesis
--
prettyAccParens :: Int -> OpenAcc aenv a -> Doc
prettyAccParens lvl acc@(Avar _) = prettyAcc lvl acc
prettyAccParens lvl acc          = parens (prettyAcc lvl acc)

-- Pretty print a function over scalar expressions.
--
prettyFun :: Int -> OpenFun env aenv fun -> Doc
prettyFun alvl fun =
  let (n, bodyDoc) = count n fun
  in
  char '\\' <> hsep [text $ 'x' : show idx | idx <- [0..n]] <+>
  text "->" <+> bodyDoc
  where
     count :: Int -> OpenFun env aenv fun -> (Int, Doc)
     count lvl (Body body) = (-1, prettyExp lvl alvl noParens body)
     count lvl (Lam fun)   = let (n, body) = count lvl fun in (1 + n, body)

-- Pretty print an expression.
--
-- * Apply the wrapping combinator (1st argument) to any compound expressions.
--
prettyExp :: forall t env aenv.
             Int -> Int -> (Doc -> Doc) -> OpenExp env aenv t -> Doc
prettyExp lvl _ _ (Var idx)
  = text $ 'x' : show (lvl - idxToInt idx)
prettyExp _ _ _ (Const v)
  = text $ show (toElt v :: t)
prettyExp lvl alvl _ (Tuple tup)
  = prettyTuple lvl alvl tup
prettyExp lvl alvl wrap (Prj idx e)
  = wrap $ prettyTupleIdx idx <+> prettyExp lvl alvl parens e
prettyExp _lvl _alvl wrap IndexNil
  = wrap $ text "index Z"
prettyExp lvl alvl wrap (IndexCons t h)
  = wrap $
      text "index" <+>
      parens (prettyExp lvl alvl parens t <+> text ":." <+> prettyExp lvl alvl parens h)
prettyExp lvl alvl wrap (IndexHead ix)
  = wrap $ text "indexHead" <+> prettyExp lvl alvl parens ix
prettyExp lvl alvl wrap (IndexTail ix)
  = wrap $ text "indexTail" <+> prettyExp lvl alvl parens ix
prettyExp lvl alvl wrap (Cond c t e)
  = wrap $ sep [prettyExp lvl alvl parens c <+> char '?',
                parens (prettyExp lvl alvl noParens t <> comma <+>
                        prettyExp lvl alvl noParens e)]
prettyExp _ _ _ (PrimConst a)
 = prettyConst a
prettyExp lvl alvl wrap (PrimApp p a)
  = wrap $ prettyPrim p <+> prettyExp lvl alvl parens a
prettyExp lvl alvl wrap (IndexScalar idx i)
  = wrap $ cat [prettyAccParens alvl idx, char '!', prettyExp lvl alvl parens i]
prettyExp _lvl alvl wrap (Shape idx)
  = wrap $ text "shape" <+> prettyAccParens alvl idx
prettyExp _lvl alvl wrap (Size idx)
  = wrap $ text "size" <+> prettyAccParens alvl idx

-- Pretty print nested pairs as a proper tuple.
--
prettyTuple :: Int -> Int -> Tuple (OpenExp env aenv) t -> Doc
prettyTuple lvl alvl e = parens $ sep (map (<> comma) (init es) ++ [last es])
  where
    es = collect e
    --
    collect :: Tuple (OpenExp env aenv) t -> [Doc]
    collect NilTup          = []
    collect (SnocTup tup e) = collect tup ++ [prettyExp lvl alvl noParens e]

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
prettyPrim (PrimAdd _)         = text "(+)"
prettyPrim (PrimSub _)         = text "(-)"
prettyPrim (PrimMul _)         = text "(*)"
prettyPrim (PrimNeg _)         = text "negate"
prettyPrim (PrimAbs _)         = text "abs"
prettyPrim (PrimSig _)         = text "signum"
prettyPrim (PrimQuot _)        = text "quot"
prettyPrim (PrimRem _)         = text "rem"
prettyPrim (PrimIDiv _)        = text "div"
prettyPrim (PrimMod _)         = text "mod"
prettyPrim (PrimBAnd _)        = text "(.&.)"
prettyPrim (PrimBOr _)         = text "(.|.)"
prettyPrim (PrimBXor _)        = text "xor"
prettyPrim (PrimBNot _)        = text "complement"
prettyPrim (PrimBShiftL _)     = text "shiftL"
prettyPrim (PrimBShiftR _)     = text "shiftR"
prettyPrim (PrimBRotateL _)    = text "rotateL"
prettyPrim (PrimBRotateR _)    = text "rotateR"
prettyPrim (PrimFDiv _)        = text "(/)"
prettyPrim (PrimRecip _)       = text "recip"
prettyPrim (PrimSin _)         = text "sin"
prettyPrim (PrimCos _)         = text "cos"
prettyPrim (PrimTan _)         = text "tan"
prettyPrim (PrimAsin _)        = text "asin"
prettyPrim (PrimAcos _)        = text "acos"
prettyPrim (PrimAtan _)        = text "atan"
prettyPrim (PrimAsinh _)       = text "asinh"
prettyPrim (PrimAcosh _)       = text "acosh"
prettyPrim (PrimAtanh _)       = text "atanh"
prettyPrim (PrimExpFloating _) = text "exp"
prettyPrim (PrimSqrt _)        = text "sqrt"
prettyPrim (PrimLog _)         = text "log"
prettyPrim (PrimFPow _)        = text "(**)"
prettyPrim (PrimLogBase _)     = text "logBase"
prettyPrim (PrimAtan2 _)       = text "atan2"
prettyPrim (PrimLt _)          = text "(<*)"
prettyPrim (PrimGt _)          = text "(>*)"
prettyPrim (PrimLtEq _)        = text "(<=*)"
prettyPrim (PrimGtEq _)        = text "(>=*)"
prettyPrim (PrimEq _)          = text "(==*)"
prettyPrim (PrimNEq _)         = text "(/=*)"
prettyPrim (PrimMax _)         = text "max"
prettyPrim (PrimMin _)         = text "min"
prettyPrim PrimLAnd            = text "&&*"
prettyPrim PrimLOr             = text "||*"
prettyPrim PrimLNot            = text "not"
prettyPrim PrimOrd             = text "ord"
prettyPrim PrimChr             = text "chr"
prettyPrim PrimRoundFloatInt   = text "round"
prettyPrim PrimTruncFloatInt   = text "trunc"
prettyPrim PrimIntFloat        = text "intFloat"
prettyPrim PrimBoolToInt       = text "boolToInt"

{-
-- Pretty print type
--
prettyAnyType :: ScalarType a -> Doc
prettyAnyType ty = text $ show ty
-}

prettyArray :: forall dim a. Array dim a -> Doc
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


-- Auxilliary pretty printing combinators
--

noParens :: Doc -> Doc
noParens = id

-- Auxilliary ops
--

-- Convert a typed de Brujin index to the corresponding integer
--
idxToInt :: Idx env t -> Int
idxToInt ZeroIdx       = 0
idxToInt (SuccIdx idx) = 1 + idxToInt idx

-- Auxilliary dictionary operations
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
