{-# LANGUAGE GADTs, FlexibleInstances, TypeOperators, ScopedTypeVariables, RankNTypes #-}
-- |
-- Module      : Data.Array.Accelerate.Pretty.Print
-- Copyright   : [2008..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
--               [2009..2012] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Pretty.Print (

  -- * Pretty printing functions
  PrettyAcc,
  prettyPreAcc,  prettyAcc,
  prettyPreExp,  prettyExp,
  prettyPreAfun, prettyAfun,
  prettyPreFun,  prettyFun,
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

prettyPreAcc
    :: forall acc aenv a.
       PrettyAcc acc
    -> Int
    -> (Doc -> Doc)
    -> PreOpenAcc acc aenv a
    -> Doc
prettyPreAcc pp alvl wrap (Alet acc1 acc2)
  = wrap
  $ sep [ hang (text "let a" <> int alvl <+> char '=') 2 $
            pp alvl noParens acc1
        , text "in" <+> pp (alvl + 1) noParens acc2
        ]
prettyPreAcc _  alvl _    (Avar idx)
  = text $ 'a' : show (alvl - idxToInt idx - 1)
prettyPreAcc pp alvl wrap (Aprj ix arrs)
  = wrap $ char '#' <> prettyTupleIdx ix <+> pp alvl parens arrs
prettyPreAcc pp alvl _    (Atuple tup)
  = prettyAtuple pp alvl tup
prettyPreAcc pp alvl wrap (Apply afun acc)
  = wrap $ sep [parens (prettyPreAfun pp alvl afun), pp alvl parens acc]
prettyPreAcc pp alvl wrap (Acond e acc1 acc2)
  = wrap $ prettyArrOp "cond" [prettyPreExp pp 0 alvl parens e, pp alvl parens acc1, pp alvl parens acc2]
prettyPreAcc _  _    wrap (Use arr)
  = wrap $ prettyArrOp "use" [prettyArrays (arrays (undefined::a)) arr]
prettyPreAcc pp alvl wrap (Unit e)
  = wrap $ prettyArrOp "unit" [prettyPreExp pp 0 alvl parens e]
prettyPreAcc pp alvl wrap (Generate sh f)
  = wrap
  $ prettyArrOp "generate" [prettyPreExp pp 0 alvl parens sh, parens (prettyPreFun pp alvl f)]
prettyPreAcc pp alvl wrap (Transform sh ix f acc)
  = wrap
  $ prettyArrOp "transform" [ prettyPreExp pp 0 alvl parens sh
                            , parens (prettyPreFun pp alvl ix)
                            , parens (prettyPreFun pp alvl f)
                            , pp alvl parens acc ]
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

prettyPreFun :: PrettyAcc acc -> Int -> PreOpenFun acc env aenv fun -> Doc
prettyPreFun pp alvl fun = prettyPreOpenFun pp 0 alvl fun

prettyPreOpenFun :: forall acc env aenv fun. PrettyAcc acc -> Int -> Int -> PreOpenFun acc env aenv fun -> Doc
prettyPreOpenFun pp lvl alvl fun =
  let (n, bodyDoc) = count n fun
  in
  char '\\' <> hsep [text $ 'x' : show idx | idx <- [lvl..n]] <+>
  text "->" <+> bodyDoc
  where
     count :: Int -> PreOpenFun acc env' aenv' fun' -> (Int, Doc)
     count l (Body body) = (lvl-1, prettyPreExp pp (l + 1) alvl noParens body)
     count l (Lam  fun') = let (n, body) = count l fun' in (1 + n, body)

-- Pretty print an expression.
--
-- * Apply the wrapping combinator (3rd argument) to any compound expressions.
--
prettyExp :: Int -> Int -> (Doc -> Doc) -> OpenExp env aenv t -> Doc
prettyExp = prettyPreExp prettyAcc

prettyPreExp :: forall acc t env aenv.
                PrettyAcc acc -> Int -> Int -> (Doc -> Doc) -> PreOpenExp acc env aenv t -> Doc
prettyPreExp pp lvl alvl wrap (Let e1 e2)
  | not (isLet e1) && isLet e2
  = wrap $ sep [ text "let" <+> x <+> equals <+> e1' <+> text "in"
               , e2' ]
  --
  | otherwise
  = wrap $ sep [ hang (text "let" <+> x <+> equals) 2 e1'
               , text "in" <+> e2' ]
  where
    isLet (Let _ _)     = True
    isLet _             = False
    e1'                 = prettyPreExp pp lvl     alvl noParens e1
    e2'                 = prettyPreExp pp (lvl+1) alvl noParens e2
    x                   = char 'x' <> int lvl

prettyPreExp _pp lvl _ _ (Var idx)
  = text $ 'x' : show (lvl - idxToInt idx - 1)
prettyPreExp _pp _ _ _ (Const v)
  = text $ show (toElt v :: t)
prettyPreExp pp lvl alvl _ (Tuple tup)
  = prettyTuple pp lvl alvl tup
prettyPreExp pp lvl alvl wrap (Prj idx e)
  = wrap $ char '#' <> prettyTupleIdx idx <+> prettyPreExp pp lvl alvl parens e
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
prettyPreExp pp lvl alvl wrap (IndexSlice _ slix sh)
  = wrap $ text "indexSlice" <+> parens (prettyPreExp pp lvl alvl parens slix)
                             <+> prettyPreExp pp lvl alvl parens sh
prettyPreExp pp lvl alvl wrap (IndexFull _ slix sl)
  = wrap $ text "indexFull" <+> parens (prettyPreExp pp lvl alvl parens slix)
                            <+> prettyPreExp pp lvl alvl parens sl
prettyPreExp pp lvl alvl wrap (ToIndex sh ix)
  = wrap $ text "toIndex" <+> prettyPreExp pp lvl alvl parens sh
                          <+> prettyPreExp pp lvl alvl parens ix
prettyPreExp pp lvl alvl wrap (FromIndex sh ix)
  = wrap $ text "fromIndex" <+> prettyPreExp pp lvl alvl parens sh
                            <+> prettyPreExp pp lvl alvl parens ix
prettyPreExp pp lvl alvl wrap (Cond c t e)
  = wrap $ sep [prettyPreExp pp lvl alvl parens c <+> char '?',
                parens (prettyPreExp pp lvl alvl noParens t <> comma <+>
                        prettyPreExp pp lvl alvl noParens e)]
prettyPreExp pp lvl alvl wrap (Iterate i fun a)
  = wrap $ text "iterate" <>  brackets (int i)
                          <+> wrap   (prettyPreExp     pp lvl alvl parens a)
                          <+> parens (prettyPreOpenFun pp lvl alvl fun)

prettyPreExp _pp _ _ _ (PrimConst a)
 = prettyConst a
prettyPreExp pp lvl alvl wrap (PrimApp p a)
  | infixOp, Tuple (NilTup `SnocTup` x `SnocTup` y) <- a
  = wrap $ prettyPreExp pp lvl alvl parens x <+> f <+> prettyPreExp pp lvl alvl parens y

  | otherwise
  = wrap $ f' <+> prettyPreExp pp lvl alvl parens a
  where
    -- sometimes the infix function arguments are obstructed by. If so, add
    -- parentheses and print prefix.
    --
    (infixOp, f) = prettyPrim p
    f'           = if infixOp then parens f else f

prettyPreExp pp lvl alvl wrap (IndexScalar idx i)
  = wrap $ cat [pp alvl parens idx, char '!', prettyPreExp pp lvl alvl parens i]
prettyPreExp pp _lvl alvl wrap (Shape idx)
  = wrap $ text "shape" <+> pp alvl parens idx
prettyPreExp pp lvl alvl wrap (ShapeSize idx)
  = wrap $ text "shapeSize" <+> parens (prettyPreExp pp lvl alvl parens idx)
prettyPreExp pp lvl alvl wrap (Intersect sh1 sh2)
  = wrap $ text "intersect" <+> prettyPreExp pp lvl alvl wrap sh1
                            <+> prettyPreExp pp lvl alvl wrap sh2

-- Pretty print nested pairs as a proper tuple.
--
prettyAtuple :: forall acc aenv t.
                PrettyAcc acc
             -> Int
             -> Atuple (acc aenv) t
             -> Doc
prettyAtuple pp alvl = encloseSep lparen rparen comma . collect
  where
    collect :: Atuple (acc aenv) t' -> [Doc]
    collect NilAtup          = []
    collect (SnocAtup tup a) = collect tup ++ [pp alvl id a]

prettyTuple :: forall acc env aenv t.
               PrettyAcc acc -> Int -> Int -> Tuple (PreOpenExp acc env aenv) t -> Doc
prettyTuple pp lvl alvl = encloseSep lparen rparen comma . collect
  where
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

-- Pretty print a primitive operation. The first parameter indicates whether the
-- operator should be printed infix.
--
prettyPrim :: PrimFun a -> (Bool, Doc)
prettyPrim (PrimAdd _)            = (True,  char '+')
prettyPrim (PrimSub _)            = (True,  char '-')
prettyPrim (PrimMul _)            = (True,  char '*')
prettyPrim (PrimNeg _)            = (False, text "negate")
prettyPrim (PrimAbs _)            = (False, text "abs")
prettyPrim (PrimSig _)            = (False, text "signum")
prettyPrim (PrimQuot _)           = (False, text "quot")
prettyPrim (PrimRem _)            = (False, text "rem")
prettyPrim (PrimIDiv _)           = (False, text "div")
prettyPrim (PrimMod _)            = (False, text "mod")
prettyPrim (PrimBAnd _)           = (True,  text ".&.")
prettyPrim (PrimBOr _)            = (True,  text ".|.")
prettyPrim (PrimBXor _)           = (False, text "xor")
prettyPrim (PrimBNot _)           = (False, text "complement")
prettyPrim (PrimBShiftL _)        = (False, text "shiftL")
prettyPrim (PrimBShiftR _)        = (False, text "shiftR")
prettyPrim (PrimBRotateL _)       = (False, text "rotateL")
prettyPrim (PrimBRotateR _)       = (False, text "rotateR")
prettyPrim (PrimFDiv _)           = (True,  char '/')
prettyPrim (PrimRecip _)          = (False, text "recip")
prettyPrim (PrimSin _)            = (False, text "sin")
prettyPrim (PrimCos _)            = (False, text "cos")
prettyPrim (PrimTan _)            = (False, text "tan")
prettyPrim (PrimAsin _)           = (False, text "asin")
prettyPrim (PrimAcos _)           = (False, text "acos")
prettyPrim (PrimAtan _)           = (False, text "atan")
prettyPrim (PrimAsinh _)          = (False, text "asinh")
prettyPrim (PrimAcosh _)          = (False, text "acosh")
prettyPrim (PrimAtanh _)          = (False, text "atanh")
prettyPrim (PrimExpFloating _)    = (False, text "exp")
prettyPrim (PrimSqrt _)           = (False, text "sqrt")
prettyPrim (PrimLog _)            = (False, text "log")
prettyPrim (PrimFPow _)           = (True,  text "**")
prettyPrim (PrimLogBase _)        = (False, text "logBase")
prettyPrim (PrimTruncate _ _)     = (False, text "truncate")
prettyPrim (PrimRound _ _)        = (False, text "round")
prettyPrim (PrimFloor _ _)        = (False, text "floor")
prettyPrim (PrimCeiling _ _)      = (False, text "ceiling")
prettyPrim (PrimAtan2 _)          = (False, text "atan2")
prettyPrim (PrimLt _)             = (True,  text "<*")
prettyPrim (PrimGt _)             = (True,  text ">*")
prettyPrim (PrimLtEq _)           = (True,  text "<=*")
prettyPrim (PrimGtEq _)           = (True,  text ">=*")
prettyPrim (PrimEq _)             = (True,  text "==*")
prettyPrim (PrimNEq _)            = (True,  text "/=*")
prettyPrim (PrimMax _)            = (False, text "max")
prettyPrim (PrimMin _)            = (False, text "min")
prettyPrim PrimLAnd               = (True,  text "&&*")
prettyPrim PrimLOr                = (True,  text "||*")
prettyPrim PrimLNot               = (False, text "not")
prettyPrim PrimOrd                = (False, text "ord")
prettyPrim PrimChr                = (False, text "chr")
prettyPrim PrimBoolToInt          = (False, text "boolToInt")
prettyPrim (PrimFromIntegral _ _) = (False, text "fromIntegral")

{-
-- Pretty print type
--
prettyAnyType :: ScalarType a -> Doc
prettyAnyType ty = text $ show ty
-}

-- TLM: seems to flatten the nesting structure
--
prettyArrays :: ArraysR arrs -> arrs -> Doc
prettyArrays arrs = encloseSep lparen rparen comma . collect arrs
  where
    collect :: ArraysR arrs -> arrs -> [Doc]
    collect ArraysRunit         _        = []
    collect ArraysRarray        arr      = [prettyArray arr]
    collect (ArraysRpair r1 r2) (a1, a2) = collect r1 a1 ++ collect r2 a2

prettyArray :: forall dim e. Array dim e -> Doc
prettyArray arr@(Array sh _)
  = parens $
      hang (text "Array") 2 $
        sep [ parens . text $ showShape (toElt sh :: dim)
            , dataDoc]
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

encloseSep :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseSep left right p ds =
  case ds of
    []  -> left <> right
    [d] -> left <> d <> right
    _   -> left <> sep (punctuate p ds) <> right

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
