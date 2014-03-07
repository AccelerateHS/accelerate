{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
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
  prettyPreAcc,  prettyOpenAcc,
  prettyPreExp,  prettyExp,
  prettyPreAfun, prettyAfun,
  prettyPreFun,  prettyFun,
  prettyPrim,
  noParens

) where

-- standard libraries
import Prelude                                  hiding ( exp )
import Data.List
import Text.PrettyPrint

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
prettyOpenAcc :: PrettyAcc OpenAcc
prettyOpenAcc alvl wrap (OpenAcc acc) = prettyPreAcc prettyOpenAcc alvl wrap acc

prettyPreAcc
    :: forall acc aenv arrs.
       PrettyAcc acc
    -> Int                                      -- level of array variables
    -> (Doc -> Doc)                             -- apply to compound expressions
    -> PreOpenAcc acc aenv arrs
    -> Doc
prettyPreAcc prettyAcc alvl wrap = pp
  where
    ppE :: PreOpenExp acc env aenv e -> Doc
    ppE = prettyPreExp prettyAcc 0 alvl parens

    ppF :: PreOpenFun acc env aenv f -> Doc
    ppF = parens . prettyPreFun prettyAcc alvl

    ppA :: acc aenv a -> Doc
    ppA = prettyAcc alvl parens

    ppAF :: PreOpenAfun acc aenv f -> Doc
    ppAF = parens . prettyPreAfun prettyAcc alvl

    ppB :: forall sh e. Elt e
        => {-dummy-} acc aenv (Array sh e)
        -> Boundary (EltRepr e)
        -> Doc
    ppB _ Clamp        = text "Clamp"
    ppB _ Mirror       = text "Mirror"
    ppB _ Wrap         = text "Wrap"
    ppB _ (Constant e) = parens $ text "Constant" <+> text (show (toElt e :: e))

    -- pretty print a named array operation with its arguments
    name .$ docs = wrap $ hang (text name) 2 (sep docs)

    -- The main pretty-printer
    -- -----------------------
    --
    pp :: PreOpenAcc acc aenv arrs -> Doc
    pp (Alet acc1 acc2)
      | not (isAlet acc1') && isAlet acc2'
      = wrap $ vcat [ text "let" <+> a <+> equals <+> acc1' <+> text "in", acc2' ]
      | otherwise
      = wrap $ vcat [ hang (text "let" <+> a <+> equals) 2 acc1', text "in" <+> acc2' ]
      where
        -- TLM: derp, can't unwrap into a PreOpenAcc to pattern match on Alet
        isAlet doc  = "let" `isPrefixOf` render doc
        acc1'       = prettyAcc alvl     noParens acc1
        acc2'       = prettyAcc (alvl+1) noParens acc2
        a           = char 'a' <> int alvl

    pp (Awhile p afun acc)      = "awhile" .$ [ppAF p, ppAF afun, ppA acc]
    pp (Atuple tup)             = prettyAtuple prettyAcc alvl tup
    pp (Avar idx)               = text $ 'a' : show (alvl - idxToInt idx - 1)
    pp (Aprj ix arrs)           = wrap $ char '#' <> prettyTupleIdx ix <+> ppA arrs
    pp (Apply afun acc)         = wrap $ sep [ ppAF afun, ppA acc ]
    pp (Acond e acc1 acc2)      = wrap $ sep [ ppE e, text "?|", tuple [ppA acc1, ppA acc2] ]
    pp (Slice _ty acc ix)       = wrap $ sep [ ppA acc, char '!', prettyPreExp prettyAcc 0 alvl noParens ix ]
    pp (Use arrs)               = "use"         .$ [ prettyArrays (arrays (undefined :: arrs)) arrs ]
    pp (Unit e)                 = "unit"        .$ [ ppE e ]
    pp (Generate sh f)          = "generate"    .$ [ ppE sh, ppF f ]
    pp (Transform sh ix f acc)  = "transform"   .$ [ ppE sh, ppF ix, ppF f, ppA acc ]
    pp (Reshape sh acc)         = "reshape"     .$ [ ppE sh, ppA acc ]
    pp (Replicate _ty ix acc)   = "replicate"   .$ [ prettyPreExp prettyAcc 0 alvl noParens ix, ppA acc ]
    pp (Map f acc)              = "map"         .$ [ ppF f, ppA acc ]
    pp (ZipWith f acc1 acc2)    = "zipWith"     .$ [ ppF f, ppA acc1, ppA acc2 ]
    pp (Fold f e acc)           = "fold"        .$ [ ppF f, ppE e, ppA acc ]
    pp (Fold1 f acc)            = "fold1"       .$ [ ppF f, ppA acc ]
    pp (FoldSeg f e acc1 acc2)  = "foldSeg"     .$ [ ppF f, ppE e, ppA acc1, ppA acc2 ]
    pp (Fold1Seg f acc1 acc2)   = "fold1Seg"    .$ [ ppF f, ppA acc1, ppA acc2 ]
    pp (Scanl f e acc)          = "scanl"       .$ [ ppF f, ppE e, ppA acc ]
    pp (Scanl' f e acc)         = "scanl'"      .$ [ ppF f, ppE e, ppA acc ]
    pp (Scanl1 f acc)           = "scanl1"      .$ [ ppF f, ppA acc ]
    pp (Scanr f e acc)          = "scanr"       .$ [ ppF f, ppE e, ppA acc ]
    pp (Scanr' f e acc)         = "scanr'"      .$ [ ppF f, ppE e, ppA acc ]
    pp (Scanr1 f acc)           = "scanr1"      .$ [ ppF f, ppA acc ]
    pp (Permute f dfts p acc)   = "permute"     .$ [ ppF f, ppA dfts, ppF p, ppA acc ]
    pp (Backpermute sh p acc)   = "backpermute" .$ [ ppE sh, ppF p, ppA acc ]
    pp (Aforeign ff _afun acc)  = "aforeign"    .$ [ text (strForeign ff), {- ppAf afun, -} ppA acc ]
    pp (Stencil sten bndy acc)  = "stencil"     .$ [ ppF sten, ppB acc bndy, ppA acc ]
    pp (Stencil2 sten bndy1 acc1 bndy2 acc2)
                                = "stencil2"    .$ [ ppF sten, ppB acc1 bndy1, ppA acc1,
                                                               ppB acc2 bndy2, ppA acc2 ]


-- Pretty print a function over array computations.
--
prettyAfun :: Int -> OpenAfun aenv t -> Doc
prettyAfun = prettyPreAfun prettyOpenAcc

prettyPreAfun :: forall acc aenv fun. PrettyAcc acc -> Int -> PreOpenAfun acc aenv fun -> Doc
prettyPreAfun pp alvl fun =
  let (n, bodyDoc) = count n fun
  in
  char '\\' <> hsep [text $ 'a' : show idx | idx <- [alvl..alvl + n]] <+>
  text "->" <+> bodyDoc
  where
     count :: Int -> PreOpenAfun acc aenv' fun' -> (Int, Doc)
     count lvl (Abody body) = (-1, pp (lvl + alvl + 1) noParens body)
     count lvl (Alam  fun') = let (n, body) = count lvl fun' in (1 + n, body)

-- Pretty print a function over scalar expressions.
--
prettyFun :: Int -> OpenFun env aenv fun -> Doc
prettyFun = prettyPreFun prettyOpenAcc

prettyPreFun :: PrettyAcc acc -> Int -> PreOpenFun acc env aenv fun -> Doc
prettyPreFun pp = prettyPreOpenFun pp 0

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
prettyExp = prettyPreExp prettyOpenAcc

prettyPreExp
    :: forall acc t env aenv.
       PrettyAcc acc
    -> Int                                      -- level of scalar variables
    -> Int                                      -- level of array variables
    -> (Doc -> Doc)                             -- apply to compound expressions
    -> PreOpenExp acc env aenv t
    -> Doc
prettyPreExp prettyAcc lvl alvl wrap = pp
  where
    ppE, ppE' :: PreOpenExp acc env aenv e -> Doc
    ppE  = prettyPreExp prettyAcc lvl alvl parens
    ppE' = prettyPreExp prettyAcc lvl alvl noParens

    ppF :: PreOpenFun acc env aenv f -> Doc
    ppF = parens . prettyPreOpenFun prettyAcc lvl alvl

    ppA :: acc aenv a -> Doc
    ppA = prettyAcc alvl parens

    -- pretty print a named array operation with its arguments
    name .$ docs = wrap $ text name <+> sep docs

    -- The main pretty-printer
    -- -----------------------
    --
    pp :: PreOpenExp acc env aenv t -> Doc
    pp (Let e1 e2)
      | not (isLet e1) && isLet e2
      = wrap $ vcat [ text "let" <+> x <+> equals <+> e1' <+> text "in", e2' ]
      | otherwise
      = wrap $ vcat [ hang (text "let" <+> x <+> equals) 2 e1', text "in" <+> e2' ]
      where
        isLet (Let _ _)     = True
        isLet _             = False
        e1'                 = prettyPreExp prettyAcc lvl     alvl noParens e1
        e2'                 = prettyPreExp prettyAcc (lvl+1) alvl noParens e2
        x                   = char 'x' <> int lvl

    pp (PrimApp p a)
      | infixOp, Tuple (NilTup `SnocTup` x `SnocTup` y) <- a
      = wrap $ ppE x <+> f <+> ppE y
      | otherwise
      = wrap $ f' <+> ppE a
      where
        -- sometimes the infix function arguments are obstructed. If so, add
        -- parentheses and print prefix.
        --
        (infixOp, f) = prettyPrim p
        f'           = if infixOp then parens f else f

    pp (PrimConst a)            = prettyConst a
    pp (Tuple tup)              = prettyTuple prettyAcc lvl alvl tup
    pp (Var idx)                = text $ 'x' : show (lvl - idxToInt idx - 1)
    pp (Const v)                = text $ show (toElt v :: t)
    pp (Prj idx e)              = wrap $ char '#' <> prettyTupleIdx idx <+> ppE e
    pp (Cond c t e)             = wrap $ sep [ ppE c, char '?' , tuple [ ppE' t, ppE' e ]]
    pp IndexNil                 = char 'Z'
    pp (IndexAny)               = text "indexAny"
    pp (IndexCons t h)          = wrap $ ppE' t <+> text ":." <+> ppE' h
    pp (IndexHead ix)           = "indexHead"  .$ [ ppE ix ]
    pp (IndexTail ix)           = "indexTail"  .$ [ ppE ix ]
    pp (IndexSlice _ slix sh)   = "indexSlice" .$ [ ppE slix, ppE sh ]
    pp (IndexFull _ slix sl)    = "indexFull"  .$ [ ppE slix, ppE sl ]
    pp (ToIndex sh ix)          = "toIndex"    .$ [ ppE sh, ppE ix ]
    pp (FromIndex sh ix)        = "fromIndex"  .$ [ ppE sh, ppE ix ]
    pp (While p f x)            = "while"      .$ [ ppF p, ppF f, ppE x ]
    pp (Foreign ff _f e)        = "foreign"    .$ [ text (strForeign ff), {- ppF f, -} ppE e ]
    pp (Shape idx)              = "shape"      .$ [ ppA idx ]
    pp (ShapeSize idx)          = "shapeSize"  .$ [ parens (ppE idx) ]
    pp (Intersect sh1 sh2)      = "intersect"  .$ [ ppE sh1, ppE sh2 ]
    pp (Index idx i)            = wrap $ cat [ ppA idx, char '!',  ppE i ]
    pp (LinearIndex idx i)      = wrap $ cat [ ppA idx, text "!!", ppE i ]


-- Pretty print nested pairs as a proper tuple.
--
prettyAtuple :: forall acc aenv t.
                PrettyAcc acc
             -> Int
             -> Atuple (acc aenv) t
             -> Doc
prettyAtuple pp alvl = tuple . collect
  where
    collect :: Atuple (acc aenv) t' -> [Doc]
    collect NilAtup          = []
    collect (SnocAtup tup a) = collect tup ++ [pp alvl noParens a]

prettyTuple :: forall acc env aenv t.
               PrettyAcc acc -> Int -> Int -> Tuple (PreOpenExp acc env aenv) t -> Doc
prettyTuple pp lvl alvl = tuple . collect
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
prettyArrays arrs = tuple . collect arrs
  where
    collect :: ArraysR arrs -> arrs -> [Doc]
    collect ArraysRunit         _        = []
    collect ArraysRarray        arr      = [prettyArray arr]
    collect (ArraysRpair r1 r2) (a1, a2) = collect r1 a1 ++ collect r2 a2

prettyArray :: forall dim e. Array dim e -> Doc
prettyArray arr@(Array sh _)
  = hang (text "Array") 2
  $ sep [ parens . text $ showShape (toElt sh :: dim)
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

tuple :: [Doc] -> Doc
tuple = encloseSep lparen rparen comma

encloseSep :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseSep left right p ds =
  case ds of
    []  -> left <> right
    [d] -> left <> d <> right
    _   -> cat (zipWith (<>) (left : repeat p) ds) <> right


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

