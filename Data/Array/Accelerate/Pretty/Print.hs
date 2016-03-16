{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      : Data.Array.Accelerate.Pretty.Print
-- Copyright   : [2008..2014] Manuel M T Chakravarty, Gabriele Keller
--               [2008..2009] Sean Lee
--               [2009..2014] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Pretty.Print (

  -- * Pretty printing
  -- ** 'OpenAcc'
  --
  prettyOpenAcc,
  prettyOpenAfun,
  prettyOpenExp,
  prettyOpenFun,

  -- ** 'PreOpenAcc'
  PrettyAcc,
  prettyPreOpenAcc,
  prettyPreOpenAfun,
  prettySeq,
  prettyPreExp, prettyPreOpenExp,
  prettyPreFun, prettyPreOpenFun,
  prettyPrim,
  prettyArrays,
  prettyTupleIdx,

  -- ** Utilities
  Val(..), PrettyEnv(..), prj, sizeEnv,
  noParens,
  tuple,

) where

-- standard libraries
import Prelude                                          hiding ( exp, seq )
import Data.List
import Text.PrettyPrint

-- friends
import Data.Array.Accelerate.Array.Representation       ( SliceIndex(..) )
import Data.Array.Accelerate.Array.Sugar                hiding ( tuple )
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.AST                        hiding ( Val(..), prj )
import Data.Array.Accelerate.Type


-- Pretty printing
-- ===============

-- Pretty printing for the knot-tied 'OpenAcc'
-- -------------------------------------------

-- Pretty print an array expression
--
prettyOpenAcc :: PrettyAcc OpenAcc
prettyOpenAcc wrap aenv (OpenAcc acc) = prettyPreOpenAcc prettyOpenAcc wrap aenv acc

prettyOpenAfun :: Val aenv -> OpenAfun aenv t -> Doc
prettyOpenAfun = prettyPreOpenAfun prettyOpenAcc


-- Pretty print scalar expressions
--
prettyOpenFun :: Val env -> Val aenv -> OpenFun env aenv fun -> Doc
prettyOpenFun = prettyPreOpenFun prettyOpenAcc

prettyOpenExp :: (Doc -> Doc) -> Val env -> Val aenv -> OpenExp env aenv t -> Doc
prettyOpenExp = prettyPreOpenExp prettyOpenAcc


-- Pretty printing for open 'PreOpenAcc'
-- -------------------------------------

-- The type of pretty printing functions for array computations.
--
type PrettyAcc acc = forall aenv t.
       (Doc -> Doc)
    -> Val aenv
    -> acc aenv t
    -> Doc

prettyPreOpenAcc
    :: forall acc aenv arrs.
       PrettyAcc acc
    -> (Doc -> Doc)                             -- apply to compound expressions
    -> Val aenv                                 -- environment of array variables
    -> PreOpenAcc acc aenv arrs
    -> Doc
prettyPreOpenAcc prettyAcc wrap aenv = pp
  where
    ppE :: PreExp acc aenv e -> Doc
    ppE = prettyPreExp prettyAcc parens aenv

    ppSh :: PreExp acc aenv sh -> Doc
    ppSh = parens . prettyPreExp prettyAcc noParens aenv

    ppF :: PreFun acc aenv f -> Doc
    ppF = parens . prettyPreFun prettyAcc aenv

    ppA :: acc aenv a -> Doc
    ppA = prettyAcc parens aenv

    ppAF :: PreOpenAfun acc aenv f -> Doc
    ppAF = parens . prettyPreOpenAfun prettyAcc aenv

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
        acc1'       = prettyAcc noParens aenv            acc1
        acc2'       = prettyAcc noParens (aenv `Push` a) acc2
        a           = char 'a' <> int (sizeEnv aenv)

    pp (Awhile p afun acc)      = "awhile" .$ [ppAF p, ppAF afun, ppA acc]
    pp (Atuple tup)             = prettyAtuple prettyAcc aenv tup
    pp (Avar idx)               = prj idx aenv
    pp (Aprj ix arrs)           = wrap $ char '#' <> prettyTupleIdx ix <+> ppA arrs
    pp (Apply afun acc)         = wrap $ sep [ ppAF afun, ppA acc ]
    pp (Acond e acc1 acc2)      = wrap $ sep [ ppE e, text "?|", tuple [ppA acc1, ppA acc2] ]
    pp (Slice _ty acc ix)       = wrap $ sep [ ppA acc, char '!', prettyPreExp prettyAcc noParens aenv ix ]
    pp (Use arrs)               = "use"         .$ [ prettyArrays (arrays (undefined :: arrs)) arrs ]
    pp (Subarray ix sh arrs)    = "subarray"    .$ [ ppE ix, ppE sh, prettyArrays (arrays (undefined :: arrs)) arrs ]
    pp (Unit e)                 = "unit"        .$ [ ppE e ]
    pp (Generate sh f)          = "generate"    .$ [ ppSh sh, ppF f ]
    pp (Transform sh ix f acc)  = "transform"   .$ [ ppSh sh, ppF ix, ppF f, ppA acc ]
    pp (Reshape sh acc)         = "reshape"     .$ [ ppSh sh, ppA acc ]
    pp (Replicate _ty ix acc)   = "replicate"   .$ [ ppSh ix, ppA acc ]
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
    pp (Backpermute sh p acc)   = "backpermute" .$ [ ppSh sh, ppF p, ppA acc ]
    pp (Aforeign ff _afun acc)  = "aforeign"    .$ [ text (strForeign ff), {- ppAf afun, -} ppA acc ]
    pp (Stencil sten bndy acc)  = "stencil"     .$ [ ppF sten, ppB acc bndy, ppA acc ]
    pp (Stencil2 sten bndy1 acc1 bndy2 acc2)
                                = "stencil2"    .$ [ ppF sten, ppB acc1 bndy1, ppA acc1,
                                                               ppB acc2 bndy2, ppA acc2 ]

    pp (Collect s cs)           = wrap $ hang (text "collect") 2
                                       $ encloseSep lbrace rbrace semi
                                       $ maybe (prettySeq prettyAcc wrap aenv s)
                                               (prettySeq prettyAcc wrap aenv)
                                               cs

prettySeq
    :: forall idx acc aenv arrs.
       PrettyAcc acc
    -> (Doc -> Doc)                             -- apply to compound expressions
    -> Val aenv
    -> PreOpenSeq idx acc aenv arrs
    -> [Doc]
prettySeq prettyAcc wrap aenv seq =
  case seq of
    Producer p s' ->
      prettyP p : prettySeq prettyAcc wrap (aenv `Push` avar (sizeEnv aenv)) s'
    Consumer c    ->
      [prettyC c]
    Reify ix      -> ["reify" ..$ [ppA ix]]
  where
    avar n         = char 'a' <> int n
    name .$  docs = wrap $ hang (avar (sizeEnv aenv) <+> text ":=" <+> text name) 2 (sep docs)
    name ..$ docs = wrap $ hang (text name) 2 (sep docs)

    ppE :: PreExp acc aenv e -> Doc
    ppE = prettyPreExp prettyAcc parens aenv

    ppA :: acc aenv a -> Doc
    ppA = prettyAcc parens aenv

    ppAF :: PreOpenAfun acc aenv f -> Doc
    ppAF = parens . prettyPreOpenAfun prettyAcc aenv

    ppL :: Maybe (PreExp acc aenv e) -> Doc
    ppL Nothing  = text "forever"
    ppL (Just l) = ppE l

    prettyP :: forall a. Producer idx acc aenv a -> Doc
    prettyP p =
      case p of
        Pull _               -> "pull"         .$ [ text "[..]" ]
        Subarrays sh _       -> "subarrays"    .$ [ ppE sh, text "[..]" ]
        Produce l f          -> "produce"      .$ [ ppL l, ppAF f ]
        MapAccumFlat f a x   -> "mapAccumFlat" .$ [ ppAF f, ppA a, ppA x ]
        ProduceAccum l f a   -> "produceAccum" .$ [ ppL l, ppAF f, ppA a ]

    prettyC :: forall a. Consumer idx acc aenv a -> Doc
    prettyC c =
      case c of
        FoldSeqFlatten f a x -> "foldSeqFlatten" ..$ [ ppAF f, ppA a, ppA x ]
        Iterate l f a        -> "iterate"        ..$ [ ppL l, ppAF f, ppA a ]
        Conclude a d         -> "conclude"       ..$ [ ppA a, ppA d]
        Stuple t             -> tuple (prettyT t)

    prettyT :: forall t. Atuple (PreOpenSeq idx acc aenv) t -> [Doc]
    prettyT NilAtup        = []
    prettyT (SnocAtup t c) = prettyT t ++ prettySeq prettyAcc wrap aenv c


-- Pretty print a function over array computations.
--
prettyPreOpenAfun
    :: forall acc aenv f.
       PrettyAcc acc
    -> Val aenv
    -> PreOpenAfun acc aenv f
    -> Doc
prettyPreOpenAfun pp aenv afun = char '\\' <> next aenv afun
  where
    next :: Val aenv' -> PreOpenAfun acc aenv' f' -> Doc
    next aenv' (Abody body) = text "->" <+> pp noParens aenv' body
    next aenv' (Alam afun') =
      let a = char 'a' <> int (sizeEnv aenv')
      in  a <+> next (aenv' `Push` a) afun'


-- Pretty print a scalar function.
--
prettyPreFun :: PrettyAcc acc -> Val aenv -> PreFun acc aenv fun -> Doc
prettyPreFun pp = prettyPreOpenFun pp Empty

prettyPreOpenFun
    :: forall acc env aenv f.
       PrettyAcc acc
    -> Val env                                  -- environment of scalar variables
    -> Val aenv                                 -- environment of array variables
    -> PreOpenFun acc env aenv f
    -> Doc
prettyPreOpenFun pp env aenv fun = char '\\' <> next env fun
  where
    next :: Val env' -> PreOpenFun acc env' aenv f' -> Doc
    next env' (Body body) = text "->" <+> prettyPreOpenExp pp noParens env' aenv body
    next env' (Lam fun')  =
      let x = char 'x' <> int (sizeEnv env')
      in  x <+> next (env' `Push` x) fun'


-- Pretty print a scalar expression.
--
prettyPreExp :: PrettyAcc acc -> (Doc -> Doc) -> Val aenv -> PreExp acc aenv t -> Doc
prettyPreExp pp wrap = prettyPreOpenExp pp wrap Empty

prettyPreOpenExp
    :: forall acc t env aenv.
       PrettyAcc acc
    -> (Doc -> Doc)                             -- apply to compound expressions
    -> Val env                                  -- environment of scalar variables
    -> Val aenv                                 -- environment of array variables
    -> PreOpenExp acc env aenv t
    -> Doc
prettyPreOpenExp prettyAcc wrap env aenv = pp
  where
    ppE, ppE' :: PreOpenExp acc env aenv e -> Doc
    ppE  = prettyPreOpenExp prettyAcc parens env aenv
    ppE' = prettyPreOpenExp prettyAcc noParens env aenv

    ppSh :: Shape sh => PreOpenExp acc env aenv sh -> Doc
    ppSh = parens . ppE'

    ppSl :: Elt sl => PreOpenExp acc env aenv sl -> Doc
    ppSl = parens . ppE'

    ppSI :: SliceIndex slix sl co sh -> Doc
    ppSI SliceNil        = char 'Z'
    ppSI (SliceAll sl)   = ppSI sl <+> text ":. All"
    ppSI (SliceFixed sl) = ppSI sl <+> text ":. Fixed"

    ppF :: PreOpenFun acc env aenv f -> Doc
    ppF = parens . prettyPreOpenFun prettyAcc env aenv

    ppA :: acc aenv a -> Doc
    ppA = prettyAcc parens aenv

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
        e1'                 = prettyPreOpenExp prettyAcc noParens env            aenv e1
        e2'                 = prettyPreOpenExp prettyAcc noParens (env `Push` x) aenv e2
        x                   = char 'x' <> int (sizeEnv env)

    pp (PrimApp p a)
      | infixOp, Tuple (NilTup `SnocTup` x `SnocTup` y) <- a
      = wrap $ sep [ppE x, f, ppE y]
      | otherwise
      = wrap $ f' <+> ppE a
      where
        -- sometimes the infix function arguments are obstructed. If so, add
        -- parentheses and print prefix.
        --
        (infixOp, f) = prettyPrim p
        f'           = if infixOp then parens f else f

    pp (PrimConst a)            = prettyConst a
    pp (Tuple tup)              = prettyTuple prettyAcc env aenv tup
    pp (Var idx)                = prj idx env
    pp (Const v)                = text $ show (toElt v :: t)
    pp (Prj idx e)              = wrap $ char '#' <> prettyTupleIdx idx <+> ppE e
    pp (Cond c t e)             = wrap $ sep [ ppE c, char '?' , tuple [ ppE' t, ppE' e ]]
    pp IndexNil                 = char 'Z'
    pp IndexAny                 = text "indexAny"
    pp (IndexCons t h)          = ppE' t <+> text ":." <+> ppE' h
    pp (IndexHead ix)           = "indexHead"  .$ [ ppSl ix ]
    pp (IndexTail ix)           = "indexTail"  .$ [ ppSl ix ]
    pp (IndexTrans ix)          = "indexTrans" .$ [ ppSh ix ]
    pp (IndexSlice slix _ sh)   = "indexSlice" .$ [ parens (ppSI slix), ppSh sh ]
    pp (IndexFull _ slix sl)    = "indexFull"  .$ [ ppSl slix, ppSh sl ]
    pp (ToIndex sh ix)          = "toIndex"    .$ [ ppSh sh, ppSh ix ]
    pp (FromIndex sh ix)        = "fromIndex"  .$ [ ppSh sh, ppE ix ]
    pp (ToSlice slix sh ix)     = "toSlice"    .$ [ parens (ppSI slix), ppSh sh, ppE ix ]
    pp (While p f x)            = "while"      .$ [ ppF p, ppF f, ppE x ]
    pp (Foreign ff _f e)        = "foreign"    .$ [ text (strForeign ff), {- ppF f, -} ppE e ]
    pp (Shape idx)              = "shape"      .$ [ ppA idx ]
    pp (ShapeSize idx)          = "shapeSize"  .$ [ ppSh idx ]
    pp (Intersect sh1 sh2)      = "intersect"  .$ [ ppSh sh1, ppSh sh2 ]
    pp (Union sh1 sh2)          = "union"      .$ [ ppSh sh1, ppSh sh2 ]
    pp (Index idx i)            = wrap $ cat [ ppA idx, char '!',  ppE i ]
    pp (LinearIndex idx i)      = wrap $ cat [ ppA idx, text "!!", ppE i ]


-- Pretty print nested pairs as a proper tuple.
--
prettyAtuple
    :: forall acc aenv t.
       PrettyAcc acc
    -> Val aenv
    -> Atuple (acc aenv) t
    -> Doc
prettyAtuple pp aenv = tuple . collect
  where
    collect :: Atuple (acc aenv) t' -> [Doc]
    collect NilAtup          = []
    collect (SnocAtup tup a) = collect tup ++ [pp noParens aenv a]

prettyTuple
    :: forall acc env aenv t.
       PrettyAcc acc
    -> Val env
    -> Val aenv
    -> Tuple (PreOpenExp acc env aenv) t
    -> Doc
prettyTuple pp env aenv = tuple . collect
  where
    collect :: Tuple (PreOpenExp acc env aenv) t' -> [Doc]
    collect NilTup          = []
    collect (SnocTup tup e) = collect tup ++ [prettyPreOpenExp pp noParens env aenv e]


-- Pretty print an index for a tuple projection
--
prettyTupleIdx :: TupleIdx t e -> Doc
prettyTupleIdx = int . toInt
  where
    toInt :: TupleIdx t e -> Int
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
prettyPrim (PrimQuotRem _)        = (False, text "quotRem")
prettyPrim (PrimIDiv _)           = (False, text "div")
prettyPrim (PrimMod _)            = (False, text "mod")
prettyPrim (PrimDivMod _)         = (False, text "divMod")
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
prettyPrim (PrimSinh _)           = (False, text "sinh")
prettyPrim (PrimCosh _)           = (False, text "cosh")
prettyPrim (PrimTanh _)           = (False, text "tanh")
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
prettyPrim (PrimIsNaN _)          = (False, text "isNaN")
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


-- Environments
-- ------------

data Val env where
  Empty ::                   Val ()
  Push  :: Val env -> Doc -> Val (env, t)

class PrettyEnv env where
  prettyEnv :: Val env

instance PrettyEnv () where
  prettyEnv = Empty

instance PrettyEnv env => PrettyEnv (env, t) where
  prettyEnv =
    let env = prettyEnv :: Val env
        x   = char 'a' <> int (sizeEnv env)
    in
    env `Push` x

sizeEnv :: Val env -> Int
sizeEnv Empty        = 0
sizeEnv (Push env _) = 1 + sizeEnv env

prj :: Idx env t -> Val env -> Doc
prj ZeroIdx      (Push _ v)   = v
prj (SuccIdx ix) (Push env _) = prj ix env
prj _            _            = error "inconsistent valuation"



-- Auxiliary operations
-- --------------------

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
