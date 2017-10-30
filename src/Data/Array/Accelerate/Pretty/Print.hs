{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      : Data.Array.Accelerate.Pretty.Print
-- Copyright   : [2008..2017] Manuel M T Chakravarty, Gabriele Keller
--               [2009..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
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
  -- prettyPreOpenSeq,
  prettyPreExp, prettyPreOpenExp,
  prettyPreFun, prettyPreOpenFun,
  prettyPrim,
  prettyArrays,
  prettyTupleIdx,

  -- ** Utilities
  Val(..), PrettyEnv(..), prj, sizeEnv,
  noParens,

) where

-- standard libraries
import Prelude                                          hiding ( (<$>), exp, seq )
import Data.List                                        ( isPrefixOf )
import Text.PrettyPrint.ANSI.Leijen                     hiding ( parens, tupled )
import qualified Text.PrettyPrint.ANSI.Leijen           as PP

-- friends
import Data.Array.Accelerate.AST                        hiding ( Val(..), prj )
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Product


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
    ppSh x = encase (prettyPreExp prettyAcc noParens aenv x)
      where
        encase = case x of
                   Var{}    -> id
                   IndexNil -> id
                   IndexAny -> id
                   Const{}  -> id
                   _        -> parens

    ppF :: PreFun acc aenv f -> Doc
    ppF = parens . prettyPreFun prettyAcc aenv

    ppA :: acc aenv a -> Doc
    ppA = prettyAcc parens aenv

    ppAF :: PreOpenAfun acc aenv f -> Doc
    ppAF = parens . prettyPreOpenAfun prettyAcc aenv

    ppB :: forall sh e. (Shape sh, Elt e)
        => PreBoundary acc aenv (Array sh e)
        -> Doc
    ppB Clamp        = text "clamp"
    ppB Mirror       = text "mirror"
    ppB Wrap         = text "wrap"
    ppB (Constant e) = parens $ text "constant" <+> text (show (toElt e :: e))
    ppB (Function f) = ppF f

    -- pretty print a named array operation with its arguments
    name .$ docs = wrap $ hang 2 (sep (manifest (text name) : docs))

    -- The main pretty-printer
    -- -----------------------
    --
    pp :: PreOpenAcc acc aenv arrs -> Doc
    pp (Alet acc1 acc2)
      | isAlet acc2'
      = if isAlet acc1'
          then wrap $ vsep [ let_ <+> a <+> equals <$> indent 2 acc1'    <+> in_, acc2' ]
          else wrap $ vsep [ hang 2 (sep [let_ <+> a <+> equals, acc1']) <+> in_, acc2' ]

      | otherwise
      = wrap $ vsep [ hang 2 (sep [let_ <+> a <+> equals, acc1']), in_ </> acc2' ]
      where
        -- TLM: derp, can't unwrap into a PreOpenAcc to pattern match on Alet
        render doc  = displayS (renderCompact (plain doc)) ""
        isAlet doc  = "let" `isPrefixOf` render doc
        acc1'       = prettyAcc noParens aenv            acc1
        acc2'       = prettyAcc noParens (aenv `Push` a) acc2
        a           = char 'a' <> int (sizeEnv aenv)

    pp (Awhile p afun acc)      = "awhile" .$ [ppAF p, ppAF afun, ppA acc]
    pp (Atuple tup)             = prettyAtuple prettyAcc aenv tup
    pp (Avar idx)               = prj idx aenv
    pp (Aprj ix arrs)           = wrap $ prettyTupleIdx ix <+> ppA arrs
    pp (Apply afun acc)         = wrap $ sep [ ppAF afun, ppA acc ]
    pp (Acond e acc1 acc2)      = wrap $ hang 3 (vsep [if_ <+> ppE e, then_ <+> ppA acc1, else_ <+> ppA acc2])
    pp (Slice _ty acc ix)       = "slice"       .$ [ ppA acc, ppE ix ]
    pp (Use arrs)               = "use"         .$ [ prettyArrays (arrays (undefined :: arrs)) arrs ]
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
    pp (Stencil sten bndy acc)  = "stencil"     .$ [ ppF sten, ppB bndy, ppA acc ]
    pp (Stencil2 sten bndy1 acc1 bndy2 acc2)
                                = "stencil2"    .$ [ ppF sten, ppB bndy1, ppA acc1, ppB bndy2, ppA acc2 ]

    -- pp (Collect s)              = wrap $ hang (text "collect") 2
    --                                    $ encloseSep lbrace rbrace semi
    --                                    $ prettyPreOpenSeq prettyAcc wrap aenv Empty s


{--
-- Pretty print a computation over sequences
--
prettyPreOpenSeq
    :: forall acc aenv senv arrs.
       PrettyAcc acc
    -> (Doc -> Doc)                             -- apply to compound expressions
    -> Val aenv                                 -- environment of array variables
    -> Val senv                                 -- environment of sequence variables
    -> PreOpenSeq acc aenv senv arrs
    -> [Doc]
prettyPreOpenSeq prettyAcc wrap aenv senv seq =
  case seq of
    Producer p s' -> prettyP p : prettyPreOpenSeq prettyAcc wrap aenv (senv `Push` var (sizeEnv senv)) s'
    Consumer c    -> [prettyC c]
    Reify ix      -> [var (idxToInt ix)]
  where
    var n         = char 's' <> int n
    name .$  docs = wrap $ hang (var (sizeEnv senv) <+> text ":=" <+> text name) 2 (sep docs)
    name ..$ docs = wrap $ hang (text name) 2 (sep docs)

    ppE :: PreExp acc aenv e -> Doc
    ppE = prettyPreExp prettyAcc parens aenv

    ppF :: PreFun acc aenv f -> Doc
    ppF = parens . prettyPreFun prettyAcc aenv

    ppA :: acc aenv a -> Doc
    ppA = prettyAcc parens aenv

    ppAF :: PreOpenAfun acc aenv f -> Doc
    ppAF = parens . prettyPreOpenAfun prettyAcc aenv

    ppX :: Idx aenv' a -> Doc
    ppX x = var (idxToInt x)

    ppSlix :: SliceIndex slix sl co sh -> Doc
    ppSlix SliceNil       = text "Z"
    ppSlix (SliceAll s)   = sep [ ppSlix s, text ":.", text "All"   ]
    ppSlix (SliceFixed s) = sep [ ppSlix s, text ":.", text "Split" ]

    prettyP :: forall a. Producer acc aenv senv a -> Doc
    prettyP p =
      case p of
        StreamIn _        -> "streamIn"      .$ [ text "..." ]
        ToSeq slix _ a    -> "toSeq"         .$ [ ppSlix slix, ppA a ]
        MapSeq f x        -> "mapSeq"        .$ [ ppAF f , ppX x ]
        ChunkedMapSeq f x -> "chunkedMapSeq" .$ [ ppAF f , ppX x ]
        ZipWithSeq f x y  -> "zipWithSeq"    .$ [ ppAF f , ppX x , ppX y ]
        ScanSeq f e x     -> "foldSeq"       .$ [ ppF f , ppE e , ppX x ]

    prettyC :: forall a. Consumer acc aenv senv a -> Doc
    prettyC c =
      case c of
        FoldSeq f e x        -> "foldSeq"        ..$ [ ppF f , ppE e , ppX x ]
        FoldSeqFlatten f a x -> "foldSeqFlatten" ..$ [ ppAF f , ppA a , ppX x ]
        Stuple t             -> tupled (prettyT t)

    prettyT :: forall t. Atuple (Consumer acc aenv senv) t -> [Doc]
    prettyT NilAtup        = []
    prettyT (SnocAtup t c) = prettyT t ++ [prettyC c]
--}


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
    next aenv' (Abody body) = text "->" <+> align (pp noParens aenv' body)
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
    next env' (Body body) = text "->" <+> align (prettyPreOpenExp pp noParens env' aenv body)
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

    ppE'' :: PreOpenExp acc env aenv sh -> Doc
    ppE'' x = encase (ppE' x)
      where
        encase = case x of
                   Var{}    -> id
                   IndexNil -> id
                   IndexAny -> id
                   Const{}  -> id
                   _        -> parens

    ppF :: PreOpenFun acc env aenv f -> Doc
    ppF = parens . prettyPreOpenFun prettyAcc env aenv

    ppA :: acc aenv a -> Doc
    ppA = prettyAcc parens aenv

    -- pretty print a named array operation with its arguments
    name .$ docs = wrap $ hang 2 (sep (text name : docs))

    -- The main pretty-printer
    -- -----------------------
    --
    pp :: PreOpenExp acc env aenv t -> Doc
    pp (Let e1 e2)
      | isLet e2
      = if isLet e1
          then wrap $ vsep [ let_ <+> x <+> equals <$> indent 2 e1'    <+> in_, e2' ]
          else wrap $ vsep [ hang 2 (sep [let_ <+> x <+> equals, e1']) <+> in_, e2' ]
      | otherwise
      = wrap $ vsep [ hang 2 (sep [let_ <+> x <+> equals, e1']), in_ </> e2' ]
      where
        isLet (Let _ _)     = True
        isLet _             = False
        e1'                 = align $ prettyPreOpenExp prettyAcc noParens env            aenv e1
        e2'                 = align $ prettyPreOpenExp prettyAcc noParens (env `Push` x) aenv e2
        x                   = char 'x' <> int (sizeEnv env)

    pp (PrimApp p a)
      | Tuple (NilTup `SnocTup` x `SnocTup` y) <- a
      = if infixOp
          then wrap $ sep [ppE x, f, ppE y]
          else hang 2 (sep [f, ppE'' x, ppE'' y])
      | otherwise
      = wrap $ hang 2 (sep [f', ppE a])
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
    pp (Prj idx e)              = wrap $ prettyTupleIdx idx <+> ppE e
    pp (Cond c t e)             = wrap $ hang 3 (vsep [ if_ <+> ppE' c, then_ <+> ppE' t, else_ <+> ppE' e ])
    pp IndexNil                 = char 'Z'
    pp IndexAny                 = text "indexAny"
    pp (IndexCons t h)          = sep [ ppE' t, text ":.", ppE' h ]
    pp (IndexHead ix)           = "indexHead"  .$ [ ppE ix ]
    pp (IndexTail ix)           = "indexTail"  .$ [ ppE ix ]
    pp (IndexSlice _ slix sh)   = "indexSlice" .$ [ ppE slix, ppE sh ]
    pp (IndexFull _ slix sl)    = "indexFull"  .$ [ ppE slix, ppE sl ]
    pp (ToIndex sh ix)          = "toIndex"    .$ [ ppE'' sh, ppE'' ix ]
    pp (FromIndex sh ix)        = "fromIndex"  .$ [ ppE'' sh, ppE ix ]
    pp (While p f x)            = "while"      .$ [ ppF p, ppF f, ppE x ]
    pp (Foreign ff _f e)        = "foreign"    .$ [ text (strForeign ff), {- ppF f, -} ppE e ]
    pp (Shape idx)              = "shape"      .$ [ ppA idx ]
    pp (ShapeSize idx)          = "shapeSize"  .$ [ ppE'' idx ]
    pp (Intersect sh1 sh2)      = "intersect"  .$ [ ppE'' sh1, ppE'' sh2 ]
    pp (Union sh1 sh2)          = "union"      .$ [ ppE'' sh1, ppE'' sh2 ]
    pp (Index idx i)            = wrap $ cat [ ppA idx, char '!',  ppE'' i ]
    pp (LinearIndex idx i)      = wrap $ cat [ ppA idx, text "!!", ppE'' i ]


-- Pretty print nested pairs as a proper tuple.
--
prettyAtuple
    :: forall acc aenv t.
       PrettyAcc acc
    -> Val aenv
    -> Atuple (acc aenv) t
    -> Doc
prettyAtuple pp aenv = tupled . collect
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
prettyTuple pp env aenv = tupled . collect
  where
    collect :: Tuple (PreOpenExp acc env aenv) t' -> [Doc]
    collect NilTup          = []
    collect (SnocTup tup e) = collect tup ++ [prettyPreOpenExp pp noParens env aenv e]


-- Pretty print an index for a tuple projection
--
prettyTupleIdx :: TupleIdx t e -> Doc
prettyTupleIdx ix = char '#' <> int (toInt ix)
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
prettyPrim PrimAdd{}                = (True,  char '+')
prettyPrim PrimSub{}                = (True,  char '-')
prettyPrim PrimMul{}                = (True,  char '*')
prettyPrim PrimNeg{}                = (False, text "negate")
prettyPrim PrimAbs{}                = (False, text "abs")
prettyPrim PrimSig{}                = (False, text "signum")
prettyPrim PrimQuot{}               = (False, text "quot")
prettyPrim PrimRem{}                = (False, text "rem")
prettyPrim PrimQuotRem{}            = (False, text "quotRem")
prettyPrim PrimIDiv{}               = (False, text "div")
prettyPrim PrimMod{}                = (False, text "mod")
prettyPrim PrimDivMod{}             = (False, text "divMod")
prettyPrim PrimBAnd{}               = (True,  text ".&.")
prettyPrim PrimBOr{}                = (True,  text ".|.")
prettyPrim PrimBXor{}               = (False, text "xor")
prettyPrim PrimBNot{}               = (False, text "complement")
prettyPrim PrimBShiftL{}            = (False, text "shiftL")
prettyPrim PrimBShiftR{}            = (False, text "shiftR")
prettyPrim PrimBRotateL{}           = (False, text "rotateL")
prettyPrim PrimBRotateR{}           = (False, text "rotateR")
prettyPrim PrimPopCount{}           = (False, text "popCount")
prettyPrim PrimCountLeadingZeros{}  = (False, text "countLeadingZeros")
prettyPrim PrimCountTrailingZeros{} = (False, text "countTrailingZeros")
prettyPrim PrimFDiv{}               = (True,  char '/')
prettyPrim PrimRecip{}              = (False, text "recip")
prettyPrim PrimSin{}                = (False, text "sin")
prettyPrim PrimCos{}                = (False, text "cos")
prettyPrim PrimTan{}                = (False, text "tan")
prettyPrim PrimAsin{}               = (False, text "asin")
prettyPrim PrimAcos{}               = (False, text "acos")
prettyPrim PrimAtan{}               = (False, text "atan")
prettyPrim PrimSinh{}               = (False, text "sinh")
prettyPrim PrimCosh{}               = (False, text "cosh")
prettyPrim PrimTanh{}               = (False, text "tanh")
prettyPrim PrimAsinh{}              = (False, text "asinh")
prettyPrim PrimAcosh{}              = (False, text "acosh")
prettyPrim PrimAtanh{}              = (False, text "atanh")
prettyPrim PrimExpFloating{}        = (False, text "exp")
prettyPrim PrimSqrt{}               = (False, text "sqrt")
prettyPrim PrimLog{}                = (False, text "log")
prettyPrim PrimFPow{}               = (True,  text "**")
prettyPrim PrimLogBase{}            = (False, text "logBase")
prettyPrim PrimTruncate{}           = (False, text "truncate")
prettyPrim PrimRound{}              = (False, text "round")
prettyPrim PrimFloor{}              = (False, text "floor")
prettyPrim PrimCeiling{}            = (False, text "ceiling")
prettyPrim PrimAtan2{}              = (False, text "atan2")
prettyPrim PrimIsNaN{}              = (False, text "isNaN")
prettyPrim PrimIsInfinite{}         = (False, text "isInfinite")
prettyPrim PrimLt{}                 = (True,  text "<")
prettyPrim PrimGt{}                 = (True,  text ">")
prettyPrim PrimLtEq{}               = (True,  text "<=")
prettyPrim PrimGtEq{}               = (True,  text ">=")
prettyPrim PrimEq{}                 = (True,  text "==")
prettyPrim PrimNEq{}                = (True,  text "/=")
prettyPrim PrimMax{}                = (False, text "max")
prettyPrim PrimMin{}                = (False, text "min")
prettyPrim PrimLAnd                 = (True,  text "&&")
prettyPrim PrimLOr                  = (True,  text "||")
prettyPrim PrimLNot                 = (False, text "not")
prettyPrim PrimOrd                  = (False, text "ord")
prettyPrim PrimChr                  = (False, text "chr")
prettyPrim PrimBoolToInt            = (False, text "boolToInt")
prettyPrim PrimFromIntegral{}       = (False, text "fromIntegral")
prettyPrim PrimToFloating{}         = (False, text "toFloating")
prettyPrim (PrimCoerce _ t)         = (False, text "reinterpret_cast" <> char '<' <> text (show t) <> char '>')

{-
-- Pretty print type
--
prettyAnyType :: ScalarType a -> Doc
prettyAnyType ty = text $ show ty
-}

-- TLM: seems to flatten the nesting structure
--
prettyArrays :: ArraysR arrs -> arrs -> Doc
prettyArrays arrs = tupled . collect arrs
  where
    collect :: ArraysR arrs -> arrs -> [Doc]
    collect ArraysRunit         _        = []
    collect ArraysRarray        arr      = [prettyArray arr]
    collect (ArraysRpair r1 r2) (a1, a2) = collect r1 a1 ++ collect r2 a2

prettyArray :: forall dim e. Array dim e -> Doc
prettyArray arr@(Array sh _)
  = hang 2 $ sep [ text "Array"
                 , parens . text $ showShape (toElt sh :: dim)
                 , dataDoc ]
  where
    showDoc :: forall a. Show a => a -> Doc
    showDoc = text . show
    l       = toList arr
    dataDoc | length l <= 1000 = showDoc l
            | otherwise        = showDoc (take 1000 l) <+>
                                 text "{truncated at 1000 elements}"


-- Auxiliary pretty printing combinators
--

parens :: Doc -> Doc
parens = PP.parens . align

noParens :: Doc -> Doc
noParens = id

tupled :: [Doc] -> Doc
tupled = PP.tupled . map align


-- ANSI colourisation
--

control :: Doc -> Doc
control = dullyellow

manifest :: Doc -> Doc
manifest = blue

-- delayed :: Doc -> Doc
-- delayed = green

let_, in_ :: Doc
let_ = control (text "let")
in_  = control (text "in")

if_, then_, else_ :: Doc
if_   = control (text "if")
then_ = control (text "then")
else_ = control (text "else")


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
#if __GLASGOW_HASKELL__ < 800
prj _            _            = error "inconsistent valuation"
#endif


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

