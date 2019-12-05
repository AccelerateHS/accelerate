{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module      : Data.Array.Accelerate.Pretty.Print
-- Copyright   : [2008..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Pretty.Print (

  PrettyAcc, ExtractAcc,
  prettyPreOpenAcc,
  prettyPreOpenAfun,
  prettyPreOpenExp,
  prettyPreOpenFun,
  prettyArray,
  prettyConst,

  -- ** Internals
  Adoc,
  Val(..),
  PrettyEnv(..),
  Context(..),
  Keyword(..),
  Operator(..),
  parensIf, needsParens,
  ansiKeyword,
  shiftwidth,
  context0,
  app,
  manifest, delayed,
  primOperator,
  isInfix,
  prj, sizeEnv,

) where

import Data.Char
import Data.String
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Typeable                                                ( Typeable, typeOf, showsTypeRep )
import Prelude                                                      hiding ( exp )

import Data.Array.Accelerate.AST                                    hiding ( Val(..), prj )
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Type


-- Implementation
-- --------------

type PrettyAcc  acc = forall aenv a. Context -> Val aenv -> acc aenv a -> Adoc
type ExtractAcc acc = forall aenv a. acc aenv a -> PreOpenAcc acc aenv a

type Adoc = Doc Keyword

data Keyword
  = Statement     -- do | case of | let in
  | Conditional   -- if then else
  | Manifest      -- collective operations (kernel functions)
  | Delayed       -- fused operators
  deriving (Eq, Show)

let_, in_ :: Adoc
let_ = annotate Statement "let"
in_  = annotate Statement "in"

if_, then_, else_ :: Adoc
if_   = annotate Statement "if"
then_ = annotate Statement "then"
else_ = annotate Statement "else"

manifest :: Operator -> Adoc
manifest = annotate Manifest . opName

delayed :: Operator -> Adoc
delayed = annotate Delayed . opName

ansiKeyword :: Keyword -> AnsiStyle
ansiKeyword Statement   = colorDull Yellow
ansiKeyword Conditional = colorDull Yellow
ansiKeyword Manifest    = color Blue
ansiKeyword Delayed     = color Green


-- Array computations
-- ------------------

prettyPreOpenAfun
    :: forall acc aenv f.
       PrettyAcc acc
    -> Val aenv
    -> PreOpenAfun acc aenv f
    -> Adoc
prettyPreOpenAfun prettyAcc aenv0 = next (pretty '\\') aenv0
  where
    next :: Adoc -> Val aenv' -> PreOpenAfun acc aenv' f' -> Adoc
    next vs aenv (Abody body)   = hang shiftwidth (sep [vs <> "->", prettyAcc context0 aenv body])
    next vs aenv (Alam lhs lam) =
      let (aenv', lhs') = prettyLHS aenv lhs
       in next (vs <> lhs' <> space) aenv' lam

prettyPreOpenAcc
    :: forall acc aenv arrs.
       Context
    -> PrettyAcc acc
    -> ExtractAcc acc
    -> Val aenv
    -> PreOpenAcc acc aenv arrs
    -> Adoc
prettyPreOpenAcc ctx prettyAcc extractAcc aenv pacc =
  case pacc of
    Avar (ArrayVar idx)     -> prj idx aenv
    Alet{}                  -> prettyAlet ctx prettyAcc extractAcc aenv pacc
    Apair{}                 -> prettyAtuple prettyAcc extractAcc aenv pacc
    Anil                    -> "()"
    Apply f a               -> apply
      where
        op    = Operator ">->" Infix L 1
        apply = sep [ ppAF f, group (sep [opName op, ppA a]) ]

    Acond p t e             -> flatAlt multi single
      where
        p' = ppE p
        t' = ppA t
        e' = ppA e
        --
        single = parensIf (needsParens ctx (Operator "?|:" Infix N 0))
               $ sep [ p', "?|", t', pretty ':', e' ]
        multi  = hang 3
               $ vsep [ if_ <+> p'
                      , hang shiftwidth (sep [ then_, t' ])
                      , hang shiftwidth (sep [ else_, e' ]) ]

    Aforeign ff _f a        -> "aforeign"     .$ [ pretty (strForeign ff), ppA a ]
    Awhile p f a            -> "awhile"       .$ [ ppAF p, ppAF f, ppA a ]
    Use arr                 -> "use"          .$ [ prettyArray arr ]
    Unit e                  -> "unit"         .$ [ ppE e ]
    Reshape sh a            -> "reshape"      .$ [ ppE sh, ppA a ]
    Generate sh f           -> "generate"     .$ [ ppE sh, ppF f ]
    Transform sh p f a      -> "transform"    .$ [ ppE sh, ppF p, ppF f, ppA a ]
    Replicate _ ix a        -> "replicate"    .$ [ ppE ix, ppA a ]
    Slice _ a ix            -> "slice"        .$ [ ppE ix, ppA a ]
    Map f a                 -> "map"          .$ [ ppF f,  ppA a ]
    ZipWith f a b           -> "zipWith"      .$ [ ppF f,  ppA a, ppA b ]
    Fold f z a              -> "fold"         .$ [ ppF f,  ppE z, ppA a ]
    Fold1 f a               -> "fold1"        .$ [ ppF f,  ppA a ]
    FoldSeg f z a s         -> "foldSeg"      .$ [ ppF f,  ppE z, ppA a, ppA s ]
    Fold1Seg f a s          -> "fold1Seg"     .$ [ ppF f,  ppA a, ppA s ]
    Scanl f z a             -> "scanl"        .$ [ ppF f,  ppE z, ppA a ]
    Scanl' f z a            -> "scanl'"       .$ [ ppF f,  ppE z, ppA a ]
    Scanl1 f a              -> "scanl1"       .$ [ ppF f,  ppA a ]
    Scanr f z a             -> "scanr"        .$ [ ppF f,  ppE z, ppA a ]
    Scanr' f z a            -> "scanr'"       .$ [ ppF f,  ppE z, ppA a ]
    Scanr1 f a              -> "scanr1"       .$ [ ppF f,  ppA a ]
    Permute f d p s         -> "permute"      .$ [ ppF f,  ppA d, ppF p, ppA s ]
    Backpermute sh f a      -> "backpermute"  .$ [ ppE sh, ppF f, ppA a ]
    Stencil f b a           -> "stencil"      .$ [ ppF f,  ppB b, ppA a ]
    Stencil2 f b1 a1 b2 a2  -> "stencil2"     .$ [ ppF f,  ppB b1, ppA a1, ppB b2, ppA a2 ]
  where
    infixr 0 .$
    f .$ xs
      = parensIf (needsParens ctx f)
      $ hang shiftwidth (sep (manifest f : xs))

    ppA :: acc aenv a -> Adoc
    ppA = prettyAcc app aenv

    ppAF :: PreOpenAfun acc aenv f -> Adoc
    ppAF = parens . prettyPreOpenAfun prettyAcc aenv

    ppE :: PreExp acc aenv t -> Adoc
    ppE = prettyPreOpenExp app prettyAcc extractAcc Empty aenv

    ppF :: PreFun acc aenv t -> Adoc
    ppF = parens . prettyPreOpenFun prettyAcc extractAcc Empty aenv

    ppB :: forall sh e. Elt e
        => PreBoundary acc aenv (Array sh e)
        -> Adoc
    ppB Clamp        = "clamp"
    ppB Mirror       = "mirror"
    ppB Wrap         = "wrap"
    ppB (Constant e) = prettyConst (toElt e :: e)
    ppB (Function f) = ppF f


prettyAlet
    :: forall acc aenv arrs.
       Context
    -> PrettyAcc acc
    -> ExtractAcc acc
    -> Val aenv
    -> PreOpenAcc acc aenv arrs
    -> Adoc
prettyAlet ctx prettyAcc extractAcc aenv0
  = parensIf (needsParens ctx "let")
  . align . wrap . collect aenv0
  where
    collect :: Val aenv' -> PreOpenAcc acc aenv' a -> ([Adoc], Adoc)
    collect aenv =
      \case
        Alet lhs a1 a2 ->
          let (aenv', v)      = prettyLHS aenv lhs
              a1'             = ppA aenv a1
              bnd | isAlet a1 = nest shiftwidth (vsep [v <+> equals, a1'])
                  | otherwise = v <+> align (equals <+> a1')
              (bnds, body)    = collect aenv' (extractAcc a2)
          in
          (bnd:bnds, body)
        --
        next       -> ([], prettyPreOpenAcc context0 prettyAcc extractAcc aenv next)

    isAlet :: acc aenv' a -> Bool
    isAlet (extractAcc -> Alet{}) = True
    isAlet _                      = False

    ppA :: Val aenv' -> acc aenv' a -> Adoc
    ppA = prettyAcc context0

    wrap :: ([Adoc], Adoc) -> Adoc
    wrap ([],   body) = body  -- shouldn't happen!
    wrap ([b],  body)
      = sep [ nest shiftwidth (sep [let_, b]), in_, body ]
    wrap (bnds, body)
      = vsep [ nest shiftwidth (vsep (let_:bnds))
             , in_
             , body
             ]

prettyAtuple
    :: forall acc aenv arrs.
       PrettyAcc acc
    -> ExtractAcc acc
    -> Val aenv
    -> PreOpenAcc acc aenv arrs
    -> Adoc
prettyAtuple prettyAcc extractAcc aenv0
  = align . wrap . collect aenv0
  where
    wrap [x] = x
    wrap xs  = tupled xs

    collect :: Val aenv' -> PreOpenAcc acc aenv' a -> [Adoc]
    collect aenv =
      \case
        Anil        -> []
        Apair a1 a2 -> collect aenv (extractAcc a1) ++ [prettyAcc context0 aenv a2]
        next        -> [prettyPreOpenAcc context0 prettyAcc extractAcc aenv next]

prettyLHS :: Val aenv -> LeftHandSide arrs aenv aenv' -> (Val aenv', Adoc)
prettyLHS aenv0 = fmap wrap . go aenv0
  where
    wrap [x] = x
    wrap xs  = tupled xs

    go :: Val aenv -> LeftHandSide arrs aenv aenv' -> (Val aenv', [Adoc])
    go aenv (LeftHandSideWildcard ArraysRunit) = (aenv, [])
    go aenv (LeftHandSideWildcard _)           = (aenv, ["_"])
    go aenv LeftHandSideArray                  = (aenv `Push` v, [v])
      where
        v = pretty 'a' <> pretty (sizeEnv aenv)
    go aenv (LeftHandSidePair a b)             = (aenv2, doc1 ++ [doc2])
      where
        (aenv1, doc1) = go aenv a
        (aenv2, doc2) = prettyLHS aenv1 b

prettyArray :: (Shape sh, Elt e) => Array sh e -> Adoc
prettyArray = parens . viaShow


-- Scalar expressions
-- ------------------

prettyPreOpenFun
    :: forall acc env aenv f.
       PrettyAcc acc
    -> ExtractAcc acc
    -> Val env
    -> Val aenv
    -> PreOpenFun acc env aenv f
    -> Adoc
prettyPreOpenFun prettyAcc extractAcc env0 aenv = next (pretty '\\') env0
  where
    next :: Adoc -> Val env' -> PreOpenFun acc env' aenv f' -> Adoc
    next vs env (Body body)
      --   PrimApp f x                             <- body
      -- , op                                      <- primOperator f
      -- , isInfix op
      -- , Tuple (NilTup `SnocTup` a `SnocTup` b)  <- x
      -- , Var (SuccIdx ZeroIdx)                   <- a
      -- , Var ZeroIdx                             <- b
      -- = opName op -- surrounding context will add parens
      --
      = hang shiftwidth (sep [ vs <> "->"
                             , prettyPreOpenExp context0 prettyAcc extractAcc env aenv body])
    next vs env (Lam  lam)  =
      let x = pretty 'x' <> pretty (sizeEnv env)
      in  next (vs <> x <> space) (env `Push` x) lam

prettyPreOpenExp
    :: forall acc env aenv t.
       Context
    -> PrettyAcc acc
    -> ExtractAcc acc
    -> Val env
    -> Val aenv
    -> PreOpenExp acc env aenv t
    -> Adoc
prettyPreOpenExp ctx prettyAcc extractAcc env aenv exp =
  case exp of
    Var idx             -> prj idx env
    Let{}               -> prettyLet ctx prettyAcc extractAcc env aenv exp
    PrimApp f x
      | Tuple (NilTup `SnocTup` a `SnocTup` b) <- x -> ppF2 op  (ppE a) (ppE b)
      | otherwise                                   -> ppF1 op' (ppE x)
      where
        op  = primOperator f
        op' = isInfix op ? (Operator (parens (opName op)) App L 10, op)
    --
    PrimConst c           -> prettyPrimConst c
    Const c               -> prettyConst (toElt c :: t)
    Tuple t               -> prettyTuple (eltType @t) prettyAcc extractAcc env aenv t
    Prj tix e             -> ppF2 (Operator "#" Infix L 8) (ppE e) (\_ -> pretty (tupleIdxToInt tix))
    Cond p t e            -> flatAlt multi single
      where
        p' = ppE p context0
        t' = ppE t context0
        e' = ppE e context0
        --
        single = parensIf (needsParens ctx (Operator "?:" Infix N 0))
               $ sep [ p', pretty '?', t', pretty ':', e' ]
        multi  = hang 3
               $ vsep [ if_ <+> p'
                      , hang shiftwidth (sep [ then_, t' ])
                      , hang shiftwidth (sep [ else_, e' ]) ]
    --
    IndexAny              -> "Any"
    IndexNil              -> pretty 'Z'
    IndexCons sh sz       -> ppF2 (Operator ":." Infix L 3) (ppE sh) (ppE sz)
    IndexHead sh          -> ppF1 "indexHead"   (ppE sh)
    IndexTail sh          -> ppF1 "indexTail"   (ppE sh)
    IndexSlice _ slix sh  -> ppF2 "indexSlice"  (ppE slix) (ppE sh)
    IndexFull _ slix sl   -> ppF2 "indexFull"   (ppE slix) (ppE sl)
    ToIndex sh ix         -> ppF2 "toIndex"     (ppE sh) (ppE ix)
    FromIndex sh ix       -> ppF2 "fromIndex"   (ppE sh) (ppE ix)
    While p f x           -> ppF3 "while"       (ppF p) (ppF f) (ppE x)
    Foreign ff _f e       -> ppF2 "foreign"     (\_ -> pretty (strForeign ff)) (ppE e)
    Shape arr             -> ppF1 "shape"       (ppA arr)
    ShapeSize sh          -> ppF1 "shapeSize"   (ppE sh)
    Intersect sh1 sh2     -> ppF2 "intersect"   (ppE sh1) (ppE sh2)
    Union sh1 sh2         -> ppF2 "union"       (ppE sh1) (ppE sh2)
    Index arr ix          -> ppF2 (Operator (pretty '!') Infix L 9) (ppA arr) (ppE ix)
    LinearIndex arr ix    -> ppF2 (Operator "!!"         Infix L 9) (ppA arr) (ppE ix)
    Coerce x              -> ppF1 (Operator (withTypeRep "coerce") App L 10) (ppE x)
    Undef                 -> withTypeRep "undef"

  where
    ppE :: PreOpenExp acc env aenv e -> Context -> Adoc
    ppE e c = prettyPreOpenExp c prettyAcc extractAcc env aenv e

    ppA :: acc aenv a -> Context -> Adoc
    ppA acc _ = prettyAcc app aenv acc

    ppF :: PreOpenFun acc env aenv f -> Context -> Adoc
    ppF f _ = parens $ prettyPreOpenFun prettyAcc extractAcc env aenv f

    ppF1 :: Operator -> (Context -> Adoc) -> Adoc
    ppF1 op x
      = parensIf (needsParens ctx op)
      $ combine [ opName op, x ctx' ]
      where
        ctx'    = isPrefix op ? (arg op R, app)
        combine = isPrefix op ? (cat, hang 2 . sep)

    ppF2 :: Operator -> (Context -> Adoc) -> (Context -> Adoc) -> Adoc
    ppF2 op x y
      = parensIf (needsParens ctx op)
      $ if isInfix op
          then sep [ x (arg op L), group (sep [opName op, y (arg op R)]) ]
          else hang 2 $ sep [ opName op, x app, y app ]

    ppF3 :: Operator -> (Context -> Adoc) -> (Context -> Adoc) -> (Context -> Adoc) -> Adoc
    ppF3 op x y z
      = parensIf (needsParens ctx op)
      $ hang 2
      $ sep [ opName op, x app, y app, z app ]

    withTypeRep :: Typeable t => Adoc -> Adoc
    withTypeRep op = op <> enclose langle rangle (pretty (showsTypeRep (typeOf (undefined::t)) ""))


prettyLet
    :: forall acc env aenv t.
       Context
    -> PrettyAcc acc
    -> ExtractAcc acc
    -> Val env
    -> Val aenv
    -> PreOpenExp acc env aenv t
    -> Adoc
prettyLet ctx prettyAcc extractAcc env0 aenv
  = parensIf (needsParens ctx "let")
  . align . wrap . collect env0
  where
    collect :: Val env' -> PreOpenExp acc env' aenv e -> ([Adoc], Adoc)
    collect env =
      \case
        Let e1 e2 ->
          let env'            = env `Push` v
              v               = pretty 'x' <> pretty (sizeEnv env)
              e1'             = ppE env e1
              bnd | isLet e1  = nest shiftwidth (vsep [v <+> equals, e1'])
                  | otherwise = v <+> align (equals <+> e1')
              (bnds, body)    = collect env' e2
          in
          (bnd:bnds, body)
        --
        next     -> ([], ppE env next)

    isLet :: PreOpenExp acc env' aenv t' -> Bool
    isLet Let{} = True
    isLet _     = False

    ppE :: Val env' -> PreOpenExp acc env' aenv t' -> Adoc
    ppE env = prettyPreOpenExp context0 prettyAcc extractAcc env aenv

    wrap :: ([Adoc], Adoc) -> Adoc
    wrap ([],   body) = body  -- shouldn't happen!
    wrap ([b],  body)
      = sep [ nest shiftwidth (sep [let_, b]), in_, body ]
    wrap (bnds, body)
      = vsep [ nest shiftwidth (vsep (let_ : bnds))
             , in_
             , body
             ]

prettyTuple
    :: forall acc env aenv t p.
       TupleType t
    -> PrettyAcc acc
    -> ExtractAcc acc
    -> Val env
    -> Val aenv
    -> Tuple (PreOpenExp acc env aenv) p
    -> Adoc
prettyTuple tt prettyAcc extractAcc env aenv = wrap . collect []
  where
    collect :: [Adoc] -> Tuple (PreOpenExp acc env aenv) s -> [Adoc]
    collect acc =
      \case
        NilTup        -> acc
        SnocTup tup e -> collect (align (prettyPreOpenExp context0 prettyAcc extractAcc env aenv e) : acc) tup
    --
    wrap
      | TypeRscalar VectorScalarType{} <- tt = group . encloseSep (flatAlt "< " "<") (flatAlt " >" ">") ", "
      | otherwise                            = tupled -- as above, with parenthesis


prettyConst :: Elt e => e -> Adoc
prettyConst x =
  let y = show x
  in  parensIf (any isSpace y) (pretty y)

prettyPrimConst :: PrimConst a -> Adoc
prettyPrimConst PrimMinBound{} = "minBound"
prettyPrimConst PrimMaxBound{} = "maxBound"
prettyPrimConst PrimPi{}       = "pi"


-- Primitive operators
-- -------------------
--
-- The core of the pretty printer is how to correctly handle precedence,
-- associativity, and fixity of the primitive scalar operators.
--

data Direction = L | N | R
  deriving Eq

data Fixity = App | Infix | Prefix
  deriving Eq

type Precedence    = Int
type Associativity = Direction

data Context = Context
  { ctxAssociativity  :: Associativity
  , ctxPosition       :: Direction
  , ctxPrecedence     :: Precedence
  }

data Operator = Operator
  { opName            :: Adoc
  , opFixity          :: Fixity
  , opAssociativity   :: Associativity
  , opPrecedence      :: Precedence
  }

instance IsString Operator where
  fromString s = Operator (fromString s) App L 10

needsParens :: Context -> Operator -> Bool
needsParens Context{..} Operator{..}
  | ctxPrecedence     < opPrecedence    = False
  | ctxPrecedence     > opPrecedence    = True
  | ctxAssociativity /= opAssociativity = True
  | otherwise                           = ctxPosition /= opAssociativity

context0 :: Context
context0 = Context N N 0

app :: Context
app = Context L N 10

arg :: Operator -> Direction -> Context
arg Operator{..} side = Context opAssociativity side opPrecedence

isPrefix :: Operator -> Bool
isPrefix Operator{..} = opFixity == Prefix

isInfix :: Operator -> Bool
isInfix Operator{..}  = opFixity == Infix

primOperator :: PrimFun a -> Operator
primOperator PrimAdd{}                = Operator (pretty '+')         Infix  L 6
primOperator PrimSub{}                = Operator (pretty '-')         Infix  L 6
primOperator PrimMul{}                = Operator (pretty '*')         Infix  L 7
primOperator PrimNeg{}                = Operator (pretty '-')         Prefix L 6  -- Haskell's only prefix operator
primOperator PrimAbs{}                = Operator "abs"                App    L 10
primOperator PrimSig{}                = Operator "signum"             App    L 10
primOperator PrimQuot{}               = Operator "quot"               App    L 10
primOperator PrimRem{}                = Operator "rem"                App    L 10
primOperator PrimQuotRem{}            = Operator "quotRem"            App    L 10
primOperator PrimIDiv{}               = Operator "div"                App    L 10
primOperator PrimMod{}                = Operator "mod"                App    L 10
primOperator PrimDivMod{}             = Operator "divMod"             App    L 10
primOperator PrimBAnd{}               = Operator ".&."                Infix  L 7
primOperator PrimBOr{}                = Operator ".|."                Infix  L 5
primOperator PrimBXor{}               = Operator "xor"                App    L 10
primOperator PrimBNot{}               = Operator "complement"         App    L 10
primOperator PrimBShiftL{}            = Operator "shiftL"             App    L 10
primOperator PrimBShiftR{}            = Operator "shiftR"             App    L 10
primOperator PrimBRotateL{}           = Operator "rotateL"            App    L 10
primOperator PrimBRotateR{}           = Operator "rotateR"            App    L 10
primOperator PrimPopCount{}           = Operator "popCount"           App    L 10
primOperator PrimCountLeadingZeros{}  = Operator "countLeadingZeros"  App    L 10
primOperator PrimCountTrailingZeros{} = Operator "countTrailingZeros" App    L 10
primOperator PrimFDiv{}               = Operator (pretty '/')         Infix  L 7
primOperator PrimRecip{}              = Operator "recip"              App    L 10
primOperator PrimSin{}                = Operator "sin"                App    L 10
primOperator PrimCos{}                = Operator "cos"                App    L 10
primOperator PrimTan{}                = Operator "tan"                App    L 10
primOperator PrimAsin{}               = Operator "asin"               App    L 10
primOperator PrimAcos{}               = Operator "acos"               App    L 10
primOperator PrimAtan{}               = Operator "atan"               App    L 10
primOperator PrimSinh{}               = Operator "sinh"               App    L 10
primOperator PrimCosh{}               = Operator "cosh"               App    L 10
primOperator PrimTanh{}               = Operator "tanh"               App    L 10
primOperator PrimAsinh{}              = Operator "asinh"              App    L 10
primOperator PrimAcosh{}              = Operator "acosh"              App    L 10
primOperator PrimAtanh{}              = Operator "atanh"              App    L 10
primOperator PrimExpFloating{}        = Operator "exp"                App    L 10
primOperator PrimSqrt{}               = Operator "sqrt"               App    L 10
primOperator PrimLog{}                = Operator "log"                App    L 10
primOperator PrimFPow{}               = Operator "**"                 Infix  R 8
primOperator PrimLogBase{}            = Operator "logBase"            App    L 10
primOperator PrimTruncate{}           = Operator "truncate"           App    L 10
primOperator PrimRound{}              = Operator "round"              App    L 10
primOperator PrimFloor{}              = Operator "floor"              App    L 10
primOperator PrimCeiling{}            = Operator "ceiling"            App    L 10
primOperator PrimAtan2{}              = Operator "atan2"              App    L 10
primOperator PrimIsNaN{}              = Operator "isNaN"              App    L 10
primOperator PrimIsInfinite{}         = Operator "isInfinite"         App    L 10
primOperator PrimLt{}                 = Operator "<"                  Infix  N 4
primOperator PrimGt{}                 = Operator ">"                  Infix  N 4
primOperator PrimLtEq{}               = Operator "<="                 Infix  N 4
primOperator PrimGtEq{}               = Operator ">="                 Infix  N 4
primOperator PrimEq{}                 = Operator "=="                 Infix  N 4
primOperator PrimNEq{}                = Operator "/="                 Infix  N 4
primOperator PrimMax{}                = Operator "max"                App    L 10
primOperator PrimMin{}                = Operator "min"                App    L 10
primOperator PrimLAnd                 = Operator "&&"                 Infix  R 3
primOperator PrimLOr                  = Operator "||"                 Infix  R 2
primOperator PrimLNot                 = Operator "not"                App    L 10
primOperator PrimOrd                  = Operator "ord"                App    L 10
primOperator PrimChr                  = Operator "chr"                App    L 10
primOperator PrimBoolToInt            = Operator "boolToInt"          App    L 10
primOperator PrimFromIntegral{}       = Operator "fromIntegral"       App    L 10
primOperator PrimToFloating{}         = Operator "toFloating"         App    L 10


-- Environments
-- ------------

data Val env where
  Empty ::                    Val ()
  Push  :: Val env -> Adoc -> Val (env, t)

class PrettyEnv env where
  prettyEnv :: Adoc -> Val env

instance PrettyEnv () where
  prettyEnv _ = Empty

instance PrettyEnv env => PrettyEnv (env, t) where
  prettyEnv v =
    let env = prettyEnv v :: Val env
        x   = v <> pretty (sizeEnv env)
    in
    env `Push` x

sizeEnv :: Val env -> Int
sizeEnv Empty        = 0
sizeEnv (Push env _) = 1 + sizeEnv env

prj :: Idx env t -> Val env -> Adoc
prj ZeroIdx      (Push _ v)   = v
prj (SuccIdx ix) (Push env _) = prj ix env
#if __GLASGOW_HASKELL__ < 800
prj _            _            = error "inconsistent valuation"
#endif


-- Utilities
-- ---------

shiftwidth :: Int
shiftwidth = 2

infix 0 ?
(?) :: Bool -> (a, a) -> a
True  ? (t,_) = t
False ? (_,f) = f

parensIf :: Bool -> Doc ann -> Doc ann
parensIf True  = group . parens . align
parensIf False = id

