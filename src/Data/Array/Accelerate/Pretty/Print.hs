{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
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
-- Copyright   : [2008..2020] The Accelerate Team
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
  prettyOpenExp, prettyExp,
  prettyOpenFun, prettyFun,
  prettyArray,
  prettyConst,
  prettyELhs,
  prettyALhs,
  prettyAnn,
  prettyAnn',

  -- ** Configuration
  PrettyConfig(..),
  AnnotationVerbosity(..),
  configPlain,
  configVerbose,
  defaultConfig,

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

import Data.Array.Accelerate.Annotations
import Data.Array.Accelerate.AST                                    hiding ( Direction )
import Data.Array.Accelerate.AST.Idx
import Data.Array.Accelerate.AST.LeftHandSide
import Data.Array.Accelerate.AST.Var
import Data.Array.Accelerate.Debug.Internal.Flags
import Data.Array.Accelerate.Representation.Array
import Data.Array.Accelerate.Representation.Elt
import Data.Array.Accelerate.Representation.Stencil
import Data.Array.Accelerate.Representation.Tag
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Sugar.Foreign
import Data.Array.Accelerate.Type
import qualified Data.Array.Accelerate.AST                          as AST
import qualified Data.Array.Accelerate.Analysis.Hash                as Hash
import qualified Data.Array.Accelerate.Trafo.Delayed                as Delayed

import Data.Char
import Data.Maybe
import Data.String
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import GHC.Stack
import Prelude                                                      hiding ( exp )
import System.IO.Unsafe


-- Implementation
-- --------------

type PrettyAcc acc =
  forall aenv a. PrettyConfig acc -> Context -> Val aenv -> acc aenv a -> Adoc
type ExtractAcc acc =
  forall aenv a. acc aenv a -> PreOpenAcc acc aenv a

type Adoc = Doc Keyword

data Keyword
  = Statement     -- do | case of | let in
  | Conditional   -- if then else
  | Manifest      -- collective operations (kernel functions)
  | Delayed       -- fused operators
  | Annotation    -- source locations and optimization flags
  deriving (Eq, Show)

let_, in_ :: Adoc
let_ = annotate Statement "let"
in_  = annotate Statement "in"

case_, of_ :: Adoc
case_ = annotate Statement "case"
of_   = annotate Statement "of"

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
ansiKeyword Annotation  = color Black

-- Configuration for the pretty-printing functions
data PrettyConfig acc
  = PrettyConfig { confOperator :: forall aenv arrs.
                                   PreOpenAcc acc aenv arrs
                                -> String
                                -> Operator
                 , confAnnotationVerbosity :: AnnotationVerbosity
                 }

-- | Controls how much annotation information to display when pretty printing an
-- AST, since the output can become very verbose and not all information is
-- always necessary.
data AnnotationVerbosity
  -- | Don't print anything related to annotations.
  = Quiet
  -- | Print enabled optimization flags, but don't print any source information.
  | OptsOnly
  -- | Print both source information and optimization flags, but if there is no
  -- source information available don't indicate that to the user and just skip
  -- over it.
  | Normal
  -- | Print both source information and optimization flags. If there is no
  -- source information available, we'll print @<unknown>@ instead to indicate
  -- this to the user.
  | Verbose
  deriving (Eq, Ord)

configPlain :: PrettyConfig acc
configPlain = PrettyConfig { confOperator            = const fromString
                           , confAnnotationVerbosity = Normal
                           }

-- | Include kernel hashes and more verbose annotation information in the pretty
-- printer output.
configVerbose :: PrettyConfig Delayed.DelayedOpenAcc
configVerbose =
  PrettyConfig
    { confOperator = \pacc name ->
        let hashval = Hash.hashPreOpenAccWith
                          (Hash.defaultHashOptions { Hash.perfect = False })
                          Delayed.encodeDelayedOpenAcc
                          pacc
        in fromString (name ++ "_" ++ show hashval)
    , confAnnotationVerbosity = Verbose
    }


-- | The default pretty printer config.
defaultConfig :: PrettyConfig Delayed.DelayedOpenAcc
defaultConfig = if shouldPrintHash then configVerbose else configPlain


-- Array computations
-- ------------------

prettyPreOpenAfun
    :: forall acc aenv f.
       PrettyConfig acc
    -> PrettyAcc acc
    -> Val aenv
    -> PreOpenAfun acc aenv f
    -> Adoc
prettyPreOpenAfun config prettyAcc aenv0 = next (pretty '\\') aenv0
  where
    next :: Adoc -> Val aenv' -> PreOpenAfun acc aenv' f' -> Adoc
    next vs aenv (Abody body)   =
      hang shiftwidth (sep [vs <> "->", prettyAcc config context0 aenv body])
    next vs aenv (Alam lhs lam) =
      let (aenv', lhs') = prettyALhs True aenv lhs
      in  next (vs <> lhs' <> space) aenv' lam

prettyPreOpenAcc
    :: forall acc aenv arrs.
       PrettyConfig acc
    -> Context
    -> PrettyAcc acc
    -> ExtractAcc acc
    -> Val aenv
    -> PreOpenAcc acc aenv arrs
    -> Adoc
prettyPreOpenAcc config ctx prettyAcc extractAcc aenv pacc =
  maybeWithAnn config False pacc $ case pacc of
    Avar (Var _ _ idx)               -> prj idx aenv
    Alet{}                           -> prettyAlet config ctx prettyAcc extractAcc aenv pacc
    Apair{}                          -> prettyAtuple config ctx prettyAcc extractAcc aenv pacc
    Anil{}                           -> "()"
    Apply _ _ f a                    -> apply
      where
        op    = Operator ">->" Infix L 1
        apply = sep [ ppAF f, group (sep [opName op, ppA a]) ]

    Acond _ p t e                    -> flatAlt multi single
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


    Atrace _ (Message _ _ msg) as bs -> ppN "atrace"      .$ [ fromString (show msg), ppA as, ppA bs ]
    Aforeign _ _ ff _ a              -> ppN "aforeign"    .$ [ pretty (strForeign ff), ppA a ]
    Awhile _ p f a                   -> ppN "awhile"      .$ [ ppAF p, ppAF f, ppA a ]
    Use _ repr arr                   -> ppN "use"         .$ [ prettyArray repr arr ]
    Unit _ _ e                       -> ppN "unit"        .$ [ ppE e ]
    Reshape _ _ sh a                 -> ppN "reshape"     .$ [ ppE sh, ppA a ]
    Generate _ _ sh f                -> ppN "generate"    .$ [ ppE sh, ppF f ]
    Transform _ _ sh p f a           -> ppN "transform"   .$ [ ppE sh, ppF p, ppF f, ppA a ]
    Replicate _ _ ix a               -> ppN "replicate"   .$ [ ppE ix, ppA a ]
    Slice _ _ a ix                   -> ppN "slice"       .$ [ ppE ix, ppA a ]
    Map _ _ f a                      -> ppN "map"         .$ [ ppF f,  ppA a ]
    ZipWith _ _ f a b                -> ppN "zipWith"     .$ [ ppF f,  ppA a, ppA b ]
    Fold _ f (Just z) a              -> ppN "fold"        .$ [ ppF f,  ppE z, ppA a ]
    Fold _ f Nothing  a              -> ppN "fold1"       .$ [ ppF f,  ppA a ]
    FoldSeg _ _ f (Just z) a s       -> ppN "foldSeg"     .$ [ ppF f,  ppE z, ppA a, ppA s ]
    FoldSeg _ _ f Nothing  a s       -> ppN "fold1Seg"    .$ [ ppF f,  ppA a, ppA s ]
    Scan  _ d f (Just z) a           -> ppD "scan" d ""   .$ [ ppF f,  ppE z, ppA a ]
    Scan  _ d f Nothing  a           -> ppD "scan" d "1"  .$ [ ppF f,  ppA a ]
    Scan' _ d f z a                  -> ppD "scan" d "'"  .$ [ ppF f,  ppE z, ppA a ]
    Permute _ f d p s                -> ppN "permute"     .$ [ ppF f,  ppA d, ppF p, ppA s ]
    Backpermute _ _ sh f a           -> ppN "backpermute" .$ [ ppE sh, ppF f, ppA a ]
    Stencil _ s _ f b a              -> ppN "stencil"     .$ [ ppF f,  ppB (stencilEltR s) b, ppA a ]
    Stencil2 _ s1 s2 _ f b1 a1 b2 a2 -> ppN "stencil2"    .$ [ ppF f,  ppB (stencilEltR s1) b1, ppA a1, ppB (stencilEltR s2) b2, ppA a2 ]
  where
    infixr 0 .$
    f .$ xs
      = parensIf (needsParens ctx f)
      $ hang shiftwidth (sep (manifest f : xs))

    ppN :: String -> Operator
    ppN = confOperator config pacc

    ppA :: acc aenv a -> Adoc
    ppA = prettyAcc config app aenv

    ppAF :: PreOpenAfun acc aenv f -> Adoc
    ppAF = parens . prettyPreOpenAfun config prettyAcc aenv

    ppE :: Exp aenv t -> Adoc
    ppE = prettyOpenExp config app Empty aenv

    ppF :: Fun aenv t -> Adoc
    ppF = parens . prettyOpenFun config Empty aenv

    ppB :: forall sh e.
           TypeR e
        -> Boundary aenv (Array sh e)
        -> Adoc
    ppB _  Clamp        = "clamp"
    ppB _  Mirror       = "mirror"
    ppB _  Wrap         = "wrap"
    ppB tp (Constant e) = prettyConst tp e
    ppB _  (Function f) = ppF f

    ppD :: String -> AST.Direction -> String -> Operator
    ppD f AST.LeftToRight k = ppN (f <> "l" <> k)
    ppD f AST.RightToLeft k = ppN (f <> "r" <> k)


prettyAlet
    :: forall acc aenv arrs.
       PrettyConfig acc
    -> Context
    -> PrettyAcc acc
    -> ExtractAcc acc
    -> Val aenv
    -> PreOpenAcc acc aenv arrs
    -> Adoc
prettyAlet config ctx prettyAcc extractAcc aenv0
  = parensIf (ctxPrecedence ctx > 0)
  . align . wrap . collect aenv0
  where
    collect :: Val aenv' -> PreOpenAcc acc aenv' a -> ([Adoc], Adoc)
    collect aenv =
      \case
        Alet _ lhs a1 a2 ->
          let (aenv', v)      = prettyALhs False aenv lhs
              a1'             = ppA aenv a1
              bnd | isAlet a1 = nest shiftwidth (vsep [v <+> equals, a1'])
                  | otherwise = v <+> align (equals <+> a1')
              (bnds, body)    = collect aenv' (extractAcc a2)
          in
          (bnd:bnds, body)
        --
        next       -> ([], prettyPreOpenAcc config context0 prettyAcc extractAcc aenv next)

    isAlet :: acc aenv' a -> Bool
    isAlet (extractAcc -> Alet{}) = True
    isAlet _                      = False

    ppA :: Val aenv' -> acc aenv' a -> Adoc
    ppA = prettyAcc config context0

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
       PrettyConfig acc
    -> Context
    -> PrettyAcc acc
    -> ExtractAcc acc
    -> Val aenv
    -> PreOpenAcc acc aenv arrs
    -> Adoc
prettyAtuple config ctx prettyAcc extractAcc aenv0 acc = case collect acc of
    Nothing  -> align $ ppPair acc
    Just tup ->
      case tup of
        []  -> "()"
        _   -> align $ parensIf (ctxPrecedence ctx > 0) ("T" <> pretty (length tup) <+> align (sep tup))
  where
    ppPair :: PreOpenAcc acc aenv arrs' -> Adoc
    ppPair (Apair _ a1 a2) =
      "(" <> ppPair (extractAcc a1) <> "," <+> prettyAcc config context0 aenv0 a2 <> ")"
    ppPair a             = prettyPreOpenAcc config context0 prettyAcc extractAcc aenv0 a

    collect :: PreOpenAcc acc aenv arrs' -> Maybe [Adoc]
    collect Anil{}        = Just []
    collect (Apair _ a1 a2)
      | Just tup <- collect $ extractAcc a1
                          = Just $ tup ++ [prettyAcc config app aenv0 a2]
    collect _             = Nothing

-- TODO: Should we also print the types of the declared variables? And the types of wildcards?
prettyALhs :: Bool -> Val env -> LeftHandSide s arrs env env' -> (Val env', Adoc)
prettyALhs requiresParens = prettyLhs requiresParens 'a'

prettyELhs :: Bool -> Val env -> LeftHandSide s arrs env env' -> (Val env', Adoc)
prettyELhs requiresParens = prettyLhs requiresParens 'x'

-- TODO: We never print the annotations stored in the 'LeftHandSideSingle's.
--       Doing so would probably make everything more difficult to read, but we
--       do print the annotations on the use site of the bindings which might be
--       a bit inconsistent.
prettyLhs :: forall s env env' arrs. Bool -> Char -> Val env -> LeftHandSide s arrs env env' -> (Val env', Adoc)
prettyLhs requiresParens x env0 lhs = case collect lhs of
  Nothing          -> ppPair lhs
  Just (env1, tup) ->
    case tup of
      []  -> (env1, "()")
      _   -> (env1, parensIf requiresParens (pretty 'T' <> pretty (length tup) <+> align (sep tup)))
  where
    ppPair :: LeftHandSide s arrs' env env'' -> (Val env'', Adoc)
    ppPair LeftHandSideUnit       = (env0, "()")
    ppPair LeftHandSideWildcard{} = (env0, "_")
    ppPair LeftHandSideSingle{}   = (env0 `Push` v, v)
      where
        v = pretty x <> pretty (sizeEnv env0)
    ppPair (LeftHandSidePair a b)          = (env2, tupled [doc1, doc2])
      where
        (env1, doc1) = ppPair a
        (env2, doc2) = prettyLhs False x env1 b

    collect :: LeftHandSide s arrs' env env'' -> Maybe (Val env'', [Adoc])
    collect (LeftHandSidePair l1 l2)
      | Just (env1, tup ) <- collect l1
      ,      (env2, doc2) <- prettyLhs True x env1 l2 = Just (env2, tup ++ [doc2])
    collect (LeftHandSideWildcard TupRunit) = Just (env0, [])
    collect _ = Nothing

prettyArray :: ArrayR (Array sh e) -> Array sh e -> Adoc
prettyArray aR@(ArrayR _ eR) = parens . fromString . showArray (showsElt eR) aR


-- Annotations
-- -----------

prettyAnn :: PrettyConfig acc -> Ann -> Adoc
prettyAnn config ann = fromMaybe emptyDoc (prettyAnn' config ann)

-- | Format an annotation based on the current verbosity setting. If we would
-- end up printing an empty document, we'll return Nothing so we can omit it
-- when printing the AST. Note that we purposely don't use wild cards or the
-- record's functions. Otherwise it can be easy to forget to add a new
-- annotation to the pretty printer.
--
-- TODO: Make this, well, prettier
-- TODO: We may also want to show the information added by `context`, although
--       cluttering up the pretty printer output too much likely also won't do
--       us any good.
prettyAnn' :: PrettyConfig acc -> Ann -> Maybe Adoc
prettyAnn' config (Ann src (Optimizations { optAlwaysInline, optUnrollIters })) =
  case catMaybes [prettyLoc, prettyOpts] of
    [] -> Nothing
    xs -> Just $ annotate Annotation (hsep xs)
 where
  verbosity = confAnnotationVerbosity config

  -- Depending on the verbosity setting, we'll want to either print no source
  -- information, only print known source information, or also signal to the
  -- programmer that source information is missing.
  prettyLoc :: Maybe Adoc
  prettyLoc = case mergeLocs src of
    -- We'll only show the 'SrcLoc' belonging to the topmost entry of the call
    -- stack. This topmost function will be one of the Accelerate frontend
    -- functions so the name isn't very important, but the source location will
    -- be in user code. (if we did everything right with freezing call stacks)
    (((_fn, loc) : _) : rest) | verbosity >= Normal ->
      let prettyBegin = srcLocFile loc
            <> ":"
            <> show (srcLocStartLine loc)
            <> ":"
            <> show (srcLocStartCol loc)
          prettyEnd = show (srcLocEndLine loc)
            <> ":"
            <> show (srcLocEndCol loc)
          -- TODO: Should we always show how many other disjoint source
          --       locations a node has, or should we only show this in verbose
          --       mode?
          nRest = length rest
          prettyRest = if nRest > 0 then " (+" <> show nRest <> ")" else ""
      in  Just . pretty $ if verbosity >= Verbose
            then prettyBegin <> "-" <> prettyEnd <> prettyRest
            else prettyBegin
    [] | verbosity >= Verbose -> Just "<unknown>"
    _                         -> Nothing

  -- We'll print the enabled optimizations like a Haskell record. If no
  -- optimization flags are enabled, then we won't print anything to avoid
  -- cluttering the pretty printer output.
  prettyOpts :: Maybe Adoc
  prettyOpts =
    let enabledOpts = catMaybes
          [ if optAlwaysInline
              then Just "alwaysInline = True"
              else Nothing
          , case optUnrollIters of
              Just n -> Just $ "unrollIters = " <> unsafeViaShow n
              _      -> Nothing
          ]
    in  if not (null enabledOpts)
          then Just $ align (encloseSep "{ " " }" ", " enabledOpts)
          else Nothing

-- | If the argument has an annotation, append that annotation to the pretty
-- printer output.
--
-- TODO: Doing it this way results in a ton of clutter, and it can be difficult
--       to read so this is more of a temporary solution. To help with ambiguity
--       we'll just always add parentheses around scalar-level expressions for
--       now. 'align' also doesn't always place the annotation where you'd
--       expect it to be.
maybeWithAnn :: HasAnnotations a => PrettyConfig acc -> Bool -> a -> Adoc -> Adoc
maybeWithAnn config withParens x doc
  | confAnnotationVerbosity config > Quiet
  , Just ann <- getAnn x
  , Just pAnn <- prettyAnn' config ann
  = parensIf withParens $ sep [doc, align . annotate Annotation $ comment pAnn]
  | otherwise
  = doc


-- Scalar expressions
-- ------------------

prettyFun :: PrettyConfig acc -> Val aenv -> Fun aenv f -> Adoc
prettyFun config = prettyOpenFun config Empty

prettyExp :: PrettyConfig acc -> Val aenv -> Exp aenv t -> Adoc
prettyExp config = prettyOpenExp config context0 Empty

prettyOpenFun
    :: forall acc env aenv f.
       PrettyConfig acc
    -> Val env
    -> Val aenv
    -> OpenFun env aenv f
    -> Adoc
prettyOpenFun config env0 aenv = next (pretty '\\') env0
  where
    next :: Adoc -> Val env' -> OpenFun env' aenv f' -> Adoc
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
                             , prettyOpenExp config context0 env aenv body])
    next vs env (Lam lhs lam) =
      let (env', lhs') = prettyELhs True env lhs
      in  next (vs <> lhs' <> space) env' lam

-- TODO: On the default verbosity level we probably don't need to show all
--       expression-level source information. We may want to still display this
--       information in a couple of situations, like when indexing arrays.
prettyOpenExp
    :: forall acc env aenv t.
       PrettyConfig acc
    -> Context
    -> Val env
    -> Val aenv
    -> OpenExp env aenv t
    -> Adoc
prettyOpenExp config ctx env aenv exp =
  maybeWithAnn config True exp $ case exp of
    Evar (Var _ _ idx)     -> prj idx env
    Let{}                  -> prettyLet config ctx env aenv exp
    PrimApp _ f x
      | Pair _ a b <- x    -> ppF2 op  (ppE a) (ppE b)
      | otherwise          -> ppF1 op' (ppE x)
      where
        op  = primOperator f
        op' = isInfix op ? (Operator (parens (opName op)) App L 10, op)
    --
    PrimConst _ c          -> prettyPrimConst c
    Const _ tp c           -> prettyConst (TupRsingle tp) c
    Pair{}                 -> prettyTuple config ctx env aenv exp
    Nil _                  -> "()"
    VecPack   _ _ e        -> ppF1 "pack"   (ppE e)
    VecUnpack _ _ e        -> ppF1 "unpack" (ppE e)
    Case _ x xs d          -> prettyCase config env aenv x xs d
    Cond _ p t e           -> flatAlt multi single
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
    IndexSlice _ _ slix sh -> ppF2 "indexSlice"  (ppE slix) (ppE sh)
    IndexFull _ _ slix sl  -> ppF2 "indexFull"   (ppE slix) (ppE sl)
    ToIndex _ _ sh ix      -> ppF2 "toIndex"     (ppE sh) (ppE ix)
    FromIndex _ _ sh ix    -> ppF2 "fromIndex"   (ppE sh) (ppE ix)
    While _ p f x          -> ppF3 "while"       (ppF p) (ppF f) (ppE x)
    Foreign _ _ ff _ e     -> ppF2 "foreign"     (\_ -> pretty (strForeign ff)) (ppE e)
    Shape _ arr            -> ppF1 "shape"       (ppA arr)
    ShapeSize _ _ sh       -> ppF1 "shapeSize"   (ppE sh)
    Index _ arr ix         -> ppF2 (Operator (pretty '!') Infix L 9) (ppA arr) (ppE ix)
    LinearIndex _ arr ix   -> ppF2 (Operator "!!"         Infix L 9) (ppA arr) (ppE ix)
    Coerce _ _ tp x        -> ppF1 (Operator (withTypeRep tp "coerce") App L 10) (ppE x)
    Undef _ tp             -> withTypeRep tp "undef"

  where
    ppE :: OpenExp env aenv e -> Context -> Adoc
    ppE e c = prettyOpenExp config c env aenv e

    ppA :: ArrayVar aenv a -> Context -> Adoc
    ppA acc _ = prettyArrayVar aenv acc

    ppF :: OpenFun env aenv f -> Context -> Adoc
    ppF f _ = parens $ prettyOpenFun config env aenv f

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

    withTypeRep :: ScalarType t -> Adoc -> Adoc
    withTypeRep t op = op <+> "@" <> pretty (show t)

prettyArrayVar
    :: forall aenv a.
       Val aenv
    -> ArrayVar aenv a
    -> Adoc
prettyArrayVar aenv (Var _ _ idx) = prj idx aenv

prettyLet
    :: forall acc env aenv t.
       PrettyConfig acc
    -> Context
    -> Val env
    -> Val aenv
    -> OpenExp env aenv t
    -> Adoc
prettyLet config ctx env0 aenv
  = parensIf (ctxPrecedence ctx > 0)
  . align . wrap . collect env0
  where
    collect :: Val env' -> OpenExp env' aenv e -> ([Adoc], Adoc)
    collect env =
      \case
        Let _ lhs e1 e2 ->
          let (env', v)       = prettyELhs False env lhs
              e1'             = ppE env e1
              bnd | isLet e1  = nest shiftwidth (vsep [v <+> equals, e1'])
                  | otherwise = v <+> align (equals <+> e1')
              (bnds, body)    = collect env' e2
          in
          (bnd:bnds, body)
        --
        next     -> ([], ppE env next)

    isLet :: OpenExp env' aenv t' -> Bool
    isLet Let{} = True
    isLet _     = False

    ppE :: Val env' -> OpenExp env' aenv t' -> Adoc
    ppE env = prettyOpenExp config context0 env aenv

    wrap :: ([Adoc], Adoc) -> Adoc
    wrap ([],   body) = body  -- shouldn't happen!
    wrap ([b],  body)
      = sep [ nest shiftwidth (sep [let_, b]), in_, body ]
    wrap (bnds, body)
      = vsep [ nest shiftwidth (vsep [let_, sepBy (flatAlt "" " ; ") bnds])
             , in_
             , body
             ]

    sepBy :: Adoc -> [Adoc] -> Adoc
    sepBy = encloseSep mempty mempty

prettyTuple
    :: forall acc env aenv t.
       PrettyConfig acc
    -> Context
    -> Val env
    -> Val aenv
    -> OpenExp env aenv t
    -> Adoc
prettyTuple config ctx env aenv exp = case collect exp of
    Nothing  -> align $ ppPair exp
    Just tup ->
      case tup of
        []  -> "()"
        _   -> align $ parensIf (ctxPrecedence ctx > 0) ("T" <> pretty (length tup) <+> align (sep tup))
  where
    ppPair :: OpenExp env aenv t' -> Adoc
    ppPair (Pair _ e1 e2) = "(" <> ppPair e1 <> "," <+> prettyOpenExp config context0 env aenv e2 <> ")"
    ppPair e              = prettyOpenExp config context0 env aenv e

    collect :: OpenExp env aenv t' -> Maybe [Adoc]
    collect (Nil _)            = Just []
    collect (Pair _ e1 e2)
      | Just tup <- collect e1 = Just $ tup ++ [prettyOpenExp config app env aenv e2]
    collect _                  = Nothing

prettyCase
    :: PrettyConfig acc
    -> Val env
    -> Val aenv
    -> OpenExp env aenv a
    -> [(TAG, OpenExp env aenv b)]
    -> Maybe (OpenExp env aenv b)
    -> Adoc
prettyCase config env aenv x xs def
  = hang shiftwidth
  $ vsep [ case_ <+> x' <+> of_
         , flatAlt (vcat xs') (encloseSep "{ " " }" "; " xs')
         ]
  where
    x'  = prettyOpenExp config context0 env aenv x
    xs' = map (\(t,e) -> pretty t <+> "->" <+> prettyOpenExp config context0 env aenv e) xs
       ++ case def of
            Nothing -> []
            Just d  -> ["_" <+> "->" <+> prettyOpenExp config context0 env aenv d]

{-

prettyAtuple
    :: forall acc aenv arrs.
       PrettyAcc acc
    -> ExtractAcc acc
    -> Val aenv
    -> PreOpenAcc acc aenv arrs
    -> Adoc
prettyAtuple prettyAcc extractAcc aenv0 acc = case collect acc of
  Just tup -> align $ "T" <> pretty (length tup) <+> sep tup
  Nothing  -> align $ ppPair acc
  where
    ppPair :: PreOpenAcc acc aenv arrs' -> Adoc
    ppPair (Apair a1 a2) = "(" <> ppPair (extractAcc a1) <> "," <+> prettyAcc context0 aenv0 a2 <> ")"
    ppPair a             = prettyPreOpenAcc context0 prettyAcc extractAcc aenv0 a

    collect :: PreOpenAcc acc aenv arrs' -> Maybe [Adoc]
    collect Anil          = Just []
    collect (Apair a1 a2)
      | Just tup <- collect $ extractAcc a1
                          = Just $ tup ++ [prettyAcc app aenv0 a2]
    collect _             = Nothing
-}

prettyConst :: TypeR e -> e -> Adoc
prettyConst tp x =
  let y = showElt tp x
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

-- | Format something as if it were enclosed in Haskell block comments. Useful
-- for adding additional information while still mostly keeping the ability to
-- convert the pretty printed AST back to Haskell code.
comment :: Doc ann -> Doc ann
comment = enclose "{- " " -}"


-- Debugging
-- ---------

-- Unfortunately, using unsafePerformIO here means that the getFlag will be
-- evaluated only once when the first 'show' is performed on a Delayed value;
-- afterwards, the thunk will have been evaluated, and all future pretty-print
-- outputs will use the same result.
-- This cannot be prevented using a NOINLINE pragma, since then the function
-- itself is still a thunk that will only be evaluated once.
--
-- The practical result of this is that @setFlag verbose@ will not change
-- anything after a Delayed has already been printed once.
--
-- TODO: The verbose flag now also controls the verbosity level of the
--       annotations in the pretty printer. We should probably rename this.
shouldPrintHash :: Bool
shouldPrintHash = unsafePerformIO $ getFlag verbose
