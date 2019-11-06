{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PatternGuards        #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns         #-}
-- |
-- Module      : Data.Array.Accelerate.Pretty.Graphviz
-- Copyright   : [2015..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Data.Array.Accelerate.Pretty.Graphviz (

  Graph,
  PrettyGraph(..), Detail(..),

  graphDelayedAcc, graphDelayedAfun,

) where

-- standard libraries
import Control.Applicative                              hiding ( Const, empty )
import Control.Arrow                                    ( (&&&) )
import Control.Monad.State                              ( modify, gets, state )
import Data.HashSet                                     ( HashSet )
import Data.List
import Data.Maybe
import Data.String
import Data.Text.Prettyprint.Doc
import System.IO.Unsafe                                 ( unsafePerformIO )
import Prelude                                          hiding ( exp )
import qualified Data.HashSet                           as Set
import qualified Data.Sequence                          as Seq

-- friends
import Data.Array.Accelerate.AST                        ( PreOpenAcc(..), PreOpenAfun(..), PreOpenFun(..), PreOpenExp(..), PreBoundary(..), LeftHandSide(..), ArrayVar(..), Idx(..) )
import Data.Array.Accelerate.Array.Sugar                ( Array, Elt, Tuple(..), ArraysR(..), toElt, strForeign )
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Pretty.Graphviz.Monad
import Data.Array.Accelerate.Pretty.Graphviz.Type
import Data.Array.Accelerate.Pretty.Print               hiding ( Keyword(..) )
import Data.Array.Accelerate.Trafo.Base


-- Configuration options
-- ---------------------

cfgIncludeShape, cfgUnique :: Bool
cfgIncludeShape = False             -- draw edges for uses of shape information
cfgUnique       = False             -- draw a single edge per data dependency


-- Environments
-- ------------

-- This is the standard environment typed by de Bruijn indices, where at each
-- index we need to record both the pretty printed label as well its 'NodeId',
-- which we use to track data dependencies.
--
data Aval env where
  Aempty ::                                Aval ()
  Apush  :: Aval env -> NodeId -> Label -> Aval (env, t)

-- Convert to the 'Val' used by the base pretty printing module by stripping out
-- the 'NodeId' part.
--
avalToVal :: Aval aenv -> Val aenv
avalToVal Aempty           = Empty
avalToVal (Apush aenv _ v) = Push (avalToVal aenv) (pretty v)

aprj :: Idx aenv t -> Aval aenv -> (NodeId, Label)        -- TLM: (Vertex, Label) ??
aprj ZeroIdx      (Apush _    n v) = (n,v)
aprj (SuccIdx ix) (Apush aenv _ _) = aprj ix aenv
#if __GLASGOW_HASKELL__ < 800
aprj _            _                = $internalError "aprj" "inconsistent valuation"
#endif


-- Graph construction
-- ------------------

mkNode :: PNode -> Maybe Label -> Dot NodeId
mkNode (PNode ident tree deps) label =
  let node  = Node label ident tree
      edges = Seq.fromList
            $ map (\(from, to) -> Edge from (Vertex ident to))
            $ if cfgUnique then nub deps else deps
  in
  state $ \s ->
    ( ident
    , s { dotNodes = node  Seq.<| dotNodes s
        , dotEdges = edges Seq.>< dotEdges s
        }
    )


-- Add [T|F] ports underneath the given tree.
--
mkTF :: Tree (Maybe Port, Adoc) -> Tree (Maybe Port, Adoc)
mkTF this =
  Forest [ this
         , Forest [ Leaf (Just "T", "T")
                  , Leaf (Just "F", "F")
                  ]
         ]


-- Pretty Printing
-- ===============
--
-- The use of unsafePerformIO in the below is safe in the sense that we only
-- require IO to recover the stable names of terms. At worst, if we do not
-- recover the correct stable name for some reason, we will be left with
-- dandling edges in the graph.
--

class PrettyGraph g where
  ppGraph :: Detail -> g -> Graph

instance PrettyGraph (DelayedAcc a) where
  ppGraph = graphDelayedAcc

instance PrettyGraph (DelayedAfun a) where
  ppGraph = graphDelayedAfun

data Detail = Simple | Full

simple :: Detail -> Bool
simple Simple = True
simple _      = False

-- | Generate a dependency graph for the given computation
--
{-# NOINLINE graphDelayedAcc #-}
graphDelayedAcc :: Detail -> DelayedAcc a -> Graph
graphDelayedAcc detail acc =
  unsafePerformIO $! evalDot (graphDelayedOpenAcc detail Aempty acc)

-- | Generate a dependency graph for an array function
--
{-# NOINLINE graphDelayedAfun #-}
graphDelayedAfun :: Detail -> DelayedAfun f -> Graph
graphDelayedAfun detail afun = unsafePerformIO . evalDot $! do
  l <- prettyDelayedAfun detail Aempty afun
  state $ \s ->
    case Seq.viewl (dotGraph s) of
      g@(Graph l' _) Seq.:< gs | l == l' -> (g, s { dotGraph = gs })
      _                                  -> $internalError "graphDelaydAfun" "unexpected error"


-- Pretty-printing data-dependency graphs
-- --------------------------------------

-- Partially constructed graph nodes, consists of some body text and a list of
-- vertices which we will draw edges from (and later, the port we connect into).
--
data PDoc  = PDoc Adoc [Vertex]
data PNode = PNode NodeId (Tree (Maybe Port, Adoc)) [(Vertex, Maybe Port)]

graphDelayedOpenAcc
    :: Detail
    -> Aval aenv
    -> DelayedOpenAcc aenv a
    -> Dot Graph
graphDelayedOpenAcc detail aenv acc = do
  r <- prettyDelayedOpenAcc detail context0 aenv acc
  i <- mkNodeId r
  v <- mkNode r Nothing
  _ <- mkNode (PNode i (Leaf (Nothing,"result")) [(Vertex v Nothing, Nothing)]) Nothing
  mkGraph

-- Generate a graph for the given term.
--
prettyDelayedOpenAcc
    :: forall aenv arrs.
       Detail                               -- simplified output: only print operator name
    -> Context
    -> Aval aenv
    -> DelayedOpenAcc aenv arrs
    -> Dot PNode
prettyDelayedOpenAcc _      _   _    Delayed{}            = $internalError "prettyDelayedOpenAcc" "expected manifest array"
prettyDelayedOpenAcc detail ctx aenv atop@(Manifest pacc) =
  case pacc of
    Avar ix                 -> pnode (avar ix)
    Alet lhs bnd body       -> do
      bnd'@(PNode ident _ _) <- prettyDelayedOpenAcc detail context0 aenv bnd
      (aenv1, a) <- prettyLetLeftHandSide ident aenv lhs
      _ <- mkNode bnd' (Just a)
      body' <- prettyDelayedOpenAcc detail context0 aenv1 body
      return body'

    Acond p t e             -> do
      ident <- mkNodeId atop
      vt    <- lift t
      ve    <- lift e
      PDoc p' vs <- ppE p
      let port = Just "P"
          doc  = mkTF $ Leaf (port, if simple detail then "?|" else p')
          deps = (vt, Just "T") : (ve, Just "F") : map (,port) vs
      return $ PNode ident doc deps

    Apply afun acc          -> apply <$> prettyDelayedAfun    detail     aenv afun
                                     <*> prettyDelayedOpenAcc detail ctx aenv acc

    Awhile p f x            -> do
      ident <- mkNodeId atop
      x'    <- replant =<< prettyDelayedOpenAcc detail app aenv x
      p'    <- prettyDelayedAfun detail aenv p
      f'    <- prettyDelayedAfun detail aenv f
      --
      let PNode _ (Leaf (Nothing,xb)) fvs = x'
          loop                            = nest 2 (sep ["awhile", pretty p', pretty f', xb ])
      return $ PNode ident (Leaf (Nothing,loop)) fvs

    a@(Apair a1 a2)         -> mkNodeId a >>= prettyDelayedApair detail aenv a1 a2

    Anil                    -> "()"          .$ []

    Use repr arrs           -> "use"         .$ [ return $ PDoc (prettyArrays repr arrs) [] ]
    Unit e                  -> "unit"        .$ [ ppE e ]
    Generate sh f           -> "generate"    .$ [ ppE sh, ppF f ]
    Transform sh ix f xs    -> "transform"   .$ [ ppE sh, ppF ix, ppF f, ppA xs ]
    Reshape sh xs           -> "reshape"     .$ [ ppE sh, ppA xs ]
    Replicate _ty ix xs     -> "replicate"   .$ [ ppE ix, ppA xs ]
    Slice _ty xs ix         -> "slice"       .$ [ ppA xs, ppE ix ]
    Map f xs                -> "map"         .$ [ ppF f, ppA xs ]
    ZipWith f xs ys         -> "zipWith"     .$ [ ppF f, ppA xs, ppA ys ]
    Fold f e xs             -> "fold"        .$ [ ppF f, ppE e, ppA xs ]
    Fold1 f xs              -> "fold1"       .$ [ ppF f, ppA xs ]
    FoldSeg f e xs ys       -> "foldSeg"     .$ [ ppF f, ppE e, ppA xs, ppA ys ]
    Fold1Seg f xs ys        -> "fold1Seg"    .$ [ ppF f, ppA xs, ppA ys ]
    Scanl f e xs            -> "scanl"       .$ [ ppF f, ppE e, ppA xs ]
    Scanl' f e xs           -> "scanl'"      .$ [ ppF f, ppE e, ppA xs ]
    Scanl1 f xs             -> "scanl1"      .$ [ ppF f, ppA xs ]
    Scanr f e xs            -> "scanr"       .$ [ ppF f, ppE e, ppA xs ]
    Scanr' f e xs           -> "scanr'"      .$ [ ppF f, ppE e, ppA xs ]
    Scanr1 f xs             -> "scanr1"      .$ [ ppF f, ppA xs ]
    Permute f dfts p xs     -> "permute"     .$ [ ppF f, ppA dfts, ppF p, ppA xs ]
    Backpermute sh p xs     -> "backpermute" .$ [ ppE sh, ppF p, ppA xs ]
    Stencil sten bndy xs    -> "stencil"     .$ [ ppF sten, ppB bndy, ppA xs ]
    Stencil2 sten bndy1 acc1 bndy2 acc2
                            -> "stencil2"    .$ [ ppF sten, ppB bndy1, ppA acc1, ppB bndy2, ppA acc2 ]
    Aforeign ff _afun xs    -> "aforeign"    .$ [ return (PDoc (pretty (strForeign ff)) []), {- ppAf afun, -} ppA xs ]
    -- Collect{}               -> error "Collect"

  where
    (.$) :: Operator -> [Dot PDoc] -> Dot PNode
    name .$ docs = pnode =<< fmt name docs

    fmt :: Operator -> [Dot PDoc] -> Dot PDoc
    fmt name docs = do
      docs' <- sequence docs
      let args = [ x | PDoc x _ <- docs' ]
          fvs  = [ x | PDoc _ x <- docs' ]
          doc  = if simple detail
                   then manifest name
                   else parensIf (needsParens ctx name)
                      $ nest shiftwidth
                      $ sep ( manifest name : args )
      return $ PDoc doc (concat fvs)

    pnode :: PDoc -> Dot PNode
    pnode (PDoc doc vs) = do
      let port = Nothing
      ident <- mkNodeId atop
      return $ PNode ident (Leaf (port, doc)) (map (,port) vs)

    -- Free variables
    --
    fvA :: FVAcc DelayedOpenAcc
    fvA env (Manifest (Avar (ArrayVar ix))) = [ Vertex (fst $ aprj ix env) Nothing ]
    fvA _   _                               = $internalError "graphviz" "expected array variable"

    fvF :: DelayedFun aenv t -> [Vertex]
    fvF = fvPreOpenFun fvA Empty aenv

    fvE :: DelayedExp aenv t -> [Vertex]
    fvE = fvPreOpenExp fvA Empty aenv

    -- Pretty-printing
    --
    avar :: ArrayVar aenv t -> PDoc
    avar (ArrayVar ix) = let (ident, v) = aprj ix aenv
                         in  PDoc (pretty v) [Vertex ident Nothing]

    aenv' :: Val aenv
    aenv' = avalToVal aenv

    ppA :: DelayedOpenAcc aenv a -> Dot PDoc
    ppA (Manifest (Avar ix)) = return (avar ix)
    ppA acc@Manifest{}       = do
      -- Lift out and draw as a separate node. This can occur with the manifest
      -- array arguments to permute (defaults array) and stencil[2].
      acc'  <- prettyDelayedOpenAcc detail app aenv acc
      v     <- mkLabel
      ident <- mkNode acc' (Just v)
      return $ PDoc (pretty v) [Vertex ident Nothing]
    ppA (Delayed sh f _)
      | Shape a    <- sh                                             -- identical shape
      , Just Refl  <- match f (Lam (Body (Index a (Var ZeroIdx))))   -- identity function
      = ppA a
    ppA (Delayed sh f _) = do
      PDoc d v <- "Delayed" `fmt` [ ppE sh, ppF f ]
      return    $ PDoc (parens d) v

    ppB :: forall sh e. Elt e
        => PreBoundary DelayedOpenAcc aenv (Array sh e)
        -> Dot PDoc
    ppB Clamp        = return (PDoc "clamp"  [])
    ppB Mirror       = return (PDoc "mirror" [])
    ppB Wrap         = return (PDoc "wrap"   [])
    ppB (Constant e) = return (PDoc (prettyConst (toElt e :: e)) [])
    ppB (Function f) = ppF f

    ppF :: DelayedFun aenv t -> Dot PDoc
    ppF = return . uncurry PDoc . (parens . prettyDelayedFun aenv' &&& fvF)

    ppE :: DelayedExp aenv t -> Dot PDoc
    ppE = return . uncurry PDoc . (prettyDelayedExp aenv' &&& fvE)

    lift :: DelayedOpenAcc aenv a -> Dot Vertex
    lift Delayed{}                       = $internalError "prettyDelayedOpenAcc" "expected manifest array"
    lift (Manifest (Avar (ArrayVar ix))) = return $ Vertex (fst (aprj ix aenv)) Nothing
    lift acc                             = do
      acc'  <- prettyDelayedOpenAcc detail context0 aenv acc
      ident <- mkNode acc' Nothing
      return $ Vertex ident Nothing

    apply :: Label -> PNode -> PNode
    apply f (PNode ident x vs) =
      let x' = case x of
                 Leaf (p,d) -> Leaf (p, pretty f <+> d)
                 Forest ts  -> Forest (Leaf (Nothing,pretty f) : ts)
      in
      PNode ident x' vs


-- Pretty print array functions as separate sub-graphs, and return the name of
-- the sub-graph as if it can be called like a function. We will add additional
-- nodes at the top of the graph to represent the bound variables.
--
-- Note: [Edge placement]
--
-- If a node belongs to a particular graph, so too must all its edges (and
-- vertices). This means that if the subgraph references anything from the
-- enclosing environment, we must lift those edges out of this subgraph,
-- otherwise the referenced node will be drawn inside of the subgraph.
--
prettyDelayedAfun
    :: Detail
    -> Aval aenv
    -> DelayedOpenAfun aenv afun
    -> Dot Label
prettyDelayedAfun detail aenv afun = do
  Graph _ ss  <- mkSubgraph (go aenv afun)
  n           <- Seq.length <$> gets dotGraph
  let label         = "afun" <> fromString (show (n+1))
      outer         = collect aenv
      (lifted,ss')  =
        flip partition ss $ \s ->
        case s of
          E (Edge (Vertex ident _) _) -> Set.member ident outer
          _                           -> False
  --
  modify $ \s -> s { dotGraph = dotGraph s                         Seq.|> Graph label ss'
                   , dotEdges = Seq.fromList [ e | E e <- lifted ] Seq.>< dotEdges s
                   }
  return label
  where
    go :: Aval aenv' -> DelayedOpenAfun aenv' a' -> Dot Graph
    go aenv' (Abody b) = graphDelayedOpenAcc detail aenv' b
    go aenv' (Alam lhs f) = do
      aenv'' <- prettyLambdaLeftHandSide aenv' lhs
      go aenv'' f

    collect :: Aval aenv' -> HashSet NodeId
    collect Aempty        = Set.empty
    collect (Apush a i _) = Set.insert i (collect a)

prettyLetLeftHandSide
  :: forall repr aenv aenv'.
     NodeId
  -> Aval aenv
  -> LeftHandSide repr aenv aenv'
  -> Dot (Aval aenv', Label)
prettyLetLeftHandSide _     aenv (LeftHandSideWildcard repr) = return (aenv, doc)
  where
    doc = case repr of
      ArraysRunit -> "()"
      _           -> "_"
prettyLetLeftHandSide ident aenv LeftHandSideArray = do
  a <- mkLabel
  return (Apush aenv ident a, a)
prettyLetLeftHandSide ident aenv (LeftHandSidePair lhs1 lhs2) = do
  (aenv1, d1) <- prettyLetLeftHandSide ident aenv  lhs1
  (aenv2, d2) <- prettyLetLeftHandSide ident aenv1 lhs2
  return (aenv2, "(" <> d1 <> ", " <> d2 <> ")")

prettyLambdaLeftHandSide
    :: forall repr aenv aenv'.
       Aval aenv
    -> LeftHandSide repr aenv aenv'
    -> Dot (Aval aenv')
prettyLambdaLeftHandSide aenv (LeftHandSideWildcard _) = return aenv
prettyLambdaLeftHandSide aenv lhs@LeftHandSideArray = do
  a     <- mkLabel
  ident <- mkNodeId lhs
  _     <- mkNode (PNode ident (Leaf (Nothing, pretty a)) []) Nothing
  return $ Apush aenv ident a
prettyLambdaLeftHandSide aenv (LeftHandSidePair lhs1 lhs2) = do
  aenv1 <- prettyLambdaLeftHandSide aenv lhs1
  prettyLambdaLeftHandSide aenv1 lhs2

-- Display array tuples. This is a little tricky...
--
prettyDelayedApair
    :: forall aenv a1 a2.
       Detail
    -> Aval aenv
    -> DelayedOpenAcc aenv a1
    -> DelayedOpenAcc aenv a2
    -> NodeId
    -> Dot PNode
prettyDelayedApair detail aenv a1 a2 ident = do
  PNode id1 t1 v1 <- prettyElem a1
  PNode id2 t2 v2 <- prettyElem a2
  modify $ \s -> s { dotEdges = fmap (redirect ident [id1, id2]) (dotEdges s) }
  return $ PNode ident (forest [t1, t2]) (v1 ++ v2)
  where
    prettyElem :: DelayedOpenAcc aenv a -> Dot PNode
    prettyElem a = replant =<< prettyDelayedOpenAcc detail context0 aenv a

    -- Redirect any edges that pointed into one of the nodes now part of this
    -- tuple, to instead point to the container node.
    --
    redirect :: NodeId -> [NodeId] -> Edge -> Edge
    redirect new subs edge@(Edge from (Vertex to port))
      | to `elem` subs = Edge from (Vertex new port)
      | otherwise      = edge

    -- Since we have lifted out any non-leaves into separate nodes, we can
    -- simply tuple-up all of the elements.
    --
    forest :: [Tree (Maybe Port, Adoc)] -> Tree (Maybe Port, Adoc)
    forest leaves = Leaf (Nothing, tupled [ align d | Leaf (Nothing,d) <- leaves ])


-- Lift out anything that isn't a Leaf node and output it to the graph
-- immediately as a new labelled node.
--
replant :: PNode -> Dot PNode
replant pnode@(PNode ident tree _) =
  case tree of
    Leaf (Nothing, _) -> return pnode
    _                 -> do
      vacuous <- mkNodeId pnode
      a       <- mkLabel
      _       <- mkNode pnode (Just a)
      return   $ PNode vacuous (Leaf (Nothing, pretty a)) [(Vertex ident Nothing, Nothing)]


-- Pretty printing scalar functions and expressions
-- ------------------------------------------------
--
-- This is done with the usual machinery. Note that we rely on knowing that all
-- array operations will be lifted out of scalar expressions. This means that we
-- don't really need to recurse into the scalar terms to uncover new graph
-- nodes.
--

prettyDelayedFun :: Val aenv -> DelayedFun aenv f -> Adoc
prettyDelayedFun = prettyDelayedOpenFun Empty

prettyDelayedExp :: Val aenv -> DelayedExp aenv t -> Adoc
prettyDelayedExp = prettyDelayedOpenExp context0 Empty


prettyDelayedOpenFun
    :: forall env aenv f.
       Val env
    -> Val aenv
    -> DelayedOpenFun env aenv f
    -> Adoc
prettyDelayedOpenFun env0 aenv = next "\\\\" env0
  where
    -- graphviz will silently not print a label containing the string "->",
    -- so instead we use the special token "&rarr" for a short right arrow.
    --
    next :: Adoc -> Val env' -> PreOpenFun DelayedOpenAcc env' aenv f' -> Adoc
    next vs env (Body body) =
      nest shiftwidth (sep [ vs <> "&rarr;"
                           , prettyDelayedOpenExp context0 env aenv body ])
    next vs env (Lam  lam)  =
      let x = pretty 'x' <> pretty (sizeEnv env)
      in  next (vs <> x <> space) (env `Push` x) lam

prettyDelayedOpenExp
    :: Context
    -> Val env
    -> Val aenv
    -> DelayedOpenExp env aenv t
    -> Adoc
prettyDelayedOpenExp context = prettyPreOpenExp context pp ex
  where
    pp :: PrettyAcc DelayedOpenAcc
    pp _ aenv (Manifest (Avar (ArrayVar ix))) = prj ix aenv
    pp _ _    _                               = $internalError "prettyDelayedOpenExp" "expected array variable"

    ex :: ExtractAcc DelayedOpenAcc
    ex (Manifest pacc) = pacc
    ex Delayed{}       = $internalError "prettyDelayedOpenExp" "expected manifest array"


-- Data dependencies
-- -----------------
--
-- Return the data-dependencies of the given term. This is just a tree traversal
-- to extract all of the free variables. We will draw an edge from each of those
-- nodes (vertices) into the current term.
--

type FVAcc acc = forall aenv a. Aval aenv -> acc aenv a -> [Vertex]

fvPreOpenFun
    :: forall acc env aenv fun.
       FVAcc acc
    -> Val env
    -> Aval aenv
    -> PreOpenFun acc env aenv fun
    -> [Vertex]
fvPreOpenFun fvA env aenv (Body b) = fvPreOpenExp fvA env                                          aenv b
fvPreOpenFun fvA env aenv (Lam f)  = fvPreOpenFun fvA (env `Push` (pretty 'x' <> pretty (sizeEnv env))) aenv f

fvPreOpenExp
    :: forall acc env aenv exp.
       FVAcc acc
    -> Val env
    -> Aval aenv
    -> PreOpenExp acc env aenv exp
    -> [Vertex]
fvPreOpenExp fvA env aenv = fv
  where
    fvT :: Tuple (PreOpenExp acc env aenv) t -> [Vertex]
    fvT NilTup          = []
    fvT (SnocTup tup e) = concat [ fv e, fvT tup ]

    fvF :: PreOpenFun acc env aenv f -> [Vertex]
    fvF = fvPreOpenFun fvA env aenv

    fv :: PreOpenExp acc env aenv e -> [Vertex]
    fv (Shape acc)              = if cfgIncludeShape then fvA aenv acc else []
    fv (Index acc i)            = concat [ fvA aenv acc, fv i ]
    fv (LinearIndex acc i)      = concat [ fvA aenv acc, fv i ]
    --
    fv (Let e1 e2)              = concat [ fv e1, fvPreOpenExp fvA (env `Push` (pretty 'x' <> pretty (sizeEnv env))) aenv e2 ]
    fv Var{}                    = []
    fv Undef                    = []
    fv Const{}                  = []
    fv PrimConst{}              = []
    fv (PrimApp _ x)            = fv x
    fv (Tuple tup)              = fvT tup
    fv (Prj _ e)                = fv e
    fv IndexNil                 = []
    fv IndexAny                 = []
    fv (IndexHead sh)           = fv sh
    fv (IndexTail sh)           = fv sh
    fv (IndexCons t h)          = concat [ fv t, fv h ]
    fv (IndexSlice _ slix sh)   = concat [ fv slix, fv sh ]
    fv (IndexFull _ slix sh)    = concat [ fv slix, fv sh ]
    fv (ToIndex sh ix)          = concat [ fv sh, fv ix ]
    fv (FromIndex sh ix)        = concat [ fv sh, fv ix ]
    fv (Union sh1 sh2)          = concat [ fv sh1, fv sh2 ]
    fv (Intersect sh1 sh2)      = concat [ fv sh1, fv sh2 ]
    fv (ShapeSize sh)           = fv sh
    fv Foreign{}                = []
    fv (Cond p t e)             = concat [ fv p, fv t, fv e ]
    fv (While p f x)            = concat [ fvF p, fvF f, fv x ]
    fv (Coerce e)               = fv e

