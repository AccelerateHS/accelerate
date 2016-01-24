{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE ViewPatterns         #-}
-- |
-- Module      : Data.Array.Accelerate.Pretty.Graphviz
-- Copyright   : [2015] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Data.Array.Accelerate.Pretty.Graphviz
  where

-- standard libraries
import Data.List
import Data.Maybe
import Data.HashSet                                     ( HashSet )
import Text.PrettyPrint
import Control.Monad.State                              ( modify, gets )
import Control.Arrow                                    ( (***), (&&&), first )
import Control.Applicative                              hiding ( Const, empty )
import Prelude                                          hiding ( exp )
import qualified Data.Sequence                          as Seq
import qualified Data.HashSet                           as Set

-- friends
import Data.Array.Accelerate.AST                        ( PreOpenAcc(..), PreOpenAfun(..), PreOpenFun(..), PreOpenExp(..), Idx(..) )
import Data.Array.Accelerate.Array.Sugar                ( Array, Elt, EltRepr, Tuple(..), Atuple(..), arrays, toElt, strForeign )
import Data.Array.Accelerate.Type                       ( Boundary(..) )
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Trafo.Base
import Data.Array.Accelerate.Pretty.Print
import Data.Array.Accelerate.Pretty.Graphviz.Monad
import Data.Array.Accelerate.Pretty.Graphviz.Type


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
avalToVal (Apush aenv _ v) = Push (avalToVal aenv) v

aprj :: Idx aenv t -> Aval aenv -> (NodeId, Label)        -- TLM: (Vertex, Label) ??
aprj ZeroIdx      (Apush _    n v) = (n,v)
aprj (SuccIdx ix) (Apush aenv _ _) = aprj ix aenv
aprj _            _                = error "inconsistent valuation"


-- Graph construction
-- ------------------

mkNode :: a -> PNode -> Maybe Label -> Dot NodeId
mkNode this (tree, deps) label = do
  ident <- mkNodeId this
  --
  let node  = Node label ident tree
      edges = Seq.fromList
            $ map (\(from, to) -> Edge from (Vertex ident to))
            $ if cfgUnique then nub deps else deps
  --
  modify $ \s ->
    s { dotNodes = node  Seq.<| dotNodes s
      , dotEdges = edges Seq.>< dotEdges s
      }
  return ident

-- Add [T|F] ports underneath the given tree.
--
addTFPorts :: Tree (Maybe Port, Doc) -> Tree (Maybe Port, Doc)
addTFPorts this =
  Forest [ this
         , Forest [ Leaf (Just "T", "T")
                  , Leaf (Just "F", "F")
                  ]
         ]


-- Pretty-printing data-dependency graphs
-- --------------------------------------

-- Partially constructed graph nodes, consists of some body text and a list of
-- vertices which we will draw edges from (and later, the port we connect into).
--
type PDoc  = (Doc,                    [Vertex])
type PNode = (Tree (Maybe Port, Doc), [(Vertex, Maybe Port)])


graphDelayedAcc :: Bool -> DelayedAcc a -> Dot Graph
graphDelayedAcc simple = graphDelayedOpenAcc simple Aempty

graphDelayedOpenAcc
    :: Bool
    -> Aval aenv
    -> DelayedOpenAcc aenv a
    -> Dot Graph
graphDelayedOpenAcc simple aenv acc = do
  r <- prettyDelayedOpenAcc simple noParens aenv acc
  _ <- mkNode r r Nothing -- terminal node, so its NodeID doesn't matter
  mkGraph

-- Generate a graph for the given term.
--
prettyDelayedOpenAcc
    :: forall aenv arrs.
       Bool                                 -- simplified output: only print operator name
    -> (Doc -> Doc)
    -> Aval aenv
    -> DelayedOpenAcc aenv arrs
    -> Dot PNode
prettyDelayedOpenAcc _      _    _    Delayed{}       = $internalError "prettyDelayedOpenAcc" "expected manifest array"
prettyDelayedOpenAcc simple wrap aenv (Manifest pacc) = pp pacc
  where
    ppM :: DelayedOpenAcc aenv a -> Dot PDoc
    ppM (Manifest (Avar ix)) = return (avar ix)
    ppM _                    = $internalError "prettyDelayedOpenAcc" "expected array variable"

    ppD :: DelayedOpenAcc aenv a -> Dot PDoc
    ppD (Delayed sh f _)
      | Shape a    <- sh                                             -- identical shape
      , Just REFL  <- match f (Lam (Body (Index a (Var ZeroIdx))))   -- identity function
      = ppM a
    ppD (Delayed sh f _) = first parens <$> "Delayed" `fmt` [ ppSh sh, ppF f ]
    ppD Manifest{}       = $internalError "prettyDelayedOpenAcc" "expected delayed array"

    (.$) :: String -> [Dot PDoc] -> Dot PNode
    name .$ docs = pnode <$> fmt name docs

    fmt :: String -> [Dot PDoc] -> Dot PDoc
    fmt name docs = do
      (args,fvs) <- unzip <$> sequence docs
      return      $ ( hang (text name) 2 (if simple then empty else sep args)
                    , concat fvs )

    pnode :: PDoc -> PNode
    pnode (doc,vs) =
      let port = Nothing
      in  (Leaf (port, doc), map (,port) vs)

    -- Free variables
    --
    fvA :: FVAcc DelayedOpenAcc
    fvA env (Manifest (Avar ix)) = [ Vertex (fst $ aprj ix env) Nothing ]
    fvA _   _                    = $internalError "graphviz" "expected array variable"

    fvF :: DelayedFun aenv t -> [Vertex]
    fvF = fvPreOpenFun fvA Empty aenv

    fvE :: DelayedExp aenv t -> [Vertex]
    fvE = fvPreOpenExp fvA Empty aenv

    -- Pretty-printing
    --
    avar :: Idx aenv t -> PDoc
    avar ix = let (ident, v) = aprj ix aenv
              in  (v, [Vertex ident Nothing])

    aenv' :: Val aenv
    aenv' = avalToVal aenv

    ppB :: forall sh e. Elt e
        => {-dummy-} DelayedOpenAcc aenv (Array sh e)
        -> Boundary (EltRepr e)
        -> Dot PDoc
    ppB _ Clamp        = return ("Clamp",  [])
    ppB _ Mirror       = return ("Mirror", [])
    ppB _ Wrap         = return ("Wrap",   [])
    ppB _ (Constant e) = return (parens $ "Constant" <+> text (show (toElt e :: e)), [])

    ppF :: DelayedFun aenv t -> Dot PDoc
    ppF = return . (parens . prettyDelayedFun aenv' &&& fvF)

    ppE :: DelayedExp aenv t -> Dot PDoc
    ppE = return . (prettyDelayedExp parens aenv' &&& fvE)

    ppSh :: DelayedExp aenv sh -> Dot PDoc
    ppSh = return . (parens . prettyDelayedExp noParens aenv' &&& fvE)

    lift :: DelayedOpenAcc aenv a -> Dot Vertex
    lift Delayed{}            = $internalError "prettyDelayedOpenAcc" "expected manifest array"
    lift (Manifest (Avar ix)) = return $ Vertex (fst (aprj ix aenv)) Nothing
    lift acc                  = do
      acc'  <- prettyDelayedOpenAcc simple noParens aenv acc
      ident <- mkNode acc acc' Nothing
      return $ Vertex ident Nothing

    apply :: DelayedOpenAfun aenv (a -> b) -> DelayedOpenAcc aenv a -> Dot PNode
    apply f a = do
      (a',deps) <- prettyDelayedOpenAcc simple parens aenv a
      f'        <- prettyDelayedAfun    simple        aenv f
      return $ case a' of
        Leaf (p,d)   -> ( Leaf (p, f' <+> d),                  deps )
        Forest trees -> ( Forest (Leaf (Nothing, f') : trees), deps ) -- XXX: ???

    -- Our main pretty-printer needs to return a PNode structure, because cases
    -- such as 'Acond' require the additional port structure. *sigh*
    --
    pp :: forall a. PreOpenAcc DelayedOpenAcc aenv a -> Dot PNode
    pp (Avar ix)                = return (pnode (avar ix))
    pp (Alet bnd body)          = do
      bnd'  <- prettyDelayedOpenAcc simple noParens aenv                 bnd
      a     <- mkLabel
      ident <- mkNode bnd bnd' (Just a)
      body' <- prettyDelayedOpenAcc simple noParens (Apush aenv ident a) body
      return body'

    pp (Acond p t e)            = do
      vt       <- lift t
      ve       <- lift e
      (p', vs) <- ppE p
      let port = Just "P"
          doc  = addTFPorts $ Leaf (port, if simple then "?|" else p')
          deps = (vt, Just "T") : (ve, Just "F") : map (,port) vs
      return (doc,deps)

    pp (Apply afun acc)         = apply afun acc
    pp Awhile{}                 = error "Awhile"

    pp (Atuple atup)            = prettyDelayedAtuple simple aenv atup
    pp (Aprj ix atup)           = do
      (atup', deps) <- prettyDelayedOpenAcc simple noParens aenv atup
      return $ case atup' of
        Leaf (port,doc)  -> (Leaf (port, wrap (char '#' <> prettyTupleIdx ix <+> doc)), deps )
        Forest rest      -> (Forest [ Leaf (Nothing, char '#' <> prettyTupleIdx ix), Forest rest], deps )

    pp (Use arrs)               = "use"         .$ [ return $ (prettyArrays (arrays (undefined::a)) arrs, []) ]
    pp (Unit e)                 = "unit"        .$ [ ppE e ]
    pp (Generate sh f)          = "generate"    .$ [ ppSh sh, ppF f ]
    pp (Transform sh ix f xs)   = "transform"   .$ [ ppSh sh, ppF ix, ppF f, ppD xs ]
    pp (Reshape sh xs)          = "reshape"     .$ [ ppSh sh, ppM xs ]
    pp (Replicate _ty ix xs)    = "replicate"   .$ [ ppSh ix, ppD xs ]
    pp (Slice _ty xs ix)        = "slice"       .$ [ ppD xs, ppSh ix ]
    pp (Map f xs)               = "map"         .$ [ ppF f, ppD xs ]
    pp (ZipWith f xs ys)        = "zipWith"     .$ [ ppF f, ppD xs, ppD ys ]
    pp (Fold f e xs)            = "fold"        .$ [ ppF f, ppE e, ppD xs ]
    pp (Fold1 f xs)             = "fold1"       .$ [ ppF f, ppD xs ]
    pp (FoldSeg f e xs ys)      = "foldSeg"     .$ [ ppF f, ppE e, ppD xs, ppD ys ]
    pp (Fold1Seg f xs ys)       = "fold1Seg"    .$ [ ppF f, ppD xs, ppD ys ]
    pp (Scanl f e xs)           = "scanl"       .$ [ ppF f, ppE e, ppD xs ]
    pp (Scanl' f e xs)          = "scanl'"      .$ [ ppF f, ppE e, ppD xs ]
    pp (Scanl1 f xs)            = "scanl1"      .$ [ ppF f, ppD xs ]
    pp (Scanr f e xs)           = "scanr"       .$ [ ppF f, ppE e, ppD xs ]
    pp (Scanr' f e xs)          = "scanr'"      .$ [ ppF f, ppE e, ppD xs ]
    pp (Scanr1 f xs)            = "scanr1"      .$ [ ppF f, ppD xs ]
    pp (Permute f dfts p xs)    = "permute"     .$ [ ppF f, ppM dfts, ppF p, ppD xs ]
    pp (Backpermute sh p xs)    = "backpermute" .$ [ ppSh sh, ppF p, ppD xs ]
    pp (Aforeign ff _afun xs)   = "aforeign"    .$ [ return (text (strForeign ff), []), {- ppAf afun, -} ppM xs ]
    pp (Stencil sten bndy xs)   = "stencil"     .$ [ ppF sten, ppB xs bndy, ppM xs ]
    pp (Stencil2 sten bndy1 acc1 bndy2 acc2)
                                = "stencil2"    .$ [ ppF sten, ppB acc1 bndy1, ppM acc1,
                                                               ppB acc2 bndy2, ppM acc2 ]
    pp Collect{}                = error "Collect"


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
    :: Bool
    -> Aval aenv
    -> DelayedOpenAfun aenv afun
    -> Dot Label
prettyDelayedAfun simple aenv afun = do
  Graph _ ss  <- mkSubgraph (go aenv afun)
  n           <- Seq.length <$> gets dotGraph
  let label         = "afun" <> int (n+1)
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
    go aenv' (Abody b) = graphDelayedOpenAcc simple aenv' b
    go aenv' (Alam  f) = do
      a     <- mkLabel
      ident <- mkNode f (Leaf (Nothing,a), []) Nothing
      go (Apush aenv' ident a) f

    collect :: Aval aenv' -> HashSet NodeId
    collect Aempty        = Set.empty
    collect (Apush a i _) = Set.insert i (collect a)


-- Display array tuples, which ends up being a bit tricky (and potentially quite
-- ugly>
--
prettyDelayedAtuple
    :: forall aenv atup.
       Bool
    -> Aval aenv
    -> Atuple (DelayedOpenAcc aenv) atup
    -> Dot PNode
prettyDelayedAtuple simple aenv atup = (forest *** concat) . unzip . reverse <$> collect 0 atup
  where
    collect :: Int -> Atuple (DelayedOpenAcc aenv) t -> Dot [PNode]
    collect _ NilAtup        = return []
    collect n (SnocAtup t a) = (:) <$> relabel n `fmap` prettyDelayedOpenAcc simple noParens aenv a
                                   <*> collect (n+1) t

    -- relabel ports and incoming edges in order to point to the correct
    -- sub-regions of the node
    --
    relabel :: Int -> PNode -> PNode
    relabel n (ts,vs) =
      let
          paint Nothing     = Just (int n)
          paint (Just port) = Just (port <> int n)
      in
      ( fmap (\(p,d) -> (paint p, d)) ts
      , fmap (\(v,p) -> (v, paint p)) vs )

    -- How should we display array tuples?
    --
    -- The problem is when we have tuples containing Acond, and thus a [T|F]
    -- sub-structure in the node. Since there could be multiple such structures
    -- in this node, we display all fields of the tuple in a separate box. This
    -- looks a little strange, but makes the dependency relationships clear.
    --
    -- For the common case where we don't have this sub-structure, then just
    -- output a single node containing the usual pretty-printed output. This
    -- tends to make the arrows drawn by graphviz look better as well since it
    -- has more flexibility in their placement.
    --
    forest :: [Tree (Maybe Port, Doc)] -> Tree (Maybe Port, Doc)
    forest trees =
      let leaves Forest{} = False
          leaves Leaf{}   = True
      in
      if all leaves trees
         then Leaf (Nothing, tuple [ d | Leaf (_,d) <- trees ])
         else Forest trees


-- Pretty printing scalar functions and expressions
-- ------------------------------------------------
--
-- This is done with the usual machinery. Note that we rely on knowing that all
-- array operations will be lifted out of scalar expressions. This means that we
-- don't really need to recurse into the scalar terms to uncover new graph
-- nodes.
--

prettyDelayedFun :: Val aenv -> DelayedFun aenv f -> Doc
prettyDelayedFun = prettyDelayedOpenFun Empty

prettyDelayedExp :: (Doc -> Doc) -> Val aenv -> DelayedExp aenv t -> Doc
prettyDelayedExp wrap = prettyDelayedOpenExp wrap Empty


prettyDelayedOpenFun
    :: forall env aenv f.
       Val env
    -> Val aenv
    -> DelayedOpenFun env aenv f
    -> Doc
prettyDelayedOpenFun env aenv fun = "\\\\" <> next env fun
  where
    -- graphviz will silently not print a label containing the string "->",
    -- so instead we use the special token "&rarr" for a short right arrow.
    --
    next :: Val env' -> PreOpenFun DelayedOpenAcc env' aenv f' -> Doc
    next env' (Body body) = "&rarr;" <+> prettyDelayedOpenExp noParens env' aenv body
    next env' (Lam fun')  =
      let x = char 'x' <> int (sizeEnv env')
      in  x <+> next (env' `Push` x) fun'

prettyDelayedOpenExp
    :: (Doc -> Doc)
    -> Val env
    -> Val aenv
    -> DelayedOpenExp env aenv t
    -> Doc
prettyDelayedOpenExp = prettyPreOpenExp pp
  where
    pp :: PrettyAcc DelayedOpenAcc
    pp _ aenv (Manifest (Avar ix)) = prj ix aenv
    pp _ _    _                    = $internalError "prettyDelayedOpenExp" "expected array variable"


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
fvPreOpenFun fvA env aenv (Lam f)  = fvPreOpenFun fvA (env `Push` (char 'x' <> int (sizeEnv env))) aenv f

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
    fv (Let e1 e2)              = concat [ fv e1, fvPreOpenExp fvA (env `Push` (char 'x' <> int (sizeEnv env))) aenv e2 ]
    fv Var{}                    = []
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

