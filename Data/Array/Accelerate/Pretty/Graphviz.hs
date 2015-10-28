{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}
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
import Data.IntMap                                      ( IntMap )
import Text.Printf
import Text.PrettyPrint
import System.Mem.StableName
import Control.Monad.State
import Control.Arrow                                    ( (***), (&&&), first )
import Control.Applicative                              hiding ( Const, empty )
import Prelude                                          hiding ( exp )
import qualified Data.IntMap                            as IM

-- friends
import Data.Array.Accelerate.AST                        ( PreOpenAcc(..), PreOpenFun(..), PreOpenExp(..) )
import Data.Array.Accelerate.Array.Sugar                ( Array, Elt, EltRepr, Tuple(..), Atuple(..), arrays, toElt, strForeign )
import Data.Array.Accelerate.Type                       ( Boundary(..) )
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Trafo.Base
import Data.Array.Accelerate.Pretty.Print               as PP
import qualified Data.Array.Accelerate.AST              as AST


-- Configuration options
--
cfgUniqueOnly, cfgIncludeShape, cfgSimple :: Bool
cfgUniqueOnly   = False
cfgIncludeShape = False
cfgSimple       = False


type Dot a    = StateT DotState IO a
data DotState = DotState
  { fresh       :: {-# UNPACK #-} !Int
  , nodeMap     :: IntMap NodeId
  , dotEdges    :: [Edge]
  , dotNodes    :: [Node]
  , dotGraph    :: [Graph]
  }


nodeIdOfLabel :: Label -> Dot NodeId
nodeIdOfLabel l =
  case show l of
    'a':rest | [(n,[])] <- reads rest -> state $ \s -> (nodeMap s IM.! n, s)
    _                                 -> $internalError "nodeIdOfLabel" "expected array variable"

mkLabel :: NodeId -> Dot Label
mkLabel nid = state $ \s ->
  let n = fresh s
  in
  ( char 'a' <> int n
  , s { fresh   = n + 1
      , nodeMap = IM.insert n nid (nodeMap s)
      }
  )

mkNodeId :: a -> Dot NodeId
mkNodeId node = do
  sn    <- liftIO $ makeStableName node
  return $ sn `seq` NodeId (text (printf "Node_%#0x" (hashStableName sn))) Nothing

addNode :: NodeId -> Maybe Label -> Doc -> Dot ()
addNode nid label body =
  modify (\s -> s { dotNodes = Node nid label body : dotNodes s })

addEdge :: NodeId -> NodeId -> Dot ()
addEdge from to =
  modify (\s -> s { dotEdges = Edge from to : dotEdges s })


graphDelayedAcc
    :: Val aenv
    -> DelayedOpenAcc aenv a
    -> IO Graph
graphDelayedAcc aenv acc = do
  s <- flip execStateT (DotState 0 IM.empty [] [] [])
      $ do
          (r, fvs) <- prettyDelayedOpenAcc noParens aenv acc
          to       <- mkNodeId r
          addNode to Nothing r
          mapM (flip addEdge to) =<< mapM nodeIdOfLabel fvs
  return
    $ Graph empty
    $ [ N n | n <- dotNodes s ] ++
      [ E e | e <- dotEdges s ] ++
      [ G g | g <- dotGraph s ]



-- Partially pretty-printed node, containing the body and free variables
-- which we'll need to draw edges for.
--
type Node' = (Doc, [Label])

prettyDelayedOpenAcc
    :: forall aenv arrs.
       (Doc -> Doc)
    -> Val aenv
    -> DelayedOpenAcc aenv arrs
    -> Dot Node'
prettyDelayedOpenAcc wrap aenv = ppA
  where
    (.$) :: String -> [Dot Node'] -> Dot Node'
    name .$ ns
      = (wrap . hang (text name) 2 . sep *** uniq . concat)
      . first (if cfgSimple then const [] else id)
      . unzip <$> sequence ns

    ppF :: DelayedFun aenv t -> Dot Node'
    ppF = return . (parens . prettyDelayedFun aenv &&& fvDelayedFun aenv)

    ppE :: DelayedExp aenv t -> Dot Node'
    ppE  = return . (prettyDelayedExp parens aenv &&& fvDelayedExp aenv)

    ppSh :: DelayedExp aenv sh -> Dot Node'
    ppSh = return . (parens . prettyDelayedExp noParens aenv &&& fvDelayedExp aenv)

    ppA :: DelayedOpenAcc aenv a -> Dot Node'
    ppA (Manifest pacc)  = pp pacc
    ppA (Delayed sh f _)
      | Shape a   <- sh
      , Just REFL <- match f (Lam (Body (Index a (Var AST.ZeroIdx))))
      = ppA a
      --
      | otherwise
      = first parens <$> "Delayed" .$ [ ppSh sh, ppF f ]

    ppB :: forall sh e. Elt e
        => {-dummy-} DelayedOpenAcc aenv (Array sh e)
        -> Boundary (EltRepr e)
        -> Dot Node'
    ppB _ Clamp        = return (text "Clamp", [])
    ppB _ Mirror       = return (text "Mirror", [])
    ppB _ Wrap         = return (text "Wrap", [])
    ppB _ (Constant e) = return (parens $ text "Constant" <+> text (show (toElt e :: e)), [])

    pp :: forall a. PreOpenAcc DelayedOpenAcc aenv a -> Dot Node'
    pp (Avar ix)       = let v = prj ix aenv in return (v, [v])
    pp (Alet bnd body) = do
      to          <- mkNodeId bnd
      a           <- mkLabel to
      --
      (bnd', fvs) <- prettyDelayedOpenAcc noParens aenv            bnd
      body'       <- prettyDelayedOpenAcc noParens (aenv `Push` a) body
      --
      addNode to (Just a) bnd'
      mapM (flip addEdge to) =<< mapM nodeIdOfLabel fvs
      return body'

    -- pp (Apply afun acc)         = error "Apply" -- wrap $ sep [ ppAF afun, ppA acc ]
    -- pp (Awhile p afun acc)      = error "Awhile" -- "awhile" .$ [ppAF p, ppAF afun, ppA acc]
    -- pp (Acond e acc1 acc2)      = error "Acond" -- wrap $ sep [ ppE e, text "?|", tuple [ppA acc1, ppA acc2] ]

    pp (Atuple atup)            = prettyDelayedAtuple aenv atup
    pp (Aprj ix arrs)           = first (wrap . (char '#' <> prettyTupleIdx ix <+>)) <$> ppA arrs

    pp (Use arrs)               = "use"         .$ [ return $ (prettyArrays (arrays (undefined::a)) arrs, []) ]
    pp (Unit e)                 = "unit"        .$ [ ppE e ]
    pp (Generate sh f)          = "generate"    .$ [ ppSh sh, ppF f ]
    pp (Transform sh ix f xs)   = "transform"   .$ [ ppSh sh, ppF ix, ppF f, ppA xs ]
    pp (Reshape sh xs)          = "reshape"     .$ [ ppSh sh, ppA xs ]
    pp (Replicate _ty ix xs)    = "replicate"   .$ [ ppSh ix, ppA xs ]
    pp (Slice _ty xs ix)        = "slice"       .$ [ ppA xs, ppSh ix ]
    pp (Map f xs)               = "map"         .$ [ ppF f, ppA xs ]
    pp (ZipWith f xs ys)        = "zipWith"     .$ [ ppF f, ppA xs, ppA ys ]
    pp (Fold f e xs)            = "fold"        .$ [ ppF f, ppE e, ppA xs ]
    pp (Fold1 f xs)             = "fold1"       .$ [ ppF f, ppA xs ]
    pp (FoldSeg f e xs ys)      = "foldSeg"     .$ [ ppF f, ppE e, ppA xs, ppA ys ]
    pp (Fold1Seg f xs ys)       = "fold1Seg"    .$ [ ppF f, ppA xs, ppA ys ]
    pp (Scanl f e xs)           = "scanl"       .$ [ ppF f, ppE e, ppA xs ]
    pp (Scanl' f e xs)          = "scanl'"      .$ [ ppF f, ppE e, ppA xs ]
    pp (Scanl1 f xs)            = "scanl1"      .$ [ ppF f, ppA xs ]
    pp (Scanr f e xs)           = "scanr"       .$ [ ppF f, ppE e, ppA xs ]
    pp (Scanr' f e xs)          = "scanr'"      .$ [ ppF f, ppE e, ppA xs ]
    pp (Scanr1 f xs)            = "scanr1"      .$ [ ppF f, ppA xs ]
    pp (Permute f dfts p xs)    = "permute"     .$ [ ppF f, prettyDelayedOpenAcc parens aenv dfts, ppF p, ppA xs ]
    pp (Backpermute sh p xs)    = "backpermute" .$ [ ppSh sh, ppF p, ppA xs ]
    pp (Aforeign ff _afun xs)   = "aforeign"    .$ [ return (text (strForeign ff), []), {- ppAf afun, -} ppA xs ]
    pp (Stencil sten bndy xs)   = "stencil"     .$ [ ppF sten, ppB xs bndy, prettyDelayedOpenAcc parens aenv xs ]
    pp (Stencil2 sten bndy1 acc1 bndy2 acc2)
                                = "stencil2"    .$ [ ppF sten, ppB acc1 bndy1, prettyDelayedOpenAcc parens aenv acc1,
                                                               ppB acc2 bndy2, prettyDelayedOpenAcc parens aenv acc2 ]

    pp (Collect s)              = error "Collect"


prettyDelayedAtuple
    :: forall aenv atup.
       Val aenv
    -> Atuple (DelayedOpenAcc aenv) atup
    -> Dot Node'
prettyDelayedAtuple aenv atup = (tuple . reverse *** concat) <$> collect atup
  where
    collect :: Atuple (DelayedOpenAcc aenv) t -> Dot ([Doc], [[Label]])
    collect NilAtup           = return ([], [])
    collect (SnocAtup t a) = do
      a' <- prettyDelayedOpenAcc noParens aenv a
      t' <- collect t
      return (fst a':fst t', snd a':snd t')


prettyDelayedFun :: Val aenv -> DelayedFun aenv f -> Doc
prettyDelayedFun = prettyDelayedOpenFun Empty

prettyDelayedOpenFun
    :: forall env aenv f.
       Val env
    -> Val aenv
    -> DelayedOpenFun env aenv f
    -> Doc
prettyDelayedOpenFun env aenv fun = text "\\\\" <> next env fun
  where
    -- graphviz will silently not print a label containing the string "->",
    -- so instead we use the special token "&rarr" for a short right arrow.
    --
    next :: Val env' -> PreOpenFun DelayedOpenAcc env' aenv f' -> Doc
    next env' (Body body) = text "&rarr;" <+> prettyDelayedOpenExp noParens env' aenv body
    next env' (Lam fun')  =
      let x = char 'x' <> int (sizeEnv env')
      in  x <+> next (env' `Push` x) fun'


prettyDelayedExp
    :: (Doc -> Doc)
    -> Val aenv
    -> DelayedExp aenv t
    -> Doc
prettyDelayedExp wrap = prettyDelayedOpenExp wrap Empty

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



-- Extract all the free array variables used by a scalar expression.
--
-- TODO: This only returns unique labels, but graphviz is happy to draw
--       duplicate edges. Perhaps we can use this, and draw _all_ edges
--       that come from array accesses (only), ignoring shapes since those
--       will not contribute to fusion.
--
fvDelayedExp :: Val aenv -> DelayedExp aenv t -> [Label]
fvDelayedExp = fvDelayedOpenExp Empty

fvDelayedOpenExp
    :: forall env aenv exp.
       Val env
    -> Val aenv
    -> DelayedOpenExp env aenv exp
    -> [Label]
fvDelayedOpenExp env aenv = uniq . fv
  where
    fvA :: DelayedOpenAcc aenv a -> [Label]
    fvA (Manifest (Avar idx))   = [ prj idx aenv ]
    fvA _                       = $internalError "GraphViz.freeVars" "expected array variable"

    fvT :: Tuple (DelayedOpenExp env aenv) t -> [Label]
    fvT NilTup          = []
    fvT (SnocTup tup e) = fv e ++ fvT tup

    fvF :: DelayedOpenFun env aenv f -> [Label]
    fvF = fvDelayedOpenFun env aenv

    fv :: DelayedOpenExp env aenv t -> [Label]
    fv (Shape acc)              = if cfgIncludeShape then fvA acc else []
    fv (Index acc i)            = concat [ fvA acc, fv i ]
    fv (LinearIndex acc i)      = concat [ fvA acc, fv i ]
    --
    fv (Let e1 e2)              = concat [ fv e1, fvDelayedOpenExp (env `Push` (char 'x' <> int (sizeEnv env))) aenv e2 ]
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


fvDelayedFun :: Val aenv -> DelayedFun aenv f -> [Label]
fvDelayedFun = fvDelayedOpenFun Empty

fvDelayedOpenFun
    :: Val env
    -> Val aenv
    -> DelayedOpenFun env aenv f
    -> [Label]
fvDelayedOpenFun env aenv (Body b) = fvDelayedOpenExp env aenv b
fvDelayedOpenFun env aenv (Lam f)  = fvDelayedOpenFun (env `Push` (char 'x' <> int (sizeEnv env))) aenv f



-- GraphViz
-- --------

-- Simple data-type for pretty-printing graphs
--
type Label     = Doc
type Port      = Label
data Statement = N Node | E Edge | G Graph
data NodeId    = NodeId Label (Maybe Port)
data Node      = Node NodeId (Maybe Label) Doc
data Edge      = Edge {- from -} NodeId {- to -} NodeId
data Graph     = Graph Label [Statement]


-- Pretty print a (directed) graph
--
ppGraph :: Graph -> Doc
ppGraph (Graph l ss) =
  vcat [ text "digraph" <+> l <+> lbrace
       , nest 4 $ vcat
                $ punctuate semi
                $ text "graph [compound=true]"
                : text "node  [shape=record,fontsize=10]"
                : map ppStatement ss
       , rbrace
       ]

ppSubgraph :: Graph -> Doc
ppSubgraph (Graph l ss) =
  vcat [ text "subgraph cluster_" <> l <+> lbrace
       , nest 4 $ vcat
                $ punctuate semi
                $ map ppStatement ss
       , rbrace
       ]

ppStatement :: Statement -> Doc
ppStatement (N n) = ppNode n
ppStatement (E e) = ppEdge e
ppStatement (G g) = ppSubgraph g

ppEdge :: Edge -> Doc
ppEdge (Edge from to) = ppNodeId from <+> text "->" <+> ppNodeId to

ppNode :: Node -> Doc
ppNode (Node nid xl l) =
  hcat [ ppNodeId nid
       , brackets
       $ hcat
       $ punctuate comma
       $ catMaybes [ fmap ((text "xlabel" <> equals <>) . doubleQuotes) xl
                   , Just (text "label"  <> equals <> doubleQuotes (braces (ljust l)))
                   ]
       ]
  where
    -- In order for the text to be properly rendered by graphviz, we need
    -- to escape some special characters. Newlines '\n' also need be be
    -- replaced with '\l', to ensure that the text is left justified rather
    -- than centred. The last line also needs a final '\l'.
    --
    ljust :: Doc -> Doc
    ljust doc = (text . concatMap escape $ renderStyle wide doc) <> text "\\l"

    wide :: Style
    wide = style { lineLength = 200 }

    escape :: Char -> String
    escape ' '  = "\\ "         -- don't collapse multiple spaces
    escape '>'  = "\\>"
    escape '<'  = "\\<"
    escape '|'  = "\\|"
    escape '\n' = "\\l"
    escape c    = [c]

ppNodeId :: NodeId -> Doc
ppNodeId (NodeId l p) = l <> maybe empty (colon<>) p

uniq :: Eq a => [a] -> [a]
uniq = if cfgUniqueOnly then nub else id

