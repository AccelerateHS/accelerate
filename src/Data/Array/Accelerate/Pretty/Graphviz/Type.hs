{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns       #-}
-- |
-- Module      : Data.Array.Accelerate.Pretty.Graphviz.Type
-- Copyright   : [2015..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Simple data types for representing (simple, directed) graphs and
-- pretty-printing to Graphviz dot format.
--
module Data.Array.Accelerate.Pretty.Graphviz.Type
  where

import Data.Hashable
import Data.Maybe
import Data.Text                                          ( Text )
import Prettyprinter
import Text.Printf
import qualified Data.Text                                as Text

import Data.Array.Accelerate.Pretty.Print                 ( Adoc, Keyword )


-- Rose tree, with all information at the leaves.
--
data Tree a = Leaf a
            | Forest [Tree a]

instance Functor Tree where
  fmap f (Leaf x)    = Leaf (f x)
  fmap f (Forest xs) = Forest (map (fmap f) xs)


-- Representation for simple Graphviz graphs
--
data Graph      = Graph Label [Statement]
data Statement  = N Node | E Edge | G Graph

data Node       = Node (Maybe Label) NodeId (Tree (Maybe Port, Adoc))
data NodeId     = NodeId !Int

type Label      = Text
type Port       = Text

data Vertex     = Vertex NodeId (Maybe Port)
data Edge       = Edge {- from -} Vertex
                       {-  to  -} Vertex

deriving instance Eq NodeId
deriving instance Eq Vertex

instance Hashable NodeId where
  hashWithSalt salt (NodeId ident) = hashWithSalt salt ident

instance Show Graph where
  show = show . ppGraph

-- Pretty print a (directed) graph to dot format
--
ppGraph :: Graph -> Adoc
ppGraph (Graph l ss) =
  vcat [ "digraph" <+> pretty l <+> lbrace
       , nest 4 $ vcat
                $ punctuate semi
                $ "graph [compound=true]"
                : "node  [shape=record,fontsize=10]"
                : map ppStatement ss
       , rbrace
       ]

ppSubgraph :: Graph -> Adoc
ppSubgraph (Graph l ss) =
  vcat [ "subgraph cluster_" <> pretty l <+> lbrace
       , nest 4 $ vcat
                $ punctuate semi
                $ "label" <> equals <> pretty l
                : map ppStatement ss
       , rbrace
       ]

ppStatement :: Statement -> Adoc
ppStatement (N n) = ppNode n
ppStatement (E e) = ppEdge e
ppStatement (G g) = ppSubgraph g

ppEdge :: Edge -> Adoc
ppEdge (Edge from to) = ppVertex from <+> "->" <+> ppVertex to

ppVertex :: Vertex -> Adoc
ppVertex (Vertex n p) = ppNodeId n <> maybe mempty (colon<>) (fmap pretty p)

ppNode :: Node -> Adoc
ppNode (Node label nid body) =
  hcat [ ppNodeId nid
       , brackets
       $ hcat
       $ punctuate comma
       $ catMaybes [ fmap ((\x -> "xlabel" <> equals <> x) . dquotes . pretty) label
                   , Just (       "label"  <> equals <>      dquotes (ppNodeTree body))
                   ]
       ]

ppNodeTree :: Tree (Maybe Port, Adoc) -> Adoc
ppNodeTree (Forest trees)      = braces $ hcat (punctuate (pretty '|') (map ppNodeTree trees))
ppNodeTree (Leaf (port, body)) = maybe mempty (\p -> pretty '<' <> p <> pretty '>') (fmap pretty port) <> pp body
  where
    -- In order for the text to be properly rendered by graphviz, we need to
    -- escape some special characters. If the text takes up more than one line,
    -- then newlines '\n' need be be replaced with '\l', to ensure that the text
    -- is left justified rather than centred. The last line also needs a final
    -- '\l'. Single lines of text remain centred, which provides better
    -- formatting for short statements and port labels.
    --
    pp :: Adoc -> Adoc
    pp = encode . layoutSmart defaultLayoutOptions
    -- pp = encode . renderSmart 0.7 120

    encode :: SimpleDocStream Keyword -> Adoc
    encode doc =
      let
          go SFail          = error "unexpected failure rendering SimpleDoc"
          go SEmpty         = (mempty, False)
          go (SChar c x)    = let (x',m) = go x in (pretty (escape c) <> x', m)
          go (SText _ t x)  = let (x',m) = go x in (pretty (Text.concatMap escape t) <> x', m)
          go (SLine i x)    = let (x',_) = go x in ("\\l" <> spaces i <> x', True)  -- [1] left justify
          go (SAnnPush a x) = let (x',m) = go x in (annotate a x', m)
          go (SAnnPop x)    = let (x',m) = go x in (unAnnotate x', m)

          (doc',multiline) = go doc
      in
      doc' <> if multiline
                then "\\l"
                else mempty

    spaces :: Int -> Doc ann
    spaces i | i <= 0    = mempty
             | otherwise = pretty (Text.replicate i "\\ ")

    escape :: Char -> Text
    escape ' '  = "\\ "         -- don't collapse multiple spaces
    escape '>'  = "\\>"
    escape '<'  = "\\<"
    escape '|'  = "\\|"
    -- escape '\n' = "\\l"      -- handled at [1] instead
    escape c    = Text.singleton c

ppNodeId :: NodeId -> Adoc
ppNodeId (NodeId nid) = pretty (printf "Node_%#0x" nid :: String)

