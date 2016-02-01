{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns       #-}
-- |
-- Module      : Data.Array.Accelerate.Pretty.Graphviz.Type
-- Copyright   : [2015] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Simple data types for representing (simple, directed) graphs and
-- pretty-printing to Graphviz dot format.
--
module Data.Array.Accelerate.Pretty.Graphviz.Type
  where

import Data.Maybe
import Data.Hashable
import Data.List
import Text.Printf
import Text.PrettyPrint


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

data Node       = Node (Maybe Label) NodeId (Tree (Maybe Port, Doc))
data NodeId     = NodeId !Int

-- XXX: Changed from 'Doc' to 'String' because the version of 'pretty' included
--      with ghc-7.8 does not have an Eq Doc instance, which was added in
--      pretty-1.1.1.2. However, we don't want to simply depend on a newer
--      version of the library, because this will indirectly lead to
--      a dependency on multiple versions (through, e.g., template-haskell).
--
type Label      = String
type Port       = String

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
ppGraph :: Graph -> Doc
ppGraph (Graph l ss) =
  vcat [ text "digraph" <+> text l <+> lbrace
       , nest 4 $ vcat
                $ punctuate semi
                $ text "graph [compound=true]"
                : text "node  [shape=record,fontsize=10]"
                : map ppStatement ss
       , rbrace
       ]

ppSubgraph :: Graph -> Doc
ppSubgraph (Graph l ss) =
  vcat [ text "subgraph cluster_" <> text l <+> lbrace
       , nest 4 $ vcat
                $ punctuate semi
                $ text "label" <> equals <> text l
                : map ppStatement ss
       , rbrace
       ]

ppStatement :: Statement -> Doc
ppStatement (N n) = ppNode n
ppStatement (E e) = ppEdge e
ppStatement (G g) = ppSubgraph g

ppEdge :: Edge -> Doc
ppEdge (Edge from to) = ppVertex from <+> text "->" <+> ppVertex to

ppVertex :: Vertex -> Doc
ppVertex (Vertex n p) = ppNodeId n <> maybe empty (colon<>) (fmap text p)

ppNode :: Node -> Doc
ppNode (Node label nid body) =
  hcat [ ppNodeId nid
       , brackets
       $ hcat
       $ punctuate comma
       $ catMaybes [ fmap ((text "xlabel" <> equals <>) . doubleQuotes . text) label
                   , Just ( text "label"  <> equals <>    doubleQuotes (ppNodeTree body))
                   ]
       ]

ppNodeTree :: Tree (Maybe Port, Doc) -> Doc
ppNodeTree (Forest trees)      = braces $ hcat (punctuate (char '|') (map ppNodeTree trees))
ppNodeTree (Leaf (port, body)) = maybe empty (\p -> char '<' <> p <> char '>') (fmap text port) <> pp body
  where
    -- In order for the text to be properly rendered by graphviz, we need to
    -- escape some special characters. If the text takes up more than one line,
    -- then newlines '\n' need be be replaced with '\l', to ensure that the text
    -- is left justified rather than centred. The last line also needs a final
    -- '\l'. Single lines of text are left centred, which provides better
    -- formatting for short statements and port labels.
    --
    pp :: Doc -> Doc
    pp (lines . concatMap escape . renderStyle wide -> doc) =
      case doc of
        []  -> empty
        [x] -> text x
        xs  -> text (intercalate "\\l" xs) <> text "\\l"  -- [1] left justify

    wide :: Style
    wide = style { lineLength = 200 }

    escape :: Char -> String
    escape ' '  = "\\ "         -- don't collapse multiple spaces
    escape '>'  = "\\>"
    escape '<'  = "\\<"
    escape '|'  = "\\|"
    -- escape '\n' = "\\l"      -- handled at [1] instead
    escape c    = [c]

ppNodeId :: NodeId -> Doc
ppNodeId (NodeId nid) = text (printf "Node_%#0x" nid)

