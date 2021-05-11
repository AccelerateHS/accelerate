{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- |
-- Module      : Data.Array.Accelerate.Pretty.Graphviz.Monad
-- Copyright   : [2015..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Data.Array.Accelerate.Pretty.Graphviz.Monad
  where

import Control.Monad.State
import Data.Foldable                                    ( toList )
import Data.Sequence                                    ( Seq )
import qualified Data.Sequence                          as Seq
import qualified Data.Text                              as Text

import Data.Array.Accelerate.Pretty.Graphviz.Type


-- Graph construction state ----------------------------------------------------

type Dot a    = State DotState a
data DotState = DotState
  { freshLabel  :: !Int
  , freshID     :: !Int  -- keep internal node ids in a separate counter from the user-visible node labels
  , dotGraph    :: Seq Graph
  , dotEdges    :: Seq Edge
  , dotNodes    :: Seq Node
  }

emptyState :: DotState
emptyState =  DotState 0 0 Seq.empty Seq.empty Seq.empty

runDot :: Dot a -> (a, DotState)
runDot dot = runState dot emptyState

evalDot :: Dot a -> a
evalDot dot = fst (runDot dot)

execDot :: Dot a -> DotState
execDot dot = snd (runDot dot)


-- Utilities -------------------------------------------------------------------

mkLabel :: Dot Label
mkLabel = state $ \s ->
  let n = freshLabel s
  in  ( Text.pack ('a' : show n), s { freshLabel = n + 1 } )

genNodeId :: Dot NodeId
genNodeId = state $ \s ->
  let n = freshID s
  in  ( NodeId n, s { freshID = n + 1 } )

mkGraph :: Dot Graph
mkGraph =
  state $ \DotState{..} ->
    ( Graph mempty (toList $ fmap N dotNodes Seq.>< fmap E dotEdges Seq.>< fmap G dotGraph)
    , emptyState { freshLabel = freshLabel }
    )

mkSubgraph :: Dot Graph -> Dot Graph
mkSubgraph g = do
  n       <- gets freshLabel
  let (r, s') = runDot $ do
        modify $ \s -> s { freshLabel = n }
        g
  state $ \s -> (r, s { freshLabel = freshLabel s' })

