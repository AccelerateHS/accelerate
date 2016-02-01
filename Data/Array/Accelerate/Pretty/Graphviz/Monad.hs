{-# LANGUAGE RecordWildCards #-}
-- |
-- Module      : Data.Array.Accelerate.Pretty.Graphviz.Monad
-- Copyright   : [2015] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Data.Array.Accelerate.Pretty.Graphviz.Monad
  where

import Control.Applicative
import Control.Monad.State
import Data.Foldable                                    ( toList )
import Data.Sequence                                    ( Seq )
import System.Mem.StableName
import Prelude
import qualified Data.Sequence                          as Seq

import Data.Array.Accelerate.Pretty.Graphviz.Type


-- Graph construction state ----------------------------------------------------

type Dot a    = StateT DotState IO a
data DotState = DotState
  { fresh       :: !Int
  , dotGraph    :: Seq Graph
  , dotEdges    :: Seq Edge
  , dotNodes    :: Seq Node
  }

emptyState :: DotState
emptyState =  DotState 0 Seq.empty Seq.empty Seq.empty

runDot :: Dot a -> IO (a, DotState)
runDot dot = runStateT dot emptyState

evalDot :: Dot a -> IO a
evalDot dot = fst <$> runDot dot

execDot :: Dot a -> IO DotState
execDot dot = snd <$> runDot dot


-- Utilities -------------------------------------------------------------------

mkLabel :: Dot Label
mkLabel = state $ \s ->
  let n = fresh s
  in  ( 'a' : show n, s { fresh = n + 1 } )

mkNodeId :: a -> Dot NodeId
mkNodeId node = do
  sn    <- liftIO $ makeStableName node
  return $ NodeId (hashStableName sn)

mkGraph :: Dot Graph
mkGraph =
  state $ \DotState{..} ->
    ( Graph [] (toList $ fmap N dotNodes Seq.>< fmap E dotEdges Seq.>< fmap G dotGraph)
    , emptyState { fresh = fresh }
    )

mkSubgraph :: Dot Graph -> Dot Graph
mkSubgraph g = do
  n       <- gets fresh
  (r, s') <- lift . runDot $ do
    modify $ \s -> s { fresh = n }
    g
  state $ \s -> (r, s { fresh = fresh s' })

