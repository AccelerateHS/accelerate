{-# LANGUAGE CPP #-}
-- |
-- Module      : Data.Array.Accelerate.Internal.Trace
-- Copyright   : [2009..2011] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Internal.Trace (

  -- debug messaging infrastructure
  Verbosity(..), message, trace

) where

import Control.Monad.IO.Class
import qualified Debug.Trace as T

data Verbosity = Quiet | Normal | Loud
  deriving (Eq, Ord)


{-# INLINE verbosity #-}
verbosity :: Verbosity
#ifdef ACCELERATE_DEBUG
verbosity = Normal
#else
verbosity = Quiet
#endif


message :: MonadIO m => Verbosity -> FilePath -> Int -> String -> m ()
message lvl file line msg
  | verbosity >= lvl = liftIO $ T.putTraceMsg (trace_msg file line msg)
  | otherwise        = return ()

trace :: Verbosity -> FilePath -> Int -> String -> a -> a
trace lvl file line msg expr
  | verbosity >= lvl = T.trace (trace_msg file line msg) expr
  | otherwise        = expr

trace_msg :: FilePath -> Int -> String -> String
trace_msg file line msg = file ++ ":" ++ show line ++ ": " ++ msg

