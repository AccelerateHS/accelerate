{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Debug
-- Copyright   : [2008..2017] Manuel M T Chakravarty, Gabriele Keller
--               [2009..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Embedded array processing language: debugging support (internal). This module
-- provides functionality that is useful for developers of the library. It is
-- not meant for library users.
--

module Data.Array.Accelerate.Debug (

  module Debug,

  dumpGraph,
  dumpSimplStats,

  monitoringIsEnabled,
  debuggingIsEnabled,

) where

import Data.Array.Accelerate.Debug.Flags                as Debug
import Data.Array.Accelerate.Debug.Monitoring           as Debug
import Data.Array.Accelerate.Debug.Stats                as Debug
import Data.Array.Accelerate.Debug.Timed                as Debug
import Data.Array.Accelerate.Debug.Trace                as Debug

import Data.Array.Accelerate.Pretty.Graphviz

import Control.Monad.Trans                              ( MonadIO )

#if ACCELERATE_DEBUG
import Control.Exception                                ( bracket )
import Control.Monad.Trans                              ( liftIO )
import System.Directory                                 ( getTemporaryDirectory, createDirectoryIfMissing )
import System.FilePath                                  ( (</>) )
import System.IO                                        ( Handle, openTempFile, hPutStrLn, hPrint, hClose, stderr )

#if   defined(UNIX)
import System.Posix.Process                             ( getProcessID )
#elif defined(WIN32)
import System.Win32.Process                             ( ProcessId )
#else
#error "I don't know what operating system I am"
#endif
#endif


debuggingIsEnabled :: Bool
#if ACCELERATE_DEBUG
debuggingIsEnabled = True
#else
debuggingIsEnabled = False
#endif

monitoringIsEnabled :: Bool
#if ACCELERATE_MONITORING
monitoringIsEnabled = True
#else
monitoringIsEnabled = False
#endif


-- | Display simplifier statistics. The counts are reset afterwards.
--
{-# INLINEABLE dumpSimplStats #-}
dumpSimplStats :: MonadIO m => m ()
#if ACCELERATE_DEBUG
dumpSimplStats = do
  liftIO $ Debug.when dump_simpl_stats $ do
    stats <- simplCount
    putTraceMsg (show stats)
    resetSimplCount
#else
dumpSimplStats = return ()
#endif


-- | Write a representation of the given input (a closed array expression or
-- function) to file in Graphviz dot format in the temporary directory.
--
{-# INLINEABLE dumpGraph #-}
dumpGraph :: (MonadIO m, PrettyGraph g) => g -> m ()
#if ACCELERATE_DEBUG
dumpGraph g =
  liftIO $ do
    Debug.when dump_dot       $ writeGraph Full   g
    Debug.when dump_simpl_dot $ writeGraph Simple g
#else
dumpGraph _ = return ()
#endif

#if ACCELERATE_DEBUG
writeGraph :: PrettyGraph g => Detail -> g -> IO ()
writeGraph simple g = do
  withTemporaryFile "acc.dot" $ \path hdl -> do
    hPrint hdl (ppGraph simple g)
    hPutStrLn stderr ("program graph: " ++ path)

withTemporaryFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTemporaryFile template go = do
  pid <- getProcessID
  tmp <- getTemporaryDirectory
  let dir = tmp </> "accelerate-" ++ show pid
  createDirectoryIfMissing True dir
  bracket (openTempFile dir template) (hClose . snd) (uncurry go)

#ifdef WIN32
getProcessID :: IO ProcessId
getProcessID = return 0xaaaa
#endif
#endif
