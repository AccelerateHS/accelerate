{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Debug.Internal.Graph
-- Copyright   : [2008..2021] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Debug.Internal.Graph (

  dumpGraph

) where

import Data.Array.Accelerate.Pretty.Graphviz

import Control.Monad.Trans                                          ( MonadIO )

#ifdef ACCELERATE_DEBUG
import Control.Exception                                            ( bracket )
import Control.Monad.Trans                                          ( liftIO )
import System.Directory                                             ( getTemporaryDirectory, createDirectoryIfMissing )
import System.FilePath                                              ( (</>) )
import System.IO                                                    ( Handle, openTempFile, hPutStrLn, hPrint, hClose, stderr )
import qualified Data.Array.Accelerate.Debug.Internal.Flags         as Debug

#if defined(mingw32_HOST_OS)
import System.Win32.Process                                         ( ProcessId )
#else
import System.Posix.Process                                         ( getProcessID )
#endif
#endif

-- | Write a representation of the given input (a closed array expression or
-- function) to file in Graphviz dot format in the temporary directory.
--
{-# INLINEABLE dumpGraph #-}
dumpGraph :: (MonadIO m, PrettyGraph g) => g -> m ()
#ifdef ACCELERATE_DEBUG
dumpGraph g =
  liftIO $ do
    Debug.when Debug.dump_dot       $ writeGraph Full   g
    Debug.when Debug.dump_simpl_dot $ writeGraph Simple g
#else
dumpGraph _ = return ()
#endif

#ifdef ACCELERATE_DEBUG
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

#if defined(mingw32_HOST_OS)
getProcessID :: IO ProcessId
getProcessID = return 0xaaaa
#endif
#endif

