#! /usr/bin/env runhaskell

import Control.Monad
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import System.Directory

main :: IO ()
main = defaultMainWithHooks autoconfUserHooks { preConf = preConfHook }
  where
    preConfHook args flags = do
      let verbosity = fromFlag (configVerbosity flags)

      confExists <- doesFileExist "configure"
      unless confExists $
        rawSystemExit verbosity "autoconf" []

      preConf autoconfUserHooks args flags

