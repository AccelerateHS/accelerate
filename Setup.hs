
module Main where

import Distribution.Extra.Doctest
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.Verbosity

import Control.Monad
import Data.Maybe
import System.Directory


main :: IO ()
main =
  defaultMainWithHooks (doctestsUserHooks "doctest")
    { preConf = preConfHook
    }

preConfHook :: Args -> ConfigFlags -> IO HookedBuildInfo
preConfHook args configFlags = do
  let verbosity = fromFlagOrDefault normal $ configVerbosity configFlags
      debugging = fromMaybe False $ lookupFlagAssignment (mkFlagName "debug") (configConfigurationsFlags configFlags)

  when debugging $ do
    yes <- doesFileExist "cbits/tracy/TracyClient.cpp"
    if yes
      then
        -- Nix (and apparently future versions of stack) automatically update
        -- submodules, so there is no need to do so again.
        return ()
      else do
        -- Stack and cabal based builds require updating the submodules
        git <- doesDirectoryExist ".git"
        if git
           then rawSystemExit verbosity "git" ["submodule", "update", "--init", "--recursive"]
           else do
             -- XXX: This must be kept up to date with the git submodule revision
             let archive = "v0.7.8.tar.gz"
             createDirectoryIfMissing True "cbits/tracy"
             rawSystemExit verbosity "curl" ["-LO", "https://github.com/wolfpld/tracy/archive/refs/tags/" ++ archive]
             rawSystemExit verbosity "tar" ["-xzf", archive, "-C", "cbits/tracy", "--strip-components", "1"]
             removeFile archive

  preConf simpleUserHooks args configFlags

