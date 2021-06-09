
module Main where

import Distribution.Extra.Doctest
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.Types.Flag
import Distribution.Types.HookedBuildInfo
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
      profiling = fromMaybe False $ lookupFlagAssignment (mkFlagName "prof") (configConfigurationsFlags configFlags)

  when profiling $ do
    yes <- doesFileExist "cbits/tracy/TracyClient.cpp"
    if yes
      then
        -- Nix (and apparently future versions of stack) automatically update
        -- submodules, so there is no need to do so again.
        return ()
      else
        -- Stack and cabal based builds require updating the submodules
        rawSystemExit verbosity "git" ["submodule", "update", "--init", "--recursive"]

  preConf simpleUserHooks args configFlags

