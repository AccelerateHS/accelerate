{-# LANGUAGE MultiWayIf #-}
module Main where

import Distribution.Extra.Doctest
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.System
import Distribution.Verbosity

import Control.Monad
import Data.Maybe
import System.Directory
import System.FilePath
import Text.Printf


main :: IO ()
main =
  defaultMainWithHooks (doctestsUserHooks "doctest")
    { preConf   = preConfHook
    , postBuild = postBuildHook
    }

preConfHook :: Args -> ConfigFlags -> IO HookedBuildInfo
preConfHook args config_flags = do
  let verbosity = fromFlagOrDefault normal $ configVerbosity config_flags
      debugging = fromMaybe False $ lookupFlagAssignment (mkFlagName "debug") (configConfigurationsFlags config_flags)

  when debugging $ do
    yes <- doesFileExist "cbits/tracy/public/TracyClient.cpp"
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
             let archive = "v0.11.1.tar.gz"
             createDirectoryIfMissing True "cbits/tracy"
             rawSystemExit verbosity "curl" ["-LO", "https://github.com/wolfpld/tracy/archive/refs/tags/" ++ archive]
             rawSystemExit verbosity "tar" ["-xzf", archive, "-C", "cbits/tracy", "--strip-components", "1"]
             removeFile archive

  preConf simpleUserHooks args config_flags

-- TODO: only build the executables which are enabled
--
postBuildHook :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
postBuildHook args build_flags pkg_desc lbi = do
  let config_flags  = configFlags lbi
      Platform _ os = hostPlatform lbi
      verbosity     = fromFlagOrDefault normal $ buildVerbosity build_flags
      debugging     = fromMaybe False $ lookupFlagAssignment (mkFlagName "debug") (configConfigurationsFlags config_flags)
      targets       = [ ("tracy-capture", "capture",  "tracy-capture")
                      , ("tracy",         "profiler", "tracy-profiler") ]

  when debugging $ do
    case os of
      Windows -> return ()  -- XXX TODO: Windows users get the dummy executable that just throws an error
      _       ->
        forM_ targets $ \(hs_exe, c_dir, c_exe) -> do
          let c_projdir  = "cbits/tracy" </> c_dir
              hs_builddir = buildDir lbi </> hs_exe
              hs_tmpdir   = hs_builddir </> hs_exe ++ "-tmp"

          setupMessage verbosity (printf "Building executable '%s' from Tracy C++ sources for" hs_exe) (package pkg_desc)

          -- TODO: This creates a separate build directory for each tracy
          -- executable (of which we build two, at the time of writing). This
          -- means that some duplicate work is done (building capstone twice).
          -- Could we share a build directory between the two?

          rawSystemExit verbosity "cmake" ["-B", hs_tmpdir, "-S", c_projdir, "-DCMAKE_BUILD_TYPE=Release"]

          -- Build in parallel with 2 jobs because likely, accelerate is one of
          -- the last dependencies in a build, so we aren't stealing CPU time
          -- from other packages, and tracy takes way too long to build
          rawSystemExit verbosity "cmake" ["--build", hs_tmpdir, "--config", "Release", "-j", "2"]

          renameFile (hs_tmpdir </> c_exe) (hs_builddir </> hs_exe)

  postBuild simpleUserHooks args build_flags pkg_desc lbi
