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
             let archive = "v0.9.1.tar.gz"
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
      targets       = [ ("tracy-capture", "capture",  "capture-release")
                      , ("tracy",         "profiler", "Tracy-release") ]

  when debugging $ do
    case os of
      Windows -> return ()  -- XXX TODO: Windows users get the dummy executable that just throws an error
      _       ->
        forM_ targets $ \(hs_exe, c_dir, c_exe) -> do
          let c_builddir  = "cbits/tracy" </> c_dir </> "build/unix"
              hs_builddir = buildDir lbi </> hs_exe
              c_tmpdir    = c_builddir </> "obj"
              hs_tmpdir   = hs_builddir </> hs_exe ++ "-tmp"

          setupMessage verbosity (printf "Building executable '%s' for" hs_exe) (package pkg_desc)

          -- Symlink the C build directory into the HS build directories.
          -- We need to take care here: the symlink might already exist but point
          -- to a nonexistent path, in which case doesDirectoryExist returns False
          -- but it is still a symlink.
          whenM (pathIsSymbolicLinkSafe c_tmpdir) $ removeFile c_tmpdir
          objExists <- doesPathExist c_tmpdir
          objExistsDir <- doesDirectoryExist c_tmpdir
          createdObjSymlink <-
            if | objExists && not objExistsDir ->
                  dieNoVerbosity $ c_tmpdir ++ " is a file; remove this to continue compiling accelerate."
               | objExistsDir ->
                  return False
               | otherwise -> do
                  -- Make the symlink (relative from c_builddir, hence the 5x ..)
                  createDirectoryLink ("../../../../.." </> hs_tmpdir) c_tmpdir
                  -- Create that temp directory if necessary. This is required if the accelerate
                  -- package is built as a dependency from another cabal project, because then only
                  -- the library is built, and not the executables such as tracy-capture.
                  createDirectoryIfMissing True hs_tmpdir
                  return True

          -- prevent having to re-link every time we build the library
          executable <- doesFileExist (hs_builddir </> hs_exe)
          when executable $ renameFile (hs_builddir </> hs_exe) (c_builddir </> c_exe)

          -- build
          rawSystemExit verbosity "make" ["-C", c_builddir]

          -- move executable to the final destination
          renameFile (c_builddir </> c_exe) (hs_builddir </> hs_exe)

          -- clean up after ourselves
          when createdObjSymlink $ removeDirectoryLink c_tmpdir

  postBuild simpleUserHooks args build_flags pkg_desc lbi

-- | Returns True if there is a symbolic link at the given path, False otherwise.
pathIsSymbolicLinkSafe :: FilePath -> IO Bool
pathIsSymbolicLinkSafe fp =
  handleDoesNotExist False $ pathIsSymbolicLink fp

whenM :: IO Bool -> IO () -> IO ()
whenM mcond action = mcond >>= \cond -> when cond action
