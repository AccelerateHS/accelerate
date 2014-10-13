{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
-- |
-- Module:      : Data.Array.Accelerate.Examples.Internal.TH
-- Copyright    : [2014] Trevor L. McDonell
-- License      : BSD3
--
-- Maintainer   : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Examples.Internal.TH (

  _HEAD, _BRANCH,

) where

import Data.Version
import Control.Exception
import Language.Haskell.TH
import System.Process

import Paths_accelerate_examples


-- Try to determine the git version of the repository. If this fails, then we
-- aren't in a git (development) repository, so assume that we are a released
-- version and report the cabal version instead.
--
_HEAD :: ExpQ
_HEAD = packQ =<< runIO (gitVersion `orElse` cabalVersion)
  where
    gitVersion :: IO String
    gitVersion = init `fmap` readProcess "git" ["rev-parse", "HEAD"] []

    -- Based on the cabal version, we can look up the SHA of that version. Of
    -- course, there is a circular dependency here...
    --
    cabalVersion :: String
    cabalVersion =
      case versionBranch version of
        [0,16,0,0]      -> "TODO: release/0.16 SHA"
        _               -> "(no commit)"


-- Try to determine the git branch of the repository. If this fails, then we
-- aren't in a git (development) repository, so assume a release branch.
--
_BRANCH :: ExpQ
_BRANCH = packQ =<< runIO (gitBranch `orElse` releaseBranch)
  where
    gitBranch :: IO String
    gitBranch = init `fmap` readProcess "git" ["rev-parse", "--abbrev-ref", "HEAD"] []

    releaseBranch :: String
    releaseBranch =
      case versionBranch version of
        (m:n:_)         -> "release/" ++ show m ++ "." ++ show n
        _               -> "(no branch)"


orElse :: IO a -> a -> IO a
orElse x y = x `catch` \ (_ :: IOError) -> return y

packQ :: String -> ExpQ
packQ x = varE (mkName "Data.Text.pack") `appE` stringE x

