-- |
-- Module      : Darcs
-- Copyright   : [2011] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--

module Darcs where

import Config
import BuildBox

import System.Locale
import BuildBox.Data.Log (Log)
import qualified Data.Sequence          as Seq
import qualified BuildBox.Data.Log      as Log
import qualified Data.ByteString.Char8  as B

type Patch = Log

darcs :: String -> Build [Patch]
darcs cmd = do
  (status, logOut, logErr) <- systemTeeLog False cmd Log.empty
  case status of
    ExitSuccess -> return $ splitPatches logOut
    _           -> throw  $ ErrorSystemCmdFailed cmd status logOut logErr


patchesAfter :: LocalTime -> Build [Patch]
patchesAfter time =
  darcs $ "darcs changes --matches='date \"after " ++ show time ++ "\"'"

patchesLast :: Int -> Build [Patch]
patchesLast n =
  darcs $ "darcs changes --last=" ++ show n


splitPatches :: Log -> [Patch]
splitPatches l | Seq.null l = []
               | otherwise  = let (h,t) = Seq.breakl B.null l
                              in  h : splitPatches (Seq.dropWhileL B.null t)

-- Extract the author and record time of the given patch
--
patchInfo :: Patch -> (LocalTime, EmailAddress)
patchInfo p =
  ( readTime defaultTimeLocale "%a %b %e %H:%M:%S %Z %Y" (unwords time)
  , unwords author)
  where
    header        = Seq.index p 0
    toks          = words (B.unpack header)
    (time,author) = splitAt 6 toks

