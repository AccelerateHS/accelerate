{-# LANGUAGE CPP #-}

-- |
-- Module      : Data.Array.Accelerate.Test.Annotations
-- Copyright   : [2022] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Test.Annotations where

import qualified Data.HashSet as S
import Data.List
import GHC.Stack
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Trafo as A
import Data.Array.Accelerate.Annotations


tests :: TestTree
tests =
  testGroup
    "annotations"
    [ mergeLocsTests
    , fusionTests
    ]


mergeLocsTests :: TestTree
mergeLocsTests =
  testGroup
    "mergeLocs"
    [ testCase "empty"
        $ assertEqual "" [] (mergeLocs S.empty)
      -- The source locations are initially stored in a hash set, so the order
      -- is undefined, but the mergeLocs result should always be the same
      -- independent of that order
    , testCase "permutations"
        $ assertBool "call stack insertion order in the hash set should not matter"
        $ length (nub . map (mergeLocs . S.fromList) . permutations $ mergeableLocs) == 1
      -- This test is quite a chonky one, but it catches a lot of edge cases
    , testGroup "merging" $
        let mergableCandidates = nub . filter (not . null) . concatMap subsequences . permutations $ mergeableLocs
         in flip map mergableCandidates $ \locs ->
              let name = "[" <> intercalate ", " (baseLocName : disjointStartLocName : disjointEndLocName : map (fst . head . getCallStack) locs) <> "]"
                  merged = mergeLocs (S.fromList $ baseLoc : disjointStartLoc : disjointEndLoc : locs)
              in testCase name $
                    assertEqual "any combination of the mergeable locations, the base location, and two disjoint locations should merge to two locations"
                      3
                      (length merged)
    , testCase "merged names" $
        let [((mergedFns, _) : _)] = mergeLocs (S.fromList $ baseLoc : mergeableLocs)
            expectedNames          = baseLocName : mergeableLocNames
         in assertBool "the merged call stack should contain all original names" $
              all (`isInfixOf` mergedFns) expectedNames
    , testCase "merged tail" $
        let [(_ : rest)] = mergeLocs (S.fromList $ baseLoc : mergeableLocs)
         in assertEqual "the original tail of the first item in the call stack should be preserved"
              (tail $ getCallStack adjacentStartLoc) rest
    , testCase "different files" $
        assertEqual "locations from different files should not be merged" 2 $
          length $ mergeLocs (S.fromList [baseLoc, otherFileLoc])
    , testCase "[base, contained] bounds" $
        assertEqual "the srcLocs should match the original baseLoc"
          (map snd $ getCallStack baseLoc)
          (map snd . head $ mergeLocs (S.fromList [baseLoc, containedLoc]))
    , testCase "[base, adjacentStart] bounds" $ do
        let [((_, loc) : _)] = mergeLocs (S.fromList [baseLoc, adjacentStartLoc])
        assertEqual "the start line should be extended to match both"   5  (srcLocStartLine loc)
        assertEqual "the start column should be extended to match both" 0  (srcLocStartCol loc)
        assertEqual "the end line should remain the same"               15 (srcLocEndLine loc)
        assertEqual "the end column should remain the same"             20 (srcLocEndCol loc)
    , testCase "[base, adjacentEnd] bounds" $ do
        let [((_, loc) : _)] = mergeLocs (S.fromList [baseLoc, adjacentEndLoc])
        assertEqual "the start line should remain the same"             10 (srcLocStartLine loc)
        assertEqual "the start column should remain the same"           0  (srcLocStartCol loc)
        assertEqual "the end line should be extended to match both"     18 (srcLocEndLine loc)
        assertEqual "the end column should be extended to match both"   20 (srcLocEndCol loc)
    ]
  where
    -- The locations that can be merged with @baseLoc@
    mergeableLocNames = [containedLocName, overlappingLocName, adjacentStartLocName, adjacentEndLocName]
    mergeableLocs = [containedLoc, overlappingLoc, adjacentStartLoc, adjacentEndLoc]
    _allLocs = mergeableLocs ++ [baseLoc, otherFileLoc, disjointStartLoc, disjointEndLoc, otherFileLoc]

    baseLocName = "base"
    baseLoc = fromCallSiteList [(baseLocName, mkLoc (10, 0) (15, 20)), ("aux1", mkLoc (100, 0) (100, 15))]

    -- A source location contained within @baseLoc@
    containedLocName = "contained"
    containedLoc = fromCallSiteList [(containedLocName, mkLoc (12, 15) (12, 30)), ("aux2", mkLoc (101, 0) (101, 15))]

    -- A source location partially overlapping with the end of @baseLoc@
    overlappingLocName = "overlapping"
    overlappingLoc = fromCallSiteList [(overlappingLocName, mkLoc (14, 0) (20, 20)), ("aux3", mkLoc (102, 0) (102, 15))]

    -- A source location that start or ends on the line after or before
    -- @baseLoc@. With our heuristic these two should be merged.
    adjacentStartLocName = "adjacentStart"
    adjacentStartLoc = fromCallSiteList [(adjacentStartLocName, mkLoc (5, 0) (9, 20)), ("aux4", mkLoc (103, 0) (103, 15))]
    adjacentEndLocName = "adjacentEnd"
    adjacentEndLoc = fromCallSiteList [(adjacentEndLocName, mkLoc (16, 0) (18, 20)), ("aux7", mkLoc (106, 0) (106, 15))]

    -- Completely disjoin source locations that should never be merged
    disjointStartLocName = "disjointStart"
    disjointStartLoc = fromCallSiteList [(disjointStartLocName, mkLoc (0, 0) (1, 20)), ("aux5", mkLoc (104, 0) (104, 15))]
    disjointEndLocName = "disjointEnd"
    disjointEndLoc = fromCallSiteList [(disjointEndLocName, mkLoc (40, 0) (45, 20)), ("aux6", mkLoc (105, 0) (105, 15))]

    -- A source location with the same line numbers as @baseLoc@, but that lives
    -- in another file
    otherFileLocName = "otherFile"
    otherFileLoc = fromCallSiteList [(otherFileLocName, mkLocFile "Other.hs" (10, 0) (15, 20)), ("aux7", mkLoc (106, 0) (106, 15))]

    mkLoc :: ((Int, Int)) -> ((Int, Int)) -> SrcLoc
    mkLoc = mkLocFile "Example.hs"

    mkLocFile :: String -> ((Int, Int)) -> ((Int, Int)) -> SrcLoc
    mkLocFile filename (startLine, startCol) (endLine, endCol) =
      SrcLoc "package" "Module" filename startLine startCol endLine endCol

fusionTests :: TestTree
fusionTests =
  testGroup
    "fusion"
    [ testCase "fused map sourcelocs" $ do
        let expectedStart = __LINE__ + 1
            ys            = A.map (* 2)     (A.use xs)
            ys'           = A.map (+ 2)     ys
            ys''          = A.map (`div` 2) ys'
            expectedEnd   = __LINE__ - 1
            expectedFile  = __FILE__

            Just ann      = getAnn (A.convertAcc ys'')
            locs          = mergeLocs (locations ann)

        assertEqual "there should be only one location" 1 (length locs)
        let [((fn, loc) : _)] = locs
        assertEqual "the merged function name should be correct" "map, use, map, map" fn
        assertEqual "srcLocFile loc == expectedFile" expectedFile (srcLocFile loc)
        assertEqual "srcLocStartLine loc == expectedStart" expectedStart (srcLocStartLine loc)
        assertEqual "srcLocEndLine loc == expectedEnd" expectedEnd (srcLocEndLine loc)
    ]
  where
    xs :: A.Vector Int
    xs = A.fromList (A.Z A.:. 10) [1..]
