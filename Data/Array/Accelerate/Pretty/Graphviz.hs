{-# LANGUAGE GADTs, TypeOperators, KindSignatures, ScopedTypeVariables #-}
-- |Embedded array processing language: graphviz pretty printing
--
--  Copyright (c) 2010 Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Sean Seefried
--
--  License: BSD3
--
--- Description ---------------------------------------------------------------
--
--  Functions for printing out Graphviz graphs in DOT format.


module Data.Array.Accelerate.Pretty.Graphviz (

  -- * Graphviz printing functions
  dumpAcc

) where

-- standard libraries
import Data.List
import IO hiding (catch)
import System
import System.IO
import System.Directory
-- import System.FilePath.Posix(joinPath)
import Control.Exception (finally)
import Control.Monad.State
import Text.Printf

-- friends
import Data.Array.Accelerate.Pretty.Traverse
import Data.Array.Accelerate.AST

-- | Detects if the dot command line tool from the Graphviz package exists.
-- If it does outputs a Postscript file, otherwise a ".dot" file.
--
dumpAcc :: String -> OpenAcc aenv a -> IO ()
dumpAcc basename acc = do
  exitCode <- system ("which dot > /dev/null 2>&1")
  case exitCode of
    ExitSuccess   -> withTempFile "ast.dot" writePSFile
    ExitFailure _ -> do
      putStrLn "Couldn't find `dot' tool. Just writing DOT file."
      writeDotFile
  where
    writePSFile file h = do
      hPutStr h (dotAcc acc)
      hFlush h
      let cmd = concat $ intersperse " " ["dot", file,"-Tps ","-o" ++ basename ++ ".ps" ]
      exitCode <- system cmd
      case exitCode of
        ExitSuccess -> putStrLn ("PS file successfully written to `" ++ basename ++ ".ps'")
        ExitFailure _ -> do
          putStrLn "dot failed to write Postscript file. Just writing the DOT file."
          writeDotFile-- fall back to writing the dot file
    writeDotFile :: IO ()
    writeDotFile  = catch (writeDotFile') handler
    writeDotFile'  = do
      let path = basename ++ ".dot"
      h <- openFile path WriteMode
      hPutStr h (dotAcc acc)
      putStrLn ("DOT file successfully written to `" ++ path ++ "'")
      hClose h
    handler :: IOError -> IO ()
    handler e = do
      case True of
        _ | isAlreadyInUseError e -> putStrLn "isAlreadyInUseError"
          | isDoesNotExistError e -> putStrLn "isDoesNotExistError"
          | isFullError e         -> putStrLn "isFullError"
          | isEOFError e          -> putStrLn "isEOFError"
          | isPermissionError   e -> putStrLn "isPermissionError"
          | isIllegalOperation e  -> putStrLn "isIllegalOperation"
          | isUserError e         -> putStrLn "isUserError"
          | otherwise             -> putStrLn "Unknown error"


withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile pattern f = do
  tempDir <- catch (getTemporaryDirectory) (\_ -> return ".")
  (tempFile, tempH) <- openTempFile tempDir pattern
  finally (f tempFile tempH) (hClose tempH >> removeFile tempFile)

dotAcc :: OpenAcc aenv a -> String
dotAcc = toDigraph (travAcc dotLabels combineDot leafDot)

data Node     = Node     { nodeId :: String, label :: String, childNodes :: [String], transitions :: [String] }
data DotState = DotState { counter :: Int }

mkNodeId :: Int -> String
mkNodeId = printf "node%03d"

mkNode :: String -> [String] -> [String] -> State DotState Node
mkNode lbl childNodes' transitions' = do
  s <- get
  let c = counter s
  put $  DotState { counter = c + 1 }
  return (Node { nodeId = mkNodeId c, label = lbl, childNodes = childNodes', transitions = transitions' })

dotLabels :: Labels
dotLabels = Labels { accFormat = "yellow"
                   , expFormat = "orange"
                   , funFormat = "blue"
                   , tupleFormat = "green"
                   , primFunFormat = "red"
                   , arrayFormat = "purple"
                   , boundaryFormat = "cyan" }



combineDot  :: String -> String -> [State DotState Node] -> State DotState Node
combineDot color source targets = do
   targetNodes <- sequence targets
   s <- get
   let newNodeId  = mkNodeId (counter s)
       childNodes1 = [(nodeDef newNodeId source color) ]
       childNodes2 = concatMap childNodes targetNodes
       lines1     = map (digraphLine newNodeId) targetNodes
       lines2     = concat (map transitions targetNodes)
   mkNode source (childNodes1 ++ childNodes2) (lines1 ++ lines2)
  where
    digraphLine :: String -> Node -> String
    digraphLine sourceNodeId targetNode =
      sourceNodeId ++ " -> " ++ nodeId targetNode ++ ";"

leafDot :: String -> String -> State DotState Node
leafDot color lbl = do
  s <- get
  let c = counter s
  put $  DotState { counter = c + 1 }
  return (Node { nodeId = mkNodeId c, label = lbl
               , childNodes = [ nodeDef (mkNodeId c) lbl color ]
               , transitions = [] })

nodeDef :: String -> String -> String -> String
nodeDef nodeId' label' color = printf "%s [ color=\"%s\", label=\"%s\" ];" nodeId' color label'

toDigraph :: (a -> State DotState Node) -> a -> String
toDigraph f e =
  header ++ unlines (childNodes node) ++ unlines (transitions node) ++ footer
   where
     node = evalState (f e) (DotState { counter = 0 })
     header = unlines $ [ "/* Automatically generated by Accelerate */"
                        , "digraph AST {"
                        , "size=\"7.5,11\";"
                        , "ratio=\"compress\";"
                        , "node[color=lightblue2, style=filled];"]
     footer = "}"
