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
  dotAcc, dotExp, dotFun, dumpAcc

) where

-- standard libraries
import Data.List
import IO hiding (catch)
import System
import System.IO
import System.Directory
import System.FilePath.Posix(joinPath)
import Control.Exception (finally)
import Control.Monad.State
import Text.Printf

-- friends
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Type

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
      writeDotFile basename
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
          writeDotFile basename-- fall back to writing the dot file
    writeDotFile :: String -> IO ()
    writeDotFile basename = catch (writeDotFile' basename) handler
    writeDotFile' basename = do
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
dotAcc = toDigraph dotAcc'


data Node     = Node     { nodeId :: String, label :: String, childNodes :: [String], transitions :: [String] }
data DotState = DotState { counter :: Int }

mkNodeId :: Int -> String
mkNodeId = printf "node%03d"

mkNode :: String -> [String] -> [String] -> State DotState Node
mkNode lbl childNodes transitions = do
  s <- get
  let c = counter s
  put $  DotState { counter = c + 1 }
  return (Node { nodeId = mkNodeId c, label = lbl, childNodes = childNodes, transitions = transitions })

leafNode :: String -> String -> State DotState Node
leafNode color lbl = do
  s <- get
  let c = counter s
  put $  DotState { counter = c + 1 }
  return (Node { nodeId = mkNodeId c, label = lbl
               , childNodes = [ nodeDef (mkNodeId c) lbl color ]
               , transitions = [] })



accColor = "yellow"
expColor = "orange"
funColor = "blue"
tupleColor = "green"
primFunColor = "red"
arrayColor = "purple"
boundaryColor = "cyan"

linesForDigraphAcc = linesForDigraphWithColor accColor
linesForDigraphExp = linesForDigraphWithColor expColor
linesForDigraphFun = linesForDigraphWithColor funColor
linesForDigraphTuple = linesForDigraphWithColor tupleColor

dotAcc' :: OpenAcc aenv a -> State DotState Node
dotAcc' (Let acc1 acc2) = linesForDigraphAcc "Let" [dotAcc' acc1, dotAcc' acc2]
dotAcc' (Let2 acc1 acc2) = linesForDigraphAcc "Let2" [ dotAcc' acc1, dotAcc' acc2 ]
dotAcc' (Avar idx) = leafNode accColor ("AVar " ++ show (idxToInt idx))
dotAcc' (Use arr) = linesForDigraphAcc "Use" [ dotArray' arr ]
dotAcc' (Unit e) = linesForDigraphAcc "Unit" [ dotExp' e ]
dotAcc' (Generate sh f) = linesForDigraphAcc "Generate" [ dotExp' sh, dotFun' f ]
dotAcc' (Reshape sh acc) = linesForDigraphAcc "Reshape" [ dotExp' sh, dotAcc' acc ]
dotAcc' (Replicate _ ix acc) = linesForDigraphAcc "Replicate" [ dotExp' ix, dotAcc' acc ]
dotAcc' (Index _ acc ix) = linesForDigraphAcc "Index" [ dotAcc' acc, dotExp' ix ]
dotAcc' (Map f acc) = linesForDigraphAcc "Map" [ dotFun' f, dotAcc' acc ]
dotAcc' (ZipWith f acc1 acc2) = linesForDigraphAcc "ZipWith" [ dotFun' f, dotAcc' acc1, dotAcc' acc2 ]
dotAcc' (Fold f e acc) = linesForDigraphAcc "Fold" [ dotFun' f, dotExp' e, dotAcc' acc]
dotAcc' (Fold1 f acc) = linesForDigraphAcc "Fold1" [ dotFun' f, dotAcc' acc]
dotAcc' (FoldSeg f e acc1 acc2) = linesForDigraphAcc "FoldSeg" [ dotFun' f, dotExp' e, dotAcc' acc1, dotAcc' acc2 ]
dotAcc' (Fold1Seg f acc1 acc2) = linesForDigraphAcc "FoldSeg1" [ dotFun' f, dotAcc' acc1, dotAcc' acc2 ]
dotAcc' (Scanl f e acc) = linesForDigraphAcc "Scanl" [ dotFun' f, dotExp' e, dotAcc' acc ]
dotAcc' (Scanl' f e acc) = linesForDigraphAcc "Scanl'" [ dotFun' f, dotExp' e, dotAcc' acc ]
dotAcc' (Scanl1 f acc) = linesForDigraphAcc "Scanl1" [ dotFun' f, dotAcc' acc ]
dotAcc' (Scanr f e acc) = linesForDigraphAcc "Scanr" [ dotFun' f, dotExp' e, dotAcc' acc ]
dotAcc' (Scanr' f e acc) = linesForDigraphAcc "Scanr'" [ dotFun' f, dotExp' e, dotAcc' acc ]
dotAcc' (Scanr1 f acc) = linesForDigraphAcc "Scanr1" [ dotFun' f, dotAcc' acc ]
dotAcc' (Permute f dfts p acc) = linesForDigraphAcc "Permute" [ dotFun' f, dotAcc' dfts, dotFun' p, dotAcc' acc]
dotAcc' (Backpermute sh p acc) = linesForDigraphAcc "Backpermute" [ dotExp' sh, dotFun' p, dotAcc' acc]
dotAcc' (Stencil sten bndy acc) = linesForDigraphAcc "Stencil" [ dotFun' sten, dotBoundary' acc bndy, dotAcc' acc]
dotAcc' (Stencil2 sten bndy1 acc1 bndy2 acc2) = linesForDigraphAcc "Stencil2" [ dotFun' sten, dotBoundary' acc1 bndy1,
                                                               dotAcc' acc1, dotBoundary' acc2 bndy2, dotAcc' acc2]



dotExp :: OpenExp env aenv a -> String
dotExp = toDigraph dotExp'

dotExp' :: forall env aenv a. OpenExp env aenv a -> State DotState Node
dotExp' (Var idx)           = leafNode expColor ("Var "   ++ show (idxToInt idx))
dotExp' (Const v)           = leafNode expColor ("Const " ++ show (toElt v :: a))
dotExp' (Tuple tup)         = linesForDigraphExp "Tuple" [ dotTuple' tup ]
dotExp' (Prj idx e)         = linesForDigraphExp ("Prj " ++ show (tupleIdxToInt idx)) [ dotExp' e ]
dotExp' (IndexNil)          = leafNode expColor "IndexNil"
dotExp' (IndexCons t h)     = linesForDigraphExp "IndexCons" [ dotExp' t, dotExp' h]
dotExp' (IndexHead ix)      = linesForDigraphExp "IndexHead" [ dotExp' ix ]
dotExp' (IndexTail ix)      = linesForDigraphExp "IndexTail" [ dotExp' ix ]
dotExp' (Cond c t e)        = linesForDigraphExp "Cond" [dotExp' c, dotExp' t, dotExp' e]
dotExp' (PrimConst a)       = leafNode expColor ("PrimConst " ++ labelForConst a)
dotExp' (PrimApp p a)       = linesForDigraphExp "PrimApp" [ (leafNode primFunColor (labelForPrimFun p) ), dotExp' a ]
dotExp' (IndexScalar idx i) = linesForDigraphExp "IndexScalar" [ dotAcc' idx, dotExp' i]
dotExp' (Shape idx)         = linesForDigraphExp "Shape" [ dotAcc' idx ]
dotExp' (Size idx)          = linesForDigraphExp "Size" [ dotAcc' idx ]

dotFun :: OpenFun env aenv fun -> String
dotFun = toDigraph dotFun'

dotFun' :: OpenFun env aenv fun -> State DotState Node
dotFun' (Body body) = linesForDigraphFun "Body" [ dotExp' body ]
dotFun' (Lam fun)   = linesForDigraphFun "Lam"  [ dotFun' fun ]

dotArray' :: forall dim a. Array dim a -> State DotState Node
dotArray' (Array sh _) = leafNode arrayColor ("Array" ++ show (toElt sh :: dim))

dotBoundary' :: forall aenv dim e. Elt e => {-dummy-}OpenAcc aenv (Array dim e) -> Boundary (EltRepr e)
             -> State DotState Node
dotBoundary' _ Clamp        = leafNode boundaryColor "Clamp"
dotBoundary' _ Mirror       = leafNode boundaryColor ("Mirror")
dotBoundary' _ Wrap         = leafNode boundaryColor ("Wrap")
dotBoundary' _ (Constant e) = leafNode boundaryColor ("Constant " ++ show (toElt e :: e))


dotTuple' :: Tuple (OpenExp env aenv) t -> State DotState Node
dotTuple' NilTup          = leafNode tupleColor ("NilTup")
dotTuple' (SnocTup tup e) = linesForDigraphTuple "SnocTup" [ dotTuple' tup, dotExp' e ]

labelForPrimFun :: PrimFun a -> String
labelForPrimFun (PrimAdd _)         = "PrimAdd"
labelForPrimFun (PrimSub _)         = "PrimSub"
labelForPrimFun (PrimMul _)         = "PrimMul"
labelForPrimFun (PrimNeg _)         = "PrimNeg"
labelForPrimFun (PrimAbs _)         = "PrimAbs"
labelForPrimFun (PrimSig _)         = "PrimSig"
labelForPrimFun (PrimQuot _)        = "PrimQuot"
labelForPrimFun (PrimRem _)         = "PrimRem"
labelForPrimFun (PrimIDiv _)        = "PrimIDiv"
labelForPrimFun (PrimMod _)         = "PrimMod"
labelForPrimFun (PrimBAnd _)        = "PrimBAnd"
labelForPrimFun (PrimBOr _)         = "PrimBOr"
labelForPrimFun (PrimBXor _)        = "PrimBXor"
labelForPrimFun (PrimBNot _)        = "PrimBNot"
labelForPrimFun (PrimBShiftL _)     = "PrimBShiftL"
labelForPrimFun (PrimBShiftR _)     = "PrimBShiftR"
labelForPrimFun (PrimBRotateL _)    = "PrimBRotateL"
labelForPrimFun (PrimBRotateR _)    = "PrimBRotateR"
labelForPrimFun (PrimFDiv _)        = "PrimFDiv"
labelForPrimFun (PrimRecip _)       = "PrimRecip"
labelForPrimFun (PrimSin _)         = "PrimSin"
labelForPrimFun (PrimCos _)         = "PrimCos"
labelForPrimFun (PrimTan _)         = "PrimTan"
labelForPrimFun (PrimAsin _)        = "PrimAsin"
labelForPrimFun (PrimAcos _)        = "PrimAcos"
labelForPrimFun (PrimAtan _)        = "PrimAtan"
labelForPrimFun (PrimAsinh _)       = "PrimAsinh"
labelForPrimFun (PrimAcosh _)       = "PrimAcosh"
labelForPrimFun (PrimAtanh _)       = "PrimAtanh"
labelForPrimFun (PrimExpFloating _) = "PrimExpFloating"
labelForPrimFun (PrimSqrt _)        = "PrimSqrt"
labelForPrimFun (PrimLog _)         = "PrimLog"
labelForPrimFun (PrimFPow _)        = "PrimFPow"
labelForPrimFun (PrimLogBase _)     = "PrimLogBase"
labelForPrimFun (PrimAtan2 _)       = "PrimAtan2"
labelForPrimFun (PrimLt _)          = "PrimLt"
labelForPrimFun (PrimGt _)          = "PrimGt"
labelForPrimFun (PrimLtEq _)        = "PrimLtEq"
labelForPrimFun (PrimGtEq _)        = "PrimGtEq"
labelForPrimFun (PrimEq _)          = "PrimEq"
labelForPrimFun (PrimNEq _)         = "PrimNEq"
labelForPrimFun (PrimMax _)         = "PrimMax"
labelForPrimFun (PrimMin _)         = "PrimMin"
labelForPrimFun PrimLAnd            = "PrimLAnd"
labelForPrimFun PrimLOr             = "PrimLOr"
labelForPrimFun PrimLNot            = "PrimLNot"
labelForPrimFun PrimOrd             = "PrimOrd"
labelForPrimFun PrimChr             = "PrimChr"
labelForPrimFun PrimRoundFloatInt   = "PrimRoundFloatInt"
labelForPrimFun PrimTruncFloatInt   = "PrimTruncFloatInt"
labelForPrimFun PrimIntFloat        = "PrimIntFloat"
labelForPrimFun PrimBoolToInt       = "PrimBoolToInt"

labelForConst :: PrimConst a -> String
labelForConst (PrimMinBound _) = "PrimMinBound"
labelForConst (PrimMaxBound _) = "PrimMaxBound"
labelForConst (PrimPi       _) = "PrimPi"

tupleIdxToInt :: TupleIdx t e -> Int
tupleIdxToInt ZeroTupIdx     = 0
tupleIdxToInt (SuccTupIdx n) = 1 + tupleIdxToInt n

-- Auxilliary ops
--

-- Convert a typed de Brujin index to the corresponding integer
--
idxToInt :: Idx env t -> Int
idxToInt ZeroIdx       = 0
idxToInt (SuccIdx idx) = 1 + idxToInt idx

linesForDigraphWithColor :: String -> String -> [State DotState Node] -> State DotState Node
linesForDigraphWithColor color source targets = do
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

nodeDef :: String -> String -> String -> String
nodeDef nodeId label color = printf "%s [ color=\"%s\", label=\"%s\" ];" nodeId color label

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
