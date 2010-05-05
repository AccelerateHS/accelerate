
module Data.Array.Accelerate.CUDA.CodeGen.Device
  (
  -- The available kernels
  Kernel(..),

  -- The operations
  fold, map, zipWith,
  )
  where

import Prelude   hiding (map, zipWith)
import qualified Prelude

import Data.Array.Accelerate.CUDA.Scalar
import Data.Array.Accelerate.CUDA.Syntax

data Kernel = BPermute
            | Compact
            | Map
            | Permute
            | Replicate
            | Scan4
            | SegmentedScan4
            | VectorAddUniform4
            | VectorSegmentedAddUniform4
            | Zip
            | ZipWith
            | ZipWith3
            deriving Show

--
-- The Device Code Generator
--
-- It takes the list of the device functions to be generated, and generates
-- the code accordingly.
--
bpermute :: String -> Scalar -> TransUnit
bpermute progName scalar = TransUnit
  [LocalInclude $ Ident $ progName ++ ".h"]
  [FuncDef
    [ DeclnStSpec $ Extern (Just $ StrLit "C")
    , DeclnTyQual Global, DeclnTySpec Void]
    (Declr Nothing (IdentDeclr $ Ident $ "_" ++ progName))
    (Prelude.map
      (\ (ty, ptr, varName) -> Decln
        ty [InitDeclr (Declr ptr (IdentDeclr $ Ident varName)) Nothing])
      [ ( [DeclnTyQual Const, DeclnTySpec ty]
        , Just $ Pointer [[]], "d_xs")
      , ( [DeclnTyQual Const, DeclnTySpec (Int $ Just Unsigned)]
        , Just $ Pointer [[]], "d_idx")
      , ( [DeclnTySpec ty], Just $ Pointer [[]], "d_out")
      , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "n")
      , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "isFullBlock")])
    (Blk $
      (Prelude.map
        (\ (ty, ptr, varName, exp) -> DeclnItem $ Decln
          ty [InitDeclr (Declr ptr (IdentDeclr $ Ident varName)) exp])
        [ ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "i", Nothing)
        , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "thid"
          , Just $ AssignExp $ toAssignExp $ StructMem
            (toPostfixExp $ Ident "threadIdx") (Ident "x"))
        , ( [DeclnVecTySpec $ Vector ty 4]
          , Nothing, "tempData", Nothing)
        , ( [DeclnVecTySpec $ Vector (Int $ Just Unsigned) 4], Nothing, "tempIdx", Nothing)
        , ( [DeclnVecTySpec $ Vector (Int $ Just Unsigned) 4]
          , Just $ Pointer [[]], "idxData"
          , Just $ AssignExp $ toAssignExp $ TyCast
            (TyName
              [SpecQualVecTySpec $ Vector (Int $ Just Unsigned) 4]
              (Just $ AbstDeclrPointer $ Pointer[[]]))
            (toCastExp $ Ident "d_idx"))
        , ( [DeclnVecTySpec $ Vector ty 4]
          , Just $ Pointer [[]], "outData"
          , Just $ AssignExp $ toAssignExp $ TyCast
            (TyName
              [SpecQualVecTySpec $ Vector ty 4]
              (Just $ AbstDeclrPointer $ Pointer[[]]))
            (toCastExp $ Ident "d_out"))
        , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "devOffset"
          , Just $ AssignExp $ toAssignExp $ FuncCall
            (toPostfixExp $ Ident "__umul24")
            (ArgExpList
              [ toAssignExp $ StructMem
                (toPostfixExp $ Ident "blockIdx") (Ident "x")
              , toAssignExp $ LShft
                (toShftExp $ StructMem
                  (toPostfixExp $ Ident "blockDim") (Ident "x"))
                (toAddExp $ IntegerConst 1)]))
        , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "aiDev"
          , Just $ AssignExp $ toAssignExp $ Add
            (toAddExp $ Ident "devOffset") (toMulExp $ Ident "thid"))
        , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "biDev"
          , Just $ AssignExp $ toAssignExp $ Add
            (toAddExp $ Ident "aiDev")
            (toMulExp $ StructMem
              (toPostfixExp $ Ident "blockDim") (Ident "x")))])
      ++
      (concatMap
        (\ indexDev ->
          [ StmtItem $ ExpStmt $ Just $ Exp [toAssignExp $ FuncCall
            (toPostfixExp $ Ident "__syncthreads") (ArgExpList [])]
          , StmtItem $ ExpStmt $ Just $ Exp [Assign
            (toUnaryExp $ Ident "i")
            (toAssignExp $ Mul
              (toMulExp $ indexDev) (toCastExp $ IntegerConst 4))]
          , StmtItem $ ExpStmt $ Just $ Exp [Assign
            (toUnaryExp $ Ident "tempIdx")
            (toAssignExp $ toArrayElem (Ident "idxData") indexDev)]
          , StmtItem $ SelectStmt $ IfElse
            (Exp [toAssignExp $ Prelude.foldl
              LgcAnd
              (toLgcAndExp $ NestedExp $ Exp [toAssignExp $ LgcOr
                (toLgcOrExp $ Ident "isFullBlock")
                (toLgcAndExp $ Lt
                  (toRelExp $ Add
                    (toAddExp $ Ident "i") (toMulExp $ IntegerConst 3))
                  (toShftExp $ Ident "n"))])
              (Prelude.map
                (\ fieldName -> toOrExp $ Lt
                  (toRelExp $ StructMem
                    (toPostfixExp $ Ident "tempIdx") (Ident fieldName))
                  (toShftExp $ Ident "n"))
                ["x", "y", "z", "w"])])
            (CompStmt $ Blk $
              (Prelude.map
                (\ fieldName -> StmtItem $ ExpStmt $ Just $ Exp [Assign
                  (toUnaryExp $ StructMem
                    (toPostfixExp $ Ident "tempData") (Ident fieldName))
                  (toAssignExp $ toArrayElem
                    (Ident "d_xs")
                    (StructMem
                      (toPostfixExp $ Ident "tempIdx") (Ident fieldName)))])
                ["x", "y", "z", "w"])
              ++
              [StmtItem $ ExpStmt $ Just $ Exp [Assign
                (toUnaryExp $ toArrayElem (Ident "outData") indexDev)
                (toAssignExp $ Ident "tempData")]])
            (CompStmt $ Blk $ Prelude.zipWith
              (\ fieldName index -> StmtItem $ SelectStmt $ If
                (Exp [toAssignExp $ LgcAnd
                  (toLgcAndExp $ Lt (toRelExp  index) (toShftExp $ Ident "n"))
                  (toOrExp $ Lt
                    (toRelExp $ StructMem
                      (toPostfixExp $ Ident "tempIdx") (Ident fieldName))
                    (toShftExp $ Ident "n"))])
                (ExpStmt $ Just $ Exp [Assign
                    (toUnaryExp $ toArrayElem (Ident "d_out") index)
                    (toAssignExp $ toArrayElem
                      (Ident "d_xs")
                      (StructMem
                        (toPostfixExp $ Ident "tempIdx")
                        (Ident fieldName)))]))
              [ "x", "y", "z", "w"]
              [ toAddExp $ Ident "i"
              , Add (toAddExp $ Ident "i") (toMulExp $ IntegerConst 1)
              , Add (toAddExp $ Ident "i") (toMulExp $ IntegerConst 2)
              , Add (toAddExp $ Ident "i") (toMulExp $ IntegerConst 3)])])
        [Ident "aiDev", Ident "biDev"]))]
  where
    ty = outTy scalar

fold :: String -> Scalar -> (TySpec, String) -> TransUnit
fold progName scalar left = TransUnit
  [ ]
  [ FuncDef
    [DeclnTyQual Device, DeclnTySpec outTy']
    (Declr Nothing (IdentDeclr $ Ident $ "_" ++ progName ++ "Scalar"))
    (Prelude.map
      (\ (ty, varName) -> Decln
        [DeclnTySpec ty]
        [InitDeclr (Declr Nothing (IdentDeclr $ Ident varName)) Nothing])
      (params scalar))
    (Blk $ comp scalar)
  , loadSharedChunkFromMem4 progName outTy' True False identity'
  , warpscan progName outTy' identity'
  , scanWarps progName outTy' True identity'
  , scanCTA progName outTy' True identity'
  , scan4 progName outTy' True identity']
  where
    outTy'    = outTy scalar
    identity' = identity scalar

foldl1 :: String -> Scalar -> TransUnit
foldl1 progName scalar = TransUnit
  [ LocalInclude $ Ident $ progName ++ ".h"]
  [ FuncDef
    [DeclnTyQual Device, DeclnTySpec outTy']
    (Declr Nothing (IdentDeclr $ Ident $ "_" ++ progName ++ "Scalar"))
    (Prelude.map
      (\ (ty, varName) -> Decln
        [DeclnTySpec ty]
        [InitDeclr (Declr Nothing (IdentDeclr $ Ident varName)) Nothing])
      (params scalar))
    (Blk $ comp scalar)
  , loadSharedChunkFromMem4 progName outTy' True False identity'
  , warpscan progName outTy' identity'
  , scanWarps progName outTy' True identity'
  , scanCTA progName outTy' True identity'
  , storeSharedChunkToMem4 progName outTy' True False identity'
  , scan4 progName outTy' True identity']
  where
    outTy'    = outTy scalar
    identity' = identity scalar

foldr :: String -> Scalar -> (TySpec, String) -> TransUnit
foldr progName scalar right = TransUnit
  [ LocalInclude $ Ident $ progName ++ ".h"]
  [ FuncDef
    [DeclnTyQual Device, DeclnTySpec outTy']
    (Declr Nothing (IdentDeclr $ Ident $ "_" ++ progName ++ "Scalar"))
    (Prelude.map
      (\ (ty, varName) -> Decln
        [DeclnTySpec ty]
        [InitDeclr (Declr Nothing (IdentDeclr $ Ident varName)) Nothing])
      (params scalar))
    (Blk $ comp scalar)
  , loadSharedChunkFromMem4 progName outTy' True True identity'
  , warpscan progName outTy' identity'
  , scanWarps progName outTy' True identity'
  , scanCTA progName outTy' True identity'
  , storeSharedChunkToMem4 progName outTy' True True identity'
  , scan4 progName outTy' True identity']
  where
    outTy'    = outTy scalar
    identity' = identity scalar

foldr1 :: String -> Scalar -> TransUnit
foldr1 progName scalar = TransUnit
  [ LocalInclude $ Ident $ progName ++ ".h"]
  [ FuncDef
    [DeclnTyQual Device, DeclnTySpec outTy']
    (Declr Nothing (IdentDeclr $ Ident $ "_" ++ progName ++ "Scalar"))
    (Prelude.map
      (\ (ty, varName) -> Decln
        [DeclnTySpec ty]
        [InitDeclr (Declr Nothing (IdentDeclr $ Ident varName)) Nothing])
      (params scalar))
    (Blk $ comp scalar)
  , loadSharedChunkFromMem4 progName outTy' True True identity'
  , warpscan progName outTy' identity'
  , scanWarps progName outTy' True identity'
  , scanCTA progName outTy' True identity'
  , storeSharedChunkToMem4 progName outTy' True True identity'
  , scan4 progName outTy' True identity']
  where
    outTy'    = outTy scalar
    identity' = identity scalar

map :: String -> Scalar -> TransUnit
map progName scalar = TransUnit
  [ ]
  [ FuncDef
    [DeclnTyQual Device, DeclnTySpec outTy']
    (Declr Nothing (IdentDeclr $ Ident $ "_" ++ progName ++ "Scalar"))
    [Decln
      [DeclnTySpec xsTy]
      [InitDeclr (Declr Nothing (IdentDeclr $ Ident xsName)) Nothing]]
    (Blk $ comp scalar)
  , FuncDef
    [ DeclnStSpec $ Extern (Just $ StrLit "C")
    , DeclnTyQual Global, DeclnTySpec Void]
    (Declr Nothing (IdentDeclr $ Ident $ "_" ++ progName))
    (Prelude.map
      (\ (ty, ptr, varName) -> Decln
        ty [InitDeclr (Declr ptr (IdentDeclr $ Ident varName)) Nothing])
      [ ( [DeclnTyQual Const, DeclnTySpec xsTy]
        , Just $ Pointer [[]], "d_xs")
      , ( [DeclnTySpec outTy'], Just $ Pointer [[]], "d_out")
      , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "n")
      , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "isFullBlock")])
    (Blk $
      (Prelude.map
        (\ (ty, ptr, varName, exp) -> DeclnItem $ Decln
          ty [InitDeclr (Declr ptr (IdentDeclr $ Ident varName)) exp])
        [ ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "i", Nothing)
        , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "thid"
          , Just $ AssignExp $ toAssignExp $ StructMem
            (toPostfixExp $ Ident "threadIdx") (Ident "x"))
        , ( [DeclnVecTySpec $ Vector xsTy 2], Just $ Pointer [[]], "inData"
          , Just $ AssignExp $ toAssignExp $ TyCast
            (TyName
              [SpecQualVecTySpec $ Vector xsTy 2]
              (Just $ AbstDeclrPointer $ Pointer[[]]))
            (toCastExp $ Ident "d_xs"))
        , ( [DeclnVecTySpec $ Vector outTy' 2], Just $ Pointer [[]], "outData"
          , Just $ AssignExp $ toAssignExp $ TyCast
            (TyName
              [SpecQualVecTySpec $ Vector outTy' 2]
              (Just $ AbstDeclrPointer $ Pointer[[]]))
            (toCastExp $ Ident "d_out"))
        , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "devOffset"
          , Just $ AssignExp $ toAssignExp $ FuncCall
            (toPostfixExp $ Ident "__umul24")
            (ArgExpList
              [ toAssignExp $ StructMem
                (toPostfixExp $ Ident "blockIdx") (Ident "x")
              , toAssignExp $ StructMem
                  (toPostfixExp $ Ident "blockDim") (Ident "x")]))
        , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "aiDev"
          , Just $ AssignExp $ toAssignExp $ Add
            (toAddExp $ Ident "devOffset") (toMulExp $ Ident "thid"))])
      ++
      [ StmtItem $ ExpStmt $ Just $ Exp [Assign
        (toUnaryExp $ Ident "i")
        (toAssignExp $ Mul
          (toMulExp $ Ident "aiDev") (toCastExp $ IntegerConst 2))]
      , StmtItem $ SelectStmt $ IfElse
        (Exp [toAssignExp $ LgcOr
          (toLgcOrExp $ Ident "isFullBlock")
          (toLgcAndExp $ Lt
            (toRelExp $ Add
              (toAddExp $ Ident "i") (toMulExp $ IntegerConst 1))
            (toShftExp $ Ident "n"))])
        (CompStmt $ Blk $
          [StmtItem $ ExpStmt $ Just $ Exp [Assign
            (toUnaryExp $ toArrayElem (Ident "outData") (Ident "aiDev"))
            (toAssignExp (FuncCall
              (toPostfixExp $ Ident $ "make_" ++ (show $ Vector outTy' 2))
              (ArgExpList $ Prelude.map
                (\ fieldName -> toAssignExp $ FuncCall
                  (toPostfixExp $ Ident $ "_" ++ progName ++ "Scalar")
                  (ArgExpList [toAssignExp $ StructMem
                    (toArrayElem (Ident "inData") (Ident "aiDev"))
                    (Ident fieldName)]))
                ["x", "y"])))]])
        (SelectStmt $ Prelude.foldr1
          (\ (If exp stmt) x -> If
            exp (CompStmt $ Blk [StmtItem stmt, StmtItem $ SelectStmt x]))
          (Prelude.zipWith
            (\ fieldName index -> If
              (Exp [toAssignExp $ Lt
                (toRelExp index) (toShftExp $ Ident "n")])
              (ExpStmt $ Just $ Exp [Assign
                (toUnaryExp $ toArrayElem (Ident "d_out") index)
                (toAssignExp $ FuncCall
                  (toPostfixExp $ Ident $ "_" ++ progName ++ "Scalar")
                  (ArgExpList [toAssignExp $ StructMem
                    (toArrayElem (Ident "inData") (Ident "aiDev"))
                    (Ident fieldName)]))]))
            [ "x", "y"]
            [ toAddExp $ Ident "i"
            , Add (toAddExp $ Ident "i") (toMulExp $ IntegerConst 1)]))])]
  where
    (xsTy, xsName) = head $ params scalar
    outTy'         = outTy scalar

permute :: String -> Scalar -> TransUnit
permute progName scalar = TransUnit
  [LocalInclude $ Ident $ progName ++ ".h"]
  [FuncDef
    [ DeclnStSpec $ Extern (Just $ StrLit "C")
    , DeclnTyQual Global, DeclnTySpec Void]
    (Declr Nothing (IdentDeclr $ Ident $ "_" ++ progName))
    (Prelude.map
      (\ (ty, ptr, varName) -> Decln
        ty [InitDeclr (Declr ptr (IdentDeclr $ Ident varName)) Nothing])
      [ ( [DeclnTyQual Const, DeclnTySpec ty]
        , Just $ Pointer [[]], "d_xs")
      , ( [DeclnTyQual Const, DeclnTySpec (Int $ Just Unsigned)]
        , Just $ Pointer [[]], "d_idx")
      , ( [DeclnTySpec ty], Just $ Pointer [[]], "d_out")
      , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "n")])
    (Blk $
      (Prelude.map
        (\ (ty, ptr, varName, exp) -> DeclnItem $ Decln
          ty [InitDeclr (Declr ptr (IdentDeclr $ Ident varName)) exp])
        [ ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "i", Nothing)
        , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "thid"
          , Just $ AssignExp $ toAssignExp $ StructMem
            (toPostfixExp $ Ident "threadIdx") (Ident "x"))
        , ( [DeclnVecTySpec $ Vector ty 4], Nothing, "tempData", Nothing)
        , ( [DeclnVecTySpec $ Vector (Int $ Just Unsigned) 4], Nothing, "tempIdx", Nothing)
        , ( [DeclnVecTySpec $ Vector ty 4], Just $ Pointer [[]], "inData"
          , Just $ AssignExp $ toAssignExp $ TyCast
            (TyName
              [SpecQualVecTySpec $ Vector ty 4]
              (Just $ AbstDeclrPointer $ Pointer[[]]))
            (toCastExp $ Ident "d_xs"))
        , ( [DeclnVecTySpec $ Vector (Int $ Just Unsigned) 4]
          , Just $ Pointer [[]], "idxData"
          , Just $ AssignExp $ toAssignExp $ TyCast
            (TyName
              [SpecQualVecTySpec $ Vector (Int $ Just Unsigned) 4]
              (Just $ AbstDeclrPointer $ Pointer[[]]))
            (toCastExp $ Ident "d_idx"))
        , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "devOffset"
          , Just $ AssignExp $ toAssignExp $ FuncCall
            (toPostfixExp $ Ident "__umul24")
            (ArgExpList
              [ toAssignExp $ StructMem
                (toPostfixExp $ Ident "blockIdx") (Ident "x")
              , toAssignExp $ LShft
                (toShftExp $ StructMem
                  (toPostfixExp $ Ident "blockDim") (Ident "x"))
                (toAddExp $ IntegerConst 1)]))
        , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "aiDev"
          , Just $ AssignExp $ toAssignExp $ Add
            (toAddExp $ Ident "devOffset") (toMulExp $ Ident "thid"))
        , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "biDev"
          , Just $ AssignExp $ toAssignExp $ Add
            (toAddExp $ Ident "aiDev")
            (toMulExp $ StructMem
              (toPostfixExp $ Ident "blockDim") (Ident "x")))])
      ++
      (concatMap
        (\ idx ->
          [ StmtItem $ ExpStmt $ Just $ Exp [toAssignExp $ FuncCall
            (toPostfixExp $ Ident "__syncthreads") (ArgExpList [])]
          , StmtItem $ ExpStmt $ Just $ Exp [Assign
            (toUnaryExp $ Ident "i")
            (toAssignExp $ Mul (toMulExp $ idx) (toCastExp $ IntegerConst 4))]
          , StmtItem $ ExpStmt $ Just $ Exp $ Prelude.zipWith
            (\ x y -> Assign
              (toUnaryExp $ Ident x)
              (toAssignExp $ toArrayElem (Ident y) idx))
            ["tempData", "tempIdx"] ["inData", "idxData"]]
          ++
          (Prelude.zipWith
            (\ fieldId index -> StmtItem $ SelectStmt $ If
              (Exp [toAssignExp $ LgcAnd
                (toLgcAndExp $ Lt (toRelExp  index) (toShftExp $ Ident "n"))
                (toOrExp $ Lt
                  (toRelExp $ StructMem
                    (toPostfixExp $ Ident "tempIdx") (Ident fieldId))
                  (toShftExp $ Ident "n"))])
              (ExpStmt $ Just $ Exp [Assign
                (toUnaryExp $ toArrayElem
                  (Ident "d_out")
                  (StructMem
                    (toPostfixExp $ Ident "tempIdx") (Ident fieldId)))
                (toAssignExp $ StructMem
                  (toPostfixExp $ Ident "tempData") (Ident fieldId))]))
            [ "x", "y", "z", "w"]
            [ toAddExp $ Ident "i"
            , Add (toAddExp $ Ident "i") (toMulExp $ IntegerConst 1)
            , Add (toAddExp $ Ident "i") (toMulExp $ IntegerConst 2)
            , Add (toAddExp $ Ident "i") (toMulExp $ IntegerConst 3)]))
        [Ident "aiDev", Ident "biDev"]))]
  where
    ty = outTy scalar

replicate :: String -> Scalar -> TransUnit
replicate progName scalar = TransUnit
  [ LocalInclude $ Ident $ progName ++ ".h"]
  [ FuncDef
    [DeclnTyQual Device, DeclnTySpec ty]
    (Declr Nothing (IdentDeclr $ Ident $ "_" ++ progName ++ "Scalar"))
    []
    (Blk $ comp scalar)
  , FuncDef
    [ DeclnStSpec $ Extern (Just $ StrLit "C")
    , DeclnTyQual Global, DeclnTySpec Void]
    (Declr Nothing (IdentDeclr $ Ident $ "_" ++ progName))
    (Prelude.map
      (\ (ty, ptr, varName) -> Decln
        [ty] [InitDeclr (Declr ptr (IdentDeclr $ Ident varName)) Nothing])
      [ (DeclnTySpec ty, Just $ Pointer [[]], "d_out")
      , (DeclnTySpec (Int $ Just Unsigned), Nothing, "n")
      , (DeclnTySpec (Int $ Just Unsigned), Nothing, "isFullBlock")])
    (Blk $
      (Prelude.map
        (\ (ty, ptr, varName, exp) -> DeclnItem $ Decln
          ty [InitDeclr (Declr ptr (IdentDeclr $ Ident varName)) exp])
        [ ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "i", Nothing)
        , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "thid"
          , Just $ AssignExp $ toAssignExp $ StructMem
            (toPostfixExp $ Ident "threadIdx") (Ident "x"))
        , ( [DeclnVecTySpec $ Vector ty 4], Nothing, "tempData", Nothing)
        , ( [DeclnVecTySpec $ Vector ty 4], Just $ Pointer [[]], "outData"
          , Just $ AssignExp $ toAssignExp $ TyCast
            (TyName
              [SpecQualVecTySpec $ Vector ty 4]
              (Just $ AbstDeclrPointer $ Pointer[[]]))
            (toCastExp $ Ident "d_out"))
        , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "devOffset"
          , Just $ AssignExp $ toAssignExp $ FuncCall
            (toPostfixExp $ Ident "__umul24")
            (ArgExpList
              [ toAssignExp $ StructMem
                (toPostfixExp $ Ident "blockIdx") (Ident "x")
              , toAssignExp $ LShft
                (toShftExp $ StructMem
                  (toPostfixExp $ Ident "blockDim") (Ident "x"))
                (toAddExp $ IntegerConst 1)]))
        , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "aiDev"
          , Just $ AssignExp $ toAssignExp $ Add
            (toAddExp $ Ident "devOffset") (toMulExp $ Ident "thid"))
        , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "biDev"
          , Just $ AssignExp $ toAssignExp $ Add
            (toAddExp $ Ident "aiDev")
            (toMulExp $ StructMem
              (toPostfixExp $ Ident "blockDim") (Ident "x")))])
      ++
      (concatMap
        (\ indexDev ->
          [ StmtItem $ ExpStmt $ Just $ Exp [toAssignExp $ FuncCall
            (toPostfixExp $ Ident "__syncthreads") (ArgExpList [])]
          , StmtItem $ ExpStmt $ Just $ Exp [Assign
            (toUnaryExp $ Ident "i")
            (toAssignExp $ Mul
              (toMulExp $ indexDev) (toCastExp $ IntegerConst 4))]
          , StmtItem $ SelectStmt $ IfElse
            (Exp [toAssignExp $ LgcOr
              (toLgcOrExp $ Ident "isFullBlock")
              (toLgcAndExp $ Lt
                (toRelExp $ Add
                  (toAddExp $ Ident "i") (toMulExp $ IntegerConst 3))
                (toShftExp $ Ident "n"))])
            (CompStmt $ Blk $
              (Prelude.map
                (\ fieldName -> StmtItem $ ExpStmt $ Just $ Exp [Assign
                  (toUnaryExp $ StructMem
                    (toPostfixExp $ Ident "tempData") (Ident fieldName))
                  (toAssignExp $ FuncCall
                    (toPostfixExp $ Ident $ "_" ++ progName ++ "Scalar")
                    (ArgExpList []))])
                ["x", "y", "z", "w"])
              ++
              [StmtItem $ ExpStmt $ Just $ Exp [Assign
                (toUnaryExp $ toArrayElem (Ident "outData") indexDev)
                (toAssignExp $ Ident "tempData")]])
            (SelectStmt $ Prelude.foldr1
              (\ (If exp stmt) x -> If
                exp (CompStmt $ Blk [StmtItem stmt, StmtItem $ SelectStmt x]))
              (Prelude.map
                (\ index -> If
                  (Exp [toAssignExp $ Lt
                    (toRelExp index) (toShftExp $ Ident "n")])
                  (ExpStmt $ Just $ Exp [Assign
                    (toUnaryExp $ toArrayElem (Ident "d_out") index)
                    (toAssignExp $ FuncCall
                      (toPostfixExp $ Ident $ "_" ++ progName ++ "Scalar")
                      (ArgExpList []))]))
                [ toAddExp $ Ident "i"
                , Add (toAddExp $ Ident "i") (toMulExp $ IntegerConst 1)
                , Add (toAddExp $ Ident "i") (toMulExp $ IntegerConst 2)]))])
        [Ident "aiDev", Ident "biDev"]))]
  where
    ty = outTy scalar

scanl :: String -> Scalar -> (TySpec, String) -> TransUnit
scanl progName scalar left = TransUnit
  [ LocalInclude $ Ident $ progName ++ ".h"]
  [ FuncDef
    [DeclnTyQual Device, DeclnTySpec outTy']
    (Declr Nothing (IdentDeclr $ Ident $ "_" ++ progName ++ "Scalar"))
    (Prelude.map
      (\ (ty, varName) -> Decln
        [DeclnTySpec ty]
        [InitDeclr (Declr Nothing (IdentDeclr $ Ident varName)) Nothing])
      (params scalar))
    (Blk $ comp scalar)
  , loadSharedChunkFromMem4 progName outTy' True False identity'
  , warpscan progName outTy' identity'
  , scanWarps progName outTy' True identity'
  , scanCTA progName outTy' True identity'
  , storeSharedChunkToMem4 progName outTy' True False identity'
  , scan4 progName outTy' True identity'
  , vectorAddUniform4 progName outTy' identity']
  where
    outTy'    = outTy scalar
    identity' = identity scalar

scanl1 :: String -> Scalar -> TransUnit
scanl1 progName scalar = TransUnit
  [ LocalInclude $ Ident $ progName ++ ".h"]
  [ FuncDef
    [DeclnTyQual Device, DeclnTySpec outTy']
    (Declr Nothing (IdentDeclr $ Ident $ "_" ++ progName ++ "Scalar"))
    (Prelude.map
      (\ (ty, varName) -> Decln
        [DeclnTySpec ty]
        [InitDeclr (Declr Nothing (IdentDeclr $ Ident varName)) Nothing])
      (params scalar))
    (Blk $ comp scalar)
  , loadSharedChunkFromMem4 progName outTy' False False identity'
  , warpscan progName outTy' identity'
  , scanWarps progName outTy' False identity'
  , scanCTA progName outTy' False identity'
  , storeSharedChunkToMem4 progName outTy' False False identity'
  , scan4 progName outTy' False identity'
  , vectorAddUniform4 progName outTy' identity']
  where
    outTy'    = outTy scalar
    identity' = identity scalar

scanr :: String -> Scalar -> (TySpec, String) -> TransUnit
scanr progName scalar right = TransUnit
  [ LocalInclude $ Ident $ progName ++ ".h"]
  [ FuncDef
    [DeclnTyQual Device, DeclnTySpec outTy']
    (Declr Nothing (IdentDeclr $ Ident $ "_" ++ progName ++ "Scalar"))
    (Prelude.map
      (\ (ty, varName) -> Decln
        [DeclnTySpec ty]
        [InitDeclr (Declr Nothing (IdentDeclr $ Ident varName)) Nothing])
      (params scalar))
    (Blk $ comp scalar)
  , loadSharedChunkFromMem4 progName outTy' True True identity'
  , warpscan progName outTy' identity'
  , scanWarps progName outTy' True identity'
  , scanCTA progName outTy' True identity'
  , storeSharedChunkToMem4 progName outTy' True True identity'
  , scan4 progName outTy' True identity'
  , vectorAddUniform4 progName outTy' identity']
  where
    outTy'    = outTy scalar
    identity' = identity scalar

scanr1 :: String -> Scalar -> TransUnit
scanr1 progName scalar = TransUnit
  [ LocalInclude $ Ident $ progName ++ ".h"]
  [ FuncDef
    [DeclnTyQual Device, DeclnTySpec outTy']
    (Declr Nothing (IdentDeclr $ Ident $ "_" ++ progName ++ "Scalar"))
    (Prelude.map
      (\ (ty, varName) -> Decln
        [DeclnTySpec ty]
        [InitDeclr (Declr Nothing (IdentDeclr $ Ident varName)) Nothing])
      (params scalar))
    (Blk $ comp scalar)
  , loadSharedChunkFromMem4 progName outTy' False True identity'
  , warpscan progName outTy' identity'
  , scanWarps progName outTy' False identity'
  , scanCTA progName outTy' False identity'
  , storeSharedChunkToMem4 progName outTy' False True identity'
  , scan4 progName outTy' False identity'
  , vectorAddUniform4 progName outTy' identity']
  where
    outTy'    = outTy scalar
    identity' = identity scalar

zip :: String -> Scalar -> TransUnit
zip progName scalar = TransUnit
  [LocalInclude $ Ident $ progName ++ ".h"]
  [FuncDef
    [ DeclnStSpec $ Extern (Just $ StrLit "C")
    , DeclnTyQual Global, DeclnTySpec Void]
    (Declr Nothing (IdentDeclr $ Ident $ "_" ++ progName))
    (Prelude.map
      (\ (ty, ptr, varName) -> Decln
        ty [InitDeclr (Declr ptr (IdentDeclr $ Ident varName)) Nothing])
      [ ( [DeclnTyQual Const, DeclnTySpec xsTy]
        , Just $ Pointer [[]], "d_xs")
      , ( [DeclnTyQual Const, DeclnTySpec ysTy]
        , Just $ Pointer [[]], "d_ys")
      , ( [DeclnTySpec outTy'], Just $ Pointer [[]], "d_out")
      , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "n")
      , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "isFullBlock")])
    (Blk $
      (Prelude.map
        (\ (ty, ptr, varName, exp) -> DeclnItem $ Decln
          ty [InitDeclr (Declr ptr (IdentDeclr $ Ident varName)) exp])
        [ ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "i", Nothing)
        , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "thid"
          , Just $ AssignExp $ toAssignExp $ StructMem
            (toPostfixExp $ Ident "threadIdx") (Ident "x"))
        , ( [DeclnVecTySpec $ Vector xsTy 4], Nothing, "tempData0", Nothing)
        , ( [DeclnVecTySpec $ Vector ysTy 4], Nothing, "tempData1", Nothing)
        , ( [DeclnVecTySpec $ Vector outTy' 4], Nothing, "tempData", Nothing)
        , ( [DeclnVecTySpec $ Vector xsTy 4], Just $ Pointer [[]], "inData0"
          , Just $ AssignExp $ toAssignExp $ TyCast
            (TyName
              [SpecQualVecTySpec $ Vector xsTy 4]
              (Just $ AbstDeclrPointer $ Pointer[[]]))
            (toCastExp $ Ident "d_xs"))
        , ( [DeclnVecTySpec $ Vector ysTy 4], Just $ Pointer [[]], "inData1"
          , Just $ AssignExp $ toAssignExp $ TyCast
            (TyName
              [SpecQualVecTySpec $ Vector ysTy 4]
              (Just $ AbstDeclrPointer $ Pointer[[]]))
            (toCastExp $ Ident "d_ys"))
        , ( [DeclnVecTySpec $ Vector outTy' 4], Just $ Pointer [[]], "outData"
          , Just $ AssignExp $ toAssignExp $ TyCast
            (TyName
              [SpecQualVecTySpec $ Vector outTy' 4]
              (Just $ AbstDeclrPointer $ Pointer[[]]))
            (toCastExp $ Ident "d_out"))
        , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "devOffset"
          , Just $ AssignExp $ toAssignExp $ FuncCall
            (toPostfixExp $ Ident "__umul24")
            (ArgExpList
              [ toAssignExp $ StructMem
                (toPostfixExp $ Ident "blockIdx") (Ident "x")
              , toAssignExp $ LShft
                (toShftExp $ StructMem
                  (toPostfixExp $ Ident "blockDim") (Ident "x"))
                (toAddExp $ IntegerConst 1)]))
        , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "aiDev"
          , Just $ AssignExp $ toAssignExp $ Add
            (toAddExp $ Ident "devOffset") (toMulExp $ Ident "thid"))
        , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "biDev"
          , Just $ AssignExp $ toAssignExp $ Add
            (toAddExp $ Ident "aiDev")
            (toMulExp $ StructMem
              (toPostfixExp $ Ident "blockDim") (Ident "x")))])
      ++
      (concatMap
        (\ indexDev ->
          [ StmtItem $ ExpStmt $ Just $ Exp [toAssignExp $ FuncCall
            (toPostfixExp $ Ident "__syncthreads") (ArgExpList [])]
          , StmtItem $ ExpStmt $ Just $ Exp [Assign
            (toUnaryExp $ Ident "i")
            (toAssignExp $ Mul
              (toMulExp $ indexDev) (toCastExp $ IntegerConst 4))]]
          ++
          ( Prelude.map
            (\ (dstVarName, srcVarName) ->
              StmtItem $ ExpStmt $ Just $ Exp [Assign
                (toUnaryExp $ Ident dstVarName)
                (toAssignExp $ toArrayElem (Ident srcVarName) indexDev)])
            [("tempData0", "inData0"), ("tempData1", "inData1")])
          ++
          [ StmtItem $ SelectStmt $ IfElse
            (Exp [toAssignExp $ LgcOr
              (toLgcOrExp $ Ident "isFullBlock")
              (toLgcAndExp $ Lt
                (toRelExp $ Add
                  (toAddExp $ Ident "i") (toMulExp $ IntegerConst 3))
                (toShftExp $ Ident "n"))])
            (CompStmt $ Blk $
              (Prelude.map
                (\ fieldName ->
                  StmtItem $ ExpStmt $ Just $ Exp $ Prelude.zipWith
                    (\ fieldName' varName -> Assign
                      (toUnaryExp $ StructMem
                        (toPostfixExp $ StructMem
                          (toPostfixExp $ Ident "tempData") (Ident fieldName))
                        (Ident fieldName'))
                      (toAssignExp $ StructMem
                        (toPostfixExp $ Ident varName) (Ident fieldName)))
                    [ "x", "y"] [ "tempData0", "tempData1"])
                ["x", "y", "z", "w"])
              ++
              [StmtItem $ ExpStmt $ Just $ Exp [Assign
                (toUnaryExp $ toArrayElem (Ident "outData") indexDev)
                (toAssignExp $ Ident "tempData")]])
            (SelectStmt $ Prelude.foldr1
              (\ (If exp stmt) x -> If
                exp (CompStmt $ Blk [StmtItem stmt, StmtItem $ SelectStmt x]))
              (Prelude.zipWith
                (\ fieldName index -> If
                  (Exp [toAssignExp $ Lt
                    (toRelExp index) (toShftExp $ Ident "n")])
                  (ExpStmt $ Just $ Exp $ Prelude.zipWith
                    (\ fieldName' varName -> Assign
                      (toUnaryExp $ StructMem
                        (toArrayElem
                          (Ident "d_out") index) (Ident fieldName'))
                      (toAssignExp $ StructMem
                        (toPostfixExp $ Ident varName) (Ident fieldName)))
                    ["x", "y"] ["tempData0", "tempData1"]))
                [ "x", "y", "z"]
                [ toAddExp $ Ident "i"
                , Add (toAddExp $ Ident "i") (toMulExp $ IntegerConst 1)
                , Add (toAddExp $ Ident "i") (toMulExp $ IntegerConst 2)]))])
        [Ident "aiDev", Ident "biDev"]))]
  where
    [(xsTy, _), (ysTy, _)] = params scalar
    outTy'                 = outTy scalar

zipWith :: String -> Scalar -> TransUnit
zipWith progName scalar =
  TransUnit
    [ ]
    [ FuncDef
      [DeclnTyQual Device, DeclnTySpec outTy']
      (Declr Nothing (IdentDeclr $ Ident $ "_" ++ progName ++ "Scalar"))
      (Prelude.map
        (\ (ty, varName) -> Decln
          [DeclnTySpec ty]
          [InitDeclr (Declr Nothing (IdentDeclr $ Ident varName)) Nothing])
        (params scalar))
      (Blk $ comp scalar)
    , FuncDef
      [ DeclnStSpec $ Extern (Just $ StrLit "C")
      , DeclnTyQual Global, DeclnTySpec Void]
      (Declr Nothing (IdentDeclr $ Ident $ "_" ++ progName))
      (Prelude.map
        (\ (ty, ptr, varName) -> Decln
          ty [InitDeclr (Declr ptr (IdentDeclr $ Ident varName)) Nothing])
        [ ( [DeclnTyQual Const, DeclnTySpec xsTy]
          , Just $ Pointer [[]], "d_xs")
        , ( [DeclnTyQual Const, DeclnTySpec ysTy]
          , Just $ Pointer [[]], "d_ys")
        , ( [DeclnTySpec outTy'], Just $ Pointer [[]], "d_out")
        , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "n")
        , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "isFullBlock")])
      (Blk $
        (Prelude.map
          (\ (ty, ptr, varName, exp) -> DeclnItem $ Decln
            ty [InitDeclr (Declr ptr (IdentDeclr $ Ident varName)) exp])
          [ ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "i", Nothing)
          , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "thid"
            , Just $ AssignExp $ toAssignExp $ StructMem
              (toPostfixExp $ Ident "threadIdx") (Ident "x"))
          , ( [DeclnVecTySpec $ Vector xsTy 2], Just $ Pointer [[]], "inData0"
            , Just $ AssignExp $ toAssignExp $ TyCast
              (TyName
                [SpecQualVecTySpec $ Vector xsTy 2]
                (Just $ AbstDeclrPointer $ Pointer[[]]))
              (toCastExp $ Ident "d_xs"))
          , ( [DeclnVecTySpec $ Vector ysTy 2], Just $ Pointer [[]], "inData1"
            , Just $ AssignExp $ toAssignExp $ TyCast
              (TyName
                [SpecQualVecTySpec $ Vector ysTy 2]
                (Just $ AbstDeclrPointer $ Pointer[[]]))
              (toCastExp $ Ident "d_ys"))
          , ( [DeclnVecTySpec $ Vector outTy' 2]
            , Just $ Pointer [[]], "outData"
            , Just $ AssignExp $ toAssignExp $ TyCast
              (TyName
                [SpecQualVecTySpec $ Vector outTy' 2]
                (Just $ AbstDeclrPointer $ Pointer[[]]))
              (toCastExp $ Ident "d_out"))
          , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "devOffset"
            , Just $ AssignExp $ toAssignExp $ FuncCall
              (toPostfixExp $ Ident "__umul24")
              (ArgExpList
                [ toAssignExp $ StructMem
                  (toPostfixExp $ Ident "blockIdx") (Ident "x")
                , toAssignExp $ StructMem
                    (toPostfixExp $ Ident "blockDim") (Ident "x")]))
          , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "aiDev"
            , Just $ AssignExp $ toAssignExp $ Add
              (toAddExp $ Ident "devOffset") (toMulExp $ Ident "thid"))])
        ++
        [ StmtItem $ ExpStmt $ Just $ Exp [Assign
          (toUnaryExp $ Ident "i")
          (toAssignExp $ Mul
            (toMulExp $ Ident "aiDev") (toCastExp $ IntegerConst 2))]
        , StmtItem $ SelectStmt $ IfElse
          (Exp [toAssignExp $ LgcOr
            (toLgcOrExp $ Ident "isFullBlock")
            (toLgcAndExp $ Lt
              (toRelExp $ Add
                (toAddExp $ Ident "i") (toMulExp $ IntegerConst 1))
              (toShftExp $ Ident "n"))])
          (CompStmt $ Blk $
            [StmtItem $ ExpStmt $ Just $ Exp [Assign
              (toUnaryExp $ toArrayElem (Ident "outData") (Ident "aiDev"))
              (toAssignExp (FuncCall
                (toPostfixExp $ Ident $ "make_" ++ (show $ Vector outTy' 2))
                (ArgExpList $ Prelude.map
                  (\ fieldName -> toAssignExp $ FuncCall
                    (toPostfixExp $ Ident $ "_" ++ progName ++ "Scalar")
                    (ArgExpList $ Prelude.map
                      (\ arg -> toAssignExp $ StructMem
                        (toArrayElem (Ident arg) (Ident "aiDev"))
                        (Ident fieldName))
                      ["inData0", "inData1"]))
                  ["x", "y"])))]])
          (SelectStmt $ Prelude.foldr1
            (\ (If exp stmt) x -> If
              exp
              (CompStmt $ Blk [StmtItem stmt, StmtItem $ SelectStmt x]))
            (Prelude.zipWith
              (\ fieldName index -> If
                (Exp [toAssignExp $ Lt
                  (toRelExp index) (toShftExp $ Ident "n")])
                (ExpStmt $ Just $ Exp [Assign
                  (toUnaryExp $ toArrayElem (Ident "d_out") index)
                  (toAssignExp $ FuncCall
                    (toPostfixExp $ Ident $ "_" ++ progName ++ "Scalar")
                    (ArgExpList $ Prelude.map
                      (\ arg -> toAssignExp $ StructMem
                        (toArrayElem (Ident arg) (Ident "aiDev")) (Ident fieldName))
                      ["inData0", "inData1"]))]))
              [ "x", "y"]
              [ toAddExp $ Ident "i"
              , Add (toAddExp $ Ident "i") (toMulExp $ IntegerConst 1)]))])]
  where
    [(xsTy, _), (ysTy, _)] = params scalar
    outTy'                 = outTy scalar

zipWith3 :: String -> Scalar -> TransUnit
zipWith3 progName scalar = TransUnit
  [ LocalInclude $ Ident $ progName ++ ".h"]
  [ FuncDef
    [DeclnTyQual Device, DeclnTySpec outTy']
    (Declr Nothing (IdentDeclr $ Ident $ "_" ++ progName ++ "Scalar"))
    (Prelude.map
      (\ (ty, varName) -> Decln
        [DeclnTySpec ty]
        [InitDeclr (Declr Nothing (IdentDeclr $ Ident varName)) Nothing])
      (params scalar))
    (Blk $ comp scalar)
  , FuncDef
    [ DeclnStSpec $ Extern (Just $ StrLit "C")
    , DeclnTyQual Global, DeclnTySpec Void]
    (Declr Nothing (IdentDeclr $ Ident $ "_" ++ progName))
    (Prelude.map
      (\ (ty, ptr, varName) -> Decln
        ty [InitDeclr (Declr ptr (IdentDeclr $ Ident varName)) Nothing])
      [ ( [DeclnTyQual Const, DeclnTySpec xsTy]
        , Just $ Pointer [[]], "d_xs")
      , ( [DeclnTyQual Const, DeclnTySpec ysTy]
        , Just $ Pointer [[]], "d_ys")
      , ( [DeclnTyQual Const, DeclnTySpec zsTy]
        , Just $ Pointer [[]], "d_zs")
      , ( [DeclnTySpec outTy'], Just $ Pointer [[]], "d_out")
      , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "n")
      , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "isFullBlock")])
    (Blk $
      (Prelude.map
        (\ (ty, ptr, varName, exp) -> DeclnItem $ Decln
          ty [InitDeclr (Declr ptr (IdentDeclr $ Ident varName)) exp])
        [ ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "i", Nothing)
        , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "thid"
          , Just $ AssignExp $ toAssignExp $ StructMem
            (toPostfixExp $ Ident "threadIdx") (Ident "x"))
        , ( [DeclnVecTySpec $ Vector xsTy 4], Nothing, "tempData0", Nothing)
        , ( [DeclnVecTySpec $ Vector ysTy 4], Nothing, "tempData1", Nothing)
        , ( [DeclnVecTySpec $ Vector zsTy 4], Nothing, "tempData2", Nothing)
        , ( [DeclnVecTySpec $ Vector outTy' 4], Nothing, "tempData", Nothing)
        , ( [DeclnVecTySpec $ Vector xsTy 4], Just $ Pointer [[]], "inData0"
          , Just $ AssignExp $ toAssignExp $ TyCast
            (TyName
              [SpecQualVecTySpec $ Vector xsTy 4]
              (Just $ AbstDeclrPointer $ Pointer[[]]))
            (toCastExp $ Ident "d_xs"))
        , ( [DeclnVecTySpec $ Vector ysTy 4], Just $ Pointer [[]], "inData1"
          , Just $ AssignExp $ toAssignExp $ TyCast
            (TyName
              [SpecQualVecTySpec $ Vector ysTy 4]
              (Just $ AbstDeclrPointer $ Pointer[[]]))
            (toCastExp $ Ident "d_ys"))
        , ( [DeclnVecTySpec $ Vector zsTy 4], Just $ Pointer [[]], "inData2"
          , Just $ AssignExp $ toAssignExp $ TyCast
            (TyName
              [SpecQualVecTySpec $ Vector zsTy 4]
              (Just $ AbstDeclrPointer $ Pointer[[]]))
            (toCastExp $ Ident "d_zs"))
        , ( [DeclnVecTySpec $ Vector outTy' 4], Just $ Pointer [[]], "outData"
          , Just $ AssignExp $ toAssignExp $ TyCast
            (TyName
              [SpecQualVecTySpec $ Vector outTy' 4]
              (Just $ AbstDeclrPointer $ Pointer[[]]))
            (toCastExp $ Ident "d_out"))
        , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "devOffset"
          , Just $ AssignExp $ toAssignExp $ FuncCall
            (toPostfixExp $ Ident "__umul24")
            (ArgExpList
              [ toAssignExp $ StructMem
                (toPostfixExp $ Ident "blockIdx") (Ident "x")
              , toAssignExp $ LShft
                (toShftExp $ StructMem
                  (toPostfixExp $ Ident "blockDim") (Ident "x"))
                (toAddExp $ IntegerConst 1)]))
        , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "aiDev"
          , Just $ AssignExp $ toAssignExp $ Add
            (toAddExp $ Ident "devOffset") (toMulExp $ Ident "thid"))
        , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "biDev"
          , Just $ AssignExp $ toAssignExp $ Add
            (toAddExp $ Ident "aiDev")
            (toMulExp $ StructMem
              (toPostfixExp $ Ident "blockDim") (Ident "x")))])
      ++
      (concatMap
        (\ indexDev ->
          [ StmtItem $ ExpStmt $ Just $ Exp
            [toAssignExp $ FuncCall
              (toPostfixExp $ Ident "__syncthreads") (ArgExpList [])]
          , StmtItem $ ExpStmt $ Just $ Exp [Assign
            (toUnaryExp $ Ident "i")
            (toAssignExp $ Mul
              (toMulExp $ indexDev) (toCastExp $ IntegerConst 4))]]
          ++
          ( Prelude.map
            (\ (dstVarName, srcVarName) ->
              StmtItem $ ExpStmt $ Just $ Exp [Assign
                (toUnaryExp $ Ident dstVarName)
                (toAssignExp $ toArrayElem (Ident srcVarName) indexDev)])
            [ ("tempData0", "inData0"), ("tempData1", "inData1")
            , ("tempData2", "inData2")])
          ++
          [ StmtItem $ SelectStmt $ IfElse
            (Exp [toAssignExp $ LgcOr
              (toLgcOrExp $ Ident "isFullBlock")
              (toLgcAndExp $ Lt
                (toRelExp $ Add
                  (toAddExp $ Ident "i") (toMulExp $ IntegerConst 3))
                (toShftExp $ Ident "n"))])
            (CompStmt $ Blk $
              (Prelude.map
                (\ fieldName -> StmtItem $ ExpStmt $ Just $ Exp [Assign
                  (toUnaryExp $ StructMem
                    (toPostfixExp $ Ident "tempData") (Ident fieldName))
                  (toAssignExp $ FuncCall
                    (toPostfixExp $ Ident $ "_" ++ progName ++ "Scalar")
                    (ArgExpList $ Prelude.map
                      (\ arg -> toAssignExp $ StructMem
                        (toPostfixExp $ Ident arg) (Ident fieldName))
                      ["tempData0", "tempData1", "tempData2"]))])
                ["x", "y", "z", "w"])
              ++
              [StmtItem $ ExpStmt $ Just $ Exp [Assign
                (toUnaryExp $ toArrayElem (Ident "outData") indexDev)
                (toAssignExp $ Ident "tempData")]])
            (SelectStmt $ Prelude.foldr1
              (\ (If exp stmt) x -> If
                exp (CompStmt $ Blk [StmtItem stmt, StmtItem $ SelectStmt x]))
              (Prelude.zipWith
                (\ fieldName index -> If
                  (Exp [toAssignExp $ Lt
                    (toRelExp index) (toShftExp $ Ident "n")])
                  (ExpStmt $ Just $ Exp [Assign
                    (toUnaryExp $ toArrayElem (Ident "d_out") index)
                    (toAssignExp $ FuncCall
                      (toPostfixExp $ Ident $ "_" ++ progName ++ "Scalar")
                      (ArgExpList $ Prelude.map
                        (\ arg -> toAssignExp $ StructMem
                          (toPostfixExp $ Ident arg) (Ident fieldName))
                        ["tempData0", "tempData1", "tempData2"]))]))
                [ "x", "y", "z"]
                [ toAddExp $ Ident "i"
                , Add (toAddExp $ Ident "i") (toMulExp $ IntegerConst 1)
                , Add (toAddExp $ Ident "i") (toMulExp $ IntegerConst 2)]))])
        [Ident "aiDev", Ident "biDev"]))]
  where
    [(xsTy, _), (ysTy, _), (zsTy, _)] = params scalar
    outTy'                            = outTy scalar

------------------------------------------------------------------------------
-- Functions Shared By Multiple Operations
------------------------------------------------------------------------------
loadSharedChunkFromMem4
  :: String -> TySpec -> Bool -> Bool -> Maybe Const -> ExtDecln
loadSharedChunkFromMem4 progName ty exclusive reverse identity = FuncDef
  [DeclnTyQual Device, DeclnTySpec Void]
  (Declr
    Nothing
    (IdentDeclr $ Ident $ "_" ++ progName ++ "LoadSharedChunkFromMem4"))
  (Prelude.map
    (\ (ty, ptr, directdeclr) -> Decln
      ty [InitDeclr (Declr ptr directdeclr) Nothing])
    ( ( case identity of
        Just _  -> []
        Nothing -> if exclusive
          then
            [ ( [DeclnTySpec (Int $ Just Unsigned)], Nothing
              , IdentDeclr $ Ident "isExclusive")
          ,   ( [DeclnTySpec ty], Nothing
              , IdentDeclr $ Ident "startingVal")]
          else [])
      ++
      [ ( [DeclnTySpec ty], Just $ Pointer [[]]
        , IdentDeclr $ Ident "s_out")
      , ( [DeclnTySpec ty], Nothing
        , ArrayDeclr
          (IdentDeclr $ Ident "threadScan0")
          Nothing
          (Just $ toAssignExp $ IntegerConst 4))
      , ( [DeclnTySpec ty], Nothing
        , ArrayDeclr
          (IdentDeclr $ Ident "threadScan1")
          Nothing
          (Just $ toAssignExp $ IntegerConst 4))
      , ( [DeclnTyQual Const, DeclnTySpec ty], Just $ Pointer [[]]
        , IdentDeclr $ Ident "d_xs")
      , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing
        , IdentDeclr $ Ident "numElements")
      , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing
        , IdentDeclr $ Ident "iDataOffset")
      , ( [DeclnTySpec (Int $ Just Unsigned)], Just $ Pointer[[]]
        , IdentDeclr $ Ident "ai")
      , ( [DeclnTySpec (Int $ Just Unsigned)], Just $ Pointer[[]]
        , IdentDeclr $ Ident "bi")
      , ( [DeclnTySpec (Int $ Just Unsigned)], Just $ Pointer[[]]
        , IdentDeclr $ Ident "aiDev")
      , ( [DeclnTySpec (Int $ Just Unsigned)], Just $ Pointer[[]]
        , IdentDeclr $ Ident "biDev")
      , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing
        , IdentDeclr $ Ident "isFullBlock")]))
  (Blk $
    (Prelude.map
      (\ (ty, ptr, varName, exp) -> DeclnItem $ Decln
        ty [InitDeclr (Declr ptr (IdentDeclr $ Ident varName)) exp])
      [ ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "i", Nothing)
      , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "thid"
        , Just $ AssignExp $ toAssignExp $ StructMem
          (toPostfixExp $ Ident "threadIdx") (Ident "x"))
      , ( [DeclnVecTySpec $ Vector ty 4], Nothing, "tempData0", Nothing)
      , ( [DeclnVecTySpec $ Vector ty 4], Just $ Pointer [[]], "inData"
        , Just $ AssignExp $ toAssignExp $ TyCast
          (TyName
            [SpecQualVecTySpec $ Vector ty 4]
            (Just $ AbstDeclrPointer $ Pointer[[]]))
          (toCastExp $ Ident "d_xs"))])
    ++
    (Prelude.map
      (\ (varName, exp) -> StmtItem $ ExpStmt $ Just $ Exp [Assign
        (toUnaryExp $ PtrDeref $ toCastExp $ Ident varName) exp])
      [ ( "aiDev"
        , toAssignExp $ Add
          (toAddExp $ Ident "iDataOffset") (toMulExp $ Ident "thid"))
      , ( "biDev"
        , toAssignExp $ Add
          (toAddExp $ PtrDeref $ toCastExp $ Ident "aiDev")
          (toMulExp $ StructMem
            (toPostfixExp $ Ident "blockDim") (Ident "x")))
      , ( "ai", toAssignExp $ Ident "thid")
      , ( "bi"
        , toAssignExp $ Add
          (toAddExp $ PtrDeref $ toCastExp $ Ident "ai")
          (toMulExp $ StructMem
            (toPostfixExp $ Ident "blockDim") (Ident "x")))])
    ++
    (concatMap
      (\ (threadScan, index, indexDev) ->
        [ StmtItem $ ExpStmt $ Just $ Exp [Assign
          (toUnaryExp $ Ident "i")
          (toAssignExp $ Mul (toMulExp $ indexDev) (toCastExp $ IntegerConst 4))]]
        ++
        ( case identity of
          Just _  -> []
          Nothing -> if exclusive
            then
              [StmtItem $ SelectStmt $ If
                (Exp [toAssignExp $ LgcOr
                  (toLgcOrExp $ Ident "isFullBlock")
                  (toLgcAndExp $ Lt
                    (toRelExp $ Add
                      (toAddExp $ Ident "i") (toMulExp $ IntegerConst 3))
                    (toShftExp $ Ident "numElements"))])
                (CompStmt $ Blk
                  [ StmtItem $ ExpStmt $ Just $ Exp [Assign
                    (toUnaryExp $ Ident "tempData0")
                    (toAssignExp $ toArrayElem (Ident "inData") indexDev)]
                  , StmtItem $ SelectStmt $ If
                    (Exp [toAssignExp $ Ident "isExclusive"])
                    (ExpStmt $ Just $ Exp [Assign
                      (toUnaryExp$ toArrayElem
                        (Ident "s_out")
                        (Add (toAddExp index) (toMulExp $ IntegerConst 1)))
                      (toAssignExp $ StructMem
                        (toPostfixExp $ Ident "tempData0")
                        (Ident "w"))])])
              , StmtItem $ ExpStmt $ Just $ Exp [toAssignExp $ FuncCall
                (toPostfixExp $ Ident "__syncthreads") (ArgExpList [])]]
            else [])
        ++
        [ StmtItem $ SelectStmt $ IfElse
          ( Exp [toAssignExp $ LgcOr
            (toLgcOrExp $ Ident "isFullBlock")
            (toLgcAndExp $ Lt
              (toRelExp $ Add (toAddExp $ Ident "i") (toMulExp $ IntegerConst 3))
              (toShftExp $ Ident "numElements"))])
          ( CompStmt $ Blk $
            let initStmt = StmtItem $ ExpStmt $ Just $ Exp [Assign
                    (toUnaryExp $ toArrayElem threadScan (IntegerConst 0))
                    (toAssignExp $ StructMem
                      (toPostfixExp $ Ident "tempData0") (Ident "x"))]
                localScan fieldNames = Prelude.zipWith
                  (\ threadScanIndex fieldName ->
                    StmtItem $ ExpStmt $ Just $ Exp [Assign
                      (toUnaryExp $ toArrayElem
                        threadScan (IntegerConst threadScanIndex))
                      (toAssignExp $ FuncCall
                        (toPostfixExp $ Ident $ "_" ++ progName ++ "Scalar")
                        (ArgExpList
                          [ toAssignExp $ toArrayElem
                            threadScan (IntegerConst $ threadScanIndex - 1)
                          , toAssignExp $ StructMem
                            (toPostfixExp $ Ident "tempData0")
                            (Ident fieldName)]))])
                  [1, 2, 3] fieldNames
            in  case identity of
              Just _  ->
                ( StmtItem $ ExpStmt $ Just $ Exp [Assign
                  (toUnaryExp $ Ident "tempData0")
                  (toAssignExp $ toArrayElem (Ident "inData") indexDev)])
                : initStmt : localScan ["y", "z", "w"]
              Nothing ->
                if exclusive
                  then [StmtItem $ SelectStmt $ IfElse
                    ( Exp [toAssignExp $ Ident "isExclusive"])
                    ( CompStmt $ Blk $
                      ( StmtItem $ ExpStmt $ Just $ Exp [Assign
                        (toUnaryExp $ toArrayElem threadScan (IntegerConst 0))
                        (toAssignExp $ Cond
                          (toLgcOrExp index)
                          (Exp [toAssignExp $ toArrayElem
                            (Ident "s_out") index])
                          (Cond
                            (toLgcOrExp $ Ident "i")
                            (Exp [toAssignExp $ toArrayElem
                              (Ident "d_xs")
                              (Sub
                                (toAddExp $ Ident "i")
                                (toMulExp $ IntegerConst 1))])
                            (toCondExp $ Ident "startingVal")))])
                      : localScan ["x", "y", "z"])
                    ( CompStmt $ Blk
                    $ initStmt : localScan ["y", "z", "w"])]
                  else
                    ( StmtItem $ ExpStmt $ Just $ Exp [Assign
                      (toUnaryExp $ Ident "tempData0")
                      (toAssignExp $ toArrayElem
                      (Ident "inData") (indexDev))])
                    :(initStmt : localScan ["y", "z", "w"])
            ++case identity of
              Just _  -> []
              Nothing ->
                [ StmtItem $ ExpStmt $ Just $ Exp [Assign
                  (toUnaryExp $ toArrayElem (Ident "s_out") index)
                  (toAssignExp $ toArrayElem threadScan (IntegerConst 3))]])
          ( CompStmt $ Blk $
            let inclusiveInitStmt = case identity of
                  Just identity' ->
                    StmtItem $ ExpStmt $ Just $ Exp [Assign
                      (toUnaryExp $ toArrayElem threadScan (IntegerConst 0))
                      (toAssignExp $ Cond
                        (toLgcOrExp $ Lt
                          (toRelExp $ Ident "i")
                          (toShftExp $ Ident "numElements"))
                        (Exp [toAssignExp $ toArrayElem
                          (Ident "d_xs") (Ident "i")])
                      (toCondExp identity'))]
                  Nothing        -> StmtItem $ ExpStmt $ Just $ Exp [Assign
                    (toUnaryExp $ toArrayElem (Ident "s_out") index)
                    (Assign
                      (toUnaryExp $ toArrayElem threadScan (IntegerConst 0))
                      (toAssignExp $ toArrayElem
                        (Ident "d_xs") (Ident "i")))]
                localScan initStmt exclusive' = case identity of
                  Just identity' -> initStmt : Prelude.map
                    (\ threadScanIndex ->
                      StmtItem $ ExpStmt $ Just $ Exp [Assign
                        (toUnaryExp $ toArrayElem
                          threadScan (IntegerConst threadScanIndex))
                        (toAssignExp $ FuncCall
                          (toPostfixExp $ Ident $ "_" ++ progName ++ "Scalar")
                          (ArgExpList
                            [ toAssignExp $ toArrayElem
                              threadScan (IntegerConst $ threadScanIndex - 1)
                            , toAssignExp $ Cond
                              (toLgcOrExp $ Lt
                                (toRelExp $ Add
                                  (toAddExp $ Ident "i")
                                  (toMulExp $ IntegerConst threadScanIndex))
                                (toShftExp $ Ident "numElements"))
                              (Exp [toAssignExp $ toArrayElem
                                (Ident "d_xs")
                                (Add
                                  (toAddExp $ Ident "i")
                                  (toMulExp $ IntegerConst threadScanIndex))])
                              (toCondExp identity')]))])
                    [1, 2, 3]
                  Nothing        -> [StmtItem $ SelectStmt $ If
                    (Exp [toAssignExp $ Lt
                      (toRelExp $ Ident "i")
                      (toShftExp $ Ident "numElements")])
                    (CompStmt $ Blk $
                      [ initStmt
                      , StmtItem $ SelectStmt $ Prelude.foldr1
                        (\ (If exp stmt) x -> If exp
                          (CompStmt $ Blk
                            [StmtItem stmt, StmtItem $ SelectStmt x]))
                        (Prelude.map
                          (\ threadScanIndex -> If
                            (Exp [toAssignExp $ Lt
                              (toRelExp $ Add
                                (toAddExp $ Ident "i")
                                (toMulExp $ IntegerConst threadScanIndex))
                              (toShftExp $ Ident "numElements")])
                            (ExpStmt $ Just $ Exp [Assign
                              (toUnaryExp $ toArrayElem (Ident "s_out") index)
                              (Assign
                                (toUnaryExp $ toArrayElem
                                  threadScan (IntegerConst threadScanIndex))
                                (toAssignExp $ FuncCall
                                  ( toPostfixExp $ Ident
                                  $ "_" ++ progName ++ "Scalar")
                                  ( ArgExpList $ Prelude.map
                                    toAssignExp
                                    [ toArrayElem
                                      threadScan
                                      (IntegerConst $ threadScanIndex - 1)
                                    , toArrayElem
                                      (Ident "d_xs")
                                      (Add
                                        (toAddExp $ Ident "i")
                                        (toMulExp $ IntegerConst $ if exclusive'
                                          then threadScanIndex - 1
                                          else threadScanIndex))])))]))
                          [1, 2])])]
            in  case identity of
              Just _  -> localScan inclusiveInitStmt False
              Nothing -> [if exclusive
                then StmtItem $ SelectStmt $ IfElse
                  (Exp [toAssignExp $ Ident "isExclusive"])
                  (CompStmt $ Blk $ localScan
                    (StmtItem $ ExpStmt $ Just $ Exp [Assign
                      (toUnaryExp $ toArrayElem (Ident "s_out") index)
                      (Assign
                        (toUnaryExp $ toArrayElem threadScan (IntegerConst 0))
                        (toAssignExp $ Cond
                          (toLgcOrExp index)
                          (Exp [toAssignExp $ toArrayElem
                            (Ident "s_out") index])
                          (Cond
                            (toLgcOrExp $ Ident "i")
                            (Exp [toAssignExp $ toArrayElem
                              (Ident "d_xs")
                              (Sub
                                (toAddExp $ Ident "i")
                                (toMulExp $ IntegerConst 1))])
                            (toCondExp $ Ident "startingVal"))))])
                    True)
                  (CompStmt $ Blk $ localScan inclusiveInitStmt False)
                else StmtItem $ CompStmt $ Blk
                   $ localScan inclusiveInitStmt False])]
        ++case identity of
          Just _  ->
            [ StmtItem $ ExpStmt $ Just $ Exp [Assign
              (toUnaryExp $ toArrayElem (Ident "s_out") index)
              (toAssignExp $ toArrayElem threadScan (IntegerConst 3))]
            , StmtItem $ ExpStmt $ Just $ Exp [toAssignExp $ FuncCall
              (toPostfixExp $ Ident "__syncthreads") (ArgExpList [])]]
          Nothing ->
            [ StmtItem $ ExpStmt $ Just $ Exp [toAssignExp $ FuncCall
              (toPostfixExp $ Ident "__syncthreads") (ArgExpList [])]])
      [ ( Ident "threadScan0"
        , PtrDeref $ toCastExp $ Ident "ai"
        , PtrDeref $ toCastExp $ Ident "aiDev")
      , ( Ident "threadScan1"
        , PtrDeref $ toCastExp $ Ident "bi"
        , PtrDeref $ toCastExp $ Ident "biDev")]))

warpscan :: String -> TySpec -> Maybe Const -> ExtDecln
warpscan progName ty identity = FuncDef
  [DeclnTyQual Device, DeclnTySpec ty]
  (Declr Nothing (IdentDeclr $ Ident $ "_" ++ progName ++ "Warpscan"))
  (Prelude.map
    (\ (ty, ptr, varName) -> Decln
      ty [InitDeclr (Declr ptr (IdentDeclr $ Ident varName)) Nothing])
    [ ( [DeclnTySpec ty], Nothing, "val")
    , ( [DeclnTyQual Volatile, DeclnTySpec ty]
      , Just $ Pointer [[]], "s_data")
    , ( [DeclnTySpec (Int Nothing)], Nothing, "maxlevel")])
  (Blk $ case identity of
    Just identity' ->
      [ DeclnItem $ Decln
        [DeclnTySpec (Int $ Just Unsigned)]
        [InitDeclr
          (Declr Nothing (IdentDeclr $ Ident "idx"))
          (Just $ AssignExp $ toAssignExp $ Sub
            (toAddExp $ Mul
              (toMulExp $ IntegerConst 2)
              (toCastExp $ StructMem
                (toPostfixExp $ Ident "threadIdx") (Ident "x")))
            (toMulExp $ NestedExp $ Exp [toAssignExp $ And
              (toAndExp $ StructMem
                (toPostfixExp $ Ident "threadIdx") (Ident "x"))
              (toEqExp $ Sub
                (toAddExp $ IntegerConst 32) (toMulExp $ IntegerConst 1))]))]
      , StmtItem $ ExpStmt $ Just $ Exp [Assign
        (toUnaryExp $ toArrayElem (Ident "s_data") (Ident "idx"))
        (toAssignExp identity')]
      , StmtItem $ ExpStmt $ Just $ Exp [AddAssign
        (toUnaryExp $ Ident "idx") (toAssignExp $ IntegerConst 32)]
      , StmtItem $ ExpStmt $ Just $ Exp [Assign
        (toUnaryExp $ toArrayElem (Ident "s_data") (Ident "idx"))
        (toAssignExp $ Ident "val")]]
      ++
      (Prelude.map
        (\ (x, y) -> StmtItem $ SelectStmt $ If
          (Exp [toAssignExp $ Le
            (toRelExp $ IntegerConst x) (toShftExp $ Ident "maxlevel")])
          (ExpStmt $ Just $ Exp [Assign
            (toUnaryExp $ toArrayElem (Ident "s_data") (Ident "idx"))
            (toAssignExp $ FuncCall
              (toPostfixExp $ Ident $ "_" ++ progName ++ "Scalar")
              (ArgExpList
                [ toAssignExp $ toArrayElem
                  (Ident "s_data")
                  (Sub (toAddExp $ Ident "idx") (toMulExp $ IntegerConst y))
                , toAssignExp $ toArrayElem
                  (Ident "s_data") (Ident "idx")]))]))
        [(0, 1), (1, 2), (2, 4), (3, 8), (4, 16)])
      ++
      [ StmtItem $ JumpStmt $ Return $ Just $ Exp [toAssignExp $ toArrayElem
        (Ident "s_data")
        (Sub (toAddExp $ Ident "idx") (toMulExp $ IntegerConst 1))]]
    Nothing       ->
      [ StmtItem $ ExpStmt $ Just $ Exp [Assign
        (toUnaryExp $ toArrayElem
          (Ident "s_data")
          (StructMem (toPostfixExp $ Ident "threadIdx") (Ident "x")))
        (toAssignExp $ Ident "val")]]
      ++
      (Prelude.concatMap
        (\ level ->
          [ StmtItem $ ExpStmt $ Just $ Exp [toAssignExp $ FuncCall
            (toPostfixExp $ Ident "__syncthreads") (ArgExpList [])]
          , StmtItem $ SelectStmt $ If
            (Exp [toAssignExp $ LgcAnd
              (toLgcAndExp $ NestedExp $ Exp [toAssignExp $ Le
                (toRelExp $ IntegerConst level) (toShftExp $ Ident "maxlevel")])
              (toOrExp $ NestedExp $ Exp [if level == 0
                then toAssignExp $ And
                  (toAndExp $ StructMem
                    (toPostfixExp $ Ident "threadIdx") (Ident "x"))
                  (toEqExp $ IntegerConst 31)
                else toAssignExp $ RShft
                  (toShftExp $ NestedExp $ Exp [toAssignExp $ And
                    (toAndExp $ StructMem
                      (toPostfixExp $ Ident "threadIdx") (Ident "x"))
                    (toEqExp $ IntegerConst 31)])
                  (toAddExp $ IntegerConst level)])])
            (ExpStmt $ Just $ Exp [Assign
              (toUnaryExp $ toArrayElem
                (Ident "s_data")
                (StructMem (toPostfixExp $ Ident "threadIdx") (Ident "x")))
              (toAssignExp $ FuncCall
                (toPostfixExp $ Ident $ "_" ++ progName ++ "Scalar")
                (ArgExpList
                  [ toAssignExp $ toArrayElem
                    (Ident "s_data")
                    (Sub
                      (toAddExp $ StructMem
                        (toPostfixExp $ Ident "threadIdx") (Ident "x"))
                      (toMulExp $ IntegerConst $ 2 ^ level))
                  , toAssignExp $ toArrayElem
                    (Ident "s_data")
                    (StructMem
                      (toPostfixExp $ Ident "threadIdx") (Ident "x"))]))])])
        [0 .. 4])
      ++
      [ StmtItem $ JumpStmt $ Return $ Just $ Exp [toAssignExp $ toArrayElem
        (Ident "s_data")
        (StructMem (toPostfixExp $ Ident "threadIdx") (Ident "x"))]])

scanWarps :: String -> TySpec -> Bool -> Maybe Const -> ExtDecln
scanWarps progName ty exclusive identity = FuncDef
  [DeclnTyQual Device, DeclnTySpec Void]
  (Declr Nothing (IdentDeclr $ Ident $ "_" ++ progName ++ "ScanWarps"))
  (Prelude.map
    (\ (ty, ptr, varName) -> Decln
      ty [InitDeclr (Declr ptr (IdentDeclr $ Ident varName)) Nothing])
    [ ( [DeclnTySpec ty], Nothing, "x")
    , ( [DeclnTySpec ty], Nothing, "y")
    , ( [DeclnTySpec ty], Just $ Pointer [[]], "s_data")])
  (Blk $
    [ DeclnItem $ Decln
      [DeclnTySpec ty]
      [InitDeclr
        (Declr Nothing (IdentDeclr $ Ident "val"))
        (Just $ AssignExp $ toAssignExp $ FuncCall
          (toPostfixExp $ Ident $ "_" ++ progName ++ "Warpscan")
          (ArgExpList
            [ toAssignExp $ Ident "x"
            , toAssignExp $ Ident "s_data"
            , toAssignExp $ IntegerConst 4]))]
    , StmtItem $ ExpStmt $ Just $ Exp [toAssignExp $ FuncCall
      (toPostfixExp $ Ident "__syncthreads") (ArgExpList [])]
    , DeclnItem $ Decln
      [DeclnTySpec ty]
      [InitDeclr
        (Declr Nothing (IdentDeclr $ Ident "val2"))
        (Just $ AssignExp $ toAssignExp $ FuncCall
          (toPostfixExp $ Ident $ "_" ++ progName ++ "Warpscan")
          (ArgExpList
            [ toAssignExp $ Ident "y"
            , toAssignExp $ Ident "s_data"
            , toAssignExp $ IntegerConst 4]))]]
    ++case identity of
      Just _  -> []
      Nothing ->
        [ StmtItem $ ExpStmt $ Just $ Exp [toAssignExp $ FuncCall
          (toPostfixExp $ Ident "__syncthreads") (ArgExpList [])]]
    ++
    (Prelude.map
      (\ (varName, exp) -> DeclnItem $ Decln
        [DeclnTySpec (Int $ Just Unsigned)]
        [InitDeclr
          (Declr Nothing (IdentDeclr $ Ident varName))
          (Just $ AssignExp exp)])
      [ ( "idx"
        ,toAssignExp $ StructMem
          (toPostfixExp $ Ident "threadIdx") (Ident "x"))
      , ( "idx2"
        , toAssignExp $ Add
          (toAddExp $ Ident "idx")
          (toMulExp $ StructMem
            (toPostfixExp $ Ident "blockDim") (Ident "x")))])
    ++
    [ StmtItem $ SelectStmt $ If
      (Exp [toAssignExp $ Eq
        (toEqExp $ NestedExp $ Exp [toAssignExp $ And
          (toAndExp $ Ident "idx") (toEqExp $ IntegerConst 31)])
        (toRelExp $ IntegerConst 31)])
      (CompStmt $ Blk $ Prelude.map
        (\ (idxName, vals) -> StmtItem $ ExpStmt $ Just $ Exp [Assign
          (toUnaryExp $ toArrayElem
            (Ident "s_data")
            (RShft (toShftExp $ Ident idxName) (toAddExp $ IntegerConst 5)))
          (case identity of
            Just _  -> toAssignExp $ FuncCall
              (toPostfixExp $ Ident $ "_" ++ progName ++ "Scalar")
              (ArgExpList $ Prelude.map
                (toAssignExp . Ident) [fst vals, snd vals])
            Nothing -> toAssignExp $ Ident $ snd vals)])
        [ ("idx", ("x", "val")), ("idx2", ("y", "val2"))])
    , StmtItem $ ExpStmt $ Just $ Exp [toAssignExp $ FuncCall
      (toPostfixExp $ Ident "__syncthreads") (ArgExpList [])]]
    ++
    ( let blkItems = [ StmtItem $ ExpStmt $ Just $ Exp [Assign
            (toUnaryExp $ toArrayElem (Ident "s_data") (Ident "idx"))
            (toAssignExp $ FuncCall
              (toPostfixExp $ Ident $ "_" ++ progName ++ "Warpscan")
              (ArgExpList
                [ toAssignExp $ toArrayElem (Ident "s_data") (Ident "idx")
                , toAssignExp $ Ident "s_data"
                , toAssignExp $ IntegerConst 3
                  {-logCtaSize - logWarpSize + 1-}]))]]
      in  case identity of
        Just _  -> [StmtItem $ SelectStmt $ If
          (Exp [toAssignExp $ Lt
            (toRelExp $ Ident "idx") (toShftExp $ IntegerConst 32)])
          (CompStmt $ Blk $ if exclusive then blkItems else blkItems)]
        Nothing -> blkItems)
    ++
    [ StmtItem $ ExpStmt $ Just $ Exp [toAssignExp $ FuncCall
      (toPostfixExp $ Ident "__syncthreads") (ArgExpList [])]]
    ++
    (case identity of
      Just _  -> Prelude.map
        (\ (val, idxName) -> StmtItem $ ExpStmt $ Just $ Exp [Assign
          (toUnaryExp $ Ident val)
          (toAssignExp $ FuncCall
            (toPostfixExp $ Ident $ "_" ++ progName ++ "Scalar")
            (ArgExpList
              [ toAssignExp $ toArrayElem
                (Ident "s_data")
                (RShft
                  (toShftExp $ Ident idxName) (toAddExp $ IntegerConst 5))
              , toAssignExp $ Ident val]))])
        [ ("val", "idx"), ("val2", "idx2")]
      Nothing ->
        [ StmtItem $ ExpStmt $ Just $ Exp [Assign
          (toUnaryExp $ Ident "val")
          (toAssignExp $ Cond
            (toLgcOrExp $ Gt
              (toRelExp $ Ident "idx") (toShftExp $ IntegerConst 31))
            (Exp [toAssignExp $ FuncCall
              (toPostfixExp $ Ident $ "_" ++ progName ++ "Scalar")
              (ArgExpList
                [ toAssignExp $ toArrayElem
                  (Ident "s_data")
                  (Sub
                    (toAddExp $ NestedExp $ Exp [toAssignExp $ RShft
                      (toShftExp $ Ident "idx")
                      (toAddExp $ IntegerConst 5)])
                    (toMulExp $ IntegerConst 1))
                , toAssignExp $ Ident "val"])])
            (toCondExp $ Ident "val"))]
        , StmtItem $ ExpStmt $ Just $ Exp [Assign
          (toUnaryExp $ Ident "val2")
          (toAssignExp $ FuncCall
            (toPostfixExp $ Ident $ "_" ++ progName ++ "Scalar")
            (ArgExpList
              [ toAssignExp $ toArrayElem
                (Ident "s_data")
                (Sub
                  (toAddExp $ NestedExp $ Exp [toAssignExp $ RShft
                    (toShftExp $ Ident "idx2")
                    (toAddExp $ IntegerConst 5)])
                  (toMulExp $ IntegerConst 1))
              , toAssignExp $ Ident "val2"]))]])
    ++
    [ StmtItem $ ExpStmt $ Just $ Exp [toAssignExp $ FuncCall
      (toPostfixExp $ Ident "__syncthreads") (ArgExpList [])]]
    ++
    (Prelude.map
      (\ (val, idxName) -> StmtItem $ ExpStmt $ Just $ Exp [Assign
        (toUnaryExp $ toArrayElem
          (Ident "s_data")
          --(case identity of
          --  Just _  -> toAddExp $ Ident idxName
          --  Nothing -> Add
          --    (toAddExp $ Ident idxName) (toMulExp $ IntegerConst 1)))
          (Ident idxName))
        (toAssignExp $ Ident val)])
      [("val", "idx"), ("val2", "idx2")]))

scanCTA :: String -> TySpec -> Bool -> Maybe Const -> ExtDecln
scanCTA progName ty exclusive identity = FuncDef
  [DeclnTyQual Device, DeclnTySpec Void]
  (Declr Nothing (IdentDeclr $ Ident $ "_" ++ progName ++ "ScanCTA"))
  (Prelude.map
    (\ (ty, ptr, varName) -> Decln
      ty [InitDeclr (Declr ptr (IdentDeclr $ Ident varName)) Nothing])
    [ ([DeclnTySpec ty], Just $ Pointer [[]], "s_data")
    , ([DeclnTySpec ty], Just $ Pointer [[]], "d_blockSums")
    , ([DeclnTySpec (Int $ Just Unsigned)], Nothing, "blockSumIndex")
    , ([DeclnTySpec (Int $ Just Unsigned)], Nothing, "numElements")])
  (Blk $
    (Prelude.map
      (\ (ty, varName, exp) -> DeclnItem $ Decln
        [DeclnTySpec ty]
        [InitDeclr (Declr Nothing (IdentDeclr $ Ident varName)) (Just exp)])
      [ ( ty, "val"
        , AssignExp $ toAssignExp $ toArrayElem
          (Ident "s_data")
          (StructMem (toPostfixExp $ Ident "threadIdx") (Ident "x")))
      , ( ty, "val2"
        , AssignExp $ toAssignExp $ toArrayElem
          (Ident "s_data")
          (Add
            (toAddExp $ (StructMem
              (toPostfixExp $ Ident "threadIdx") (Ident "x")))
            (toMulExp $ (StructMem
              (toPostfixExp $ Ident "blockDim") (Ident "x")))))
      , ( Int $ Just Unsigned, "blkid"
        , AssignExp $ toAssignExp $ Add
          (toAddExp $ StructMem (toPostfixExp $ Ident "blockIdx") (Ident "x"))
          (toMulExp $ FuncCall
            (toPostfixExp $ Ident "__umul24")
            (ArgExpList
              [ toAssignExp $ StructMem
                (toPostfixExp $ Ident "blockIdx") (Ident "y")
              , toAssignExp $ StructMem
                (toPostfixExp $ Ident "gridDim") (Ident "x")])))
      , ( Int $ Just Unsigned, "gtid"
        , AssignExp $ toAssignExp $ Add
          (toAddExp $ StructMem (toPostfixExp $ Ident "threadIdx") (Ident "x"))
          (toMulExp $ FuncCall
            (toPostfixExp $ Ident "__umul24")
            (ArgExpList
              [ toAssignExp $ Ident "blkid"
              , toAssignExp $ StructMem
                (toPostfixExp $ Ident "blockDim") (Ident "x")])))
      , ( Int $ Just Unsigned, "lastBlock"
        , AssignExp $ toAssignExp $ RShft
          (toShftExp $ Sub
            (toAddExp $ Ident "numElements") (toMulExp $ IntegerConst 1))
          (toAddExp $ IntegerConst 10))
      , ( Int $ Just Unsigned, "lastElementTid"
        , AssignExp $ toAssignExp $ And
          (toAndExp $ RShft
            (toShftExp $ Sub
              (toAddExp $ Ident "numElements") (toMulExp $ IntegerConst 1))
            (toAddExp $ IntegerConst 2))
          (toEqExp $ IntegerConst 127))])
    ++
    [ StmtItem $ ExpStmt $ Just $ Exp [toAssignExp $ FuncCall
      (toPostfixExp $ Ident "__syncthreads") (ArgExpList [])]
    , StmtItem $ ExpStmt $ Just $ Exp [toAssignExp $ FuncCall
      (toPostfixExp $ Ident $ "_" ++ progName ++ "ScanWarps")
      (ArgExpList $ Prelude.map
        (toAssignExp . Ident) ["val", "val2", "s_data"])]
    , StmtItem $ ExpStmt $ Just $ Exp [toAssignExp $ FuncCall
      (toPostfixExp $ Ident "__syncthreads") (ArgExpList [])]
    , StmtItem $ SelectStmt $ IfElse
      (Exp [toAssignExp $
        LgcAnd
          (toLgcAndExp $ Neq
            (toEqExp $ Ident "blkid") (toRelExp $ Ident "lastBlock"))
          (toOrExp $ Eq
            (toEqExp $ StructMem
              (toPostfixExp $ Ident "threadIdx") (Ident "x"))
            (toRelExp $ Sub
              (toAddExp $ StructMem
                (toPostfixExp $ Ident "blockDim") (Ident "x"))
              (toMulExp $ IntegerConst 1)))])
      (ExpStmt $ Just $ Exp [Assign
        (toUnaryExp $ toArrayElem
          (Ident "d_blockSums") (Ident "blockSumIndex"))
        (toAssignExp $ toArrayElem
          (Ident "s_data")
          (Add
            (toAddExp $ StructMem
              (toPostfixExp $ Ident "threadIdx") (Ident "x"))
            (toMulExp $ StructMem
              (toPostfixExp $ Ident "blockDim") (Ident "x"))))])
      (SelectStmt $ If
        (Exp [toAssignExp $ LgcAnd
          (toLgcAndExp $ Eq
            (toEqExp $ Ident "blkid") (toRelExp $ Ident "lastBlock"))
          (toOrExp $ Eq
            (toEqExp $ StructMem
              (toPostfixExp $ Ident "threadIdx") (Ident "x"))
            (toRelExp $ Ident "lastElementTid"))])
        (CompStmt $ Blk
          [ DeclnItem $ Decln
            [DeclnTySpec $ Int $ Just Unsigned]
            [InitDeclr
              (Declr Nothing (IdentDeclr $ Ident "index"))
              (Just $ AssignExp $ toAssignExp $ Add
                (toAddExp $ StructMem
                  (toPostfixExp $ Ident "threadIdx") (Ident "x"))
                (toMulExp $ NestedExp $ Exp [toAssignExp $ Mul
                  (toMulExp $ StructMem
                    (toPostfixExp $ Ident "blockDim") (Ident "x"))
                  (toCastExp $ NestedExp $ Exp [toAssignExp $ And
                    (toAndExp $ RShft
                      (toShftExp $ Sub
                        (toAddExp $ Ident "numElements")
                        (toMulExp $ IntegerConst 1))
                      (toAddExp $ IntegerConst 9))
                    (toEqExp $ IntegerConst 1)])]))]
          , StmtItem $ ExpStmt $ Just $ Exp [Assign
            (toUnaryExp $ toArrayElem
              (Ident "d_blockSums") (Ident "blockSumIndex"))
            (toAssignExp $ toArrayElem
              (Ident "s_data") (Ident "index"))]]))])

storeSharedChunkToMem4
  :: String -> TySpec -> Bool -> Bool -> Maybe Const -> ExtDecln
storeSharedChunkToMem4 progName ty exclusive reverse identity = FuncDef
  [DeclnTyQual Device, DeclnTySpec Void]
  (Declr
    Nothing
    (IdentDeclr $ Ident $ "_" ++ progName ++ "StoreSharedChunkToMem4"))
  (Prelude.map
    (\ (ty, ptr, directdeclr) -> Decln
      ty [InitDeclr (Declr ptr directdeclr) Nothing])
    ( (case identity of
        Just _  -> if exclusive
          then
            [ ( [DeclnTySpec (Int $ Just Unsigned)], Nothing
              , IdentDeclr $ Ident "writeSums")
            , ( [DeclnTySpec ty], Nothing
              , IdentDeclr $ Ident "startingVal")]
          else
            [ ( [DeclnTySpec (Int $ Just Unsigned)], Nothing
              , IdentDeclr $ Ident "isExclusive")]
        Nothing -> [])
    ++[ ( [DeclnTySpec ty], Just $ Pointer [[]], IdentDeclr $ Ident "d_out")
      , ( [DeclnTySpec ty], Nothing
        , ArrayDeclr
          (IdentDeclr $ Ident "threadScan0")
          Nothing
          (Just $ toAssignExp $ IntegerConst 4))
      , ( [DeclnTySpec ty], Nothing
        , ArrayDeclr
          (IdentDeclr $ Ident "threadScan1")
          Nothing
          (Just $ toAssignExp $ IntegerConst 4))
      , ( [DeclnTyQual Const, DeclnTySpec ty], Just $ Pointer [[]]
        , IdentDeclr $ Ident "s_in")
      , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, IdentDeclr $ Ident "numElements")
      , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, IdentDeclr $ Ident "oDataOffset")
      , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, IdentDeclr $ Ident "ai")
      , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, IdentDeclr $ Ident "bi")
      , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, IdentDeclr $ Ident "aiDev")
      , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, IdentDeclr $ Ident "biDev")]
    ++[ ( [DeclnTySpec (Int $ Just Unsigned)], Nothing
        , IdentDeclr $ Ident "isFullBlock")]))
  (Blk $
    (Prelude.map
      (\ (ty, ptr, varName, exp) -> DeclnItem $ Decln
        ty [InitDeclr (Declr ptr (IdentDeclr $ Ident varName)) exp])
      [ ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "i", Nothing)
      , ( [DeclnVecTySpec $ Vector ty 4], Nothing, "tempData", Nothing)
      , ( [DeclnVecTySpec $ Vector ty 4], Just $ Pointer [[]], "outData"
        , Just $ AssignExp $ toAssignExp $ TyCast
          (TyName
            [SpecQualVecTySpec $ Vector ty 4]
            (Just $ AbstDeclrPointer $ Pointer[[]]))
          (toCastExp $ Ident "d_out"))
      , ( [DeclnTySpec ty], Nothing, "temp", Nothing)])
    ++
    (concatMap
      (\ (threadScan, index, indexDev) ->
        [ StmtItem $ ExpStmt $ Just $ Exp [toAssignExp $ FuncCall
          (toPostfixExp $ Ident "__syncthreads") (ArgExpList [])]
        , case identity of
            Just _ -> if exclusive
              then StmtItem $ SelectStmt $ IfElse
                (Exp [toAssignExp $ Ident "writeSums"])
                (ExpStmt $ Just $ Exp [Assign
                  (toUnaryExp $ Ident "temp")
                  (toAssignExp $ toArrayElem (Ident "s_in") index)])
                (ExpStmt $ Just $ Exp [Assign
                  (toUnaryExp $ Ident "temp")
                  (toAssignExp $ FuncCall
                    (toPostfixExp $ Ident $ "_" ++ progName ++ "Scalar")
                    (ArgExpList
                      [ toAssignExp $ Ident "startingVal"
                      , toAssignExp $ toArrayElem (Ident "s_in") index]))])
              else StmtItem $ ExpStmt $ Just $ Exp [Assign
                (toUnaryExp $ Ident "temp")
                (toAssignExp $ toArrayElem (Ident "s_in") index)]
            Nothing -> StmtItem $ ExpStmt $ Just $ Exp [Assign
                (toUnaryExp $ Ident "temp")
                (toAssignExp $ toArrayElem (Ident "s_in") index)]
        , let exclusiveLocalStore = CompStmt $ Blk $
                ( StmtItem $ ExpStmt $ Just $ Exp [Assign
                  (toUnaryExp $ StructMem
                    (toPostfixExp $ Ident "tempData") (Ident "x"))
                  (toAssignExp $ Ident "temp")])
                : Prelude.map
                  (\ (fieldName, threadScanIndex) ->
                    StmtItem $ ExpStmt $ Just $ Exp [Assign
                      (toUnaryExp $ StructMem
                        (toPostfixExp $ Ident "tempData") (Ident fieldName))
                      (toAssignExp $ FuncCall
                        (toPostfixExp $ Ident $ "_" ++ progName ++ "Scalar")
                        (ArgExpList
                          [ toAssignExp $ Ident "temp"
                          , toAssignExp $ toArrayElem
                            threadScan (IntegerConst threadScanIndex)]))])
                  [("y", 0), ("z", 1), ("w", 2)]
              inclusiveLocalStore = CompStmt $ Blk $ Prelude.map
                (\ (fieldName, threadScanIndex) ->
                  StmtItem $ ExpStmt $ Just $ Exp [Assign
                    (toUnaryExp $ StructMem
                      (toPostfixExp $ Ident "tempData") (Ident fieldName))
                    (toAssignExp $ FuncCall
                      (toPostfixExp $ Ident $ "_" ++ progName ++ "Scalar")
                      (ArgExpList
                        [ toAssignExp $ Ident "temp"
                        , toAssignExp $ toArrayElem
                          threadScan (IntegerConst threadScanIndex)]))])
                [("x", 0), ("y", 1), ("z", 2), ("w", 3)]
          in case identity of
            Just _  -> if exclusive
              then StmtItem exclusiveLocalStore
              else StmtItem $ SelectStmt $ IfElse
                (Exp [toAssignExp $ Ident "isExclusive"])
                  exclusiveLocalStore inclusiveLocalStore
            Nothing -> StmtItem $ SelectStmt $ IfElse
              (Exp [toAssignExp index])
              inclusiveLocalStore
              (CompStmt $ Blk $ Prelude.map
                (\ (fieldName, threadScanIndex) ->
                  StmtItem $ ExpStmt $ Just $ Exp [Assign
                    (toUnaryExp $ StructMem
                      (toPostfixExp $ Ident "tempData") (Ident fieldName))
                    (toAssignExp $ toArrayElem
                      (threadScan) (IntegerConst threadScanIndex))])
                [("x", 0), ("y", 1), ("z", 2), ("w", 3)])
        , StmtItem $ ExpStmt $ Just $ Exp [Assign
          (toUnaryExp $ Ident "i")
          (toAssignExp $ Mul (toMulExp $ indexDev) (toCastExp $ IntegerConst 4))]
        , StmtItem $ SelectStmt $ IfElse
          (Exp [toAssignExp $ LgcOr
            (toLgcOrExp $ Ident "isFullBlock")
            (toLgcAndExp $ Lt
              (toRelExp $ Add (toAddExp $ Ident "i") (toMulExp $ IntegerConst 3))
              (toShftExp $ Ident "numElements"))])
          (ExpStmt $ Just $ Exp [Assign
            (toUnaryExp $ toArrayElem (Ident "outData") indexDev)
            (toAssignExp $ Ident "tempData")])
          (SelectStmt $ Prelude.foldr1
              (\ (If exp stmt) x -> If
                exp (CompStmt $ Blk [StmtItem stmt, StmtItem $ SelectStmt x]))
              (Prelude.zipWith
                (\ fieldName index -> If
                  (Exp [toAssignExp $ Lt
                    (toRelExp index) (toShftExp $ Ident "numElements")])
                  (ExpStmt $ Just $ Exp [Assign
                    (toUnaryExp $ toArrayElem (Ident "d_out") index)
                    (toAssignExp $ StructMem
                      (toPostfixExp $ Ident "tempData")
                      (Ident fieldName))]))
                [ "x", "y", "z"]
                [ toAddExp $ Ident "i"
                , Add (toAddExp $ Ident "i") (toMulExp $ IntegerConst 1)
                , Add (toAddExp $ Ident "i") (toMulExp $ IntegerConst 2)]))])
      [ (Ident "threadScan0", Ident "ai", Ident "aiDev")
      , (Ident "threadScan1", Ident "bi", Ident "biDev")]))

scan4 :: String -> TySpec -> Bool -> Maybe Const -> ExtDecln
scan4 progName ty exclusive identity = FuncDef
  [ DeclnStSpec $ Extern (Just $ StrLit "C")
  , DeclnTyQual Global, DeclnTySpec Void]
  (Declr Nothing (IdentDeclr $ Ident $ "_" ++ progName ++ "Scan4"))
  (Prelude.map
    (\ (ty, ptr, varName) -> Decln
      ty [InitDeclr (Declr ptr (IdentDeclr $ Ident varName)) Nothing])
    ((case identity of
      Just _  -> if exclusive
        then
          [ ( [DeclnTySpec ty], Nothing, "startingVal")]
        else
          [ ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "isExclusive")]
      Nothing -> if exclusive
        then
          [ ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "isExclusive")
          , ( [DeclnTySpec ty], Nothing, "startingVal")]
        else [])
    ++[ ( [DeclnTyQual Const, DeclnTySpec ty]
        , Just $ Pointer [[]], "d_xs")
      , ( [DeclnTySpec ty], Just $ Pointer [[]], "d_out")
      , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "numElements")
      , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "isFullBlock")]))
  (Blk $
    [ DeclnItem $ Decln
      [ DeclnStSpec $ Extern Nothing
      , DeclnTyQual Shared, DeclnTySpec ty]
      [ InitDeclr
        (Declr
          Nothing
          (ArrayDeclr (IdentDeclr $ Ident "temp") Nothing Nothing))
        Nothing]
    , DeclnItem $ Decln
      [DeclnTySpec (Int $ Just Unsigned)]
      (Prelude.map
        (\ varName ->
          InitDeclr (Declr Nothing (IdentDeclr $ Ident varName)) Nothing)
        ["devOffset", "ai", "bi", "aiDev", "biDev"])
    , DeclnItem $ Decln
      [DeclnTySpec ty]
      (Prelude.map
        (\ varName -> InitDeclr
          (Declr
            Nothing
            (ArrayDeclr
              (IdentDeclr $ Ident varName)
              Nothing
              (Just $ toAssignExp $ IntegerConst 4)))
          Nothing)
        ["threadScan0", "threadScan1"])
    , DeclnItem $ Decln
      [DeclnTySpec (Int $ Just Unsigned)]
      [InitDeclr
        (Declr Nothing (IdentDeclr $ Ident "blkid"))
        (Just $ AssignExp $ toAssignExp $ Add
          (toAddExp $ StructMem
            (toPostfixExp $ Ident "blockIdx") (Ident "x"))
          (toMulExp $ FuncCall
            (toPostfixExp $ Ident "__umul24")
            (ArgExpList
              [ toAssignExp $ StructMem
                (toPostfixExp $ Ident "blockIdx") (Ident "y")
              , toAssignExp $ StructMem
                (toPostfixExp $ Ident "gridDim") (Ident "x")])))]
    , DeclnItem $ Decln
      [DeclnTySpec (Int $ Just Unsigned)]
      (Prelude.map
        (\ (dstVarName, srcVarName) -> InitDeclr
          (Declr Nothing (IdentDeclr $ Ident dstVarName))
          (Just $ AssignExp $ toAssignExp $ toPostfixExp $ Ident srcVarName))
        [ ( "blockN", "numElements"), ("blockSumIndex", "blkid")])
    , StmtItem $ ExpStmt $ Just $ Exp [Assign
      (toUnaryExp $ Ident "devOffset")
      (toAssignExp $ FuncCall
        (toPostfixExp $ Ident "__umul24")
        (ArgExpList
          [ toAssignExp $ Ident "blkid"
          , toAssignExp $ LShft
            (toShftExp $ StructMem
              (toPostfixExp $ Ident "blockDim") (Ident "x"))
            (toAddExp $ IntegerConst 1)]))]
    , StmtItem $ ExpStmt $ Just $ Exp [toAssignExp $ FuncCall
      (toPostfixExp $ Ident $ "_" ++ progName ++ "LoadSharedChunkFromMem4")
      (ArgExpList $
        (Prelude.map
          ( toAssignExp . Ident)
          ( case identity of
            Just _  -> 
              [ "temp", "threadScan0", "threadScan1"
              , "d_xs", "blockN", "devOffset"]
            Nothing -> if exclusive
              then
                [ "isExclusive", "startingVal", "temp", "threadScan0"
                , "threadScan1", "d_xs", "blockN", "devOffset"]
              else
                [ "temp", "threadScan0", "threadScan1", "d_xs"
                , "blockN", "devOffset"]))
        ++
        (Prelude.map
          ( toAssignExp . AddrOf . toCastExp . Ident)
          [ "ai", "bi", "aiDev", "biDev"])
        ++
        [toAssignExp $ Ident "isFullBlock"])]
    , StmtItem $ ExpStmt $ Just $ Exp [toAssignExp $ FuncCall
      (toPostfixExp $ Ident $ "_" ++ progName ++ "ScanCTA")
      (ArgExpList $ Prelude.map
        (toAssignExp . Ident)
        ["temp", "d_out", "blockSumIndex", "numElements"])]
    {-, StmtItem $ ExpStmt $ Just $ Exp [toAssignExp $ FuncCall
      (toPostfixExp $ Ident $ "_" ++ progName ++ "StoreSharedChunkToMem4")
      (ArgExpList $ Prelude.map
        (toAssignExp . Ident)
        (case identity of
          Just _  -> if exclusive
            then
              [ "writeSums", "startingVal", "d_out", "threadScan0"
              , "threadScan1", "temp", "blockN", "devOffset", "ai", "bi"
              , "aiDev", "biDev", "isFullBlock"]
            else
              [ "isExclusive", "d_out", "threadScan0", "threadScan1", "temp"
              , "blockN", "devOffset", "ai", "bi", "aiDev", "biDev"
              , "isFullBlock"]
          Nothing ->
            [ "d_out", "threadScan0", "threadScan1", "temp", "blockN"
            , "devOffset", "ai", "bi", "aiDev", "biDev", "isFullBlock"]))]-}])

vectorAddUniform4 :: String -> TySpec -> Maybe Const -> ExtDecln
vectorAddUniform4 progName ty identity = FuncDef
  [ DeclnStSpec $ Extern (Just $ StrLit "C")
  , DeclnTyQual Global, DeclnTySpec Void]
  (Declr
    Nothing (IdentDeclr $ Ident $ "_" ++ progName ++ "VectorAddUniform4"))
  (Prelude.map
    (\ (ty, ptr, varName) -> Decln
      ty [InitDeclr (Declr ptr (IdentDeclr $ Ident varName)) Nothing])
    [ ( [DeclnTySpec ty], Just $ Pointer [[]], "d_vector")
    , ( [DeclnTyQual Const, DeclnTySpec ty]
      , Just $ Pointer [[]], "d_uniforms")
    , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "numElements")])
  (Blk $
    [ DeclnItem $ Decln
      [DeclnTyQual Shared, DeclnTySpec ty]
      [InitDeclr (Declr Nothing (IdentDeclr $ Ident "uni")) Nothing]
    , DeclnItem $ Decln
      [DeclnTySpec (Int $ Just Unsigned)]
      [InitDeclr (Declr Nothing (IdentDeclr $ Ident "i")) Nothing]
    , DeclnItem $ Decln
      [DeclnTySpec (Int $ Just Unsigned)]
      [InitDeclr
        (Declr Nothing (IdentDeclr $ Ident "blkid"))
        (Just $ AssignExp $ toAssignExp $ Add
          (toAddExp $ StructMem
            (toPostfixExp $ Ident "blockIdx") (Ident "x"))
          (toMulExp $ FuncCall
            (toPostfixExp $ Ident "__umul24")
            (ArgExpList
              [ toAssignExp $ StructMem
                (toPostfixExp $ Ident "blockIdx") (Ident "y")
              , toAssignExp $ StructMem
                (toPostfixExp $ Ident "gridDim") (Ident "x")])))]
    , DeclnItem $ Decln
      [DeclnTySpec (Int $ Just Unsigned)]
      [InitDeclr
        (Declr Nothing (IdentDeclr $ Ident "address"))
        (Just $ AssignExp $ toAssignExp $ Add
          (toAddExp $ StructMem
            (toPostfixExp $ Ident "threadIdx") (Ident "x"))
          (toMulExp $ FuncCall
            (toPostfixExp $ Ident "__umul24")
            (ArgExpList
              [ toAssignExp $ Ident "blkid"
              , toAssignExp $ Mul
                (toMulExp $ StructMem
                  (toPostfixExp $ Ident "blockDim") (Ident "x"))
                (toCastExp $ IntegerConst 8)])))]
    , StmtItem $ SelectStmt $ If
      (Exp $ case identity of
        Just _  ->
          [toAssignExp $ LgcNot
            (toCastExp $ StructMem
              (toPostfixExp $ Ident "threadIdx") (Ident "x"))]
        Nothing ->
          [toAssignExp $ LgcAnd
            (toLgcAndExp $ LgcNot
              (toCastExp $ StructMem
                (toPostfixExp $ Ident "threadIdx") (Ident "x")))
            (toOrExp $ Ident "blkid")])
      (ExpStmt $ Just $ Exp [Assign
        (toUnaryExp $ Ident "uni")
        (toAssignExp $ toArrayElem
          (Ident "d_uniforms")
          (case identity of
            Just _  -> toAddExp $ Ident "blkid"
            Nothing -> Sub
              (toAddExp $ Ident "blkid") (toMulExp $ IntegerConst 1)))])
    , StmtItem $ ExpStmt $ Just $ Exp [toAssignExp $ FuncCall
      (toPostfixExp $ Ident "__syncthreads") (ArgExpList [])]
    , let loop = IterStmt $ For
            (Just $ Exp [Assign
              (toUnaryExp $ Ident "i") (toAssignExp $ IntegerConst 0)])
            (Just $ Exp [toAssignExp $ LgcAnd
              (toLgcAndExp $ Lt
                (toRelExp $ Ident "i")
                (toShftExp $ IntegerConst 8 {-nPerThread-}))
              (toOrExp $ Lt
                (toRelExp $ Ident "address")
                (toShftExp $ Ident "numElements"))])
            (Just $ Exp [toAssignExp $ UnaryInc $ toUnaryExp $ Ident "i"])
            (CompStmt $ Blk
              [ StmtItem $ ExpStmt $ Just $ Exp [Assign
                (toUnaryExp $ toArrayElem
                  (Ident "d_vector") (Ident "address"))
                (toAssignExp $ FuncCall
                  (toPostfixExp $ Ident $ "_" ++ progName ++ "Scalar")
                  (ArgExpList
                    [ toAssignExp $ Ident "uni"
                    , toAssignExp $ toArrayElem
                      (Ident "d_vector") (Ident "address")]))]
              , StmtItem $ ExpStmt $ Just $ Exp [AddAssign
                (toUnaryExp $ Ident "address")
                (toAssignExp $ StructMem
                  (toPostfixExp $ Ident "blockDim") (Ident "x"))]])
      in  case identity of
        Just _  -> StmtItem $ loop
        Nothing -> StmtItem $ SelectStmt $ If
          (Exp [toAssignExp $ Ident "blkid"]) loop])
--
filter = undefined
foldl1S = undefined
scanlS = undefined
scanl1S = undefined
