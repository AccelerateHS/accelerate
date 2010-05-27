-- |
-- Module      : Data.Array.Accelerate.CUDA.CodeGen.Map
-- Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--
-- Device code for zipWith_k class of functions
--

module Data.Array.Accelerate.CUDA.CodeGen.Map (map, zipWith, zipWith3)
  where

import Prelude   hiding (id, (.), mod, map, zipWith, zipWith3)
import qualified Prelude

import Control.Category
import Control.Exception.Extensible
import Data.Record.Label

import Data.Array.Accelerate.CUDA.Syntax
import Data.Array.Accelerate.CUDA.CodeGen.Expr



map, zipWith, zipWith3 :: String -> Expr -> TransUnit
map      name expr = assert (1 == length (get argumentTy expr)) $ mapK name expr
zipWith  name expr = assert (2 == length (get argumentTy expr)) $ mapK name expr
zipWith3 name expr = assert (3 == length (get argumentTy expr)) $ mapK name expr


-- |
-- A map function of arity-k, using the combining scalar function to
-- element-wise combine the k input vectors into a single output vector.
--
mapK :: String -> Expr -> TransUnit
mapK name fn = TransUnit
  [ ]
  [ FuncDef
    [ DeclnTyQual Device
    , DeclnTySpec (get outputTy fn) ]
    (Declr Nothing . IdentDeclr . Ident $ name ++ "_scalar")
    (Prelude.zipWith (\ty n -> Decln [DeclnTySpec ty]
                                     [InitDeclr (Declr Nothing (IdentDeclr . Ident $ 'x' : show n)) Nothing])
                     (get argumentTy fn) (enumFrom 0 :: [Int]))
    (Blk (get body fn))

  , FuncDef
    [ DeclnStSpec . Extern $ Just (StrLit "C")
    , DeclnTyQual Global
    , DeclnTySpec Void ]
    (Declr Nothing . IdentDeclr . Ident $ name)

    (Prelude.map
      (\(ty, ptr, var) -> Decln ty [InitDeclr (Declr ptr (IdentDeclr (Ident var))) Nothing])
      ( (Prelude.zipWith (\ty n -> ([DeclnTySpec ty], Just (Pointer [[]]), "d_xs" ++ show n))
                         (get argumentTy fn) (enumFrom 0 :: [Int]))
      ++
      [ ([DeclnTySpec (get outputTy fn)], Just (Pointer [[]]), "d_out")
      , ([DeclnTySpec (Int (Just Unsigned))], Nothing, "n")
      , ([DeclnTySpec (Int (Just Unsigned))], Nothing, "isFullBlock")]))

    (Blk $
      (Prelude.map
        (\ (ty, ptr, var, e) -> DeclnItem $ Decln ty [InitDeclr (Declr ptr (IdentDeclr (Ident var))) e])

        ([ ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "i", Nothing)
        , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "thid"
          , Just . AssignExp . toAssignExp $ StructMem (toPostfixExp (Ident "threadIdx")) (Ident "x"))
        ]
        ++
        Prelude.zipWith (\ty n -> ( [DeclnVecTySpec (Vector ty 2)]
                                    , Just (Pointer [[]])
                                    , "inData" ++ show n
                                    , Just . AssignExp . toAssignExp $ TyCast (TyName [SpecQualVecTySpec (Vector ty 2)]
                                                                              (Just $ AbstDeclrPointer (Pointer [[]])))
                                                                              (toCastExp . Ident $ "d_xs" ++ show n)
                                    ))
            (get argumentTy fn) (enumFrom 0 :: [Int])
        ++
        [ ( [DeclnVecTySpec $ Vector (get outputTy fn) 2], Just (Pointer [[]]), "outData"
          , Just . AssignExp . toAssignExp $ TyCast
            (TyName
              [SpecQualVecTySpec $ Vector (get outputTy fn) 2] (Just $ AbstDeclrPointer (Pointer[[]])))
            (toCastExp $ Ident "d_out"))
        , ( [DeclnTySpec (Int (Just Unsigned))], Nothing, "devOffset"
          , Just . AssignExp . toAssignExp $ FuncCall
            (toPostfixExp $ Ident "__umul24")
            (ArgExpList
              [ toAssignExp $ StructMem (toPostfixExp $ Ident "blockIdx") (Ident "x")
              , toAssignExp $ StructMem (toPostfixExp $ Ident "blockDim") (Ident "x")]))
        , ( [DeclnTySpec (Int $ Just Unsigned)], Nothing, "aiDev"
          , Just . AssignExp . toAssignExp $ Add (toAddExp $ Ident "devOffset") (toMulExp $ Ident "thid"))]))
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
              (toPostfixExp $ Ident $ "make_" ++ (show $ Vector (get outputTy fn) 2))
              (ArgExpList $ Prelude.map
                (\ fieldName -> toAssignExp $ FuncCall
                  (toPostfixExp $ Ident $ name ++ "_scalar")
                  (ArgExpList $ Prelude.map
                    (\n -> toAssignExp $ StructMem
                        (toArrayElem (Ident $ "inData" ++ show n) (Ident "aiDev"))
                        (Ident fieldName)) [0 .. length (get argumentTy fn) - 1]))
                ["x", "y"])))]])

        (SelectStmt $ Prelude.foldr1
          (\ (If e stmt) x -> If
            e (CompStmt $ Blk [StmtItem stmt, StmtItem $ SelectStmt x]))
          (Prelude.zipWith
            (\ fieldName index -> If
              (Exp [toAssignExp $ Lt
                (toRelExp index) (toShftExp $ Ident "n")])
              (ExpStmt $ Just $ Exp [Assign
                (toUnaryExp $ toArrayElem (Ident "d_out") index)
                (toAssignExp $ FuncCall
                  (toPostfixExp $ Ident $ name ++ "_scalar")
                  (ArgExpList $ Prelude.map
                    (\n -> toAssignExp $ StructMem
                        (toArrayElem (Ident $ "inData" ++ show n) (Ident "aiDev"))
                        (Ident fieldName)) [0 .. length (get argumentTy fn) - 1]))]))
            [ "x", "y" ]
            [ toAddExp $ Ident "i"
            , Add (toAddExp $ Ident "i") (toMulExp $ IntegerConst 1)]))])]

