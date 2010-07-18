-- |
-- Module      : Data.Array.Accelerate.CUDA.CodeGen.Tuple
-- Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--

module Data.Array.Accelerate.CUDA.CodeGen.Tuple (mkTupleType, mkTupleTypeAsc)
  where

import Language.C
import Data.Array.Accelerate.CUDA.CodeGen.Data
import Data.Array.Accelerate.CUDA.CodeGen.Util


mkTupleType :: Maybe Int -> [CType] -> [CExtDecl]
mkTupleType subscript ty = types ++ [accessor]
  where
    n        = length ty
    base     = maybe "Out" (\p -> "In" ++ show p) subscript
    accessor = maybe (mkSet n) (mkGet n) subscript
    types
      | n <= 1    = [ mkTypedef ("Ty"  ++ base) False (head ty), mkTypedef ("Arr" ++ base) True (head ty)]
      | otherwise = [ mkStruct  ("Ty"  ++ base) False ty,        mkStruct  ("Arr" ++ base) True ty]

-- A variant of tuple generation for associative array computations, generating
-- base get and set functions, and the given number of type synonyms.
--
mkTupleTypeAsc :: Int -> [CType] -> [CExtDecl]
mkTupleTypeAsc syn ty = types ++ synonyms ++ [mkSet n, mkGet n 0]
  where
    n	     = length ty
    synonyms = concat . take syn . flip map ([0..] :: [Int]) $ \v ->
      [ mkTypedef ("TyIn"  ++ show v) False [CTypeDef (internalIdent "TyOut")  internalNode]
      , mkTypedef ("ArrIn" ++ show v) False [CTypeDef (internalIdent "ArrOut") internalNode] ]
    types
      | n <= 1    = [ mkTypedef "TyOut" False (head ty), mkTypedef "ArrOut" True (head ty)]
      | otherwise = [ mkStruct  "TyOut" False ty,        mkStruct  "ArrOut" True ty]


-- Getter and setter functions for reading and writing (respectively) to global
-- device arrays. Since arrays of tuples are stored as tuples of arrays, we
-- retrieve each component separately and pack into a local structure.
--
-- This unfortunately also means that we can not declare an overloaded indexing
-- operator[], since it is not possible to return an l-value to the discrete
-- component arrays (we could read, but not write).
--
-- NOTE: The Accelerate language uses snoc based tuple projection, so the last
--       field of the structure is named 'a' instead of the first, while the
--       arrays themselves are still stored "in order".
--
mkGet :: Int -> Int -> CExtDecl
mkGet n prj =
  CFDefExt
    (CFunDef
      [CStorageSpec (CStatic internalNode), CTypeQual (CInlineQual internalNode), CTypeQual (CAttrQual (CAttr (internalIdent "device") [] internalNode)), CTypeSpec (CTypeDef (internalIdent ("TyIn" ++ show prj)) internalNode)]
      (CDeclr (Just (internalIdent ("get" ++ show prj))) [CFunDeclr (Right ([CDecl [CTypeQual (CConstQual internalNode), CTypeSpec (CTypeDef (internalIdent ("ArrIn" ++ show prj)) internalNode)] [(Just (CDeclr (Just arrIn) [] Nothing [] internalNode), Nothing, Nothing)] internalNode, CDecl [CTypeQual (CConstQual internalNode), CTypeSpec (CTypeDef (internalIdent "Ix") internalNode)] [(Just (CDeclr (Just (internalIdent "idx")) [] Nothing [] internalNode), Nothing, Nothing)] internalNode], False)) [] internalNode] Nothing [] internalNode)
      []
      (CCompound [] [CBlockDecl (CDecl [CTypeSpec (CTypeDef (internalIdent ("TyIn" ++ show prj)) internalNode)] [(Just (CDeclr (Just (internalIdent "x")) [] Nothing [] internalNode),Just initList,Nothing)] internalNode),CBlockStmt (CReturn (Just (CVar (internalIdent "x") internalNode)) internalNode)] internalNode)
      internalNode)
  where
    arrIn         = internalIdent ("d_in" ++ show prj)
    initList
      | n <= 1    = CInitExpr (CIndex (CVar arrIn internalNode) (CVar (internalIdent "idx") internalNode) internalNode) internalNode
      | otherwise = flip CInitList internalNode . reverse . take n . flip map (enumFrom 'a') $ \v ->
                      ([], CInitExpr (CIndex (CMember (CVar arrIn internalNode) (internalIdent [v]) False internalNode) (CVar (internalIdent "idx") internalNode) internalNode) internalNode)


mkSet :: Int -> CExtDecl
mkSet n =
  CFDefExt
    (CFunDef
      [CStorageSpec (CStatic internalNode),CTypeQual (CInlineQual internalNode),CTypeQual (CAttrQual (CAttr (internalIdent "device") [] internalNode)),CTypeSpec (CVoidType internalNode)]
      (CDeclr (Just (internalIdent "set")) [CFunDeclr (Right ([CDecl [CTypeSpec (CTypeDef (internalIdent "ArrOut") internalNode)] [(Just (CDeclr (Just (internalIdent "d_out")) [] Nothing [] internalNode),Nothing,Nothing)] internalNode,CDecl [CTypeQual (CConstQual internalNode),CTypeSpec (CTypeDef (internalIdent "Ix") internalNode)] [(Just (CDeclr (Just (internalIdent "idx")) [] Nothing [] internalNode),Nothing,Nothing)] internalNode,CDecl [CTypeQual (CConstQual internalNode),CTypeSpec (CTypeDef (internalIdent "TyOut") internalNode)] [(Just (CDeclr (Just (internalIdent "val")) [] Nothing [] internalNode),Nothing,Nothing)] internalNode],False)) [] internalNode] Nothing [] internalNode)
      []
      (CCompound [] assignList internalNode)
      internalNode)
  where
  assignList
    | n <= 1    = [CBlockStmt (CExpr (Just (CAssign CAssignOp (CIndex (CVar (internalIdent "d_out") internalNode) (CVar (internalIdent "idx") internalNode) internalNode) (CVar (internalIdent "val") internalNode) internalNode)) internalNode)]
    | otherwise = reverse . take n . flip map (enumFrom 'a') $ \v ->
                    CBlockStmt (CExpr (Just (CAssign CAssignOp (CIndex (CMember (CVar (internalIdent "d_out") internalNode) (internalIdent [v]) False internalNode) (CVar (internalIdent "idx") internalNode) internalNode) (CMember (CVar (internalIdent "val") internalNode) (internalIdent [v]) False internalNode) internalNode)) internalNode)

