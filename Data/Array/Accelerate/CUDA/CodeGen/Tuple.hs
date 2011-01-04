-- |
-- Module      : Data.Array.Accelerate.CUDA.CodeGen.Tuple
-- Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--

module Data.Array.Accelerate.CUDA.CodeGen.Tuple
  (
    mkTupleType, mkTupleTypeAsc, mkTuplePartition,
    mkStencilType, mkGather
  )
  where

import Language.C
import Data.Array.Accelerate.CUDA.CodeGen.Data
import Data.Array.Accelerate.CUDA.CodeGen.Util


mkTupleType :: Maybe Int -> [CType] -> [CExtDecl]
mkTupleType subscript ty = types ++ [accessor]
  where
    n        = length ty
    volatile = maybe True (const False) subscript
    base     = maybe "Out" (\p -> "In" ++ show p) subscript
    accessor = maybe (mkSet n) (mkGet n) subscript
    types
      | n <= 1    = [ mkTypedef ("Ty"  ++ base) False False (head ty), mkTypedef ("Arr" ++ base) volatile True (head ty)]
      | otherwise = [ mkStruct  ("Ty"  ++ base) False False ty,        mkStruct  ("Arr" ++ base) volatile True ty]

-- A variant of tuple generation for associative array computations, generating
-- base get and set functions, and the given number of type synonyms.
--
mkTupleTypeAsc :: Int -> [CType] -> [CExtDecl]
mkTupleTypeAsc syn ty = types ++ synonyms ++ [mkSet n, mkGet n 0]
  where
    n	     = length ty
    synonyms = concat . take syn . flip map ([0..] :: [Int]) $ \v ->
      [ mkTypedef ("TyIn"  ++ show v) False False [CTypeDef (internalIdent "TyOut")  internalNode]
      , mkTypedef ("ArrIn" ++ show v) False False [CTypeDef (internalIdent "ArrOut") internalNode] ]
    types
      | n <= 1    = [ mkTypedef "TyOut" False False (head ty), mkTypedef "ArrOut" True True (head ty)]
      | otherwise = [ mkStruct  "TyOut" False False ty,        mkStruct  "ArrOut" True True ty]


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
      | otherwise = flip CInitList internalNode . reverse . take n . flip map (enumFrom 0 :: [Int]) $ \v ->
                      ([], CInitExpr (CIndex (CMember (CVar arrIn internalNode) (internalIdent ('a':show v)) False internalNode) (CVar (internalIdent "idx") internalNode) internalNode) internalNode)


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
    | otherwise = reverse . take n . flip map (enumFrom 0 :: [Int]) $ \v ->
                    CBlockStmt (CExpr (Just (CAssign CAssignOp (CIndex (CMember (CVar (internalIdent "d_out") internalNode) (internalIdent ('a':show v)) False internalNode) (CVar (internalIdent "idx") internalNode) internalNode) (CMember (CVar (internalIdent "val") internalNode) (internalIdent ('a':show v)) False internalNode) internalNode)) internalNode)


mkTuplePartition :: String -> [CType] -> Bool -> CExtDecl
mkTuplePartition tyName ty isVolatile =
  CFDefExt
    (CFunDef
      [CStorageSpec (CStatic internalNode),CTypeQual (CInlineQual internalNode),CTypeQual (CAttrQual (CAttr (internalIdent "device") [] internalNode)),CTypeSpec (CTypeDef (internalIdent tyName) internalNode)]
      (CDeclr (Just (internalIdent "partition")) [CFunDeclr (Right ([CDecl [CTypeQual (CConstQual internalNode),CTypeSpec (CVoidType internalNode)] [(Just (CDeclr (Just (internalIdent "s_data")) [CPtrDeclr [] internalNode] Nothing [] internalNode),Nothing,Nothing)] internalNode,CDecl [CTypeQual (CConstQual internalNode),CTypeSpec (CIntType internalNode)] [(Just (CDeclr (Just (internalIdent "n")) [] Nothing [] internalNode),Nothing,Nothing)] internalNode],False)) [] internalNode] Nothing [] internalNode)
      []
      (CCompound [] (stmts ++ [CBlockDecl (CDecl [CTypeSpec (CTypeDef (internalIdent tyName) internalNode)] [(Just (CDeclr (Just (internalIdent "r")) [] Nothing [] internalNode),Just initp,Nothing)] internalNode) ,CBlockStmt (CReturn (Just (CVar (internalIdent "r") internalNode)) internalNode)]) internalNode)
      internalNode)
  where
    n     = length ty
    var s = CVar (internalIdent s) internalNode
    names = map (('p':) . show) [n-1,n-2..0]
    initp = mkInitList (map var names)
    volat = if isVolatile then [CTypeQual (CVolatQual internalNode)] else []
    stmts = zipWith  (\l r -> CBlockDecl (CDecl (volat ++ (map CTypeSpec l)) r internalNode)) ty
          . zipWith3 (\p t s -> [(Just (CDeclr (Just (internalIdent p)) [CPtrDeclr [] internalNode] Nothing [] internalNode),Just (CInitExpr (CCast (CDecl (map CTypeSpec t) [(Just (CDeclr Nothing [CPtrDeclr [] internalNode] Nothing [] internalNode),Nothing,Nothing)] internalNode) s internalNode) internalNode),Nothing)]) names ty
          $ var "s_data" : map (\v -> CUnary CAdrOp (CIndex (var v) (CVar (internalIdent "n") internalNode) internalNode) internalNode) names


mkStencilType :: Int -> [CType] -> Int -> [CExtDecl]
mkStencilType subscript ty size = types
  where
    n     = size * (length ty)
    base  = show subscript
    types = [mkStruct  ("TyStencil" ++ base) False False (take n $ cycle ty)]


mkGather :: Int -> [CType] -> [[Int]] -> [CExtDecl]
mkGather subscript ty ixs =
  [CDeclExt
    (CDecl
      [CStorageSpec (CStatic internalNode), CTypeQual (CInlineQual internalNode), CTypeQual (CAttrQual (CAttr (internalIdent "device") [] internalNode)), CTypeSpec (CTypeDef (internalIdent ("TyIn" ++ show subscript)) internalNode)]
      [(Just (CDeclr (Just (internalIdent ("get" ++ show subscript ++ "_for_stencil"))) [CFunDeclr (Right ([CDecl[CTypeQual (CConstQual internalNode), CTypeSpec (CTypeDef (internalIdent ("ArrIn" ++ show subscript)) internalNode)] [(Just (CDeclr (Just (internalIdent ("d_in" ++ show subscript))) [] Nothing [] internalNode), Nothing, Nothing)] internalNode, CDecl [CTypeQual (CConstQual internalNode), CTypeSpec (CTypeDef (internalIdent ("ArrDimIn" ++ show subscript)) internalNode)] [(Just (CDeclr (Just (internalIdent "sh")) [] Nothing [] internalNode), Nothing, Nothing)] internalNode, CDecl [CTypeQual (CConstQual internalNode), CTypeSpec (CTypeDef (internalIdent ("ArrDimIn" ++ show subscript)) internalNode)] [(Just (CDeclr (Just (internalIdent "ix")) [] Nothing [] internalNode), Nothing, Nothing)] internalNode], False)) [] internalNode] Nothing [] internalNode), Nothing, Nothing)]
      internalNode)]
  ++
  [CFDefExt
    (CFunDef
      [CStorageSpec (CStatic internalNode), CTypeQual (CInlineQual internalNode), CTypeQual (CAttrQual (CAttr (internalIdent "device") [] internalNode)), CTypeSpec (CTypeDef (internalIdent ("TyStencil" ++ show subscript)) internalNode)]
      (CDeclr (Just (internalIdent ("gather" ++ show subscript))) [CFunDeclr (Right ([CDecl[CTypeQual (CConstQual internalNode), CTypeSpec (CTypeDef (internalIdent ("ArrIn" ++ show subscript)) internalNode)] [(Just (CDeclr (Just (internalIdent ("d_in" ++ show subscript))) [] Nothing [] internalNode), Nothing, Nothing)] internalNode, CDecl [CTypeQual (CConstQual internalNode), CTypeSpec (CTypeDef (internalIdent ("ArrDimIn" ++ show subscript)) internalNode)] [(Just (CDeclr (Just (internalIdent "idx")) [] Nothing [] internalNode), Nothing, Nothing)] internalNode, CDecl [CTypeQual (CConstQual internalNode), CTypeSpec (CTypeDef (internalIdent ("ArrDimIn" ++ show subscript)) internalNode)] [(Just (CDeclr (Just (internalIdent "sh")) [] Nothing [] internalNode), Nothing, Nothing)] internalNode], False)) [] internalNode] Nothing [] internalNode)
      []
      (CCompound [] (ixStmts ++ getStmts ++ [CBlockDecl (CDecl [CTypeSpec (CTypeDef (internalIdent ("TyStencil" ++ show subscript)) internalNode)] [(Just (CDeclr (Just (internalIdent "r")) [] Nothing [] internalNode),Just initr,Nothing)] internalNode) ,CBlockStmt (CReturn (Just (CVar (internalIdent "r") internalNode)) internalNode)]) internalNode)
      internalNode)]
  where
    -- statements that compute each stencil element index (relative to focal point)
    ixStmts        = map ixStmt $ zip ([size-1,size-2..0]) ixs
    ixStmt (e, ix) = CBlockDecl (CDecl [CTypeSpec (CTypeDef (internalIdent ("ArrDimIn" ++ show subscript)) internalNode)] [(Just (CDeclr (Just (internalIdent ("r" ++ show e ++ "_idx"))) [] Nothing [] internalNode),Just (ixInit ix), Nothing)] internalNode)
    ixInit ix      = mkInitList (map ixExpr $ zip ns ix)
      where
        ns | dim == 1  = [Nothing]
           | otherwise = map Just [dim-1,dim-2..0]
    ixExpr (n, i)  = CBinary CAddOp (CVar (internalIdent (idx n)) internalNode) (CConst (CIntConst (cInteger $ fromIntegral i) internalNode)) internalNode
      where
        idx (Just n) = "idx.a" ++ show n
        idx Nothing  = "idx"

    -- statements that 'get' each stencil element
    getStmts  = map getStmt [size-1,size-2..0]
    getStmt e = CBlockDecl (CDecl [CTypeSpec (CTypeDef (internalIdent ("TyIn" ++ show subscript)) internalNode)] [(Just (CDeclr (Just (internalIdent ("r" ++ show e))) [] Nothing [] internalNode),Just (getFn e), Nothing)] internalNode)
    getFn e   = CInitExpr (CCall (CVar (internalIdent ("get" ++ show subscript ++ "_for_stencil")) internalNode) (getArgs e) internalNode) internalNode
    getArgs e = [(CVar (internalIdent ("d_in" ++ show subscript)) internalNode), (CVar (internalIdent "sh") internalNode), (CVar (internalIdent ("r" ++ (show e) ++ "_idx")) internalNode)]

    -- initialise stencil struct which flattens all elmenet tuples
    n     = length ty
    var s = CVar (internalIdent s) internalNode
    names | n > 1     = ["r" ++ show r ++ ".a" ++ show a | r <- [size-1,size-2..0], a <- [n-1,n-2..0]]
          | otherwise = map (('r' :) . show) [size-1,size-2..0]
    initr = mkInitList (map var names)

    --
    size = length ixs
    dim  = length $ head ixs
