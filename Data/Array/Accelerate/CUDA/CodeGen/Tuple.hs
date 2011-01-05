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
    mkTupleType, mkTupleTypeAsc, mkTexTupleTypes, mkTuplePartition,
    mkStencilType, mkGatherAndApply, mkGatherAndApply2
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


-- Declare types and getters for arrays that are accessed via textrure references. Need to
-- do all arrays in one hit because texture reference identifiers are unique for each tuple element
-- and for each array.
--
-- For example, if we have two arrays of types (Float,Float,Float) and (Int,Int)
-- respectively, then we will have texture references tex0 - tex4. We must decalare TyIn0 for the first
-- array and its getter will access via tex0, tex1 and tex2, whereas for the second array we must declare TyIn1
-- and its getter will access via tex3 and tex4.
--
mkTexTupleTypes :: [[CType]] -> [CExtDecl]
mkTexTupleTypes tys = concat $ flip map tys' $ \(subscript, texId, ty) -> mkTexTupleType subscript texId ty
  where
    tys' = zip3 ([0..]) heads tys
    heads = scanl (+) 0 $ map length tys


mkTexTupleType :: Int -> Int -> [CType] -> [CExtDecl]
mkTexTupleType subscript texIdx ty = types ++ [accessor]
  where
    n        = length ty
    base     = "In" ++ (show subscript)
    accessor = mkTexGet n texIdx subscript
    types
      | n <= 1    = [ mkTypedef ("Ty"  ++ base) False False (head ty), mkTypedef ("Arr" ++ base) False True (head ty)]
      | otherwise = [ mkStruct  ("Ty"  ++ base) False False ty,        mkStruct  ("Arr" ++ base) False True ty]


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


mkTexGet :: Int -> Int -> Int -> CExtDecl
mkTexGet n texIdx prj =
  CFDefExt
    (CFunDef
      [CStorageSpec (CStatic internalNode), CTypeQual (CInlineQual internalNode), CTypeQual (CAttrQual (CAttr (internalIdent "device") [] internalNode)), CTypeSpec (CTypeDef (internalIdent ("TyIn" ++ show prj)) internalNode)]
      (CDeclr (Just (internalIdent ("tex_get" ++ show prj))) [CFunDeclr (Right ([CDecl [CTypeQual (CConstQual internalNode), CTypeSpec (CTypeDef (internalIdent "Ix") internalNode)] [(Just (CDeclr (Just (internalIdent "idx")) [] Nothing [] internalNode), Nothing, Nothing)] internalNode], False)) [] internalNode] Nothing [] internalNode)
      []
      (CCompound [] [CBlockDecl (CDecl [CTypeSpec (CTypeDef (internalIdent ("TyIn" ++ show prj)) internalNode)] [(Just (CDeclr (Just (internalIdent "x")) [] Nothing [] internalNode),Just initList,Nothing)] internalNode),CBlockStmt (CReturn (Just (CVar (internalIdent "x") internalNode)) internalNode)] internalNode)
      internalNode)
  where
    initList
      | n <= 1    = CInitExpr (CCall (CVar (internalIdent "indexArray") internalNode) [(CVar (internalIdent ("tex" ++ show texIdx )) internalNode), (CVar (internalIdent "idx") internalNode)] internalNode) internalNode
      | otherwise = flip CInitList internalNode . take n . flip map (enumFrom texIdx :: [Int]) $ \v ->
                      ([], CInitExpr (CCall (CVar (internalIdent "indexArray") internalNode) [(CVar (internalIdent ("tex" ++ (show v))) internalNode), (CVar (internalIdent "idx") internalNode)] internalNode) internalNode)


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


-- |Generated code for stencil kernels.
--
mkStencilType :: Int -> [CType] -> Int -> [CExtDecl]
mkStencilType subscript ty size = types
  where
    n     = size * (length ty)
    base  = show subscript
    types = [mkStruct  ("TyStencil" ++ base) False False (take n $ cycle ty)]


-- |Generated function that gathers stencil elements for a given focal point and then applies the
-- stencil function.
--
mkGatherAndApply :: [CType] -> [[Int]] -> [CExpr] -> [CExtDecl]
mkGatherAndApply ty ixs expr =
  mkGetForStencilDecl 0 ++
  [CFDefExt
    (CFunDef
      [CStorageSpec (CStatic internalNode), CTypeQual (CInlineQual internalNode), CTypeQual (CAttrQual (CAttr (internalIdent "device") [] internalNode)), CTypeSpec (CTypeDef (internalIdent "TyOut") internalNode)]
      (CDeclr (Just (internalIdent "gather_and_apply")) [CFunDeclr (Right ([CDecl [CTypeQual (CConstQual internalNode), CTypeSpec (CTypeDef (internalIdent "ArrDimIn0") internalNode)] [(Just (CDeclr (Just (internalIdent "sh0")) [] Nothing [] internalNode), Nothing, Nothing)] internalNode, CDecl [CTypeQual (CConstQual internalNode), CTypeSpec (CTypeDef (internalIdent "ArrDimIn0") internalNode)] [(Just (CDeclr (Just (internalIdent "idx0")) [] Nothing [] internalNode), Nothing, Nothing)] internalNode], False)) [] internalNode] Nothing [] internalNode)
      []
      (CCompound
        []
        (mkStencilGetStmt 0 ty ixs ++
         mkStencilApplyStmt expr)
        internalNode)
      internalNode)]


-- |Generated function that gathers stencil elements from two array for a given focal point
-- and then applies the stencil function.
--
mkGatherAndApply2 :: [CType] -> [[Int]] -> [CType] -> [[Int]] -> [CExpr] -> [CExtDecl]
mkGatherAndApply2 ty0 ixs0 ty1 ixs1 expr =
  mkGetForStencilDecl 0 ++
  mkGetForStencilDecl 1 ++
  [CFDefExt
    (CFunDef
      [CStorageSpec (CStatic internalNode), CTypeQual (CInlineQual internalNode), CTypeQual (CAttrQual (CAttr (internalIdent "device") [] internalNode)), CTypeSpec (CTypeDef (internalIdent "TyOut") internalNode)]
      (CDeclr (Just (internalIdent "gather_and_apply")) [CFunDeclr (Right ([ CDecl [CTypeQual (CConstQual internalNode), CTypeSpec (CTypeDef (internalIdent "ArrDimIn0") internalNode)] [(Just (CDeclr (Just (internalIdent "sh0")) [] Nothing [] internalNode), Nothing, Nothing)] internalNode, CDecl [CTypeQual (CConstQual internalNode), CTypeSpec (CTypeDef (internalIdent "ArrDimIn0") internalNode)] [(Just (CDeclr (Just (internalIdent "idx0")) [] Nothing [] internalNode), Nothing, Nothing)] internalNode, CDecl [CTypeQual (CConstQual internalNode), CTypeSpec (CTypeDef (internalIdent "ArrDimIn1") internalNode)] [(Just (CDeclr (Just (internalIdent "sh1")) [] Nothing [] internalNode), Nothing, Nothing)] internalNode, CDecl [CTypeQual (CConstQual internalNode), CTypeSpec (CTypeDef (internalIdent "ArrDimIn1") internalNode)] [(Just (CDeclr (Just (internalIdent "idx1")) [] Nothing [] internalNode), Nothing, Nothing)] internalNode], False)) [] internalNode] Nothing [] internalNode)
      []
      (CCompound
        []
        (mkStencilGetStmt 0 ty0 ixs0 ++
         mkStencilGetStmt 1 ty1 ixs1 ++
         mkStencilApplyStmt expr)
        internalNode)
      internalNode)]



-- |Forward declaration of get(n)_for_stencil function implemented by stencil1/stencil2 kernel.
--
mkGetForStencilDecl :: Int -> [CExtDecl]
mkGetForStencilDecl subscript =
  [CDeclExt
    (CDecl
      [CStorageSpec (CStatic internalNode), CTypeQual (CInlineQual internalNode), CTypeQual (CAttrQual (CAttr (internalIdent "device") [] internalNode)), CTypeSpec (CTypeDef (internalIdent ("TyIn" ++ show subscript)) internalNode)]
      [(Just (CDeclr (Just (internalIdent ("get" ++ show subscript ++ "_for_stencil"))) [CFunDeclr (Right ([CDecl [CTypeQual (CConstQual internalNode), CTypeSpec (CTypeDef (internalIdent ("ArrDimIn" ++ show subscript)) internalNode)] [(Just (CDeclr (Just (internalIdent "sh")) [] Nothing [] internalNode), Nothing, Nothing)] internalNode, CDecl [CTypeQual (CConstQual internalNode), CTypeSpec (CTypeDef (internalIdent ("ArrDimIn" ++ show subscript)) internalNode)] [(Just (CDeclr (Just (internalIdent "ix")) [] Nothing [] internalNode), Nothing, Nothing)] internalNode], False)) [] internalNode] Nothing [] internalNode), Nothing, Nothing)]
      internalNode)]


-- |Stencil element gathering.
--
mkStencilGetStmt :: Int -> [CType] -> [[Int]] -> [CBlockItem]
mkStencilGetStmt subscript ty ixs = getStmts ++ stencilStmt
  where
    -- statements that 'get' each stencil element
    getStmts        = map getStmt $ zip ([size-1,size-2..0]) ixs
    getStmt (e, ix) = CBlockDecl (CDecl [CTypeSpec (CTypeDef (internalIdent ("TyIn" ++ show subscript)) internalNode)] [(Just (CDeclr (Just (internalIdent ("e" ++ show subscript ++ "_" ++ show e))) [] Nothing [] internalNode),Just (getFn ix), Nothing)] internalNode)
    getFn ix        = CInitExpr (CCall (CVar (internalIdent ("get" ++ show subscript ++ "_for_stencil")) internalNode) (getArgs ix) internalNode) internalNode
    getArgs ix      = [(CVar (internalIdent ("sh" ++ show subscript)) internalNode), (ixArg ix)]

    ixArg ix        = (CCall (CVar (internalIdent "shape") internalNode) (shapeArgs ix) internalNode)
    shapeArgs ix    = map ixExpr $ zip ns ix
      where
        ns | dim == 1  = [Nothing]
           | otherwise = map Just [dim-1,dim-2..0]
        dim  = length $ head ixs

    ixExpr (n, i)   = CBinary CAddOp (CVar (internalIdent (idx n)) internalNode) (CConst (CIntConst (cInteger $ fromIntegral i) internalNode)) internalNode
      where
        idx (Just n) = "idx" ++ show subscript ++ ".a" ++ show n
        idx Nothing  = "idx" ++ show subscript


    -- initialise stencil struct which flattens all elmenet tuples
    stencilStmt = [CBlockDecl (CDecl [CTypeSpec (CTypeDef (internalIdent ("TyStencil" ++ show subscript)) internalNode)] [(Just (CDeclr (Just (internalIdent ("x" ++ show subscript))) [] Nothing [] internalNode), Just initStencil, Nothing)] internalNode)]
    initStencil = mkInitList (map var names)
    n           = length ty
    var s       = CVar (internalIdent s) internalNode
    names
      | n > 1     = ["e" ++ show subscript ++ "_" ++ show e ++ ".a" ++ show a | e <- [size-1,size-2..0], a <- [n-1,n-2..0]]
      | otherwise = map ((("e" ++ show subscript ++ "_") ++) . show) [size-1,size-2..0]

    --
    size = length ixs


-- |Application of stencil function on gathered elements.
--
mkStencilApplyStmt :: [CExpr] -> [CBlockItem]
mkStencilApplyStmt expr =
  [CBlockDecl (CDecl [CTypeSpec (CTypeDef (internalIdent "TyOut") internalNode)] [(Just (CDeclr (Just (internalIdent "r")) [] Nothing [] internalNode), Just (mkInitList expr), Nothing)] internalNode), CBlockStmt (CReturn (Just (CVar (internalIdent "r") internalNode)) internalNode)]

