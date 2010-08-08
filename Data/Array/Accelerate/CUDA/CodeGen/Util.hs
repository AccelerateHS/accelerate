-- |
-- Module      : Data.Array.Accelerate.CUDA.CodeGen.Util
-- Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--

module Data.Array.Accelerate.CUDA.CodeGen.Util
  where

import Language.C
import Data.Array.Accelerate.CUDA.CodeGen.Data


fromBool :: Bool -> CExpr
fromBool True  = CConst $ CIntConst (cInteger 1) internalNode
fromBool False = CConst $ CIntConst (cInteger 0) internalNode


mkDim :: String -> [CType] -> CExtDecl
mkDim name ty =
  mkTypedef name False [CTypeDef (internalIdent ("DIM" ++ show (length ty))) internalNode]


-- typedef ty (*?) var;
--
mkTypedef :: String -> Bool -> CType -> CExtDecl
mkTypedef var ptr ty =
  CDeclExt (CDecl (CStorageSpec (CTypedef internalNode) : map CTypeSpec ty) [(Just (CDeclr (Just (internalIdent var)) [CPtrDeclr [] internalNode | ptr] Nothing [] internalNode), Nothing, Nothing)] internalNode)


-- typedef struct {
--   ... ty1 (*?) a1; ty0 (*?) a0;
-- } var;
--
-- NOTE: The Accelerate language uses snoc based tuple projection, so the last
--       field of the structure is named 'a' instead of the first.
--
mkStruct :: String -> Bool -> [CType] -> CExtDecl
mkStruct name ptr types =
  CDeclExt (CDecl [ CStorageSpec (CTypedef internalNode) , CTypeSpec (CSUType (CStruct CStructTag Nothing (Just (zipWith field names types)) [] internalNode) internalNode)] [(Just (CDeclr (Just (internalIdent name)) [] Nothing [] internalNode),Nothing,Nothing)] internalNode)
  where
    names      = reverse . take (length types) $ (enumFrom 0 :: [Int])
    field v ty = CDecl (map CTypeSpec ty)  [(Just (CDeclr (Just (internalIdent ('a':show v))) [CPtrDeclr [] internalNode | ptr] Nothing [] internalNode),Nothing,Nothing)] internalNode


{-
-- typedef struct __attribute__((aligned(n * sizeof(ty)))) {
--     ty [x, y, z, w];
-- } var;
--
mkTyVector :: String -> Int -> CType -> CExtDecl
mkTyVector var n ty =
  CDeclExt (CDecl [ CStorageSpec (CTypedef internalNode) , CTypeSpec (CSUType (CStruct CStructTag Nothing (Just [CDecl (map CTypeSpec ty) fields internalNode]) [CAttr (internalIdent "aligned") [CBinary CMulOp (CConst (CIntConst (cInteger (toInteger n)) internalNode)) (CSizeofType (CDecl (map CTypeSpec ty) [] internalNode) internalNode) internalNode] internalNode] internalNode) internalNode)] [ (Just (CDeclr (Just (internalIdent var)) [] Nothing [] internalNode), Nothing, Nothing)] internalNode)
  where
    fields = take n . flip map "xyzw" $ \f ->
      (Just (CDeclr (Just (internalIdent [f])) [] Nothing [] internalNode), Nothing, Nothing)
-}

-- static inline __attribute__((device)) TyOut identity()
-- {
--   return expr;
-- }
--
mkIdentity :: [CExpr] -> CExtDecl
mkIdentity expr =
  CFDefExt (CFunDef [CStorageSpec (CStatic internalNode),CTypeQual (CInlineQual internalNode),CTypeQual (CAttrQual (CAttr (builtinIdent "device") [] internalNode)),CTypeSpec (CTypeDef (internalIdent "TyOut") internalNode)] (CDeclr (Just (internalIdent "identity")) [CFunDeclr (Right ([],False)) [] internalNode] Nothing [] internalNode) [] (CCompound [] [CBlockDecl (CDecl [CTypeSpec (CTypeDef (internalIdent "TyOut") internalNode)] [(Just (CDeclr (Just (internalIdent "x")) [] Nothing [] internalNode),Just (mkInitList expr),Nothing)] internalNode),CBlockStmt (CReturn (Just (CVar (internalIdent "x") internalNode)) internalNode)] internalNode) internalNode)


-- static inline __attribute__((device)) TyOut
-- apply(..., const TyIn1 x1, const TyIn0 x0)
-- {
--   TyOut x; x.a = x0; x.b = x1; ...
--   return x;
-- }
--
mkApply :: Int -> [CExpr] -> CExtDecl
mkApply argc expr =
  CFDefExt (CFunDef [CStorageSpec (CStatic internalNode),CTypeQual (CInlineQual internalNode),CTypeQual (CAttrQual (CAttr (builtinIdent "device") [] internalNode)),CTypeSpec (CTypeDef (internalIdent "TyOut") internalNode)] (CDeclr (Just (internalIdent "apply")) [CFunDeclr (Right (argv,False)) [] internalNode] Nothing [] internalNode) [] (CCompound [] [CBlockDecl (CDecl [CTypeSpec (CTypeDef (internalIdent "TyOut") internalNode)] [(Just (CDeclr (Just (internalIdent "x")) [] Nothing [] internalNode),Just (mkInitList expr),Nothing)] internalNode),CBlockStmt (CReturn (Just (CVar (internalIdent "x") internalNode)) internalNode)] internalNode) internalNode)
  where
    argv = reverse . take argc . flip map (enumFrom 0 :: [Int]) $ \x ->
      let ty  = "TyIn" ++ show x
          var = 'x'    :  show x
      in
      CDecl [CTypeQual (CConstQual internalNode), CTypeSpec (CTypeDef (internalIdent ty) internalNode)]
            [(Just (CDeclr (Just (internalIdent var)) [] Nothing [] internalNode), Nothing, Nothing)]
            internalNode


-- static inline __attribute__((device)) DimOut
-- project(const DimIn0 x0)
-- {
--   DimOut x = expr;
--   return x;
-- }
--
mkProject :: [CExpr] -> CExtDecl
mkProject expr =
  CFDefExt (CFunDef [CStorageSpec (CStatic internalNode),CTypeQual (CInlineQual internalNode),CTypeQual (CAttrQual (CAttr (builtinIdent "device") [] internalNode)),CTypeSpec (CTypeDef (internalIdent "DimOut") internalNode)] (CDeclr (Just (internalIdent "project")) [CFunDeclr (Right ([CDecl [CTypeQual (CConstQual internalNode), CTypeSpec (CTypeDef (internalIdent "DimIn0") internalNode)] [(Just (CDeclr (Just (internalIdent "x0")) [] Nothing [] internalNode), Nothing, Nothing)] internalNode],False)) [] internalNode] Nothing [] internalNode) [] (CCompound [] [CBlockDecl (CDecl [CTypeSpec (CTypeDef (internalIdent "DimOut") internalNode)] [(Just (CDeclr (Just (internalIdent "x")) [] Nothing [] internalNode),Just (mkInitList expr),Nothing)] internalNode),CBlockStmt (CReturn (Just (CVar (internalIdent "x") internalNode)) internalNode)] internalNode) internalNode)


-- Not strictly necessary, just attention to detail
--
mkInitList :: [CExpr] -> CInit
mkInitList [x] = CInitExpr x internalNode
mkInitList xs  = CInitList (map (\e -> ([],CInitExpr e internalNode)) xs) internalNode

