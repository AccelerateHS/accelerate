-- |
-- Module      : Data.Array.Accelerate.CUDA.CodeGen.Util
-- Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--

module Data.Array.Accelerate.CUDA.CodeGen.Util
  where

import Language.C

type CType = [CTypeSpec]

device, global :: Ident
device = builtinIdent "device"
global = builtinIdent "global"

fromBool :: Bool -> CExpr
fromBool True  = CConst $ CIntConst (cInteger 1) internalNode
fromBool False = CConst $ CIntConst (cInteger 0) internalNode


-- typedef ty var;
--
mkTypedef :: String -> Bool -> CType -> CExtDecl
mkTypedef var isptr ty =
  let ptr = if isptr then [CPtrDeclr [] internalNode] else []
  in
  CDeclExt
    (CDecl (CStorageSpec (CTypedef internalNode) : map CTypeSpec ty)
           [(Just (CDeclr (Just (internalIdent var)) ptr Nothing [] internalNode), Nothing, Nothing)]
           internalNode)


-- typedef struct {
--   ... ty1 b; ty0 a;
-- } var;
--
-- NOTE: The Accelerate language uses snoc based tuple projection, so the last
--       field of the structure is named 'a' instead of the first.
--
mkStruct :: String -> Bool -> [CType] -> CExtDecl
mkStruct name isptr types =
  CDeclExt
    (CDecl [ CStorageSpec (CTypedef internalNode)
           , CTypeSpec
               (CSUType
                 (CStruct CStructTag Nothing
                   (Just (zipWith field names types))
                   []
                   internalNode)
                 internalNode)]
           [(Just (CDeclr (Just (internalIdent name)) [] Nothing [] internalNode),Nothing,Nothing)]
           internalNode)
  where
    ptr        = if isptr then [CPtrDeclr [] internalNode] else []
    names      = reverse . take (length types) $ enumFrom 'a'
    field v ty = CDecl (map CTypeSpec ty)  [(Just (CDeclr (Just (internalIdent [v])) ptr Nothing [] internalNode),Nothing,Nothing)] internalNode


-- typedef struct __attribute__((aligned(n * sizeof(ty)))) {
--     ty [x, y, z, w];
-- } var;
--
mkTyVector :: String -> Int -> CType -> CExtDecl
mkTyVector var n ty =
  CDeclExt
    (CDecl [ CStorageSpec (CTypedef internalNode)
           , CTypeSpec
               (CSUType
                 (CStruct CStructTag Nothing
                   (Just [CDecl (map CTypeSpec ty) fields internalNode])
                   [CAttr (internalIdent "aligned") [CBinary CMulOp (CConst (CIntConst (cInteger (toInteger n)) internalNode)) (CSizeofType (CDecl (map CTypeSpec ty) [] internalNode) internalNode) internalNode] internalNode]
                   internalNode)
                 internalNode)]
           [ (Just (CDeclr (Just (internalIdent var)) [] Nothing [] internalNode), Nothing, Nothing)]
           internalNode)
  where
    fields = take n . flip map "xyzw" $ \f ->
      (Just (CDeclr (Just (internalIdent [f])) [] Nothing [] internalNode), Nothing, Nothing)


-- static inline __attribute__((device)) TyOut identity()
-- {
--   return expr;
-- }
--
mkIdentity :: [CExpr] -> CExtDecl
mkIdentity expr =
  CFDefExt
    (CFunDef [CStorageSpec (CStatic internalNode),CTypeQual (CInlineQual internalNode),CTypeQual (CAttrQual (CAttr device [] internalNode)),CTypeSpec (CTypeDef (internalIdent "TyOut") internalNode)]
             (CDeclr (Just (internalIdent "identity")) [CFunDeclr (Right ([],False)) [] internalNode] Nothing [] internalNode)
             []
             (CCompound [] [CBlockDecl (CDecl [CTypeSpec (CTypeDef (internalIdent "TyOut") internalNode)] [(Just (CDeclr (Just (internalIdent "x")) [] Nothing [] internalNode),Just initr,Nothing)] internalNode),CBlockStmt (CReturn (Just (CVar (internalIdent "x") internalNode)) internalNode)] internalNode)
             internalNode)
  where
    initr
      | length expr <= 1 = CInitExpr (head expr) internalNode
      | otherwise        = CInitList (map (\e -> ([],CInitExpr e internalNode)) expr) internalNode


-- static inline __attribute__((device)) TyOut
-- apply(..., const TyIn1 x1, const TyIn0 x0, const Ix shape)
-- {
--   TyOut x; x.a = x0; x.b = x1; ...
--   return x;
-- }
--
mkApply :: Int -> [CExpr] -> CExtDecl
mkApply argc expr =
  CFDefExt
    (CFunDef [CStorageSpec (CStatic internalNode),CTypeQual (CInlineQual internalNode),CTypeQual (CAttrQual (CAttr device [] internalNode)),CTypeSpec (CTypeDef (internalIdent "TyOut") internalNode)]
             (CDeclr (Just (internalIdent "apply")) [CFunDeclr (Right (argv,False)) [] internalNode] Nothing [] internalNode)
             []
             (CCompound [] [CBlockDecl (CDecl [CTypeSpec (CTypeDef (internalIdent "TyOut") internalNode)] [(Just (CDeclr (Just (internalIdent "x")) [] Nothing [] internalNode),Just initr,Nothing)] internalNode),CBlockStmt (CReturn (Just (CVar (internalIdent "x") internalNode)) internalNode)] internalNode)
             internalNode)
  where
    shape = CDecl [CTypeQual (CConstQual internalNode),CTypeSpec (CTypeDef (internalIdent "Ix") internalNode)] [(Just (CDeclr (Just (internalIdent "shape")) [] Nothing [] internalNode),Nothing,Nothing)] internalNode
    initr
      | length expr <= 1 = CInitExpr (head expr) internalNode
      | otherwise        = CInitList (map (\e -> ([],CInitExpr e internalNode)) expr) internalNode

    argv  = reverse . (shape:) . take argc . flip map (enumFrom 0 :: [Int]) $ \x ->
      let ty  = "TyIn" ++ show x
          var = 'x'    :  show x
      in
      CDecl [CTypeQual (CConstQual internalNode), CTypeSpec (CTypeDef (internalIdent ty) internalNode)]
            [(Just (CDeclr (Just (internalIdent var)) [] Nothing [] internalNode), Nothing, Nothing)]
            internalNode


-- static inline __attribute__((device)) Ix project(const Ix x0)
-- {
--   return expr;
-- }
--
mkIndexFun :: CExpr -> CExtDecl
mkIndexFun expr =
  CFDefExt
    (CFunDef [CStorageSpec (CStatic internalNode),CTypeQual (CInlineQual internalNode),CTypeQual (CAttrQual (CAttr device [] internalNode)),CTypeSpec (CTypeDef (internalIdent "Ix") internalNode)]
             (CDeclr (Just (internalIdent "project")) [CFunDeclr (Right ([CDecl [CTypeQual (CConstQual internalNode),CTypeSpec (CTypeDef (internalIdent "Ix") internalNode)] [(Just (CDeclr (Just (internalIdent "x0")) [] Nothing [] internalNode),Nothing,Nothing)] internalNode],False)) [] internalNode] Nothing [] internalNode)
             []
             (CCompound [] [CBlockStmt (CReturn (Just expr) internalNode)] internalNode)
             internalNode)

