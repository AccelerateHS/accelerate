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


device, global :: Ident
device = builtinIdent "device"
global = builtinIdent "global"


fromBool :: Bool -> CExpr
fromBool True  = CConst $ CIntConst (cInteger 1) internalNode
fromBool False = CConst $ CIntConst (cInteger 0) internalNode


--
-- typedef ty var;
--
mkTypedef :: String -> [CTypeSpec] -> CExtDecl
mkTypedef var ty =
  CDeclExt
    (CDecl (CStorageSpec (CTypedef internalNode) : map CTypeSpec ty)
           [(Just (CDeclr (Just (internalIdent var)) [] Nothing [] internalNode), Nothing, Nothing)]
           internalNode)

--
-- struct __attribute__((aligned(n * sizeof(ty)))) var {
--     ty [x, y, z, w];
-- }
-- typedef struct var var;
--
mkTyVector :: String -> Int -> [CTypeSpec] -> CExtDecl
mkTyVector var n ty =
  CDeclExt
    (CDecl [ CStorageSpec (CTypedef internalNode)
           , CTypeSpec
               (CSUType
                 (CStruct CStructTag
                   (Just (internalIdent var))
                   (Just [CDecl (map CTypeSpec ty) fields internalNode])
                   [CAttr (internalIdent "aligned") [CBinary CMulOp (CConst (CIntConst (cInteger (toInteger n)) internalNode)) (CSizeofType (CDecl (map CTypeSpec ty) [] internalNode) internalNode) internalNode] internalNode]
                   internalNode)
                 internalNode)]
           [ (Just (CDeclr (Just (internalIdent var)) [] Nothing [] internalNode), Nothing, Nothing)]
           internalNode)
  where
    fields = take n . flip map "xyzw" $ \f ->
      (Just (CDeclr (Just (internalIdent [f])) [] Nothing [] internalNode), Nothing, Nothing)

--
-- __attribute__((device)) static TyOut identity()
-- {
--   return expr;
-- }
--
mkIdentity :: CExpr -> CExtDecl
mkIdentity = mkDeviceFun "identity" 0

--
-- __attribute__((device)) static TyOut apply(TyIn0 x0, TyIn1 x1 ..)
-- {
--   return expr;
-- }
--
mkApply :: Int -> CExpr -> CExtDecl
mkApply = mkDeviceFun "apply"


mkDeviceFun :: String -> Int -> CExpr -> CExtDecl
mkDeviceFun name argc expr =
  CFDefExt
    (CFunDef [ CTypeQual (CAttrQual (CAttr device [] internalNode)), CStorageSpec (CStatic internalNode), CTypeSpec (CTypeDef (internalIdent "TyOut") internalNode)]
             (CDeclr (Just (internalIdent name)) [CFunDeclr (Right (argv, False)) [] internalNode] Nothing [] internalNode)
             []
             (CCompound [] [CBlockStmt (CReturn (Just expr) internalNode)] internalNode)
             internalNode)
  where
    argv = take argc . flip map (enumFrom 0 :: [Int]) $ \n ->
      let ty  = "TyIn" ++ show n
          var = 'x'    :  show n
      in
      CDecl [CTypeQual (CConstQual internalNode), CTypeSpec (CTypeDef (internalIdent ty) internalNode)]
            [(Just (CDeclr (Just (internalIdent var)) [] Nothing [] internalNode), Nothing, Nothing)]
            internalNode

--
-- __attribute__((device)) Ix project(const Ix x0)
-- {
--   return expr;
-- }
--
mkIndexFun :: CExpr -> CExtDecl
mkIndexFun expr =
  CFDefExt
    (CFunDef [CTypeQual (CAttrQual (CAttr device [] internalNode)),CTypeSpec (CTypeDef (internalIdent "Ix") internalNode)]
             (CDeclr (Just (internalIdent "project")) [CFunDeclr (Right ([CDecl [CTypeQual (CConstQual internalNode),CTypeSpec (CTypeDef (internalIdent "Ix") internalNode)] [(Just (CDeclr (Just (internalIdent "x0")) [] Nothing [] internalNode),Nothing,Nothing)] internalNode],False)) [] internalNode] Nothing [] internalNode)
             []
             (CCompound [] [CBlockStmt (CReturn (Just expr) internalNode)] internalNode)
             internalNode)

