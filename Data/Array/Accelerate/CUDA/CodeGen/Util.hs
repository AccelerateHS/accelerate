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


mkTypedef :: String -> [CTypeSpec] -> CExtDecl
mkTypedef var ty =
  CDeclExt
    (CDecl (CStorageSpec (CTypedef internalNode) : map CTypeSpec ty)
           [(Just (CDeclr (Just (internalIdent var)) [] Nothing [] internalNode), Nothing, Nothing)]
           internalNode)

mkIdentity :: CExpr -> CExtDecl
mkIdentity = mkDeviceFun "identity" 0

mkApply :: Int -> CExpr -> CExtDecl
mkApply = mkDeviceFun "apply"

mkDeviceFun :: String -> Int -> CExpr -> CExtDecl
mkDeviceFun name argc expr =
  CFDefExt
    (CFunDef [ CTypeQual (CAttrQual (CAttr device [] internalNode)), CStorageSpec (CStatic internalNode), CTypeSpec (CTypeDef (internalIdent "tyOut") internalNode)]
             (CDeclr (Just (internalIdent name)) [CFunDeclr (Right (argv, False)) [] internalNode] Nothing [] internalNode)
             []
             (CCompound [] [CBlockStmt (CReturn (Just expr) internalNode)] internalNode)
             internalNode)
  where
    argv = take argc . (flip map) (enumFrom 0 :: [Int]) $ \n ->
      let ty  = "tyIn" ++ show n
          var = 'x'    :  show n
      in
      CDecl [CTypeQual (CConstQual internalNode), CTypeSpec (CTypeDef (internalIdent ty) internalNode)]
            [(Just (CDeclr (Just (internalIdent var)) [] Nothing [] internalNode), Nothing, Nothing)]
            internalNode

