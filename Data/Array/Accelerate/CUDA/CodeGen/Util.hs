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


--------------------------------------------------------------------------------
-- Common device functions
--------------------------------------------------------------------------------

mkIdentity :: [CExpr] -> CExtDecl
mkIdentity = mkDeviceFun "identity" (typename "TyOut") []

mkApply :: Int -> [CExpr] -> CExtDecl
mkApply argc
  = mkDeviceFun "apply" (typename "TyOut")
  $ map (\n -> (typename ("TyIn"++show n), 'x':show n)) [argc-1,argc-2..0]

mkProject :: [CExpr] -> CExtDecl
mkProject = mkDeviceFun "project" (typename "DimOut") [(typename "DimIn0","x0")]

mkSliceIndex :: [CExpr] -> CExtDecl
mkSliceIndex =
  mkDeviceFun "sliceIndex" (typename "SliceDim") [(typename "Slice","sl"), (typename "CoSlice","co")]

mkSliceReplicate :: [CExpr] -> CExtDecl
mkSliceReplicate =
  mkDeviceFun "sliceIndex" (typename "Slice") [(typename "SliceDim","dim")]


--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

typename :: String -> CType
typename var = [CTypeDef (internalIdent var) internalNode]

fromBool :: Bool -> CExpr
fromBool True  = CConst $ CIntConst (cInteger 1) internalNode
fromBool False = CConst $ CIntConst (cInteger 0) internalNode

mkDim :: String -> Int -> CExtDecl
mkDim name n =
  mkTypedef name False [CTypeDef (internalIdent ("DIM" ++ show n)) internalNode]

mkTypedef :: String -> Bool -> CType -> CExtDecl
mkTypedef var ptr ty =
  CDeclExt $ CDecl
    (CStorageSpec (CTypedef internalNode) : map CTypeSpec ty)
    [(Just (CDeclr (Just (internalIdent var)) [CPtrDeclr [] internalNode | ptr] Nothing [] internalNode), Nothing, Nothing)]
    internalNode

mkInitList :: [CExpr] -> CInit
mkInitList [x] = CInitExpr x internalNode
mkInitList xs  = CInitList (map (\e -> ([],CInitExpr e internalNode)) xs) internalNode


-- typedef struct {
--   ... ty1 (*?) a1; ty0 (*?) a0;
-- } var;
--
-- NOTE: The Accelerate language uses snoc based tuple projection, so the last
--       field of the structure is named 'a' instead of the first.
--
mkStruct :: String -> Bool -> [CType] -> CExtDecl
mkStruct name ptr types =
  CDeclExt $ CDecl
    [CStorageSpec (CTypedef internalNode) , CTypeSpec (CSUType (CStruct CStructTag Nothing (Just (zipWith field names types)) [] internalNode) internalNode)]
    [(Just (CDeclr (Just (internalIdent name)) [] Nothing [] internalNode),Nothing,Nothing)]
    internalNode
  where
    names      = reverse . take (length types) $ (enumFrom 0 :: [Int])
    field v ty = CDecl (map CTypeSpec ty)
                       [(Just (CDeclr (Just (internalIdent ('a':show v))) [CPtrDeclr [] internalNode | ptr] Nothing [] internalNode), Nothing, Nothing)]
                       internalNode


-- typedef struct __attribute__((aligned(n * sizeof(ty)))) {
--     ty [x, y, z, w];
-- } var;
--
mkTyVector :: String -> Int -> CType -> CExtDecl
mkTyVector var n ty =
  CDeclExt $ CDecl
    [CStorageSpec (CTypedef internalNode), CTypeSpec (CSUType (CStruct CStructTag Nothing (Just [CDecl (map CTypeSpec ty) fields internalNode]) [CAttr (internalIdent "aligned") [CBinary CMulOp (CConst (CIntConst (cInteger (toInteger n)) internalNode)) (CSizeofType (CDecl (map CTypeSpec ty) [] internalNode) internalNode) internalNode] internalNode] internalNode) internalNode)]
    [(Just (CDeclr (Just (internalIdent var)) [] Nothing [] internalNode), Nothing, Nothing)]
    internalNode
  where
    fields = take n . flip map "xyzw" $ \f ->
      (Just (CDeclr (Just (internalIdent [f])) [] Nothing [] internalNode), Nothing, Nothing)


-- static inline __attribute__((device)) tyout name(args)
-- {
--   tyout r = { expr };
--   return r;
-- }
--
mkDeviceFun :: String -> CType -> [(CType,String)] -> [CExpr] -> CExtDecl
mkDeviceFun name tyout args expr =
  CFDefExt $ CFunDef
    ([CStorageSpec (CStatic internalNode), CTypeQual (CInlineQual internalNode), CTypeQual (CAttrQual (CAttr (builtinIdent "device") [] internalNode))] ++ map CTypeSpec tyout)
    (CDeclr (Just (internalIdent name)) [CFunDeclr (Right (argv,False)) [] internalNode] Nothing [] internalNode)
    []
    (CCompound [] [CBlockDecl (CDecl (map CTypeSpec tyout) [(Just (CDeclr (Just (internalIdent "r")) [] Nothing [] internalNode), Just (mkInitList expr), Nothing)] internalNode), CBlockStmt (CReturn (Just (CVar (internalIdent "r") internalNode)) internalNode)] internalNode)
    internalNode
    where
      argv = flip map args $ \(ty,var) ->
        CDecl (CTypeQual (CConstQual internalNode) : map CTypeSpec ty)
              [(Just (CDeclr (Just (internalIdent var)) [] Nothing [] internalNode), Nothing, Nothing)]
              internalNode

