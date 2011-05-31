-- |
-- Module      : Data.Array.Accelerate.CUDA.CodeGen.Tuple
-- Copyright   : [2010..2011] Ben Lever
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--

module Data.Array.Accelerate.CUDA.CodeGen.Stencil (
  mkStencilType, mkStencilGet, mkStencilGather, mkStencilApply
)
where

import Language.C
import Data.Array.Accelerate.CUDA.CodeGen.Data
import Data.Array.Accelerate.CUDA.CodeGen.Util

import Data.Array.Accelerate.Type


-- Getter function for a single element of a stencil array. These arrays are
-- read via texture memory, and additionally we need to specify the boundary
-- condition handler.
--
mkStencilGet :: Int -> Boundary [CExpr] -> [CType] -> [CExtDecl]
mkStencilGet base bndy ty =
  case bndy of
    Constant e -> [mkConstant e, mkFun [constant]]
    Clamp      -> [mkFun (boundary "clamp")]
    Mirror     -> [mkFun (boundary "mirror")]
    Wrap       -> [mkFun (boundary "wrap")]
  where
    dim   = typename (subscript "DimIn")
    mkFun = mkDeviceFun' (subscript "get") (typename (subscript "TyIn")) [(dim, "sh"), (dim, "ix")]

    mkConstant = mkDeviceFun (subscript "constant") (typename (subscript "TyIn")) []

    constant   = CBlockStmt $
      CIf (ccall "inRange" [cvar "sh", cvar "ix"])
      (CCompound [] [ CBlockDecl (CDecl [CTypeQual (CConstQual internalNode), CTypeSpec (CTypeDef (internalIdent "Ix") internalNode)] [(Just (CDeclr (Just (internalIdent "i")) [] Nothing [] internalNode),Just (CInitExpr (ccall "toIndex" [cvar "sh", cvar "ix"]) internalNode),Nothing)] internalNode)
                    , initA
                    , CBlockStmt (CReturn (Just (cvar "r")) internalNode) ]
                    internalNode)
      (Just (CCompound [] [CBlockStmt (CReturn (Just (ccall (subscript "constant") [])) internalNode)] internalNode))
      internalNode

    boundary f =
      [ CBlockDecl (CDecl [CTypeQual (CConstQual internalNode), CTypeSpec (CTypeDef (internalIdent "Ix") internalNode)] [(Just (CDeclr (Just (internalIdent "i")) [] Nothing [] internalNode),Just (CInitExpr (ccall "toIndex" [cvar "sh", ccall f [cvar "sh", cvar "ix"]]) internalNode),Nothing)] internalNode)
      , initA
      , CBlockStmt (CReturn (Just (CVar (internalIdent "r") internalNode)) internalNode)
      ]

    subscript = (++ show base)
    ix        = cvar "i"
    arr c     = cvar (subscript "stencil" ++ "_a" ++ show c)

    initA = CBlockDecl
      (CDecl [CTypeSpec (CTypeDef (internalIdent (subscript "TyIn")) internalNode)]
             [( Just (CDeclr (Just (internalIdent "r")) [] Nothing [] internalNode)
              , Just . mkInitList . reverse $ zipWith indexA (reverse ty) (enumFrom 0 :: [Int])
              , Nothing)]
             internalNode)

    indexA [CDoubleType _] c = ccall "indexDArray" [arr c, ix]
    indexA _               c = ccall "indexArray"  [arr c, ix]


-- A structure to hold all components of a stencil, mimicking our nested-tuple
-- representation for neighbouring elements.
--
mkStencilType :: Int -> Int -> [CType] -> CExtDecl
mkStencilType subscript size
  = mkStruct ("Stencil" ++ show subscript) False False
  . concat . replicate size


-- Gather all neighbouring array elements for our stencil
--
mkStencilGather :: Int -> Int -> [CType] -> [[Int]] -> CExtDecl
mkStencilGather base dim ty ixs =
  mkDeviceFun' (subscript "gather") (typename (subscript "Stencil")) [(dimIn, "sh"), (dimIn, "ix")] body
  where
    dimIn     = typename (subscript "DimIn")
    subscript = (++ show base)

    plus a b  = CBinary CAddOp a b internalNode
    cint c    = CConst $ CIntConst (cInteger (toInteger c)) internalNode
    offset is
      | dim == 1  = [cvar "ix" `plus` cint (head is)]
      | otherwise = zipWith (\c i -> CMember (cvar "ix") (internalIdent ('a':show c)) False internalNode `plus` cint i) [dim-1, dim-2 ..] is

    initX x is = CBlockDecl
      (CDecl [CTypeQual (CConstQual internalNode), CTypeSpec (CTypeDef (internalIdent (subscript "TyIn")) internalNode)]
             [( Just (CDeclr (Just (internalIdent ('x':show x))) [] Nothing [] internalNode)
              , Just (CInitExpr (ccall (subscript "get") [cvar "sh", ccall "shape" (offset is)]) internalNode)
              , Nothing)]
             internalNode)

    initS =
      let xs    = let l = length ixs in [l-1, l-2 .. 0]
          names = case length ty of
            1 -> [ cvar ('x':show x) | x <- xs]
            n -> [ CMember (cvar ('x':show x)) (internalIdent ('a':show c)) False internalNode | x <- xs , c <- [n-1,n-2..0]]
      in
      CBlockDecl
      (CDecl [CTypeSpec (CTypeDef (internalIdent (subscript "Stencil")) internalNode)]
             [( Just (CDeclr (Just (internalIdent "r")) [] Nothing [] internalNode)
              , Just (mkInitList names)
              , Nothing)]
             internalNode)

    body =
      zipWith initX [0::Int ..] (reverse ixs) ++
      [ initS
      , CBlockStmt (CReturn (Just (CVar (internalIdent "r") internalNode)) internalNode) ]


mkStencilApply :: Int -> [CExpr] -> CExtDecl
mkStencilApply argc
  = mkDeviceFun "apply" (typename "TyOut")
  $ map (\n -> (typename ("Stencil" ++ show n), 'x':show n)) [argc-1, argc-2 .. 0]

