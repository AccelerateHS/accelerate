
module Data.Array.Accelerate.CUDA.Kernel.Fold (foldGen, foldDot) where

import Data.Array.Accelerate.CUDA.Scalar
import Data.Array.Accelerate.CUDA.Syntax
import Data.Array.Accelerate.CUDA.CodeGen.Device as CG

------------------------------------------------------------------------------
-- CodeGen
------------------------------------------------------------------------------
foldGen :: String -> Scalar -> (TySpec, String) -> IO ()
foldGen progName scalar left =
  if length (params scalar) == 2 && not (null $ comp scalar)
  && outTy' == xTy && outTy' == yTy && outTy' == leftTy
    then
      writeFile (progName ++ ".cu") (show $ deviceCode progName scalar left)
    else error
      "Fold requires a binary function (a -> a -> a) as the scalar."
  where
    [(xTy, _), (yTy, _)] = params scalar
    leftTy = fst left
    outTy' = outTy scalar

foldDot :: String -> Scalar -> (TySpec, String) -> IO ()
foldDot progName scalar left =
  if length (params scalar) == 2 && not (null $ comp scalar)
  && outTy' == xTy && outTy' == yTy && outTy' == leftTy
    then
      writeFile (progName ++ ".cu.dot") (dot $ deviceCode progName scalar left)
    else error
      "Fold requires a binary function (a -> a -> a) as the scalar."
  where
    [(xTy, _), (yTy, _)] = params scalar
    leftTy = fst left
    outTy' = outTy scalar

------------------------------------------------------------------------------
-- Device Code
------------------------------------------------------------------------------
deviceCode :: String -> Scalar -> (TySpec, String) -> TransUnit
deviceCode = CG.fold
