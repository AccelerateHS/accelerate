
module Data.Array.Accelerate.CUDA.Kernel.Map (mapGen, mapDot) where

import Data.Array.Accelerate.CUDA.Scalar
import Data.Array.Accelerate.CUDA.Syntax
import Data.Array.Accelerate.CUDA.CodeGen.Device as CG

------------------------------------------------------------------------------
-- CodeGen
------------------------------------------------------------------------------
mapGen :: String -> Scalar -> IO ()
mapGen progName scalar =
  if length (params scalar) == 1 && not (null $ comp scalar)
    then
      writeFile (progName ++ ".cu") (show $ deviceCode progName scalar)
    else
      error "Map requires a unary function (a -> b) as the scalar."

mapDot :: String -> Scalar -> IO ()
mapDot progName scalar = do
  if length (params scalar) == 1 && not (null $ comp scalar)
    then
      writeFile (progName ++ ".cu.dot") (dot $ deviceCode progName scalar)
    else
      error "Map requires a unary function (a -> b) as the scalar."

------------------------------------------------------------------------------
-- Device Code
------------------------------------------------------------------------------
deviceCode :: String -> Scalar -> TransUnit
deviceCode = CG.map

