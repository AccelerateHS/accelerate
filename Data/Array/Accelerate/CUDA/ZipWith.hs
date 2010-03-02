module Data.Array.Accelerate.CUDA.ZipWith (zipWithGen, zipWithDot) where

import Data.Array.Accelerate.CUDA.Scalar
import Data.Array.Accelerate.CUDA.Syntax
import Data.Array.Accelerate.CUDA.DeviceCodeGen

------------------------------------------------------------------------------
-- CodeGen
------------------------------------------------------------------------------
zipWithGen :: String -> Scalar -> IO ()
zipWithGen progName scalar =
  if length (params scalar) == 2 && not (null $ comp scalar)
    then
      writeFile (progName ++ ".cu") (show $ deviceCode progName scalar)
    else
      error "ZipWith requires a binary function (a -> b -> c) as the scalar."

zipWithDot :: String -> Scalar -> IO ()
zipWithDot progName scalar =
  if length (params scalar) == 2 && not (null $ comp scalar)
    then
      writeFile (progName ++ ".cu.dot") (dot $ deviceCode progName scalar)
    else
      error "ZipWith requires a binary function (a -> b -> c) as the scalar."

------------------------------------------------------------------------------
-- Device Code
------------------------------------------------------------------------------
deviceCode :: String -> Scalar -> TransUnit
deviceCode = Data.Array.Accelerate.CUDA.DeviceCodeGen.zipWith
