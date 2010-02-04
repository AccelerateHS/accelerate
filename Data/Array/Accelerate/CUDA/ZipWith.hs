module Data.Array.Accelerate.CUDA.ZipWith (zipWithGen, zipWithDot) where

import Data.Array.Accelerate.CUDA.Scalar
import Data.Array.Accelerate.CUDA.Syntax
import Data.Array.Accelerate.CUDA.DeviceCodeGen
import Data.Array.Accelerate.CUDA.HostCodeGen

------------------------------------------------------------------------------
-- CodeGen
------------------------------------------------------------------------------
zipWithGen :: String -> Scalar -> IO ()
zipWithGen progName scalar =
  if length (params scalar) == 2 && not (null $ comp scalar)
    then do
      writeFile (progName ++ ".cu") (show $ deviceCode progName scalar)
      writeFile (progName ++ ".cpp") (show impl)
      writeFile (progName ++ ".h") (show header)
    else error
      "ZipWith requires a binary function (a -> b -> c) as the scalar."
  where
    (header, impl) = hostCode progName scalar

zipWithDot :: String -> Scalar -> IO ()
zipWithDot progName scalar =
  if length (params scalar) == 2 && not (null $ comp scalar)
    then do
      writeFile (progName ++ ".cu.dot") (dot $ deviceCode progName scalar)
      writeFile (progName ++ ".cpp.dot") (dot impl)
      writeFile (progName ++ ".cpp.dot") (dot header)
    else error
      "ZipWith requires a binary function (a -> b -> c) as the scalar."
  where
    (header, impl) = hostCode progName scalar

------------------------------------------------------------------------------
-- Device Code
------------------------------------------------------------------------------
deviceCode :: String -> Scalar -> TransUnit
deviceCode = Data.Array.Accelerate.CUDA.DeviceCodeGen.zipWith

------------------------------------------------------------------------------
-- HostCode
------------------------------------------------------------------------------
hostCode :: String -> Scalar -> (TransUnit, TransUnit)
hostCode progName scalar = hostCodeGen $ HostCodeConfig
  { progName = progName
  , ctaSize = 64
  , nPerThread = 8
  , sharedMemSize = Nothing
  , inParams = [Array (varName ++ "s") ty | (ty, varName) <- params scalar]
  , outParam = Array "out" (outTy scalar)
  , scalar = Nothing
  , op = Data.Array.Accelerate.CUDA.HostCodeGen.Map
    {kernel = Data.Array.Accelerate.CUDA.DeviceCodeGen.ZipWith}}
