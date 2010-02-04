module Data.Array.Accelerate.CUDA.Map (mapGen, mapDot) where

import Data.Array.Accelerate.CUDA.Scalar
import Data.Array.Accelerate.CUDA.Syntax
import Data.Array.Accelerate.CUDA.DeviceCodeGen
import Data.Array.Accelerate.CUDA.HostCodeGen

------------------------------------------------------------------------------
-- CodeGen
------------------------------------------------------------------------------
mapGen :: String -> Scalar -> IO ()
mapGen progName scalar =
  if length (params scalar) == 1 && not (null $ comp scalar)
    then do
      writeFile (progName ++ ".cu") (show $ deviceCode progName scalar)
      writeFile (progName ++ ".cpp") (show impl)
      writeFile (progName ++ ".h") (show header)
    else error
      "Map requires a unary function (a -> b) as the scalar."
  where
    (header, impl) = hostCode progName scalar

mapDot :: String -> Scalar -> IO ()
mapDot progName scalar = do
  if length (params scalar) == 1 && not (null $ comp scalar)
    then do
      writeFile (progName ++ ".cu.dot") (dot $ deviceCode progName scalar)
      writeFile (progName ++ ".cpp.dot") (dot impl)
      writeFile (progName ++ ".h.dot") (dot header)
    else error
      "Map requires a unary function (a -> b) as the scalar."
  where
    (header, impl) = hostCode progName scalar

------------------------------------------------------------------------------
-- Device Code
------------------------------------------------------------------------------
deviceCode :: String -> Scalar -> TransUnit
deviceCode = Data.Array.Accelerate.CUDA.DeviceCodeGen.map

------------------------------------------------------------------------------
-- Host Code
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
    {kernel = Data.Array.Accelerate.CUDA.DeviceCodeGen.Map}}
