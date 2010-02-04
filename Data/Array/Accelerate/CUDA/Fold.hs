module Data.Array.Accelerate.CUDA.Fold (foldGen, foldDot) where

import Data.Array.Accelerate.CUDA.Scalar
import Data.Array.Accelerate.CUDA.Syntax
import Data.Array.Accelerate.CUDA.DeviceCodeGen
import Data.Array.Accelerate.CUDA.HostCodeGen

------------------------------------------------------------------------------
-- CodeGen
------------------------------------------------------------------------------
foldGen :: String -> Scalar -> (TySpec, String) -> IO ()
foldGen progName scalar left =
  if length (params scalar) == 2 && not (null $ comp scalar)
  && outTy' == xTy && outTy' == yTy && outTy' == leftTy
    then do
      writeFile (progName ++ ".cu") (show $ deviceCode progName scalar left)
      writeFile (progName ++ ".cpp") (show impl)
      writeFile (progName ++ ".h") (show header)
    else error
      "Fold requires a binary function (a -> a -> a) as the scalar."
  where
    (header, impl) = hostCode progName scalar left
    [(xTy, _), (yTy, _)] = params scalar
    leftTy = fst left
    outTy' = outTy scalar

foldDot :: String -> Scalar -> (TySpec, String) -> IO ()
foldDot progName scalar left =
  if length (params scalar) == 2 && not (null $ comp scalar)
  && outTy' == xTy && outTy' == yTy && outTy' == leftTy
    then do
      writeFile (progName ++ ".cu.dot") (dot $ deviceCode progName scalar left)
      writeFile (progName ++ ".cpp.dot") (dot impl)
      writeFile (progName ++ ".h.dot") (dot header)
    else error
      "Fold requires a binary function (a -> a -> a) as the scalar."
  where
    (header, impl) = hostCode progName scalar left
    [(xTy, _), (yTy, _)] = params scalar
    leftTy = fst left
    outTy' = outTy scalar

------------------------------------------------------------------------------
-- Device Code
------------------------------------------------------------------------------
deviceCode :: String -> Scalar -> (TySpec, String) -> TransUnit
deviceCode = Data.Array.Accelerate.CUDA.DeviceCodeGen.fold

------------------------------------------------------------------------------
-- Host Code
------------------------------------------------------------------------------
hostCode :: String -> Scalar -> (TySpec, String) -> (TransUnit, TransUnit)
hostCode progName scalar left = hostCodeGen $ HostCodeConfig
  { progName = progName
  , ctaSize = 128
  , nPerThread = 8
  , sharedMemSize = Just $ toAssignExp $ Mul
    (toMulExp $ IntegerConst $ 2 * 128)
    (toCastExp $ TySize $ TyName [SpecQualTySpec $ outTy scalar] Nothing)
  , inParams = [Variable (snd left) (fst left), Array (xName ++ "s") xTy]
  , outParam = Array "out" (outTy scalar)
  , scalar = Just scalar
  , op = Data.Array.Accelerate.CUDA.HostCodeGen.Fold
    { kernel = Data.Array.Accelerate.CUDA.DeviceCodeGen.Scan4
    , associativity = Data.Array.Accelerate.CUDA.HostCodeGen.Left "left"}}
  where
    (xTy, xName) = head $ params scalar
