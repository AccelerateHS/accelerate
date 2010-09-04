-- |
-- Module      : Data.Array.Accelerate.CUDA.Analysis.Device
-- Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--

module Data.Array.Accelerate.CUDA.Analysis.Device
  where

import Data.List
import Data.Function
import Foreign.CUDA.Driver.Device
import qualified Foreign.CUDA.Driver    as CUDA


-- Select the best of the available CUDA capable devices. This prefers devices
-- with higher compute capability, followed by maximum throughput. This does not
-- take into account any other factors, such as whether the device is currently
-- in use by another process.
--
-- Ignore the possibility of emulation-mode devices, as this has been deprecated
-- as of CUDA v3.0 (compute-capability == 9999.9999)
--
selectBestDevice :: IO (Device, DeviceProperties)
selectBestDevice = do
  dev  <- mapM CUDA.device . enumFromTo 0 . subtract 1 . fromIntegral =<< CUDA.count
  prop <- mapM CUDA.props dev
  return . head . sortBy (cmp `on` snd) $ zip dev prop
  where
    flops d = multiProcessorCount d * clockRate d
    compute = computeCapability
    cmp x y | compute x == compute y = flops x   `compare` flops y
            | otherwise              = compute x `compare` compute y

