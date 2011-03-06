--
-- Load a PGM file. MacOS X users might find the following quicklook plugin
-- useful for viewing PGM files:
--
-- http://code.google.com/p/quicklook-pfm/
--

module PGM where

import Control.Applicative
import Graphics.Pgm

import Prelude                   as P
import Data.Array.Accelerate     as Acc
import Data.Array.Unboxed        hiding (Array)
import qualified Data.ByteString as B


-- Read an 8-bit PGM file, and marshal to an Accelerate array as floating-point
-- data in the range [0,1].
--
readPGM :: FilePath -> IO (Array DIM2 Float)
readPGM fp = do
  img <- either (error . show) head . pgmsToArrays <$> B.readFile fp :: IO (UArray (Int,Int) Word8)
  return . fromIArray $ amap (\x -> P.fromIntegral x / 255) img


writePGM :: FilePath -> Array DIM2 Float -> IO ()
writePGM fp img =
  let arr = toIArray img :: UArray (Int,Int) Float
  in  arrayToFile fp $ amap (\x -> P.round (255 * x)) arr

