module Image where

import Control.Monad
import Graphics.Pgm
import Data.Array.Unboxed       (UArray, amap)
import Data.Array.Accelerate    (Array, DIM2, fromIArray, toIArray)

readPGM :: FilePath -> IO (Array DIM2 Float)
readPGM fp = do
  img <- liftM (either (error . show) head) $ pgmsFromFile fp
  return . fromIArray $ amap fromIntegral img


writePGM :: FilePath -> Array DIM2 Float -> IO ()
writePGM fp img =
  let arr = toIArray img :: UArray (Int,Int) Float
  in  arrayToFile fp $ amap round arr

