
module Common.Dump ( dumpWorld, printWorld )
  where

import Common.Body
import Common.World

import System.IO
import Data.List
import Text.Printf
import qualified Data.Array.Accelerate                  as A


-- | Dump the bodies in a world to a file.
--
dumpWorld :: World -> FilePath -> IO ()
dumpWorld world filePath
  = withFile filePath WriteMode
  $ \h -> hWriteWorld h world

-- | Print the bodies in a world to stdout
--
printWorld :: World -> IO ()
printWorld world
  = hWriteWorld stdout world


-- | Write a world to a handle
--
hWriteWorld :: Handle -> World -> IO ()
hWriteWorld h world
  = mapM_ (hWriteBody h)
            $ A.toList
            $ worldBodies world

-- | Write a single body to a handle
--
hWriteBody :: Handle -> Body -> IO ()
hWriteBody h (((px, py), mass), (vx, vy), (ax, ay))
  = hPutStrLn h
  $ concat
  $ ( (padRc 8 ' ' $ show mass)
    :  " "
    : (intersperse " " $ map (\f -> printf "%15.8f" f) [ px, py, vx, vy, ax, ay ]))


-- | Right justify a doc, padding with a given character.
--
padRc :: Int -> Char -> String -> String
padRc n c str
  = replicate (n - length str) c ++ str

