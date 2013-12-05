
module Progress 
        ( printPosition
        , printProgress
        , padR
        , padL)
where
import System.IO
import Control.Monad


-- | Print position in a stream, without a maximum value.
printPosition :: Bool -> String -> Int -> Int -> IO ()
printPosition isLast header block pos
 | pos == 0
 = do   putStr $ header ++ padR 10 (show pos)
        hFlush stdout

 | pos `mod` block == 0 || isLast
 = do   putStr $ replicate (length header + 10) '\b'
        putStr $ header ++ padR 10 (show pos)
        when isLast
         $ putStr "\n"
        hFlush stdout

 | otherwise
 =      return ()


-- | Print progress in a stream towards a maximum value.
printProgress :: String -> Int -> Int -> Int -> IO ()
printProgress header block pos len
 | pos == 0
 = do   putStr $ header ++ padR 10 (show $ pos) ++ "/" ++ padR 10 (show len)
        hFlush stdout

 |   pos `mod` block == 0 || pos == len
 = do   let n   = length header + 21
        putStr  $ replicate n '\b'
        putStr $ header ++ padR 10 (show $ pos) ++ "/" ++ padR 10 (show len)
        when (pos == len)
         $ putStr "\n"
        hFlush stdout

 | otherwise
 =      return ()


-- | Pretty print in a right-justified column.
padR :: Int -> String -> String
padR n str
 = replicate (n - length str) ' ' ++ str


-- | Pretty print in a left-justified column.
padL :: Int -> String -> String
padL n str
 = str ++ replicate (n - length str) ' '
