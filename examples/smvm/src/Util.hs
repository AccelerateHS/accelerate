module Util where

import System.IO.Unsafe

-- Lazier versions of things in Control.Monad
--
sequence' :: [IO a] -> IO [a]
sequence' ms = foldr k (return []) ms
    where k m m' = do { x <- m; xs <- unsafeInterleaveIO m'; return (x:xs) }

mapM' :: (a -> IO b) -> [a] -> IO [b]
mapM' f as = sequence' (map f as)


