
module Util where

import System.IO.Unsafe

-- Lazier version of 'Control.Monad.sequence'
--
sequence' :: [IO a] -> IO [a]
sequence' = foldr k (return [])
  where k m ms = do { x <- m; xs <- unsafeInterleaveIO ms; return (x:xs) }

mapM' :: (a -> IO b) -> [a] -> IO [b]
mapM' f xs = sequence' $ map f xs

forM' :: [a] -> (a -> IO b) -> IO [b]
forM' = flip mapM'

