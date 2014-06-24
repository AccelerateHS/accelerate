{-# LANGUAGE BangPatterns #-}
module Rank
        (rank)
where
import Load
import Step
import Count
import Page
import ParseArgs
import System.Directory
import Control.Monad
import Prelude                                  as P
import Data.Array.Accelerate                    as A
import Data.Array.Accelerate.IO                 as A
import qualified Data.Vector                    as V
import qualified Data.Vector.Storable           as S
import System.CPUTime


-- | Perform some iterations of the PageRank algorithm by loading the whole
--   links graph into memory and passes it off in chunks to Accelerate to
--   avoid running out of device memory.
rank
    :: Backend
    -> Int                  -- ^ Number of iterations to run.
    -> Int                  -- ^ Size of chunk.
    -> FilePath             -- ^ Path to links file.
    -> FilePath             -- ^ Path to titles file.
    -> IO ()
rank backend steps chunkSize pagesPath titlesPath
 = do   (_, maxPageId) <- countPages pagesPath
        putStrLn "* Loading pages."
        (!from, !to, !sizes) <- loadPages pagesPath (P.fromIntegral maxPageId)
        let pageCount   = S.length sizes
        let edgeCount   = S.length from
        let !ranks      = initialRanks backend pageCount
        start <- getCPUTime
        let arrayize vec = (A.fromVectors (Z :. S.length vec) ((), vec))
        pageRank backend steps chunkSize pageCount (arrayize from) (arrayize to) (arrayize sizes) titlesPath ranks
        end   <- getCPUTime
        putStrLn $ "Time taken: " P.++ show (P.fromIntegral (end - start) / (10^12)) P.++ " secs"
        return ()

-- | Construct the initial ranks vector.
initialRanks :: Backend -> Int -> A.Vector Rank
initialRanks backend pageCount
 = let  !startRank   = 1 / P.fromIntegral pageCount
   in   run backend $ A.fill (index1 (A.lift pageCount)) startRank


-- | Run several iterations of the internal PageRank algorithm.
pageRank
        :: Backend
        -> Int                  -- ^ Number of iterations to run.
        -> Int                  -- ^ Chunk size
        -> Int                  -- ^ Number of pages
        -> Vector PageId        -- ^ Pages graph from.
        -> Vector PageId        -- ^ Pages graph to (same length as from).
        -> A.Vector Int         -- ^ The degree of each page
        -> FilePath             -- ^ Path to titles file.
        -> A.Vector Rank        -- ^ Initial ranks.
        -> IO ()

pageRank backend maxIters chunkSize pageCount from to sizes titlesFile ranks0
 = go maxIters ranks0
 where  go :: Int -> A.Vector Rank -> IO ()
        go 0 !ranks
         = let !rankMaxIx       = maxIndex ranks
               !rankMax         = indexArray ranks (Z:.indexArray rankMaxIx Z)
               -- Sum up the ranks for all the pages,
               -- this should be very close to 1, minus some some round-off error.
           in do   -- Show the page with the maximum rank.
                  putStrLn $ "  high ix    : "  P.++ show rankMaxIx
                  putStrLn $ "  high rank  : "  P.++ show rankMax
                  return ()

        go !i !ranks
         = do   putStr "\n"
                putStrLn $ "* Step " P.++ show i

                -- Run a step of the algorithm.
                let ranks1 = stepInLoop ranks
                let ranks2 = addDangles (ranks, sizes)

                -- Sum up the ranks for all the pages,
                -- this should be very close to 1, minus some some round-off error.
                let rankSum = sum ranks2
                putStrLn $ "  rank sum   : "  P.++ show rankSum

                go (i - 1) ranks2

        -- Add to the rank the score due to dangling vectors.
        addDangles :: (Vector Rank, Vector Int) -> Vector Rank
        addDangles = run1 backend $ A.uncurry $ \ranks sizes -> let

            dangleScore = A.fold (+) 0 (A.zipWith d ranks sizes)

            dangleContrib :: Acc (Scalar Rank)
            dangleContrib = A.unit (the dangleScore / (A.lift (P.fromIntegral pageCount :: Float)))

            d r s = s ==* 0 ? (r, 0)

          in A.map (+ A.the dangleContrib) ranks

        stepInLoop :: A.Vector Rank -> A.Vector Rank
        stepInLoop = run1 backend (stepRank from to (use sizes))

        -- Computer the index of the maximum rank.
        maxIndex :: A.Vector Rank -> A.Scalar Int
        maxIndex = run1 backend $ (\ranks -> A.fold (\x y -> ranks ! index1 x >* ranks ! index1 y ? (x,y)) 0 (A.enumFromN (A.shape ranks) 0))

        sum = run1 backend A.sum
