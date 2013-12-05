
module Step
        (stepRank, PageGraph)
where
import Page
import Progress
import Control.Monad
import Data.IORef
import Data.Array.Accelerate as A


type PageGraph = Vector Link


-- | Perform one iteration step for the internal Page Rank algorithm.
stepRank
        :: Acc PageGraph       -- ^ Part of the pages graph.
        -> Acc (Vector Int)    -- ^ Number of outgoing links for each page.
        -> Acc (Vector Rank)   -- ^ Old ranks vector.
        -> Acc (Vector Rank)   -- ^ Partial ranks vector
        -> Acc (Vector Rank)   -- ^ New ranks vector.

stepRank links sizes ranks parRanks
 = let
        pageCount  = A.size sizes

        -- For every link supplied, calculate it's contribution to the page it points to.
        contribution :: Acc (Vector Float)
        contribution = A.generate (A.shape links)
                                  (\ix -> let (from, _) = unlift $ links ! ix :: (Exp PageId, Exp PageId)
                                          in ranks ! index1 (A.fromIntegral from) / A.fromIntegral (sizes ! index1 (A.fromIntegral from)))

        -- Add to the partial ranks the contribution of the supplied links.
        ranks' = A.permute (+) parRanks (\ix -> let (_, to) = unlift $ links ! ix :: (Exp PageId, Exp PageId)
                                              in index1 (A.fromIntegral to)) contribution
        in ranks'

