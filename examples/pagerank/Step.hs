module Step
        (stepRank, Update)
where
  
import Page
import Progress
import Control.Monad
import Data.IORef
import Data.Array.Accelerate                    as A
import Data.Array.Accelerate.AST                ( Idx(..) )

type Update = (PageId, Rank)

-- | Find the page rank contribution of one edge in the page graph.
contribution
        :: Acc (Vector Int)    -- ^ Number of outgoing links for each page.
        -> Acc (Vector Rank)   -- ^ Old ranks vector.
        -> Acc (Scalar PageId) -- ^ A link's from page.
        -> Acc (Scalar PageId) -- ^ A link's to page.
        -> Acc (Scalar Update) -- ^ New rank.
contribution sizes ranks from to
  = A.zipWith f from to
  where
    f from to = lift (to, ranks ! index1 (A.fromIntegral from) / A.fromIntegral (sizes ! index1 (A.fromIntegral from))) :: Exp Update

-- | Updates a vector of ranks by a given vector of updates.
addUpdates
        :: Acc (Vector Rank)   -- ^ Old partial ranks.
        -> Acc (Vector Update) -- ^ Updates.
        -> Acc (Vector Rank)   -- ^ New partial ranks.
addUpdates parRanks updates
 = let
        (to, contr) = A.unzip updates
   in A.permute (+) parRanks (index1 . A.fromIntegral . (to !)) contr

stepRank :: Vector PageId     -- Non-accelerated page graph, from page id's.
         -> Vector PageId     -- Non-accelerated page graph, to page id's.
         -> Acc (Vector Int)  -- Sizes.
         -> Acc (Vector Rank) -- Initial ranks.
         -> Acc (Vector Rank) -- Final ranks.
stepRank from to sizes ranks
  = let
      zeroes :: Acc (Vector Rank)
      zeroes = A.fill (shape ranks) 0.0

      -- Ignore shape vector.
      addUpdates' :: Acc (Vector Rank) -> Acc (Vector Z) -> Acc (Vector Update) -> Acc (Vector Rank)
      addUpdates' = const . addUpdates

    in A.asnd $ A.loop
              $ A.useLazy (A.constant (Z :. stream)) from
              $ A.useLazy (A.constant (Z :. stream)) to
              $ A.zipWithStream (contribution sizes ranks) (SuccIdx ZeroIdx) ZeroIdx
              $ A.foldStreamFlatten addUpdates' zeroes ZeroIdx
              $ A.emptyLoop
  where
    stream = maxBound :: Int
