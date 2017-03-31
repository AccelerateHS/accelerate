
module Matrix (

  CSRMatrix(..),
  matrixToCSR, nnz, rows, columns,

) where

import Data.Int
import Data.Matrix.MatrixMarket                                     ( Matrix(..), Structure(..) )
import Data.Scientific
import Data.Vector.Storable                                         ( Vector, Storable )
import System.IO.Unsafe
import System.Random.MWC
import qualified Data.Vector.Storable                               as S
import qualified Data.Vector.Unboxed                                as U
import qualified Data.Vector.Algorithms.Intro                       as V


data CSRMatrix a =
  CSRMatrix { csr_segd_length :: !(Vector Int32)    -- segment descriptor as row lengths
            , csr_segd_offset :: !(Vector Int32)    -- segment descriptor as row offset
            , csr_indices     :: !(Vector Int32)    -- column indices
            , csr_values      :: !(Vector a)        -- non-zero values
            , csr_dim         :: !(Int,Int)         -- matrix dimensions (rows, columns)
            }
  deriving Show


nnz :: Storable a => CSRMatrix a -> Int
nnz = S.length . csr_values

rows :: CSRMatrix a -> Int
rows = fst . csr_dim

columns :: CSRMatrix a -> Int
columns = snd . csr_dim


-- Convert data read from MatrixMarket format into compressed-sparse-row format
-- with zero-based indexing.
--
-- Note that for [Skew-]Symmetric and Hermitian matrices, only the lower
-- triangle is stored in the file. For those cases this routine fills in the
-- upper triangle positions as well, so that the returned CSR matrix is in
-- general format.
--
matrixToCSR
    :: GenIO
    -> Matrix Scientific
    -> IO (CSRMatrix Double)
matrixToCSR _ (RMatrix dim n structure entries)
  = return
  $ case structure of
      General  -> toCSR dim n
                $ flip map entries
                $ \(r,c,v) -> (fromIntegral (r-1), fromIntegral (c-1), toRealFloat v)
      _        -> toCSR dim (n*2)
                $ flip concatMap entries
                $ \(r,c,v) -> let v' = toRealFloat v
                                  r' = fromIntegral (r-1)
                                  c' = fromIntegral (c-1)
                              in
                              if r' == c' then [(r',c',v')]
                                          else [(r',c',v'), (c',r',v')]

matrixToCSR gen (PatternMatrix dim n structure entries)
  = case structure of
      General -> fmap (toCSR dim n)
               $ forM' entries
               $ \(r,c) -> let r' = fromIntegral (r-1)
                               c' = fromIntegral (c-1)
                           in do
                             v <- uniform gen
                             return (r', c', v)
      _       -> fmap (toCSR dim (n*2))
               $ fmap concat
               $ forM' entries
               $ \(r,c) -> let r' = fromIntegral (r-1)
                               c' = fromIntegral (c-1)
                           in do
                             v <- uniform gen
                             u <- uniform gen
                             if r' == c' then return [(r',c',v)]
                                         else return [(r',c',v), (c',r',u)]

matrixToCSR _ CMatrix{}   = error "matrixToCSR: complex matrices not supported"
matrixToCSR _ IntMatrix{} = error "matrixToCSR: integer matrices not supported"


-- Convert the given list of (row index, column index, value) triples into a CSR
-- matrix representation.
--
{-# INLINE toCSR #-}
toCSR :: (Int,Int)                        -- matrix dimensions
      -> Int                              -- #non-zero elements (hint)
      -> [(Int32,Int32,Double)]           -- (row,column,value)
      -> CSRMatrix Double
toCSR dim@(r,_) n entries =
  let cmp (r1,c1,_) (r2,c2,_)
        | r1 == r2  = compare c1 c2
        | otherwise = compare r1 r2

      sorted      = U.create $ do
                      x <- U.unsafeThaw (U.fromListN n entries)
                      V.sortBy cmp x
                      return x

      (rx,ix,vs)  = U.unzip3 sorted
      segd_len    = S.unfoldrN r (\v -> if U.null v
                                          then Nothing
                                          else let (h,t) = U.span (== U.unsafeHead v) v
                                               in  Just (fromIntegral (U.length h), t)) rx

      segd_off    = S.scanl (+) 0 segd_len
  in
  CSRMatrix { csr_segd_length = segd_len
            , csr_segd_offset = segd_off
            , csr_indices     = S.convert ix
            , csr_values      = S.convert vs
            , csr_dim         = dim
            }


-- Lazier versions of things in Control.Monad
--
sequence' :: [IO a] -> IO [a]
sequence' ms = foldr k (return []) ms
    where k m m' = do { x <- m; xs <- unsafeInterleaveIO m'; return (x:xs) }

mapM' :: (a -> IO b) -> [a] -> IO [b]
mapM' f as = sequence' (map f as)

forM' :: [a] -> (a -> IO b) -> IO [b]
forM' = flip mapM'

