
module Matrix (

  CSRMatrix(..),
  matrixToCSR, nnz, rows, columns,

) where

import Control.Monad
import Data.Int
import Data.List
import Data.Matrix.MatrixMarket                                     ( Matrix(..), Structure(..) )
import Data.Scientific
import Data.Vector.Storable                                         ( Vector, Storable )
import System.Random.MWC
import qualified Data.Vector.Storable                               as S



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
matrixToCSR _ (RMatrix dim _ structure entries)
  = return
  $ toCSR dim
  $ case structure of
      General  -> flip map entries
                $ \(r,c,v) -> (fromIntegral (r-1), fromIntegral (c-1), toRealFloat v)
      _        -> flip concatMap entries
                $ \(r,c,v) -> let v' = toRealFloat v
                                  r' = fromIntegral (r-1)
                                  c' = fromIntegral (c-1)
                              in
                              if r' == c' then [(r',c',v')]
                                          else [(r',c',v'), (c',r',v')]

matrixToCSR gen (PatternMatrix dim _ structure entries)
  = fmap (toCSR dim)
  $ case structure of
      General -> forM entries
               $ \(r,c) -> let r' = fromIntegral (r-1)
                               c' = fromIntegral (c-1)
                           in do
                             v <- uniform gen
                             return (r', c', v)
      _       -> fmap concat
               $ forM entries
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
toCSR :: (Int,Int)
      -> [(Int32,Int32,Double)]
      -> CSRMatrix Double
toCSR dim entries =
  let cmp (r1,c1,_) (r2,c2,_)
        | r1 == r2  = compare c1 c2
        | otherwise = compare r1 r2

      sorted      = sortBy cmp entries    -- possibly more efficient to sort as unboxed vector
      (rx,ix,vs)  = unzip3 sorted
      segd        = map (fromIntegral . length) (group rx)
      segd_len    = S.fromList segd
      segd_off    = S.scanl (+) 0 segd_len
  in
  CSRMatrix { csr_segd_length = segd_len
            , csr_segd_offset = segd_off
            , csr_indices     = S.fromList ix
            , csr_values      = S.fromList vs
            , csr_dim         = dim
            }

