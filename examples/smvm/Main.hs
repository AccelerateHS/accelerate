
import SMVM
import Matrix
import Config

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Examples.Internal                      as A
import Data.Array.Accelerate.IO                                     as A

import Data.Label                                                   ( get )
import Data.Matrix.MatrixMarket                                     ( readMatrix )
import System.Environment
import System.Exit
import System.Random.MWC
import Text.Printf
import Prelude                                                      as P
import qualified Data.Vector.Storable                               as S


main :: IO ()
main = do
  beginMonitoring

  (_, opts, rest)       <- parseArgs options defaults header footer
  fileIn                <- case rest of
    (i:_)       -> return i
    _           -> withArgs ["--help"] $ parseArgs options defaults [] []
                >> exitSuccess

  -- Read in the matrix file, and generate a random vector to multiply against
  --
  matrix  <- readMatrix fileIn
  csr     <- withSystemRandom $ \gen -> matrixToCSR gen matrix
  xs      <- withSystemRandom $ \gen -> uniformVector gen (rows csr) :: IO (S.Vector Double)

  -- Convert to Accelerate arrays
  --
  let vec       = fromVectors (Z :. S.length xs) xs
      segd      = fromVectors (Z :. S.length (csr_segd_length csr)) (csr_segd_length csr)
      svec      = fromVectors (Z :. nnz csr) (((), csr_indices csr), csr_values csr)
      smat      = use (segd, svec)  :: Acc (SparseMatrix Double)

      backend   = get optBackend opts

  printf "input matrix: %s\n" fileIn
  printf "        size: %d x %d\n" (rows csr) (columns csr)
  printf "   non-zeros: %d\n\n" (nnz csr)

  -- Benchmark
  --
  runBenchmarks opts (P.tail rest)
    [ bench "smvm" $ whnf (run1 backend (smvm smat)) vec
    ]

