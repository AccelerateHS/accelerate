
import SMVM
import Matrix
import Config
import ParseArgs
import Monitoring

import Prelude                                  as P
import Data.Label                               ( get )
import Criterion.Main                           ( defaultMainWith, bench, whnf )
import System.Random.MWC
import System.Exit
import System.Environment
import Data.Array.Accelerate                    as A
import qualified Data.Vector.Unboxed            as V


main :: IO ()
main = withSystemRandom $ \gen -> do
  beginMonitoring

  argv                  <- getArgs
  (conf, cconf, rest)   <- parseArgs configHelp configBackend options defaults header footer argv
  fileIn                <- case rest of
    (i:_)       -> return i
    _           -> parseArgs configHelp configBackend options defaults [] [] ("--help":argv)
                >> exitSuccess

  -- Read in the matrix file, and generate a random vector to multiply against
  --
  (segd', svec', cols) <- readCSRMatrix gen fileIn
  vec'                 <- uniformVector gen cols

  -- Convert to Accelerate arrays
  --
  let vec       = fromFunction (Z :. V.length vec')  (\(Z:.i) -> vec'  V.! i)
      segd      = fromFunction (Z :. V.length segd') (\(Z:.i) -> segd' V.! i)
      svec      = fromFunction (Z :. V.length svec') (\(Z:.i) -> svec' V.! i)
      smat      = lift (use segd, svec)

      backend   = get configBackend conf

  putStrLn $ "Reading matrix: " P.++ fileIn
  putStrLn $ "  with shape: " P.++ shows (V.length segd') " x " P.++ shows cols " and "
                              P.++ shows (V.length svec') " entries\n"

  -- Benchmark
  --
  withArgs (P.tail rest) $ defaultMainWith cconf
    [ bench "smvm" $ whnf (run1 backend (smvm smat)) vec ]

