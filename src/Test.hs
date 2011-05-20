{-# LANGUAGE CPP, ExistentialQuantification #-}

module Test (

  Title, Description, Test(..), Status(..),
  allTests, verifyTest, benchmarkTest

) where

-- individual test implementations
import qualified Map
import qualified Zip
import qualified ZipWith
import qualified Fold
import qualified ScanSeg
import qualified Stencil
import qualified Stencil2
import qualified Permute
import qualified Backpermute

import qualified SASUM
import qualified SAXPY
import qualified DotP
import qualified Filter
import qualified SMVM
import qualified BlackScholes
import qualified Radix
import qualified SliceExamples

#ifdef ACCELERATE_IO
import qualified BlockCopy
#endif

import qualified Canny
import qualified IntegralImage

-- friends
import Util
import Config
import Validate

-- libraries
import Prelude                                          hiding (catch)
import Criterion                                        (Benchmark, bench, whnf)
import Data.Maybe
import Data.Array.IArray
import Control.Monad
import Control.Exception
import System.IO
import System.IO.Unsafe
import System.Console.CmdArgs                           (getVerbosity, Verbosity(..))

import Data.Array.Accelerate                            (Acc)
import qualified Data.Array.Accelerate                  as Acc
import qualified Data.Array.Accelerate.Interpreter      as Interpreter

#ifdef ACCELERATE_CUDA_BACKEND
import qualified Data.Array.Accelerate.CUDA             as CUDA
#endif


data Status
  = Ok
  | Skipped
  | Failed String

instance Eq Status where
  Ok      == Ok      = True
  Skipped == Skipped = True
  _       == _       = False

instance Show Status where
  show Ok         = "Ok"
  show Skipped    = "Skipped"
  show (Failed s) = "Failed: " ++ s


type Title       = String
type Description = String

data Test
  -- A cannonical test program, where we have a reference implementation that
  -- the Accelerate program must match. The 'convert' field is slightly magic:
  -- we need to carry it around as a proof that ELtRepr sh ~ EltRepr ix.
  --
  = forall array ix sh e. (Similar e, Acc.Elt e, Acc.Shape sh, Show ix, Show e, IArray array e, Ix ix) => Test
  { title       :: Title
  , description :: Description
  , reference   :: () -> array ix e
  , accelerate  :: () -> Acc (Acc.Array sh e)
  , convert     :: Acc.Array sh e -> array ix e
  }

  -- No reference implementation, so the result can not be validated, but we can
  -- check that no exceptions are thrown, and benchmark the operation.
  --
  | forall sh e. (Acc.Elt e, Acc.Shape sh) => TestNoRef
  { title       :: Title
  , description :: Description
  , accelerate  :: () -> Acc (Acc.Array sh e)
  }

  -- An IO action. Run once to verify that no exceptions are thrown, do not
  -- benchmark.
  --
  | forall a. TestIO
  { title       :: Title
  , description :: Description
  , action      :: IO a
  }


allTests :: Config -> IO [Test]
allTests cfg = sequence'
  [
    -- primitive functions
    mkTest "map-abs"               "absolute value of each element"             $ Map.run "abs" n
  , mkTest "map-plus"              "add a constant to each element"             $ Map.run "plus" n
  , mkTest "map-square"            "square of each element"                     $ Map.run "square" n
  , mkTest "zip"                   "vector zip"                                 $ Zip.run n
  , mkTest "zipWith-plus"          "element-wise addition"                      $ ZipWith.run "plus" n
  , mkTest "fold-sum"              "vector reduction: fold (+) 0"               $ Fold.run "sum" n
  , mkTest "fold-product"          "vector product: fold (*) 1"                 $ Fold.run "product" n
  , mkTest "fold-maximum"          "maximum of a vector: fold1 max"             $ Fold.run "maximum" n
  , mkTest "fold-minimum"          "minimum of a vector: fold1 min"             $ Fold.run "minimum" n
  , mkTest "fold-2d-sum"           "reduction along innermost matrix dimension" $ Fold.run2d "sum-2d" n
  , mkTest "fold-2d-product"       "product along innermost matrix dimension"   $ Fold.run2d "product-2d" n
  , mkTest "scanseg-sum"           "segmented reduction"                        $ ScanSeg.run "sum" n
  , mkTest "stencil-1D"            "3-element vector"                           $ Stencil.run "1D" n
  , mkTest "stencil-2D"            "3x3 pattern"                                $ Stencil.run2D "2D" n
  , mkTest "stencil-3D"            "3x3x3 pattern"                              $ Stencil.run3D "3D" n
  , mkTest "stencil-3x3-cross"     "3x3 cross pattern"                          $ Stencil.run2D "3x3-cross" n
  , mkTest "stencil-3x3-pair"      "3x3 non-symmetric pattern with pairs"       $ Stencil.run2D "3x3-pair" n
  , mkTest "stencil2-2D"           "3x3 pattern"                                $ Stencil2.run2D "2D" n
  , mkTest "permute-hist"          "histogram"                                  $ Permute.run "histogram" n
  , mkTest "backpermute-reverse"   "reverse a vector"                           $ Backpermute.run "reverse" n
  , mkTest "backpermute-transpose" "transpose a matrix"                         $ Backpermute.run2d "transpose" n

    -- simple examples
  , mkTest "sasum"                 "sum of absolute values"                     $ SASUM.run n
  , mkTest "saxpy"                 "scalar alpha*x + y"                         $ SAXPY.run n
  , mkTest "dotp"                  "vector dot-product"                         $ DotP.run n
  , mkTest "filter"                "return elements that satisfy a predicate"   $ Filter.run n
  , mkTest "smvm"                  "sparse-matrix vector multiplication"        $ SMVM.run (cfgMatrix cfg)
  , mkTest "black-scholes"         "Black-Scholes option pricing"               $ BlackScholes.run n
  , mkTest "radixsort"             "radix sort"                                 $ Radix.run n

#ifdef ACCELERATE_IO
    -- Array IO
  , mkIO   "io"                    "array IO test"                              $ BlockCopy.run
#endif

    -- image processing
  , mkNoRef "canny"                "canny edge detection"                       $ Canny.run img
  , mkNoRef "integral-image"       "image integral (2D scan)"                   $ IntegralImage.run img
    -- slices
  , mkTest "slices"                "replicate (Z:.2:.All:.All)"                 $ SliceExamples.run1
  , mkTest "slices"                "replicate (Z:.All:.2:.All)"                 $ SliceExamples.run2
  , mkTest "slices"                "replicate (Z:.All:.All:.2)"                 $ SliceExamples.run3
  , mkTest "slices"                "replicate (Any:.2)"                         $ SliceExamples.run4
  , mkTest "slices"                "replicate (Z:.2:.2:.2)"                     $ SliceExamples.run5

  ]
  where
    n   = cfgElements cfg
    img = fromMaybe (error "no image file specified") (cfgImage cfg)
    --
    mkTest name desc builder = do
      ~(ref,acc) <- unsafeInterleaveIO builder  -- must be super lazy
      return $ Test name desc ref acc Acc.toIArray

    mkNoRef name desc builder = do
      acc <- unsafeInterleaveIO builder
      return $ TestNoRef name desc acc

#ifdef ACCELERATE_IO
    mkIO name desc act = return $ TestIO name desc act
#endif


-- How to evaluate Accelerate programs with the chosen backend?
--
backend :: Acc.Arrays a => Config -> Acc a -> a
backend cfg =
  case cfgBackend cfg of
    Interpreter -> Interpreter.run
#ifdef ACCELERATE_CUDA_BACKEND
    CUDA        -> CUDA.run
#endif


-- Verify that the Accelerate and reference implementations yield the same
-- result in the chosen backend
--
verifyTest :: Config -> Test -> IO Status
verifyTest cfg test = do
  quiet <- (==Quiet) `fmap` getVerbosity
  verify quiet `catch` \e -> let r = Failed (show (e :: SomeException))
                             in  putStrLn (show r) >> return r
  where
    run acc      = backend cfg $ acc ()
    verify quiet = do
      unless quiet $ putStr (title test ++ ": ") >> hFlush stdout
      result <- case test of
        Test _ _ ref acc cvt ->
          return $ case validate (ref ()) (cvt $ run acc) of
                     []   -> Ok
                     errs -> Failed . unlines . ("":)
                                    $ map (\(i,v) -> ">>> " ++ shows i " : " ++ show v) errs

        TestNoRef _ _ acc -> return $ run acc `seq` Ok
        TestIO _ _ act    -> act >> return Ok
      --
      unless quiet $ putStrLn (show result)
      return result


-- Benchmark a test with Criterion
--
benchmarkTest :: Config -> Test -> Maybe Benchmark
benchmarkTest cfg (Test name _ _ acc _)  = Just . bench name $ whnf (backend cfg . acc) ()
benchmarkTest cfg (TestNoRef name _ acc) = Just . bench name $ whnf (backend cfg . acc) ()
benchmarkTest _   (TestIO _ _ _)         = Nothing

