{-# LANGUAGE CPP             #-}
{-# LANGUAGE PatternGuards   #-}
{-# LANGUAGE TemplateHaskell #-}
--
-- Configuration parameters
--

module Config (

  Options,
  viscosity, diffusion, timestep, inputDensity, inputVelocity, simulationSteps,
  simulationWidth, simulationHeight, initialDensity, initialVelocity,
  displayScale, displayFramerate, optBench,

  processArgs, run, run1

) where

import Type

import Data.IORef
import Data.Label
import Control.Monad
import System.Console.GetOpt
import System.Exit
import Prelude                                          as P

import Data.Array.Accelerate                            as A
import Data.Array.Accelerate.IO                         as A
import qualified Data.Array.Accelerate.Interpreter      as I
#ifdef ACCELERATE_CUDA_BACKEND
import qualified Data.Array.Accelerate.CUDA             as CUDA
#endif

data Backend
  = Interpreter
#ifdef ACCELERATE_CUDA_BACKEND
  | CUDA
#endif
  deriving (Show, Bounded)


data Options = Options
  {
    -- simulation
    _viscosity          :: !Float
  , _diffusion          :: !Float
  , _timestep           :: !Float
  , _inputDensity       :: !Float
  , _inputVelocity      :: !Float
  , _simulationSteps    :: !Int
  , _simulationWidth    :: !Int
  , _simulationHeight   :: !Int

  -- visualisation
  , _displayScale       :: !Int
  , _displayFramerate   :: !Int
  , _initialDensity     :: DensityField
  , _initialVelocity    :: VelocityField

  -- misc
  , _optBackend         :: !Backend
  , _optBench           :: !Bool
  , _optHelp            :: !Bool
  }
  deriving Show

$(mkLabels [''Options])

defaultOptions :: Options
defaultOptions = Options
  { _viscosity          = 0
  , _diffusion          = 0
  , _timestep           = 0.1
  , _inputDensity       = 100
  , _inputVelocity      = 20
  , _simulationSteps    = 40
  , _simulationWidth    = 100
  , _simulationHeight   = 100

  , _displayScale       = 5
  , _displayFramerate   = 25
  , _initialDensity     = error "initial density??"
  , _initialVelocity    = error "initial velocity??"

  , _optBackend         = maxBound
  , _optBench           = False
  , _optHelp            = False
  }


-- Execute an Accelerate expression using the selected backend
--
run :: Arrays a => Options -> Acc a -> a
run opts = case _optBackend opts of
  Interpreter   -> I.run
#ifdef ACCELERATE_CUDA_BACKEND
  CUDA          -> CUDA.run
#endif

run1 :: (Arrays a, Arrays b) => Options -> (Acc a -> Acc b) -> a -> b
run1 opts f = case _optBackend opts of
  Interpreter   -> head . I.stream f . return
#ifdef ACCELERATE_CUDA_BACKEND
  CUDA          -> CUDA.run1 f
#endif


processArgs :: [String] -> IO (Options, [String])
processArgs argv = do

  -- Some additional options, which we will use to determine how to set up the
  -- initial conditions of the simulator.
  --
  densityBMPArg         <- newIORef Nothing
  velocityBMPArg        <- newIORef Nothing
  initialiseArg         <- newIORef False

  let setInitialise x o  = writeIORef initialiseArg x         >> return o
      setDensityBMP x o  = writeIORef densityBMPArg  (Just x) >> return o
      setVelocityBMP x o = writeIORef velocityBMPArg (Just x) >> return o

  -- Parse the command-line options
  --
  let options :: [OptDescr (Options -> IO Options)]
      options =
        -- Backend execution setup
        [ Option [] ["interpreter"]     (NoArg (return . set optBackend Interpreter)) "reference implementation (sequential)"
#ifdef ACCELERATE_CUDA_BACKEND
        , Option [] ["cuda"]            (NoArg (return . set optBackend CUDA))        "implementation for NVIDIA GPUs (parallel)"
#endif

        -- Simulation options
        , Option [] ["viscosity"]       (ReqArg (parse viscosity) "FLOAT")      (describe viscosity "viscosity for velocity damping")
        , Option [] ["diffusion"]       (ReqArg (parse diffusion) "FLOAT")      (describe diffusion "diffusion rate for mass dispersion")
        , Option [] ["delta"]           (ReqArg (parse timestep) "FLOAT")       (describe timestep "simulation time between each frame")
        , Option [] ["density"]         (ReqArg (parse inputDensity) "FLOAT")   (describe inputDensity "magnitude of user input density")
        , Option [] ["velocity"]        (ReqArg (parse inputVelocity) "FLOAT")  (describe inputVelocity "magnitude of user input velocity")
        , Option [] ["iterations"]      (ReqArg (parse simulationSteps) "INT")  (describe simulationSteps "number of iterations of the linear solver")
        , Option [] ["width"]           (ReqArg (parse simulationWidth) "INT")  (describe simulationWidth "grid width of simulation")
        , Option [] ["height"]          (ReqArg (parse simulationHeight) "INT") (describe simulationHeight "grid height of simulation")

        -- Display options
        , Option [] ["scale"]           (ReqArg (parse displayScale) "INT")     (describe displayScale "feature size of visualisation")
        , Option [] ["framerate"]       (ReqArg (parse displayFramerate) "INT") (describe displayFramerate "frame rate for visualisation")
        , Option [] ["bmp-density"]     (ReqArg setDensityBMP "FILE.bmp")       "file for initial fluid density"
        , Option [] ["bmp-velocity"]    (ReqArg setVelocityBMP "FILE.bmp")      "file for initial fluid velocity"

        -- Miscellaneous
        , Option [] ["init"]            (NoArg (setInitialise True))            "begin with a set of standard initial condition"
        , Option [] ["benchmark"]       (NoArg (return . set optBench True))    (describe optBench "benchmark instead of displaying animation")
        , Option "h?" ["help"]          (NoArg (return . set optHelp True))     (describe optHelp "show help message")
        ]

      parse f x         = return . set f (read x)
      describe f msg    = msg ++ " (" ++ show (get f defaultOptions) ++ ")"
      helpMsg errs      = concat errs ++ usageInfo header options ++ footer
      header            = unlines
        [ "accelerate-fluid (c) 2011 Trevor L. McDonell"
        , ""
        , "Usage: accelerate-fluid [OPTIONS]"
        ]

      footer = unlines
        [ ""
        , "Runtime usage:"
        , ""
        , "          click                    add density sources to the image"
        , "          shift-click              add velocity sources"
        , "          r                        reset the image"
        , "          d                        toggle display of density field"
        , "          v                        toggle display of velocity field lines"
        ]

  (opts, rest)
    <- case getOpt' RequireOrder options argv of
         (actions,_,n,[])       -> do
           o     <- foldl (>>=) (return defaultOptions) actions
           case get optHelp o of
             False                      -> return (o,n)
             _ | True <- get optBench o -> return (o,"--help":n)
             _                          -> putStrLn (helpMsg []) >> exitSuccess
         (_,_,_,errors)         -> error (helpMsg errors)

  -- Extract option values, and set up the initial conditions
  --
  initialise    <- readIORef initialiseArg
  densityBMP    <- readIORef densityBMPArg
  velocityBMP   <- readIORef velocityBMPArg

  let width     = get simulationWidth opts
      height    = get simulationHeight opts

  let mkInitialDensity
        -- Load the density from a .bmp file, using the luminance as the scalar
        -- density value.
        --
        | Just file <- densityBMP
        = do
            arr         <- either (error . show) id `fmap` readImageFromBMP file
            let Z:.h:.w  = arrayShape arr

            when (w /= width || h /= height)
              $ error "accelerate-fluid: density-bmp does not match width x height"

            return . run opts $ A.map luminanceOfRGBA32 (use arr)

        -- Prime the pumps with some initial conditions (see what I did there?)
        --
        | initialise || get optBench opts
        = let width'    = constant $ P.fromIntegral width
              height'   = constant $ P.fromIntegral height
              yc        = constant $ P.fromIntegral (height `div` 2)
              xc        = constant $ P.fromIntegral (width  `div` 2)
          in return . run opts
              $ A.generate
                  (constant $ Z :. height :. width)
                  $ \ix -> let Z:.y:.x   = unlift ix
                               x'        = A.fromIntegral x
                               y'        = A.fromIntegral y
                               kx1       = cos (10 * (x'-xc) / width')
                               ky1       = cos (10 * (y'-yc) / height')
                               d1        = kx1 * ky1
                           in
                           A.max 0 d1

        -- Set the field to zero
        --
        | otherwise
        = return . run opts $ A.fill (lift (Z:.height:.width)) 0

  let mkInitialVelocity
        -- Load the density from .bmp file, using the red and green channels as
        -- the x- and y- velocity components respectively.
        --
        | Just file <- velocityBMP
        = do
            arr         <- either (error . show) id `fmap` readImageFromBMP file
            let Z:.h:.w  = arrayShape arr

            when (w /= width || h /= height)
              $ error "accelerate-fluid: velocity-bmp does not match width x height"

            let conv rgb =
                  let (r,g,_,_) = unlift $ unpackRGBA32 rgb :: (Exp Word8, Exp Word8, Exp Word8, Exp Word8)
                      r'        = A.fromIntegral (-128 + A.fromIntegral r :: Exp Int)
                      g'        = A.fromIntegral (-128 + A.fromIntegral g :: Exp Int)
                  in lift (r' * 0.0001, g' * 0.0001)

            return . run opts $ A.map conv (use arr)

        -- Set some initial conditions
        --
        | initialise || get optBench opts
        = let width'    = constant $ P.fromIntegral width
              height'   = constant $ P.fromIntegral height
              yc        = constant $ P.fromIntegral (height `div` 2)
              xc        = constant $ P.fromIntegral (width  `div` 2)
          in return . run opts
              $ A.generate
                  (constant $ Z :. height :. width)
                  $ \ix -> let Z:.y:.x   = unlift ix
                               x'        = A.fromIntegral x
                               y'        = A.fromIntegral y
                               kx1       = cos (15 * (x'-xc) / width')
                               ky1       = cos (15 * (y'-yc) / height')
                               d2        = kx1 * ky1 / 5
                           in
                           lift (constant 0, d2)

        -- Set the field to zero
        --
        | otherwise
        = return . run opts $ A.fill (lift (Z:.height:.width)) (constant (0,0))

  -- Return the completed options structure
  --
  density       <- mkInitialDensity
  velocity      <- mkInitialVelocity

  return ( opts { _initialDensity = density, _initialVelocity = velocity }, rest )

