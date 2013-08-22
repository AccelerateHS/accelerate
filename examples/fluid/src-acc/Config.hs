{-# LANGUAGE CPP             #-}
{-# LANGUAGE PatternGuards   #-}
{-# LANGUAGE TemplateHaskell #-}
--
-- Configuration parameters
--

module Config where

import Type
import ParseArgs

import Data.Label
import Control.Monad
import Prelude                                          as P
import Data.Array.Accelerate                            as A
import Data.Array.Accelerate.IO                         as A


data Initial a
    = FromFile          FilePath
    | FromFunction      (Backend -> Int -> Int -> a)


data Config = Config
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

  -- extra options to specify initial conditions for command parsing
  , _setupDensity       :: Initial DensityField
  , _setupVelocity      :: Initial VelocityField
  }

$(mkLabels [''Config])


defaults :: Config
defaults = Config
  { _viscosity          = 0
  , _diffusion          = 0
  , _timestep           = 0.1
  , _inputDensity       = 50
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

  , _setupDensity       = FromFunction makeField_empty
  , _setupVelocity      = FromFunction makeField_empty
  }


-- | The set of available command-line options
--
options :: [OptDescr (Config -> Config)]
options =
  -- Simulation options
  [ Option  [] ["viscosity"]
            (ReqArg (parse viscosity) "FLOAT")
            (describe viscosity "viscosity for velocity damping")

  , Option  [] ["diffusion"]
            (ReqArg (parse diffusion) "FLOAT")
            (describe diffusion "diffusion rate for mass dispersion")

  , Option  [] ["delta"]
            (ReqArg (parse timestep) "FLOAT")
            (describe timestep "simulation time between each frame")

  , Option  [] ["density"]
            (ReqArg (parse inputDensity) "FLOAT")
            (describe inputDensity "magnitude of user input density")

  , Option  [] ["velocity"]
            (ReqArg (parse inputVelocity) "FLOAT")
            (describe inputVelocity "magnitude of user input velocity")

  , Option  [] ["iterations"]
            (ReqArg (parse simulationSteps) "INT")
            (describe simulationSteps "number of iterations of the linear solver")

  , Option  [] ["width"]
            (ReqArg (parse simulationWidth) "INT")
            (describe simulationWidth "grid width of simulation")

  , Option  [] ["height"]
            (ReqArg (parse simulationHeight) "INT")
            (describe simulationHeight "grid height of simulation")

  -- Display options
  , Option  [] ["scale"]
            (ReqArg (parse displayScale) "INT")
            (describe displayScale "feature size of visualisation")

  , Option  [] ["framerate"]
            (ReqArg (parse displayFramerate) "INT")
            (describe displayFramerate "frame rate for visualisation")

  -- Initial conditions
  , Option  [] ["bmp-density"]
            (ReqArg (set setupDensity . FromFile) "FILE.bmp")
            "file for initial fluid density"

  , Option  [] ["bmp-velocity"]
            (ReqArg (set setupVelocity . FromFile) "FILE.bmp")
            "file for initial fluid velocity"

  , Option  [] ["init-checks"]
            (NoArg init_checks)
            "initial density field with zero velocity field"

  , Option  [] ["init-man"]
            (NoArg init_man)
            "initial density field with swirling velocity"

  , Option  [] ["init-elk"]
            (NoArg init_elk)
            "initial density field with swirling velocity"

  -- Miscellaneous
  , Option  [] ["benchmark"]
            (NoArg (set optBench True))
            (describe optBench "benchmark instead of displaying animation")

  , Option  ['h','?'] ["help"]
            (NoArg (set optHelp True))
            (describe optHelp "show help message")
  ]
  where
    parse f x           = set f (read x)
    describe f msg      = msg P.++ " (" P.++ show (get f defaults) P.++ ")"

    init_checks         = set setupDensity  (FromFunction makeDensity_checks)
                        . set setupVelocity (FromFunction makeField_empty)

    init_man            = set setupDensity  (FromFunction makeDensity_checks)
                        . set setupVelocity (FromFunction makeVelocity_man)

    init_elk            = set setupDensity  (FromFunction makeDensity_checks)
                        . set setupVelocity (FromFunction makeVelocity_elk)


header :: [String]
header =
  [ "accelerate-fluid (c) [2011..2013] The Accelerate Team"
  , ""
  , "Usage: accelerate-fluid [OPTIONS]"
  , ""
  ]

footer :: [String]
footer =
  [ ""
  , "Runtime usage:"
  , "     click        add density sources to the image"
  , "     shift-click  add velocity sources"
  , "     r            reset the image"
  , "     d            toggle display of density field"
  , "     v            toggle display of velocity field lines"
  ]


-- Initial conditions
-- ------------------

initialiseConfig :: Config -> IO Config
initialiseConfig conf = do
  let backend   = get optBackend conf
      width     = get simulationWidth conf
      height    = get simulationHeight conf

  dens  <- case get setupDensity conf of
              FromFile fn       -> loadDensity_bmp backend fn width height
              FromFunction f    -> return (f backend width height)

  velo  <- case get setupVelocity conf of
              FromFile fn       -> loadVelocity_bmp backend fn width height
              FromFunction f    -> return (f backend width height)

  return . set initialDensity  dens
         . set initialVelocity velo
         $ conf


makeField_empty :: FieldElt e => Backend -> Int -> Int -> Field e
makeField_empty backend width height
  = run backend
  $ A.fill (constant (Z:.height:.width)) (constant zero)


makeDensity_checks :: Backend -> Int -> Int -> DensityField
makeDensity_checks backend width height
  = let width'  = constant $ P.fromIntegral width
        height' = constant $ P.fromIntegral height
        yc      = constant $ P.fromIntegral (height `div` 2)
        xc      = constant $ P.fromIntegral (width  `div` 2)

        checks ix
          = let Z :. y :. x     = unlift ix
                x'              = A.fromIntegral x
                y'              = A.fromIntegral y
                tx              = 10 * (x' - xc) / width'
                ty              = 10 * (y' - yc) / height'
                xk1             = abs tx >* 3*pi/2 ? (0 , cos tx)
                yk1             = abs ty >* 3*pi/2 ? (0 , cos ty)
                d1              = xk1 * yk1
            in
            0 `max` d1
    in
    run backend $ A.generate (constant (Z:.height:.width)) checks


makeVelocity_man :: Backend -> Int -> Int -> VelocityField
makeVelocity_man backend width height
  = let width'  = constant $ P.fromIntegral width
        height' = constant $ P.fromIntegral height
        yc      = constant $ P.fromIntegral (height `div` 2)
        xc      = constant $ P.fromIntegral (width  `div` 2)

        man ix
          = let Z :. y :. x     = unlift ix
                x'              = A.fromIntegral x
                y'              = A.fromIntegral y
                xk2             = cos (19 * (x' - xc) / width')
                yk2             = cos (17 * (y' - yc) / height')
                d2              = xk2 * yk2 / 5
            in
            lift (constant 0, d2)
    in
    run backend $ A.generate (constant (Z:.height:.width)) man


makeVelocity_elk :: Backend -> Int -> Int -> VelocityField
makeVelocity_elk backend width height
  = let width'  = constant $ P.fromIntegral width
        height' = constant $ P.fromIntegral height
        yc      = constant $ P.fromIntegral (height `div` 2)
        xc      = constant $ P.fromIntegral (width  `div` 2)

        elk ix
          = let Z :. y :. x     = unlift ix
                x'              = A.fromIntegral x
                y'              = A.fromIntegral y
                tx              = 12 * (x' - xc) / width'
                ty              = 12 * (y' - yc) / height'
                xk2             =  cos tx
                yk2             = -cos ty
                d2              = xk2 * yk2 / 5
            in
            lift (constant 0, d2)
    in
    run backend $ A.generate (constant (Z:.height:.width)) elk


loadDensity_bmp :: Backend -> FilePath -> Int -> Int -> IO DensityField
loadDensity_bmp backend filepath width height
  = do  arr             <- either (error . show) id `fmap` readImageFromBMP filepath
        let Z:.h:.w     =  arrayShape arr

        when (w /= width || h /= height)
          $ error "accelerate-fluid: density-bmp does not match width x height"

        return . run backend $ A.map luminanceOfRGBA32 (use arr)


loadVelocity_bmp :: Backend -> FilePath -> Int -> Int -> IO VelocityField
loadVelocity_bmp backend filepath width height
  = do  arr             <- either (error . show) id `fmap` readImageFromBMP filepath
        let Z:.h:.w     =  arrayShape arr

        when (w /= width || h /= height)
          $ error "accelerate-fluid: velocity-bmp does not match width x height"

        let conv rgb =
              let (r,g,_,_) = unlift (unpackRGBA32 rgb) :: (Exp Word8, Exp Word8, Exp Word8, Exp Word8)
                  r'        = A.fromIntegral (-128 + A.fromIntegral r :: Exp Int)
                  g'        = A.fromIntegral (-128 + A.fromIntegral g :: Exp Int)
              in lift (r' * 0.0001, g' * 0.0001)

        return . run backend $ A.map conv (use arr)

