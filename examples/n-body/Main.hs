--
-- An N-Body simulation
--

-- friends
import Config
import Common.Body
import Common.World
import Gloss.Draw
import Gloss.Event
import Gloss.Simulate
import Random.Array
import Random.Position
import qualified Solver.Naive                   as Naive
import qualified Solver.BarnsHut                as BarnsHut

import Data.Array.Accelerate                    as A hiding ( size )

-- system
import Prelude                                  as P
import Data.Label
import System.Environment
import System.Random.MWC                        ( uniformR )
import Criterion.Main                           ( defaultMainWith, bench, whnf )
import Graphics.Gloss.Interface.Pure.Game


main :: IO ()
main
  = do  (conf, cconf, nops)     <- parseArgs =<< getArgs

        let solver      = case get configSolver conf of
                            Naive       -> Naive.calcAccels
                            BarnsHut    -> BarnsHut.calcAccels

            n           = get configBodyCount conf
            size        = get configWindowSize conf
            fps         = get configRate conf
            epsilon     = get configEpsilon conf

            -- Generate random particle positions in a disc layout centred at
            -- the origin. Start the system rotating with particle speed
            -- proportional to distance from the origin
            --
            positions   = randomArrayOf (disc (0,0) (get configStartDiscSize conf)) (Z :. n)
            masses      = randomArrayOf (\_ -> uniformR (1, get configBodyMass conf)) (Z :. n)

            bodies      = run conf
                        $ A.map (setStartVelOfBody . constant $ get configStartSpeed conf)
                        $ A.zipWith setMassOfBody (A.use masses)
                        $ A.map (A.uncurry unitBody)
                        $ A.use positions

            -- The initial simulation state
            --
            universe    = initialise world
            world       = World { worldBodies   = bodies
                                , worldSteps    = 0
                                , worldTime     = 0 }

            -- Advancing the simulation
            --
            advance     = advanceWorld step
            step        = P.curry
                        $ run1 conf
                        $ A.uncurry
                        $ advanceBodies (solver $ constant epsilon)

        -- Forward unto dawn
        --
        if get configBenchmark conf
           then withArgs nops $ defaultMainWith cconf (return ())
                  [ bench "n-body" $ whnf (advance 0.1) world ]

           else play
                  (InWindow "N-Body" (size, size) (10, 10))     -- window size & position
                  black                                         -- background colour
                  fps                                           -- number of simulation steps per second
                  universe                                      -- initial world
                  (draw conf)                                   -- fn to convert a world into a picture
                  react                                         -- fn to handle input events
                  (simulate advance)                            -- fn to advance the world

