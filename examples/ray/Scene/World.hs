module Scene.World where

-- friends
import Common.Type
import Scene.Light
import Scene.Object

-- frenemies
import Data.Array.Accelerate                                    as A
import Data.Array.Accelerate.Data.Colour.RGB


makeLights :: Float -> Lights
makeLights _time
  = A.fromList (Z :. 1) [ Light (V3 300 (-300) (-100))
                                (RGB 150000 150000 150000)
                        ]

makeObjects :: Float -> Objects
makeObjects time
  = let
        spheres = A.fromList (Z :. 4)
          [ Sphere (V3 (40 * sin time) 80 0.0)
                   20
                   (RGB 1.0 0.3 1.0)
                   0.4

          , Sphere (V3 (200 * sin time) (-40 * sin (time + pi/2)) (200 * cos time))
                   100.0
                   (RGB 0.4 0.4 1.0)
                   0.8

          , Sphere (V3 (-200.0 * sin time) (-40 * sin (time - pi/2)) (-200 * cos time))
                   100.0
                   (RGB 0.4 0.4 1.0)
                   0.5

          , Sphere (V3 0.0 (-150.0) (-100.0))
                   50.0
                   (RGB 1.0 1.0 1.0)
                   0.8
          ]

        planes = A.fromList (Z :. 1)
          [ Plane (V3 0.0 100.0 0.0)
                  (V3 0.0 (-0.9805807) (-0.19611613))
                  (RGB 1.0 1.0 1.0)
                  0.2
          ]
    in
    (spheres, planes)

