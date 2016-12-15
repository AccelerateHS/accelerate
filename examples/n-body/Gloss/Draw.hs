
--
-- Drawing the world as a gloss picture.
--
module Gloss.Draw ( draw )
  where

import Config
import Common.Type
import Common.World
import Gloss.Simulate

import Data.Label
import Graphics.Gloss
import qualified Data.Array.Accelerate                  as A


-- | Draw the simulation, optionally showing the Barnes-Hut tree.
--
draw :: Config -> Simulate -> Picture
draw conf universe
  = let
        shouldDrawTree  = get simulateDrawTree universe
        world           = get simulateWorld    universe

        picPoints       = Color (makeColor 1 1 1 0.4)
                        $ Pictures
                        $ map (drawBody conf)
                        $ A.toList
                        $ worldBodies world

        picTree         = Blank
--      picTree         = drawBHTree
--                      $ L.buildTree
--                      $ map massPointOfBody
--                      $ V.toList
--                      $ worldBodies world

    in Pictures [ if shouldDrawTree
                     then Color (makeColor 0.5 1.0 0.5 0.2) $ picTree
                     else Blank

                , picPoints ]


{--
-- | Draw a list version Barnes-Hut tree.
drawBHTree :: L.BHTree -> Picture
drawBHTree bht
 = drawBHTree' 0 bht

drawBHTree' depth bht
 = let
        -- The bounding box
        L.Box left down right up        = L.bhTreeBox bht
        [left', down', right', up']     = map realToFrac [left, down, right, up]

        picCell         = lineLoop [(left', down'), (left', up'), (right', up'), (right', down')]


        -- Draw a circle with an area equal to the mass of the centroid.
        centroidX       = realToFrac $ L.bhTreeCenterX bht
        centroidY       = realToFrac $ L.bhTreeCenterY bht

        centroidMass    = L.bhTreeMass bht
        circleRadius    = realToFrac $ sqrt (centroidMass / pi)

        midX            = (left' + right') / 2
        midY            = (up'   + down')  / 2

        picCentroid
         | _:_  <- L.bhTreeBranch bht
         , depth >= 1
         = Color (makeColor 0.5 0.5 1.0 0.4)
                $  Pictures
                        [ Line [(midX, midY), (centroidX, centroidY)]
                        , Translate centroidX centroidY
                        $ ThickCircle
                                (circleRadius * 4 / 2)
                                (circleRadius * 4) ]

         | otherwise
         = Blank

        -- The complete picture for this cell.
        picHere         = Pictures [picCentroid, picCell]

        -- Pictures of children.
        picSubs         = map (drawBHTree' (depth + 1))
                        $ L.bhTreeBranch bht

   in   Pictures (picHere : picSubs)
--}


-- | Draw a single body. Set the size of the body depending on it's mass, in
-- five size categories.
--
drawBody :: Config -> Body -> Picture
drawBody conf ((position, mass), _, _)
  = let sizeMax = get configBodyMass conf / 5
        size    = 1 `max` mass / sizeMax
    in
    drawPoint position (size + 1)


-- | Draw a point using a filled circle.
--
drawPoint :: Position -> R -> Picture
drawPoint (V3 x y _) size
  = Translate (realToFrac x) (realToFrac y)
  $ ThickCircle (size / 2) size

