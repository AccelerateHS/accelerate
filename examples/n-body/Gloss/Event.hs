
module Gloss.Event ( react )
  where

import Gloss.Simulate

import Data.Char
import Data.Label
import Graphics.Gloss.Interface.Pure.Game


-- React to user input events
--
react :: Event -> Simulate -> Simulate
react event universe
  = let
        char ' ' Down                   = modify simulatePause not
        char 't' Down                   = modify simulateDrawTree not
        char _   _                      = id

        special _ _                     = id

    in case event of
         EventKey (Char c) s _ _        -> char (toLower c) s universe
         EventKey (SpecialKey c) s _ _  -> special c s universe
         _                              -> universe

