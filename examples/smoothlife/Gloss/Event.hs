
module Gloss.Event
  where

import Gloss.Simulate
import Graphics.Gloss.Interface.Pure.Game


react :: Event -> Simulate -> Simulate
react _ = id

