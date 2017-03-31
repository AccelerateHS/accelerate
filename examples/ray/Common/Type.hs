
module Common.Type (

  V3(..),
  module Common.Type,

) where

import Data.Array.Accelerate.Linear.V3

type Position     = V3 Float
type Direction    = V3 Float
type Noraml       = (Position, Direction)

