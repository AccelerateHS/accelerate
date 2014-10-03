
module Data.Array.Accelerate.Examples.Internal.Util (

  -- * Miscellaneous
  showFFloatSIBase,

) where

import Numeric
import Data.List


-- | Show a floating point number in scientific notation with a specific base.
--
showFFloatSIBase :: RealFloat a => Maybe Int -> a -> a -> ShowS
showFFloatSIBase p b n
  = showString
  . nubBy (\x y -> x == ' ' && y == ' ')
  $ showFFloat p n' [ ' ', si_unit ]
  where
    n'          = n / (b ^^ (pow-4))
    pow         = max 0 . min 8 . (+) 4 . floor $ logBase b n
    si_unit     = "pnµm kMGT" !! pow

