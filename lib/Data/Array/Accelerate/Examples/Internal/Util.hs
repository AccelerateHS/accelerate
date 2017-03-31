-- |
-- Module:      : Data.Array.Accelerate.Examples.Internal.Util
-- Copyright    : [2014] Trevor L. McDonell
-- License      : BSD3
--
-- Maintainer   : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Examples.Internal.Util
  where

import Numeric
import Data.List

import Data.Array.Accelerate                              as A ( Z(..), Elt, Scalar, fromList )
import Prelude                                            as P


infixr 9 $$
($$) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
($$) g f x y = g (f x y)


scalar :: Elt a => a -> Scalar a
scalar x = fromList Z [x]


-- | Show a floating point number in scientific notation with a specific base.
--
showFFloatSIBase :: P.RealFloat a => Maybe Int -> a -> a -> ShowS
showFFloatSIBase p b n
  = showString
  . nubBy (\x y -> x == ' ' && y == ' ')
  $ showFFloat p n' [ ' ', si_unit ]
  where
    n'          = n / (b P.^^ (pow-4))
    pow         = P.max 0 . P.min 8 . (+) 4 . P.floor $ P.logBase b n
    si_unit     = "pnµm kMGT" P.!! pow

