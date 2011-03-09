-- |
-- Module      : Util
-- Copyright   : [2011] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--

module Util where


-- Like 'Data.Maybe.maybe', but with arguments shuffled
--
maybe' :: Maybe a -> b -> (a -> b) -> b
maybe' Nothing  n _ = n
maybe' (Just x) _ f = f x

-- Fold over a boolean value, analogous to 'maybe' and 'either'
--
bool :: a -> a -> Bool -> a
bool x _ False = x
bool _ y True  = y

-- A singleton list
--
unit :: a -> [a]
unit = return

