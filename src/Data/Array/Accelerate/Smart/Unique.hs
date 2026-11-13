{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Smart.Unique
-- Copyright   : [2026] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Smart.Unique (

  withUnique,

) where

import Data.Unique ( newUnique, Unique )
import System.IO.Unsafe ( unsafePerformIO )


-- | Build a value with a fresh unique identity.
--
-- Note that the "identity" will only be as unique as the term in which it
-- appears, which will depend on GHC's transformation rules. A few examples
-- are listed below; note that in all cases, preventing these transformations
-- needs to happen /at use sites/, and can be very difficult in general.
--
-- == Common subexpression elimination
--
-- This might transform
--
-- > .... withUnique f .... withUnique f ....
--
-- into
--
-- > let shared = withUnique f in .... shared .... shared ....
--
-- To prevent that, you'd have to disable CSE in all modules, transitively,
-- that use 'withUnique'.
--
-- == Inlining
--
-- It might also go the other way, and apply inlining, turning
--
-- > let shared = withUnique f in .... shared .... shared ....
--
-- into
--
-- > .... withUnique f .... withUnique f ....
--
-- unless @shared@ is marked @NOINLINE@.
--
-- == Full laziness
--
-- This would turn
--
-- > \x y -> .... withUnique (f x) ....
--
-- into
--
-- > \x -> let shared = withUnique (f x) in \y -> .... shared ....
--
-- unless you disable full laziness in all modules, transitively.
--
-- == State hack
--
-- This goes the other way, turning
--
-- > \x -> let shared = withUnique (f x) in \(y :: State# s) -> .... shared ....
--
-- into
--
-- > \x (y :: State# s) -> .... withUnique (f x) ....
--
-- unless you disable the state hack in all modules, transitively. This would
-- matter when this is used in IO or ST code, for example the sharing in
--
-- > let shared = .... withUnique ....
-- > replicateM n $
-- >    .... shared ....
--
-- might be lost.
withUnique :: (Unique -> a) -> a
withUnique f = unsafePerformIO $ f <$> newUnique
