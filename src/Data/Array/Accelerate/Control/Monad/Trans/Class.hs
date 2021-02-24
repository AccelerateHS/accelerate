module Data.Array.Accelerate.Control.Monad.Trans.Class where

import Prelude ()
import Data.Array.Accelerate.Control.Monad.Trans.Monad
import Data.Array.Accelerate.Control.Monad.Trans.Functor

-- | The class of monad transformers.  Instances should satisfy the
-- following laws, which state that 'lift' is a monad transformation:
--
-- * @'lift' . 'return' = 'return'@
--
-- * @'lift' (m >>= f) = 'lift' m >>= ('lift' . f)@

class MonadTrans t where
  -- | Lift a computation from the argument monad to the constructed monad.
  lift :: (Monad' m a, Functor' m a, Functor' (t m) a) => m a -> t m a
