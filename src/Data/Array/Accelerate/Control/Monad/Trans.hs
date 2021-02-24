-- | The idea is that you can import this module only,
-- and get both the transformers and all of accelerate
-- (but without the conflicting classes).
module Data.Array.Accelerate.Control.Monad.Trans 
  ( module Accelerate

  , module Class
  , module Functor
  , module Monad
  
  , module Except
  , module Maybe
  , module State
  ) where

import Data.Array.Accelerate as Accelerate hiding (Monad, Functor, lift, (>>=), return, fmap, (<$>)) 

import Data.Array.Accelerate.Control.Monad.Trans.Class    as Class
import Data.Array.Accelerate.Control.Monad.Trans.Functor  as Functor
import Data.Array.Accelerate.Control.Monad.Trans.Monad    as Monad

import Data.Array.Accelerate.Control.Monad.Trans.Except   as Except
import Data.Array.Accelerate.Control.Monad.Trans.Maybe    as Maybe
import Data.Array.Accelerate.Control.Monad.Trans.State    as State
