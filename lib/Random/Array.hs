{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

module Random.Array (

  (:~>),
  uniform, uniformR,
  randomArray, randomArrayWithSeed, randomArrayWithSystemRandom,

) where

import Control.Monad.ST
import System.Random.MWC                        hiding ( uniform, uniformR )
import qualified System.Random.MWC              as R

import Data.Array.Accelerate                    as A
import Data.Array.Accelerate.Array.Data         as A
import Data.Array.Accelerate.Array.Sugar        as Sugar


-- | A PNRG from indices to variates
--
type sh :~> e = forall s. sh -> GenST s -> ST s e


-- | Uniformly distributed random variates.
--
uniform :: (Shape sh, Elt e, Variate e) => sh :~> e
uniform _ = R.uniform

-- | Uniformly distributed random variates in a given range.
--
uniformR :: (Shape sh, Elt e, Variate e) => (e, e) -> sh :~> e
uniformR bounds _ = R.uniformR bounds


-- | Generate an array of random values using the supplied generator function.
--   The generator for variates is initialised with a fixed seed.
--
randomArray :: (Shape sh, Elt e) => sh :~> e -> sh -> Array sh e
randomArray f sh
  = let
        (adata, _)      = runArrayData $ do
                            gen <- create
                            arr <- runRandomArray f sh gen
                            return (arr, undefined)
    in
    adata `seq` Array (fromElt sh) adata


-- | Generate an array of random values using a supplied generator function and
--   seed value.
--
randomArrayWithSeed :: (Shape sh, Elt e) => Seed -> sh :~> e -> sh -> Array sh e
randomArrayWithSeed seed f sh
  = let
        (adata, _)      = runArrayData $ do
                            gen <- restore seed
                            arr <- runRandomArray f sh gen
                            return (arr, undefined)
    in
    adata `seq` Array (fromElt sh) adata


-- | Generate an array of random values using a supplied generator function,
--   initialised with the system's source of pseudo-random numbers.
--
--   TODO: find a way to do this directly, without going via save/restore.
--
randomArrayWithSystemRandom :: forall sh e. (Shape sh, Elt e) => sh :~> e -> sh -> IO (Array sh e)
randomArrayWithSystemRandom f sh
  = do
       seed   <- withSystemRandom (asGenIO save)
       return $! randomArrayWithSeed seed f sh


-- Common function to create a mutable array and fill it with random values
--
runRandomArray :: (Shape sh, Elt e) => sh :~> e -> sh -> GenST s -> ST s (MutableArrayData s (EltRepr e))
runRandomArray f sh gen
  = let
        n = Sugar.size sh
    in do
      arr <- newArrayData n
      let write ix = unsafeWriteArrayData arr (Sugar.toIndex sh ix)
                   . fromElt =<< f ix gen

      iter sh write (>>) (return ())
      return arr

