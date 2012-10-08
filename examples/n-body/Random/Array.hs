{-# LANGUAGE RankNTypes #-}

module Random.Array
  where

import Control.Monad.ST
import System.Random.MWC

import Data.Array.Accelerate                    as A
import Data.Array.Accelerate.Array.Data         as A
import Data.Array.Accelerate.Array.Sugar        as Sugar


-- | Generate an array of random values using the supplied generator function.
--   The generator for variates is initialised with a fixed seed.
--
randomArrayOf
    :: (Shape sh, Elt e)
    => (forall s. sh -> GenST s -> ST s e)
    -> sh
    -> Array sh e
randomArrayOf f sh
  = let
        n               = Sugar.size sh
        (adata, _)      = runArrayData $ do
                            gen <- create
                            arr <- newArrayData n
                            let write ix = unsafeWriteArrayData arr (toIndex sh ix)
                                         . fromElt =<< f ix gen

                            iter sh write (>>) (return ())
                            return (arr, undefined)

    in adata `seq` Array (fromElt sh) adata


-- | Generate an array of random values using a supplied generator function and
--   seed value.
--
randomArrayOfWithSeed
    :: (Shape sh, Elt e)
    => (forall s. sh -> GenST s -> ST s e)
    -> Seed
    -> sh
    -> Array sh e
randomArrayOfWithSeed f seed sh
  = let
        n               = Sugar.size sh
        (adata, _)      = runArrayData $ do
                            gen <- restore seed
                            arr <- newArrayData n
                            let write ix = unsafeWriteArrayData arr (toIndex sh ix)
                                         . fromElt =<< f ix gen

                            iter sh write (>>) (return ())
                            return (arr, undefined)

    in adata `seq` Array (fromElt sh) adata

