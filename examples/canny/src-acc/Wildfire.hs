{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TypeOperators #-}
-- This module defines the final phase of the Canny algorithm, a recursive
-- algorithm to "connect" pixels of the output image.
--
-- This function originally written by Ben Lippmeier for Repa.
--
-- NOTE: for best performance this needs to be compiled with the following GHC options:
--       -fllvm -optlo-O3 -Odph -fno-liberate-case
--       -funfolding-use-threshold100 -funfolding-keeness-factor100
--

module Wildfire where

import Canny

import Data.Word
import Data.Array.Accelerate.IO                         ( A )
import Data.Array.Repa.Repr.Unboxed                     ( U )
import qualified Data.Array.Repa                        as R
import qualified Data.Vector.Unboxed                    as V
import qualified Data.Vector.Unboxed.Mutable            as VM


-- Repa component --------------------------------------------------------------

-- | Trace out strong edges in the final image.
--   Also trace out weak edges that are connected to strong edges.
--
wildfire
    :: R.Array A R.DIM2 Float           -- ^ Image with strong and weak edges set.
    -> R.Array A R.DIM1 Int             -- ^ Array containing flat indices of strong edges.
    -> IO (R.Array U R.DIM2 Word8)
{-# NOINLINE wildfire #-}
wildfire img arrStrong
 = do   (sh, vec)       <- wildfireIO
        return  $ sh `seq` vec `seq` R.fromUnboxed sh vec

 where  lenImg          = R.size $ R.extent img
        lenStrong       = R.size $ R.extent arrStrong
        shImg           = R.extent img

        wildfireIO
         = do   -- Stack of image indices we still need to consider.
                vStrong  <- R.toUnboxed `fmap` R.computeUnboxedP (R.delay arrStrong)
                vStrong' <- V.thaw vStrong
                vStack   <- VM.grow vStrong' (lenImg - lenStrong)

                -- Burn in new edges.
                vImg    <- VM.unsafeNew lenImg
                VM.set vImg 0
                burn vImg vStack lenStrong
                vImg'   <- V.unsafeFreeze vImg
                return  (R.extent img, vImg')


        burn :: VM.IOVector Word8 -> VM.IOVector Int -> Int -> IO ()
        burn !vImg !vStack !top
         | top == 0
         = return ()

         | otherwise
         = do   let !top'               =  top - 1
                n                       <- VM.unsafeRead vStack top'
                let (R.Z R.:. y R.:. x) = R.fromIndex (R.extent img) n

                let {-# INLINE push #-}
                    push ix t =
                      if R.inShape shImg ix
                         then pushWeak vImg vStack ix t
                         else return t

                VM.write vImg n 255
                 >>  push (R.Z R.:. y - 1 R.:. x - 1) top'
                 >>= push (R.Z R.:. y - 1 R.:. x    )
                 >>= push (R.Z R.:. y - 1 R.:. x + 1)

                 >>= push (R.Z R.:. y     R.:. x - 1)
                 >>= push (R.Z R.:. y     R.:. x + 1)

                 >>= push (R.Z R.:. y + 1 R.:. x - 1)
                 >>= push (R.Z R.:. y + 1 R.:. x    )
                 >>= push (R.Z R.:. y + 1 R.:. x + 1)

                 >>= burn vImg vStack

        -- If this ix is weak in the source then set it to strong in the
        -- result and push the ix onto the stack.
        {-# INLINE pushWeak #-}
        pushWeak vImg vStack ix top
         = do   let n           = R.toIndex (R.extent img) ix
                xDst            <- VM.unsafeRead vImg n
                let xSrc        = img `R.unsafeIndex` ix

                if   xDst == 0
                  && xSrc == edge Weak
                 then do
                        VM.unsafeWrite vStack top (R.toIndex (R.extent img) ix)
                        return (top + 1)

                 else   return top

