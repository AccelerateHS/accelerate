/* -----------------------------------------------------------------------------
 *
 * Kernel      : Replicate
 * Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
 * License     : BSD3
 *
 * Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
 * Stability   : experimental
 *
 * ---------------------------------------------------------------------------*/

extern "C"
__global__ void
replicate
(
    ArrOut              d_out,
    const ArrIn0        d_in0,
    const Slice         slice,
    const SliceDim      sliceDim
)
{
    const Ix shapeSize = size(sliceDim);
    const Ix gridSize  = __umul24(blockDim.x, gridDim.x);

    for (Ix ix = __umul24(blockDim.x, blockIdx.x) + threadIdx.x; ix < shapeSize; ix += gridSize)
    {
        SliceDim dst = fromIndex(sliceDim, ix);
        Slice    src = sliceIndex(dst);

        set(d_out, ix, get0(d_in0, toIndex(slice, src)));
    }
}

