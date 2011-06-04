/* -----------------------------------------------------------------------------
 *
 * Kernel      : Stencil
 * Copyright   : [2008..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
 * License     : BSD3
 *
 * Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
 * Stability   : experimental
 *
 * Apply the function to each element of the array that takes a neighborhood of
 * elements as its input. Each thread processes multiple elements, striding the
 * array by the grid size. To improve performance, the input array is bound as
 * a texture reference so that reads are cached.
 *
 * ---------------------------------------------------------------------------*/

extern "C"
__global__ void
stencil
(
    ArrOut        d_out,
    const DimOut  shOut
)
{
    const Ix shapeSize = size(shOut);
    const Ix gridSize  = __umul24(blockDim.x, gridDim.x);

    for (Ix ix = __umul24(blockDim.x, blockIdx.x) + threadIdx.x; ix < shapeSize; ix += gridSize)
    {
        set(d_out, ix, apply(gather0(shOut, fromIndex(shOut, ix))));
    }
}

