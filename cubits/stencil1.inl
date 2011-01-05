/* -----------------------------------------------------------------------------
 *
 * Kernel      : Stencil1
 * Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
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

#include <stencil.inl>


/*
 * The kernel.
 */
extern "C"
__global__ void
stencil1
(
    ArrOut              d_out,
    const ArrDimIn0     d_in0_shape
)
{
    Ix       idx;
    const Ix gridSize = __umul24(blockDim.x, gridDim.x);

    for (idx = __umul24(blockDim.x, blockIdx.x) + threadIdx.x; idx < size(d_in0_shape); idx += gridSize)
    {
        set(d_out, idx, gather_and_apply(d_in0_shape, fromIndex(d_in0_shape, idx)));
    }
}

