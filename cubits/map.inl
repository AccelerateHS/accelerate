/* -----------------------------------------------------------------------------
 *
 * Kernel      : Map
 * Copyright   : [2008..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
 * License     : BSD3
 *
 * Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
 * Stability   : experimental
 *
 * Apply the function to each element of the array. Each thread processes
 * multiple elements, striding the array by the grid size.
 *
 * ---------------------------------------------------------------------------*/

extern "C"
__global__ void
map
(
    ArrOut              d_out,
    const ArrIn0        d_in0,
    const Ix            shape
)
{
    Ix       idx;
    const Ix gridSize = __umul24(blockDim.x, gridDim.x);

    for (idx = __umul24(blockDim.x, blockIdx.x) + threadIdx.x; idx < shape; idx += gridSize)
    {
        set(d_out, idx, apply(get0(d_in0, idx)));
    }
}

