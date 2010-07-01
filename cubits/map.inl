/* -----------------------------------------------------------------------------
 *
 * Module    : Map
 * Copyright : (c) 2010 Trevor L. McDonell
 * License   : BSD
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

