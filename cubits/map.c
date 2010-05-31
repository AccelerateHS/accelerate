/* -----------------------------------------------------------------------------
 *
 * Module    : Map
 * Copyright : (c) 2010 Trevor L. McDonell
 * License   : BSD
 *
 * ---------------------------------------------------------------------------*/

typedef unsigned int TyOut;
typedef unsigned int TyIn0;

__device__ static TyOut
apply(const TyIn0 in0)
{
    return in0;
}


/*
 * Apply the function to each element of the array. Each thread processes
 * multiple elements, striding the array by the grid size.
 */
__global__ void
map
(
    TyOut               *d_out,
    const TyIn0         *d_in0,
    const unsigned int  length
)
{
    unsigned int       idx;
    const unsigned int gridSize = __umul24(blockDim.x, gridDim.x);

    for (idx = __umul24(blockDim.x, blockIdx.x) + threadIdx.x; idx < length; idx += gridSize)
    {
        d_out[idx] = apply(d_in0[idx]);
    }
}


// vim:filetype=cuda.c

