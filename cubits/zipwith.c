/* -----------------------------------------------------------------------------
 *
 * Module    : ZipWith
 * Copyright : (c) 2010 Trevor L. McDonell
 * License   : BSD
 *
 * ---------------------------------------------------------------------------*/

typedef unsigned int TyOut;
typedef unsigned int TyIn0;
typedef unsigned int TyIn1;

__device__ static TyOut
apply(const TyIn0 in0, const TyIn1 in1)
{
    return in0 + in1;
}


/*
 * Combine two arrays using the given binary operator. Each thread processes
 * multiple elements, striding the array by the grid size.
 */
__global__ void
zipWith
(
    TyOut               *d_out,
    const TyIn0         *d_in0,
    const TyIn1         *d_in1,
    const unsigned int  length
)
{
    unsigned int       idx;
    const unsigned int gridSize = __umul24(blockDim.x, gridDim.x);

    for (idx = __umul24(blockDim.x, blockIdx.x) + threadIdx.x; idx < length; idx += gridSize)
    {
        d_out[idx] = apply(d_in0[idx], d_in1[idx]);
    }
}


// vim:filetype=cuda.c

