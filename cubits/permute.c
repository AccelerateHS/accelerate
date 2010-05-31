/* -----------------------------------------------------------------------------
 *
 * Module    : Permute
 * Copyright : (c) [2009..2010] Trevor L. McDonell
 * License   : BSD
 *
 * ---------------------------------------------------------------------------*/

#ifndef backward
#define backward        0
#endif

typedef unsigned int TyOut;
typedef unsigned int TyIn0;


/*
 * Permute an array according to the permutation indices. This handles both
 * forward (scatter) and backward (gather) permutation, where:
 *
 *   bpermute :: [a] -> [Int] -> [a]
 *   bpermute v is = [ v!i | i <- is ]
 *
 * In this case, `length' specifies the number of elements in the `indices' and
 * `out' arrays.
 */
__global__ void
permute
(
    TyOut               *d_out,
    const TyIn0         *d_in0,
    const unsigned int  *d_indices,
    const unsigned int  length
)
{
    unsigned int       idx;
    const unsigned int gridSize = __umul24(blockDim.x, gridDim.x);

    for (idx = __umul24(blockDim.x, blockIdx.x) + threadIdx.x; idx < length; idx += gridSize)
    {
        if (backward) d_out[idx]            = d_in0[d_indices[idx]];
        else          d_out[d_indices[idx]] = d_in0[idx];
    }
}

// vim:filetype=cuda.c

