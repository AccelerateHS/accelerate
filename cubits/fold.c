/* -----------------------------------------------------------------------------
 *
 * Module    : Fold
 * Copyright : (c) [2009..2010] Trevor L. McDonell
 * License   : BSD
 *
 * ---------------------------------------------------------------------------*/

#include "utils.h"

#ifndef blockSize
#define blockSize       blockDim.x
#endif

#ifndef lengthIsPow2
#define lengthIsPow2    0
#endif

typedef unsigned int T;


__device__ static T
identity()
{
    return 0;
}

__device__ static T
apply(const T x, const T y)
{
    return x + y;
}


/*
 * Compute multiple elements per thread sequentially. This reduces the overall
 * cost of the algorithm while keeping the work complexity O(n) and the step
 * complexity O(log n). c.f. Brent's Theorem optimisation.
 *
 * Stolen from the CUDA SDK examples
 */
__global__ void
fold
(
    T                   *d_out,
    const T             *d_in0,
    const unsigned int  length
)
{
    extern __shared__ T scratch[];

    /*
     * Calculate first level of reduction reading into shared memory
     */
    unsigned int       i;
    const unsigned int tid      = threadIdx.x;
    const unsigned int gridSize = blockSize * 2 * gridDim.x;

    scratch[tid] = identity();

    /*
     * Reduce multiple elements per thread. The number is determined by the
     * number of active thread blocks (via gridDim). More blocks will result in
     * a larger `gridSize', and hence fewer elements per thread
     *
     * The loop stride of `gridSize' is used to maintain coalescing.
     */
    for (i =  blockIdx.x * blockSize * 2 + tid; i <  length; i += gridSize)
    {
        scratch[tid] = apply(scratch[tid], d_in0[i]);

        /*
         * Ensure we don't read out of bounds. This is optimised away if the
         * input length is a power of two
         */
        if (lengthIsPow2 || i + blockSize < length)
            scratch[tid] = apply(scratch[tid], d_in0[i+blockSize]);
    }
    __syncthreads();

    /*
     * Now, calculate the reduction in shared memory
     */
    if (blockSize >= 512) { if (tid < 256) { scratch[tid] = apply(scratch[tid], scratch[tid+256]); } __syncthreads(); }
    if (blockSize >= 256) { if (tid < 128) { scratch[tid] = apply(scratch[tid], scratch[tid+128]); } __syncthreads(); }
    if (blockSize >= 128) { if (tid <  64) { scratch[tid] = apply(scratch[tid], scratch[tid+ 64]); } __syncthreads(); }

#ifndef __DEVICE_EMULATION__
    if (tid < 32)
#endif
    {
        if (blockSize >= 64) { scratch[tid] = apply(scratch[tid], scratch[tid+32]);  __EMUSYNC; }
        if (blockSize >= 32) { scratch[tid] = apply(scratch[tid], scratch[tid+16]);  __EMUSYNC; }
        if (blockSize >= 16) { scratch[tid] = apply(scratch[tid], scratch[tid+ 8]);  __EMUSYNC; }
        if (blockSize >=  8) { scratch[tid] = apply(scratch[tid], scratch[tid+ 4]);  __EMUSYNC; }
        if (blockSize >=  4) { scratch[tid] = apply(scratch[tid], scratch[tid+ 2]);  __EMUSYNC; }
        if (blockSize >=  2) { scratch[tid] = apply(scratch[tid], scratch[tid+ 1]);  __EMUSYNC; }
    }

    /*
     * Write the results of this block back to global memory
     */
    if (tid == 0)
        d_out[blockIdx.x] = scratch[0];
}

// vim:filetype=cuda.c

