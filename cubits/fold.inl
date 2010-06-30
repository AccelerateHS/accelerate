/* -----------------------------------------------------------------------------
 *
 * Module    : Fold
 * Copyright : (c) [2009..2010] Trevor L. McDonell
 * License   : BSD
 *
 * Reduce an array to a single value with a binary associative function
 *
 * ---------------------------------------------------------------------------*/

#ifndef BLOCK_SIZE
#define BLOCK_SIZE              blockDim.x
#endif

#ifndef LENGTH_IS_POW_2
#define LENGTH_IS_POW_2         0
#endif


/*
 * Compute multiple elements per thread sequentially. This reduces the overall
 * cost of the algorithm while keeping the work complexity O(n) and the step
 * complexity O(log n). c.f. Brent's Theorem optimisation.
 */
extern "C"
__global__ void
fold
(
    ArrOut              d_out,
    const ArrIn0        d_in0,
    const Ix            shape
)
{
    extern __shared__ TyOut scratch[];

    /*
     * Calculate first level of reduction reading into shared memory
     */
    Ix       i;
    const Ix tid      = threadIdx.x;
    const Ix gridSize = BLOCK_SIZE * 2 * gridDim.x;

    scratch[tid] = identity();

    /*
     * Reduce multiple elements per thread. The number is determined by the
     * number of active thread blocks (via gridDim). More blocks will result in
     * a larger `gridSize', and hence fewer elements per thread
     *
     * The loop stride of `gridSize' is used to maintain coalescing.
     */
    for (i =  blockIdx.x * BLOCK_SIZE * 2 + tid; i <  shape; i += gridSize)
    {
        scratch[tid] = apply(scratch[tid], get0(d_in0, i), shape);

        /*
         * Ensure we don't read out of bounds. This is optimised away if the
         * input length is a power of two
         */
        if (LENGTH_IS_POW_2 || i + BLOCK_SIZE < shape)
            scratch[tid] = apply(scratch[tid], get0(d_in0, i+BLOCK_SIZE), shape);
    }
    __syncthreads();

    /*
     * Now, calculate the reduction in shared memory
     */
    if (BLOCK_SIZE >= 512) { if (tid < 256) { scratch[tid] = apply(scratch[tid], scratch[tid+256], shape); } __syncthreads(); }
    if (BLOCK_SIZE >= 256) { if (tid < 128) { scratch[tid] = apply(scratch[tid], scratch[tid+128], shape); } __syncthreads(); }
    if (BLOCK_SIZE >= 128) { if (tid <  64) { scratch[tid] = apply(scratch[tid], scratch[tid+ 64], shape); } __syncthreads(); }

#ifndef __DEVICE_EMULATION__
    if (tid < 32)
#endif
    {
        if (BLOCK_SIZE >= 64) { scratch[tid] = apply(scratch[tid], scratch[tid+32], shape);  __EMUSYNC; }
        if (BLOCK_SIZE >= 32) { scratch[tid] = apply(scratch[tid], scratch[tid+16], shape);  __EMUSYNC; }
        if (BLOCK_SIZE >= 16) { scratch[tid] = apply(scratch[tid], scratch[tid+ 8], shape);  __EMUSYNC; }
        if (BLOCK_SIZE >=  8) { scratch[tid] = apply(scratch[tid], scratch[tid+ 4], shape);  __EMUSYNC; }
        if (BLOCK_SIZE >=  4) { scratch[tid] = apply(scratch[tid], scratch[tid+ 2], shape);  __EMUSYNC; }
        if (BLOCK_SIZE >=  2) { scratch[tid] = apply(scratch[tid], scratch[tid+ 1], shape);  __EMUSYNC; }
    }

    /*
     * Write the results of this block back to global memory
     */
    if (tid == 0)
        set(d_out, blockIdx.x, scratch[0]);
}

