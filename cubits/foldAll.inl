/* -----------------------------------------------------------------------------
 *
 * Kernel      : FoldAll
 * Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
 * License     : BSD3
 *
 * Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
 * Stability   : experimental
 *
 * Reduce a *vector* to a single value with a binary associative function
 *
 * ---------------------------------------------------------------------------*/


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
    extern volatile __shared__ void* s_ptr[];
    ArrOut s_data = partition(s_ptr, blockDim.x);

    /*
     * Calculate first level of reduction reading into shared memory
     */
    const Ix tid      = threadIdx.x;
    const Ix gridSize = blockDim.x * gridDim.x;
          Ix    i     = blockIdx.x * blockDim.x + tid;
          TyOut sum;

    /*
     * Reduce multiple elements per thread. The number is determined by the
     * number of active thread blocks (via gridDim). More blocks will result in
     * a larger `gridSize', and hence fewer elements per thread
     *
     * The loop stride of `gridSize' is used to maintain coalescing.
     */
    if (i < shape)
    {
        sum = get0(d_in0, i);
        for (i += gridSize; i < shape; i += gridSize)
            sum = apply(sum, get0(d_in0, i));
    }

    /*
     * Each thread puts its local sum into shared memory, then threads
     * cooperatively reduce the shared array to a single value.
     */
    set(s_data, tid, sum);
    __syncthreads();

    /*
     * Compute reduction across the block
     */
    i = min(shape - gridSize, blockDim.x);

    if (i > 512) { if (tid < 512 && tid + 512 < i) { sum = apply(sum, get0(s_data, tid+512)); set(s_data, tid, sum); } __syncthreads(); }
    if (i > 256) { if (tid < 256 && tid + 256 < i) { sum = apply(sum, get0(s_data, tid+256)); set(s_data, tid, sum); } __syncthreads(); }
    if (i > 128) { if (tid < 128 && tid + 128 < i) { sum = apply(sum, get0(s_data, tid+128)); set(s_data, tid, sum); } __syncthreads(); }
    if (i >  64) { if (tid <  64 && tid +  64 < i) { sum = apply(sum, get0(s_data, tid+ 64)); set(s_data, tid, sum); } __syncthreads(); }

    if (tid < 32)
    {
        if (i > 32) { if (tid + 32 < i) { sum = apply(sum, get0(s_data, tid+32)); set(s_data, tid, sum); }}
        if (i > 16) { if (tid + 16 < i) { sum = apply(sum, get0(s_data, tid+16)); set(s_data, tid, sum); }}
        if (i >  8) { if (tid +  8 < i) { sum = apply(sum, get0(s_data, tid+ 8)); set(s_data, tid, sum); }}
        if (i >  4) { if (tid +  4 < i) { sum = apply(sum, get0(s_data, tid+ 4)); set(s_data, tid, sum); }}
        if (i >  2) { if (tid +  2 < i) { sum = apply(sum, get0(s_data, tid+ 2)); set(s_data, tid, sum); }}
        if (i >  1) { if (tid +  1 < i) { sum = apply(sum, get0(s_data, tid+ 1)); }}
    }

    /*
     * Write the results of this block back to global memory
     */
    if (tid == 0)
    {
#ifdef INCLUSIVE
        set(d_out, blockIdx.x, sum);
#else
        set(d_out, blockIdx.x, gridDim.x == 1 ? apply(sum, identity()) : sum);
#endif
    }
}

