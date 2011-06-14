/* -----------------------------------------------------------------------------
 *
 * Kernel      : FoldAll
 * Copyright   : [2008..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
 * License     : BSD3
 *
 * Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
 * Stability   : experimental
 *
 * Reduce a *vector* to a single value with a binary associative function
 *
 * ---------------------------------------------------------------------------*/

#include "reduce.inl"

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
    const int   tid      = threadIdx.x;
    const int   gridSize = blockDim.x * gridDim.x;
          int   i        = blockIdx.x * blockDim.x + tid;
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

    sum = reduce_block_n(s_data, sum, min((int) shape, blockDim.x));

    /*
     * Write the results of this block back to global memory. If we are the last
     * phase of a recursive multi-block reduction, include the seed element.
     */
    if (tid == 0)
    {
#ifdef INCLUSIVE
        set(d_out, blockIdx.x, sum);
#else
        if (shape > 0)
            set(d_out, blockIdx.x, gridDim.x == 1 ? apply(sum, identity()) : sum);
        else
            set(d_out, blockIdx.x, identity());
#endif
    }
}

