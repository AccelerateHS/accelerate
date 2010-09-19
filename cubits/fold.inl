/* -----------------------------------------------------------------------------
 *
 * Module    : Fold
 * Copyright : (c) [2009..2010] Trevor L. McDonell
 * License   : BSD
 *
 * Reduce an array to a single value with a binary associative function
 *
 * ---------------------------------------------------------------------------*/

/*
 * We require block sizes are a power of two. This value gives maximum occupancy
 * for both 1.x and 2.x class devices.
 */
#ifndef BLOCK_SIZE
#define BLOCK_SIZE              256
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
    extern volatile __shared__ void* s_ptr[];
    ArrOut s_data = partition(s_ptr, blockDim.x);

    /*
     * Calculate first level of reduction reading into shared memory
     */
    Ix       i;
    TyOut    sum      = identity();
    const Ix tid      = threadIdx.x;
    const Ix gridSize = BLOCK_SIZE * 2 * gridDim.x;

    /*
     * Reduce multiple elements per thread. The number is determined by the
     * number of active thread blocks (via gridDim). More blocks will result in
     * a larger `gridSize', and hence fewer elements per thread
     *
     * The loop stride of `gridSize' is used to maintain coalescing.
     */
    for (i =  blockIdx.x * BLOCK_SIZE * 2 + tid; i <  shape; i += gridSize)
    {
        sum = apply(sum, get0(d_in0, i));

        /*
         * Ensure we don't read out of bounds. This is optimised away if the
         * input length is a power of two
         */
        if (LENGTH_IS_POW_2 || i + BLOCK_SIZE < shape)
            sum = apply(sum, get0(d_in0, i+BLOCK_SIZE));
    }

    /*
     * Each thread puts its local sum into shared memory, then threads
     * cooperatively reduce the shared array to a single value.
     */
    set(s_data, tid, sum);
    __syncthreads();

    if (BLOCK_SIZE >= 512) { if (tid < 256) { sum = apply(sum, get0(s_data, tid+256)); set(s_data, tid, sum); } __syncthreads(); }
    if (BLOCK_SIZE >= 256) { if (tid < 128) { sum = apply(sum, get0(s_data, tid+128)); set(s_data, tid, sum); } __syncthreads(); }
    if (BLOCK_SIZE >= 128) { if (tid <  64) { sum = apply(sum, get0(s_data, tid+ 64)); set(s_data, tid, sum); } __syncthreads(); }

    if (tid < 32)
    {
        /*
         * Use an extra warps worth of elements of shared memory, to let threads
         * index beyond the input data without using any branch instructions.
         */
        sum = apply(sum, get0(s_data, tid+32)); set(s_data, tid, sum);
        sum = apply(sum, get0(s_data, tid+16)); set(s_data, tid, sum);
        sum = apply(sum, get0(s_data, tid+ 8)); set(s_data, tid, sum);
        sum = apply(sum, get0(s_data, tid+ 4)); set(s_data, tid, sum);
        sum = apply(sum, get0(s_data, tid+ 2)); set(s_data, tid, sum);
        sum = apply(sum, get0(s_data, tid+ 1));
    }

    /*
     * Write the results of this block back to global memory
     */
    if (tid == 0)
        set(d_out, blockIdx.x, sum);
}

