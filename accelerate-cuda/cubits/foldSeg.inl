/* -----------------------------------------------------------------------------
 *
 * Kernel      : FoldSeg
 * Copyright   : [2008..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
 * License     : BSD3
 *
 * Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
 * Stability   : experimental
 *
 * Reduce segments along the innermost dimension of a multidimensional array to
 * a single value for each segment, using a binary associative function
 *
 * ---------------------------------------------------------------------------*/

#include "reduce.inl"

/*
 * Each segment of the vector is assigned to a warp, which computes the
 * reduction of the i-th section, in parallel.
 *
 * This division of work implies that the data arrays are accessed in a
 * contiguous manner (if not necessarily aligned). For devices of compute
 * capability 1.2 and later, these accesses will be coalesced. A single
 * transaction will be issued if all of the addresses for a half-warp happen to
 * fall within a single 128-byte boundary. Extra transactions will be made to
 * cover any spill. The same applies for 2.x devices, except that all widths are
 * doubled since transactions occur on a per-warp basis.
 *
 * Since an entire 32-thread warp is assigned for each segment, many threads
 * will remain idle when the segments are very small. This code relies on
 * implicit synchronisation among threads in a warp.
 *
 * The offset array contains the starting index for each segment in the input
 * array. The i-th warp reduces values in the input array at indices
 * [d_offset[i], d_offset[i+1]).
 */
extern "C"
__global__ void
foldSeg
(
    ArrOut              d_out,
    const ArrIn0        d_in0,
    const ArrSeg        d_offset,
    const DimOut        shOut,
    const DimIn0        shIn0
)
{
    const int vectors_per_block = blockDim.x / warpSize;
    const int num_vectors       = vectors_per_block * gridDim.x;
    const int thread_id         = blockDim.x * blockIdx.x + threadIdx.x;
    const int vector_id         = thread_id / warpSize;
    const int thread_lane       = threadIdx.x & (warpSize - 1);
    const int vector_lane       = threadIdx.x / warpSize;

    const int num_segments      = indexHead(shOut);
    const int total_segments    = size(shOut);

    /*
     * Manually partition (dynamically-allocated) shared memory
     */
    extern volatile __shared__ Ix s_ptrs[][2];
    ArrOut s_data = partition((void*) &s_ptrs[vectors_per_block][2], blockDim.x);

    for (int seg = vector_id; seg < total_segments; seg += num_vectors)
    {
        const int s    =  seg % num_segments;
        const int base = (seg / num_segments) * indexHead(shIn0);

        /*
         * Use two threads to fetch the indices of the start and end of this
         * segment. This results in single coalesced global read, instead of two
         * separate transactions.
         */
        if (thread_lane < 2)
            s_ptrs[vector_lane][thread_lane] = d_offset[s + thread_lane];

        const int   start        = base + s_ptrs[vector_lane][0];
        const int   end          = base + s_ptrs[vector_lane][1];
        const int   num_elements = end - start;
              TyOut sum;

        /*
         * Each thread reads in values of this segment, accumulating a local sum
         */
        if (num_elements > warpSize)
        {
            /*
             * Ensure aligned access to global memory
             */
            int i = start - (start & (warpSize - 1)) + thread_lane;
            if (i >= start)
                sum = get0(d_in0, i);

            /*
             * Subsequent reads to global memory are aligned, but make sure all
             * threads have initialised their local sum.
             */
            if (i + warpSize < end)
            {
                TyOut tmp = get0(d_in0, i + warpSize);

                if (i >= start) sum = apply(sum, tmp);
                else            sum = tmp;
            }

            for (i += 2 * warpSize; i < end; i += warpSize)
                sum = apply(sum, get0(d_in0, i));
        }
        else if (start + thread_lane < end)
        {
            sum = get0(d_in0, start + thread_lane);
        }

        /*
         * Store local sums into shared memory and reduce to a single value
         */
        set(s_data, threadIdx.x, sum);
        sum = reduce_warp_n(s_data, sum, min((int) num_elements, warpSize));

        /*
         * Finally, the first thread writes the result for this segment
         */
        if (thread_lane == 0)
        {
#ifndef INCLUSIVE
            sum = num_elements > 0 ? apply(sum, identity()) : identity();
#endif
            set(d_out, seg, sum);
        }
    }
}

