/* -----------------------------------------------------------------------------
 *
 * Kernel      : FoldSegAll
 * Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
 * License     : BSD3
 *
 * Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
 * Stability   : experimental
 *
 * Reduction a vector to a single value for each segment, using a binary
 * associative function
 *
 * ---------------------------------------------------------------------------*/


/*
 * Cooperatively reduce a single warp's section of an array to a single value
 */
static __inline__ __device__ TyOut
reduce_warp_n
(
    ArrOut      s_data,
    TyOut       sum,
    Ix          n
)
{
    const Ix tid  = threadIdx.x;
    const Ix lane = threadIdx.x & (warpSize - 1);

    if (n > 16 && lane + 16 < n) { sum = apply(sum, get0(s_data, tid+16)); set(s_data, tid, sum); }
    if (n >  8 && lane +  8 < n) { sum = apply(sum, get0(s_data, tid+ 8)); set(s_data, tid, sum); }
    if (n >  4 && lane +  4 < n) { sum = apply(sum, get0(s_data, tid+ 4)); set(s_data, tid, sum); }
    if (n >  2 && lane +  2 < n) { sum = apply(sum, get0(s_data, tid+ 2)); set(s_data, tid, sum); }
    if (n >  1 && lane +  1 < n) { sum = apply(sum, get0(s_data, tid+ 1)); }

    return sum;
}


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
    const Int*          d_offset,
    const Ix            num_segments
)
{
    const Ix vectors_per_block = blockDim.x / warpSize;
    const Ix num_vectors       = vectors_per_block * gridDim.x;
    const Ix thread_id         = blockDim.x * blockIdx.x + threadIdx.x;
    const Ix vector_id         = thread_id / warpSize;
    const Ix thread_lane       = threadIdx.x & (warpSize - 1);
    const Ix vector_lane       = threadIdx.x / warpSize;

    /*
     * Manually partition (dynamically-allocated) shared memory
     */
    extern volatile __shared__ Ix s_ptrs[][2];
    ArrOut s_data = partition((void*) &s_ptrs[vectors_per_block][2], blockDim.x);

    for (Ix seg = vector_id; seg < num_segments; seg += num_vectors)
    {
        /*
         * Use two threads to fetch the indices of the start and end of this
         * segment. This results in single coalesced global read, instead of two
         * separate transactions.
         */
        if (thread_lane < 2)
            s_ptrs[vector_lane][thread_lane] = d_offset[seg + thread_lane];

        const Ix    start        = s_ptrs[vector_lane][0];
        const Ix    end          = s_ptrs[vector_lane][1];
        const Ix    num_elements = end - start;
              TyOut sum;

        /*
         * Each thread reads in values of this segment, accumulating a local sum
         */
        if (num_elements > warpSize)
        {
            /*
             * Ensure aligned access to global memory
             */
            Ix i = start - (start & (warpSize - 1)) + thread_lane;
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
        sum = reduce_warp_n(s_data, sum, min(num_elements, warpSize));

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

