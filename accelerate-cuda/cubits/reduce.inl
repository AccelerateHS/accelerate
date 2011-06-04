/* -----------------------------------------------------------------------------
 *
 * Kernel      : Reduce
 * Copyright   : [2008..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
 * License     : BSD3
 *
 * Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
 * Stability   : experimental
 *
 * ---------------------------------------------------------------------------*/

#ifndef __REDUCE__
#define __REDUCE__


/*
 * Cooperatively reduce a single warp's segment of an array to a single value
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
 * Block reduction to a single value
 */
static __inline__ __device__ TyOut
reduce_block_n
(
    ArrOut      s_data,
    TyOut       sum,
    Ix          n
)
{
    const Ix tid = threadIdx.x;

    if (n > 512) { if (tid < 512 && tid + 512 < n) { sum = apply(sum, get0(s_data, tid+512)); set(s_data, tid, sum); } __syncthreads(); }
    if (n > 256) { if (tid < 256 && tid + 256 < n) { sum = apply(sum, get0(s_data, tid+256)); set(s_data, tid, sum); } __syncthreads(); }
    if (n > 128) { if (tid < 128 && tid + 128 < n) { sum = apply(sum, get0(s_data, tid+128)); set(s_data, tid, sum); } __syncthreads(); }
    if (n >  64) { if (tid <  64 && tid +  64 < n) { sum = apply(sum, get0(s_data, tid+ 64)); set(s_data, tid, sum); } __syncthreads(); }

    if (tid < 32)
    {
        if (n > 32) { if (tid + 32 < n) { sum = apply(sum, get0(s_data, tid+32)); set(s_data, tid, sum); }}
        if (n > 16) { if (tid + 16 < n) { sum = apply(sum, get0(s_data, tid+16)); set(s_data, tid, sum); }}
        if (n >  8) { if (tid +  8 < n) { sum = apply(sum, get0(s_data, tid+ 8)); set(s_data, tid, sum); }}
        if (n >  4) { if (tid +  4 < n) { sum = apply(sum, get0(s_data, tid+ 4)); set(s_data, tid, sum); }}
        if (n >  2) { if (tid +  2 < n) { sum = apply(sum, get0(s_data, tid+ 2)); set(s_data, tid, sum); }}
        if (n >  1) { if (tid +  1 < n) { sum = apply(sum, get0(s_data, tid+ 1)); }}
    }

    return sum;
}

#endif

