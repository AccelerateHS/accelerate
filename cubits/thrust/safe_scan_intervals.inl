/*
 *  Copyright 2008-2010 NVIDIA Corporation
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

/* -----------------------------------------------------------------------------
 * A robust scan for general types
 * ---------------------------------------------------------------------------*/

template <typename T>
static __inline__ __device__ T
scan_block(T* array, T val)
{
    array[threadIdx.x] = val;

    __syncthreads();

    // copy to temporary so val and tmp have the same memory space
    if (blockDim.x >   1) { if(threadIdx.x >=   1) { T tmp = array[threadIdx.x -   1]; val = apply(tmp, val); } __syncthreads(); array[threadIdx.x] = val; __syncthreads(); }
    if (blockDim.x >   2) { if(threadIdx.x >=   2) { T tmp = array[threadIdx.x -   2]; val = apply(tmp, val); } __syncthreads(); array[threadIdx.x] = val; __syncthreads(); }
    if (blockDim.x >   4) { if(threadIdx.x >=   4) { T tmp = array[threadIdx.x -   4]; val = apply(tmp, val); } __syncthreads(); array[threadIdx.x] = val; __syncthreads(); }
    if (blockDim.x >   8) { if(threadIdx.x >=   8) { T tmp = array[threadIdx.x -   8]; val = apply(tmp, val); } __syncthreads(); array[threadIdx.x] = val; __syncthreads(); }
    if (blockDim.x >  16) { if(threadIdx.x >=  16) { T tmp = array[threadIdx.x -  16]; val = apply(tmp, val); } __syncthreads(); array[threadIdx.x] = val; __syncthreads(); }
    if (blockDim.x >  32) { if(threadIdx.x >=  32) { T tmp = array[threadIdx.x -  32]; val = apply(tmp, val); } __syncthreads(); array[threadIdx.x] = val; __syncthreads(); }
    if (blockDim.x >  64) { if(threadIdx.x >=  64) { T tmp = array[threadIdx.x -  64]; val = apply(tmp, val); } __syncthreads(); array[threadIdx.x] = val; __syncthreads(); }
    if (blockDim.x > 128) { if(threadIdx.x >= 128) { T tmp = array[threadIdx.x - 128]; val = apply(tmp, val); } __syncthreads(); array[threadIdx.x] = val; __syncthreads(); }
    if (blockDim.x > 256) { if(threadIdx.x >= 256) { T tmp = array[threadIdx.x - 256]; val = apply(tmp, val); } __syncthreads(); array[threadIdx.x] = val; __syncthreads(); }
    if (blockDim.x > 512) { if(threadIdx.x >= 512) { T tmp = array[threadIdx.x - 512]; val = apply(tmp, val); } __syncthreads(); array[threadIdx.x] = val; __syncthreads(); }

    return val;
}

#if 0
template <bool inclusive, typename T>
__device__
T scan_block_n(T* array, const unsigned int n, T val)
{
    array[threadIdx.x] = val;

    __syncthreads();

    if (blockDim.x >   1) { if(threadIdx.x < n && threadIdx.x >=   1) { T tmp = array[threadIdx.x -   1]; val = apply(tmp, val); } __syncthreads(); array[threadIdx.x] = val; __syncthreads(); }
    if (blockDim.x >   2) { if(threadIdx.x < n && threadIdx.x >=   2) { T tmp = array[threadIdx.x -   2]; val = apply(tmp, val); } __syncthreads(); array[threadIdx.x] = val; __syncthreads(); }
    if (blockDim.x >   4) { if(threadIdx.x < n && threadIdx.x >=   4) { T tmp = array[threadIdx.x -   4]; val = apply(tmp, val); } __syncthreads(); array[threadIdx.x] = val; __syncthreads(); }
    if (blockDim.x >   8) { if(threadIdx.x < n && threadIdx.x >=   8) { T tmp = array[threadIdx.x -   8]; val = apply(tmp, val); } __syncthreads(); array[threadIdx.x] = val; __syncthreads(); }
    if (blockDim.x >  16) { if(threadIdx.x < n && threadIdx.x >=  16) { T tmp = array[threadIdx.x -  16]; val = apply(tmp, val); } __syncthreads(); array[threadIdx.x] = val; __syncthreads(); }
    if (blockDim.x >  32) { if(threadIdx.x < n && threadIdx.x >=  32) { T tmp = array[threadIdx.x -  32]; val = apply(tmp, val); } __syncthreads(); array[threadIdx.x] = val; __syncthreads(); }
    if (blockDim.x >  64) { if(threadIdx.x < n && threadIdx.x >=  64) { T tmp = array[threadIdx.x -  64]; val = apply(tmp, val); } __syncthreads(); array[threadIdx.x] = val; __syncthreads(); }
    if (blockDim.x > 128) { if(threadIdx.x < n && threadIdx.x >= 128) { T tmp = array[threadIdx.x - 128]; val = apply(tmp, val); } __syncthreads(); array[threadIdx.x] = val; __syncthreads(); }
    if (blockDim.x > 256) { if(threadIdx.x < n && threadIdx.x >= 256) { T tmp = array[threadIdx.x - 256]; val = apply(tmp, val); } __syncthreads(); array[threadIdx.x] = val; __syncthreads(); }
    if (blockDim.x > 512) { if(threadIdx.x < n && threadIdx.x >= 512) { T tmp = array[threadIdx.x - 512]; val = apply(tmp, val); } __syncthreads(); array[threadIdx.x] = val; __syncthreads(); }

    if (inclusive) return val;
    else           return threadIdx.x > 0 ? array[threadIdx.x - 1] : identity();
}
#endif


static __inline__ __device__ void
scan_intervals
(
    TyOut               *sdata,
    ArrOut              d_out,
    const ArrIn0        d_in0,
    ArrIn0              d_block_results,
    const Ix            N,
    const Ix            interval_size
)
{
    const Ix interval_begin = interval_size * blockIdx.x;
    const Ix interval_end   = min(interval_begin + interval_size, N);

    TyOut val;
    Ix output = REVERSE ? interval_end - threadIdx.x - 1 : interval_begin + threadIdx.x;

    // process intervals
    for(Ix base = interval_begin + threadIdx.x; base < interval_end; base += blockDim.x)
    {
        // read data
        val = get0(d_in0, output);

        // carry in
        if (threadIdx.x == 0 && base != interval_begin)
        {
            TyOut tmp = sdata[blockDim.x - 1];
            val       = apply(tmp, val);
        }
        __syncthreads();

        // scan block
        val = scan_block(sdata, val);

        // write data
        set(d_out, output, val);

        // update output iterator
#if REVERSE
        output -= blockDim.x;
#else
        output += blockDim.x;
#endif
    }
    __syncthreads();

    // write block sum
    if (threadIdx.x == 0)
    {
#if REVERSE
      TyOut tmp = get0(d_out, interval_begin);
      set(d_block_results, blockIdx.x, tmp);
#else
      TyOut tmp = get0(d_out, interval_end - 1);
      set(d_block_results, blockIdx.x, tmp);
#endif
    }
}

