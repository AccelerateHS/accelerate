/* -----------------------------------------------------------------------------
 *
 * Module    : Fold
 * Copyright : (c) [2009..2010] Trevor L. McDonell
 * License   : BSD
 *
 * ---------------------------------------------------------------------------*/

#ifndef blockSize
#define blockSize       blockDim.x
#endif

#ifndef lengthIsPow2
#define lengthIsPow2    0
#endif

typedef int  TyOut;
typedef int  TyIn0;
typedef int  TyIn1;

typedef int* ArrOut;
typedef int* ArrIn0;
typedef int* ArrIn1;


static __inline__ __device__ void
set(ArrOut d_out, const Ix idx, const TyOut val)
{
}

static __inline__ __device__ TyIn0
get0(const ArrIn0 d_in0, const Ix idx)
{
}

static __inline__ __device__ TyIn1
get1(const ArrIn1 d_in1, const Ix idx)
{
}

static __inline__ __device__ static TyOut
identity()
{
}

static __inline__ __device__ TyOut
apply(const TyIn1 in1, const TyIn0 in0, const Ix shape)
{
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
    const Ix gridSize = blockSize * 2 * gridDim.x;

    scratch[tid] = identity();

    /*
     * Reduce multiple elements per thread. The number is determined by the
     * number of active thread blocks (via gridDim). More blocks will result in
     * a larger `gridSize', and hence fewer elements per thread
     *
     * The loop stride of `gridSize' is used to maintain coalescing.
     */
    for (i =  blockIdx.x * blockSize * 2 + tid; i <  shape; i += gridSize)
    {
        scratch[tid] = apply(scratch[tid], get0(d_in0, i), shape);

        /*
         * Ensure we don't read out of bounds. This is optimised away if the
         * input length is a power of two
         */
        if (lengthIsPow2 || i + blockSize < shape)
            scratch[tid] = apply(scratch[tid], get0(d_in0, i+blockSize), shape);
    }
    __syncthreads();

    /*
     * Now, calculate the reduction in shared memory
     */
    if (blockSize >= 512) { if (tid < 256) { scratch[tid] = apply(scratch[tid], scratch[tid+256], shape); } __syncthreads(); }
    if (blockSize >= 256) { if (tid < 128) { scratch[tid] = apply(scratch[tid], scratch[tid+128], shape); } __syncthreads(); }
    if (blockSize >= 128) { if (tid <  64) { scratch[tid] = apply(scratch[tid], scratch[tid+ 64], shape); } __syncthreads(); }

#ifndef __DEVICE_EMULATION__
    if (tid < 32)
#endif
    {
        if (blockSize >= 64) { scratch[tid] = apply(scratch[tid], scratch[tid+32], shape);  __EMUSYNC; }
        if (blockSize >= 32) { scratch[tid] = apply(scratch[tid], scratch[tid+16], shape);  __EMUSYNC; }
        if (blockSize >= 16) { scratch[tid] = apply(scratch[tid], scratch[tid+ 8], shape);  __EMUSYNC; }
        if (blockSize >=  8) { scratch[tid] = apply(scratch[tid], scratch[tid+ 4], shape);  __EMUSYNC; }
        if (blockSize >=  4) { scratch[tid] = apply(scratch[tid], scratch[tid+ 2], shape);  __EMUSYNC; }
        if (blockSize >=  2) { scratch[tid] = apply(scratch[tid], scratch[tid+ 1], shape);  __EMUSYNC; }
    }

    /*
     * Write the results of this block back to global memory
     */
    if (tid == 0)
        set(d_out, blockIdx.x, scratch[0]);
}

// vim:filetype=cuda.c

