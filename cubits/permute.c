/* -----------------------------------------------------------------------------
 *
 * Module    : Permute
 * Copyright : (c) [2009..2010] Trevor L. McDonell
 * License   : BSD
 *
 * ---------------------------------------------------------------------------*/


const Ix ignore = (Ix) -1;

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

/*
 * Index projection source -> destination
 */
static __inline__ __device__ Ix
project(const Ix idx, const Ix shape)
{
}

/*
 * Combination function
 */
static __inline__ __device__ static TyOut
apply(const TyIn1 x1, const TyIn0 x0, const Ix shape)
{
}

/*
 * Forward permutation, characterised by a function that determines for each
 * element in the source array where it should go in the target. The output
 * array should be initialised with a default value, as the permutation may be
 * between arrays of different sizes and some positions may never be touched.
 *
 * Elements from the source array are dropped for which the permutation function
 * yields the magic index `ignore`.
 */
__global__ void
permute
(
    ArrOut              d_out,
    const ArrIn0        d_in0,
    const Ix            shape
)
{
    Ix       dst;
    Ix       idx;
    const Ix gridSize = __umul24(blockDim.x, gridDim.x);

    for (idx = __umul24(blockDim.x, blockIdx.x) + threadIdx.x; idx < shape; idx += gridSize)
    {
        dst = project(idx, shape);

        if (dst != ignore)
        {
            set(d_out, dst, apply(get0(d_in0, idx), get0(d_out, dst), shape));
        }
    }
}

// vim:filetype=cuda.c

