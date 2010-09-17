/* -----------------------------------------------------------------------------
 *
 * Module    : Function
 * Copyright : (c) [2009..2010] Trevor L. McDonell
 * License   : BSD
 *
 * ---------------------------------------------------------------------------*/

#ifndef __ACCELERATE_CUDA_FUNCTION_H__
#define __ACCELERATE_CUDA_FUNCTION_H__

#include <stdint.h>
#include <cuda_runtime.h>

#ifdef __cplusplus

/* -----------------------------------------------------------------------------
 * Device functions required to support generated code
 * -------------------------------------------------------------------------- */

/*
 * Left/Right bitwise rotation
 */
template <typename T>
static __inline__ __device__ T rotateL(const T x, const int32_t i)
{
    const int32_t i8 = i & 8 * sizeof(x) - 1;
    return i8 == 0 ? x : x << i8 | x >> 8 * sizeof(x) - i8;
}

template <typename T>
static __inline__ __device__ T rotateR(const T x, const int32_t i)
{
    const int32_t i8 = i & 8 * sizeof(x) - 1;
    return i8 == 0 ? x : x >> i8 | x << 8 * sizeof(x) - i8;
}

/*
 * Integer division, truncated towards negative infinity
 */
template <typename T>
static __inline__ __device__ T idiv(const T x, const T y)
{
    return x > 0 && y < 0 ? (x - y - 1) / y : (x < 0 && y > 0 ? (x - y + 1) / y : x / y);
}

/*
 * Integer modulus, Haskell style
 */
template <typename T>
static __inline__ __device__ T mod(const T x, const T y)
{
    const T r = x % y;
    return x > 0 && y < 0 || x < 0 && y > 0 ? (r != 0 ? r + y : 0) : r;
}


/* -----------------------------------------------------------------------------
 * Additional helper functions
 * -------------------------------------------------------------------------- */

/*
 * Determine if the input is a power of two
 */
template <typename T>
static __inline__ __host__ __device__ T isPow2(const T x)
{
    return ((x&(x-1)) == 0);
}

/*
 * Compute the next highest power of two
 */
template <typename T>
static __inline__ __host__ T ceilPow2(const T x)
{
#if 0
    --x;
    x |= x >> 1;
    x |= x >> 2;
    x |= x >> 4;
    x |= x >> 8;
    x |= x >> 16;
    return ++x;
#endif

    return (isPow2(x)) ? x : 1u << (int) ceil(log2((double)x));
}

/*
 * Compute the next lowest power of two
 */
template <typename T>
static __inline__ __host__ T floorPow2(const T x)
{
#if 0
    float nf = (float) n;
    return 1 << (((*(int*)&nf) >> 23) - 127);
#endif

    int exp;
    frexp(x, &exp);
    return 1 << (exp - 1);
}

/*
 * computes next highest multiple of f from x
 */
template <typename T>
static __inline__ __host__ T multiple(const T x, const T f)
{
    return ((x + (f-1)) / f);
}

/*
 * MS Excel-style CEIL() function. Rounds x up to nearest multiple of f
 */
template <typename T>
static __inline__ __host__ T ceiling(const T x, const T f)
{
    return multiple(x, f) * f;
}

#endif  // __cplusplus
#endif  // __ACCELERATE_CUDA_FUNCTION_H__

