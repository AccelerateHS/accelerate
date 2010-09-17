/* -----------------------------------------------------------------------------
 *
 * Module    : Shape
 * Copyright : (c) [2009..2010] Trevor L. McDonell
 * License   : BSD
 *
 * ---------------------------------------------------------------------------*/

#ifndef __ACCELERATE_CUDA_SHAPE_H__
#define __ACCELERATE_CUDA_SHAPE_H__

#include <stdint.h>
#include <cuda_runtime.h>

typedef int32_t                                   Ix;
typedef Ix                                        DIM0;
typedef Ix                                        DIM1;
typedef struct { Ix a1,a0; }                      DIM2;
typedef struct { Ix a2,a1,a0; }                   DIM3;
typedef struct { Ix a3,a2,a1,a0; }                DIM4;
typedef struct { Ix a4,a3,a2,a1,a0; }             DIM5;

#ifdef __cplusplus

/*
 * Number of dimensions of a shape
 */
static __inline__ __device__ int dim(DIM1 sh) { return 1; }
static __inline__ __device__ int dim(DIM2 sh) { return 2; }
static __inline__ __device__ int dim(DIM3 sh) { return 3; }
static __inline__ __device__ int dim(DIM4 sh) { return 4; }
static __inline__ __device__ int dim(DIM5 sh) { return 5; }

/*
 * Yield the total number of elements in a shape
 */
static __inline__ __device__ int size(DIM1 sh) { return sh; }
static __inline__ __device__ int size(DIM2 sh) { return sh.a0 * sh.a1; }
static __inline__ __device__ int size(DIM3 sh) { return sh.a0 * sh.a1 * sh.a2; }
static __inline__ __device__ int size(DIM4 sh) { return sh.a0 * sh.a1 * sh.a2 * sh.a3; }
static __inline__ __device__ int size(DIM5 sh) { return sh.a0 * sh.a1 * sh.a2 * sh.a3 * sh.a4; }

/*
 * Convert the individual dimensions of a linear array into a shape
 */
static __inline__ __device__ DIM1 shape(Ix a)
{
    return a;
}

static __inline__ __device__ DIM2 shape(Ix b, Ix a)
{
    DIM2 sh = { b, a };
    return sh;
}

static __inline__ __device__ DIM3 shape(Ix c, Ix b, Ix a)
{
    DIM3 sh = { c, b, a };
    return sh;
}

static __inline__ __device__ DIM4 shape(Ix d, Ix c, Ix b, Ix a)
{
    DIM4 sh = { d, c, b, a };
    return sh;
}

static __inline__ __device__ DIM5 shape(Ix e, Ix d, Ix c, Ix b, Ix a)
{
    DIM5 sh = { e, d, c, b, a };
    return sh;
}

/*
 * Yield the index position in a linear, row-major representation of the array.
 * First argument is the shape of the array, the second the index
 */
static __inline__ __device__ Ix toIndex(DIM1 sh, DIM1 ix)
{
    return ix;
}

static __inline__ __device__ Ix toIndex(DIM2 sh, DIM2 ix)
{
    DIM1 sh_ = sh.a1;
    DIM1 ix_ = ix.a1;
    return toIndex(sh_, ix_) * sh.a0 + ix.a0;
}

static __inline__ __device__ Ix toIndex(DIM3 sh, DIM3 ix)
{
    DIM2 sh_ = { sh.a2, sh.a1 };
    DIM2 ix_ = { ix.a2, ix.a1 };
    return toIndex(sh_, ix_) * sh.a0 + ix.a0;
}

static __inline__ __device__ Ix toIndex(DIM4 sh, DIM4 ix)
{
    DIM3 sh_ = { sh.a3, sh.a2, sh.a1 };
    DIM3 ix_ = { ix.a3, ix.a2, ix.a1 };
    return toIndex(sh_, ix_) * sh.a0 + ix.a0;
}

static __inline__ __device__ Ix toIndex(DIM5 sh, DIM5 ix)
{
    DIM4 sh_ = { sh.a4, sh.a3, sh.a2, sh.a1 };
    DIM4 ix_ = { ix.a4, ix.a3, ix.a2, ix.a1 };
    return toIndex(sh_, ix_) * sh.a0 + ix.a0;
}

/*
 * Inverse of 'toIndex'
 */
static __inline__ __device__ DIM1 fromIndex(DIM1 sh, Ix ix)
{
    return ix;
}

static __inline__ __device__ DIM2 fromIndex(DIM2 sh, Ix ix)
{
    DIM1 sh_ = shape(sh.a1);
    DIM1 ix_ = fromIndex(sh_, ix / sh.a0);
    return shape(ix_, ix % sh.a0);
}

static __inline__ __device__ DIM3 fromIndex(DIM3 sh, Ix ix)
{
    DIM2 sh_ = shape(sh.a2, sh.a1);
    DIM2 ix_ = fromIndex(sh_, ix / sh.a0);
    return shape(ix_.a1, ix_.a0, ix % sh.a0);
}

static __inline__ __device__ DIM4 fromIndex(DIM4 sh, Ix ix)
{
    DIM3 sh_ = shape(sh.a3, sh.a2, sh.a1);
    DIM3 ix_ = fromIndex(sh_, ix / sh.a0);
    return shape(ix_.a2, ix_.a1, ix_.a0, ix % sh.a0);
}

static __inline__ __device__ DIM5 fromIndex(DIM5 sh, Ix ix)
{
    DIM4 sh_ = shape(sh.a4, sh.a3, sh.a2, sh.a1);
    DIM4 ix_ = fromIndex(sh_, ix / sh.a0);
    return shape(ix_.a3, ix_.a2, ix_.a1, ix_.a0, ix % sh.a0);
}


/*
 * Test for the magic index `ignore`
 */
static __inline__ __device__ int ignore(DIM1 ix)
{
    return ix == -1;
}

static __inline__ __device__ int ignore(DIM2 ix)
{
    return ix.a0 == -1 && ix.a1 == -1;
}

static __inline__ __device__ int ignore(DIM3 ix)
{
    return ix.a0 == -1 && ix.a1 == -1 && ix.a2 == -1;
}

static __inline__ __device__ int ignore(DIM4 ix)
{
    return ix.a0 == -1 && ix.a1 == -1 && ix.a2 == -1 && ix.a3 == -1;
}

static __inline__ __device__ int ignore(DIM5 ix)
{
    return ix.a0 == -1 && ix.a1 == -1 && ix.a2 == -1 && ix.a3 == -1 && ix.a4 == -1;
}

#else

int dim(Ix sh);
int size(Ix sh);
int shape(Ix sh);
int toIndex(Ix sh, Ix ix);
int fromIndex(Ix sh, Ix ix);

#endif  // __cplusplus
#endif  // __ACCELERATE_CUDA_SHAPE_H__

