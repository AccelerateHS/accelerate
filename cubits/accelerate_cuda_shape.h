/* -----------------------------------------------------------------------------
 *
 * Module      : Shape
 * Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
 * License     : BSD3
 *
 * Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
 * Stability   : experimental
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
typedef struct { Ix a5,a4,a3,a2,a1,a0; }          DIM6;
typedef struct { Ix a6,a5,a4,a3,a2,a1,a0; }       DIM7;
typedef struct { Ix a7,a6,a5,a4,a3,a2,a1,a0; }    DIM8;
typedef struct { Ix a8,a7,a6,a5,a4,a3,a2,a1,a0; } DIM9;

#ifdef __cplusplus

/*
 * Number of dimensions of a shape
 */
static __inline__ __device__ int dim(const DIM1 sh) { return 1; }
static __inline__ __device__ int dim(const DIM2 sh) { return 2; }
static __inline__ __device__ int dim(const DIM3 sh) { return 3; }
static __inline__ __device__ int dim(const DIM4 sh) { return 4; }
static __inline__ __device__ int dim(const DIM5 sh) { return 5; }
static __inline__ __device__ int dim(const DIM6 sh) { return 6; }
static __inline__ __device__ int dim(const DIM7 sh) { return 7; }
static __inline__ __device__ int dim(const DIM8 sh) { return 8; }
static __inline__ __device__ int dim(const DIM9 sh) { return 9; }

/*
 * Yield the total number of elements in a shape
 */
static __inline__ __device__ int size(const DIM1 sh) { return sh; }
static __inline__ __device__ int size(const DIM2 sh) { return sh.a0 * sh.a1; }
static __inline__ __device__ int size(const DIM3 sh) { return sh.a0 * sh.a1 * sh.a2; }
static __inline__ __device__ int size(const DIM4 sh) { return sh.a0 * sh.a1 * sh.a2 * sh.a3; }
static __inline__ __device__ int size(const DIM5 sh) { return sh.a0 * sh.a1 * sh.a2 * sh.a3 * sh.a4; }
static __inline__ __device__ int size(const DIM6 sh) { return sh.a0 * sh.a1 * sh.a2 * sh.a3 * sh.a4 * sh.a5; }
static __inline__ __device__ int size(const DIM7 sh) { return sh.a0 * sh.a1 * sh.a2 * sh.a3 * sh.a4 * sh.a5 * sh.a6; }
static __inline__ __device__ int size(const DIM8 sh) { return sh.a0 * sh.a1 * sh.a2 * sh.a3 * sh.a4 * sh.a5 * sh.a6 * sh.a7; }
static __inline__ __device__ int size(const DIM9 sh) { return sh.a0 * sh.a1 * sh.a2 * sh.a3 * sh.a4 * sh.a5 * sh.a6 * sh.a7 * sh.a8; }

/*
 * Convert the individual dimensions of a linear array into a shape
 */
static __inline__ __device__ DIM1 shape(const Ix a)
{
    return a;
}

static __inline__ __device__ DIM2 shape(const Ix b, const Ix a)
{
    DIM2 sh = { b, a };
    return sh;
}

static __inline__ __device__ DIM3 shape(const Ix c, const Ix b, const Ix a)
{
    DIM3 sh = { c, b, a };
    return sh;
}

static __inline__ __device__ DIM4 shape(const Ix d, const Ix c, const Ix b, const Ix a)
{
    DIM4 sh = { d, c, b, a };
    return sh;
}

static __inline__ __device__ DIM5 shape(const Ix e, const Ix d, const Ix c, const Ix b, const Ix a)
{
    DIM5 sh = { e, d, c, b, a };
    return sh;
}

static __inline__ __device__ DIM6 shape(const Ix f, const Ix e, const Ix d, const Ix c, const Ix b, const Ix a)
{
    DIM6 sh = { f, e, d, c, b, a };
    return sh;
}

static __inline__ __device__ DIM7 shape(const Ix g, const Ix f, const Ix e, const Ix d, const Ix c, const Ix b, const Ix a)
{
    DIM7 sh = { g, f, e, d, c, b, a };
    return sh;
}

static __inline__ __device__ DIM8 shape(const Ix h, const Ix g, const Ix f, const Ix e, const Ix d, const Ix c, const Ix b, const Ix a)
{
    DIM8 sh = { h, g, f, e, d, c, b, a };
    return sh;
}

static __inline__ __device__ DIM9 shape(const Ix i, const Ix h, const Ix g, const Ix f, const Ix e, const Ix d, const Ix c, const Ix b, const Ix a)
{
    DIM9 sh = { i, h, g, f, e, d, c, b, a };
    return sh;
}


/*
 * Yield the index position in a linear, row-major representation of the array.
 * First argument is the shape of the array, the second the index
 */
static __inline__ __device__ Ix toIndex(const DIM1 sh, const DIM1 ix)
{
    return ix;
}

static __inline__ __device__ Ix toIndex(const DIM2 sh, const DIM2 ix)
{
    DIM1 sh_ = sh.a0;
    DIM1 ix_ = ix.a0;
    return toIndex(sh_, ix_) * sh.a1 + ix.a1;
}

static __inline__ __device__ Ix toIndex(const DIM3 sh, const DIM3 ix)
{
    DIM2 sh_ = { sh.a1, sh.a0 };
    DIM2 ix_ = { ix.a1, ix.a0 };
    return toIndex(sh_, ix_) * sh.a2 + ix.a2;
}

static __inline__ __device__ Ix toIndex(const DIM4 sh, const DIM4 ix)
{
    DIM3 sh_ = { sh.a2, sh.a1, sh.a0 };
    DIM3 ix_ = { ix.a2, ix.a1, ix.a0 };
    return toIndex(sh_, ix_) * sh.a3 + ix.a3;
}

static __inline__ __device__ Ix toIndex(const DIM5 sh, const DIM5 ix)
{
    DIM4 sh_ = { sh.a3, sh.a2, sh.a1, sh.a0 };
    DIM4 ix_ = { ix.a3, ix.a2, ix.a1, ix.a0 };
    return toIndex(sh_, ix_) * sh.a4 + ix.a4;
}

static __inline__ __device__ Ix toIndex(const DIM6 sh, const DIM6 ix)
{
    DIM5 sh_ = { sh.a4, sh.a3, sh.a2, sh.a1, sh.a0 };
    DIM5 ix_ = { ix.a4, ix.a3, ix.a2, ix.a1, ix.a0 };
    return toIndex(sh_, ix_) * sh.a5 + ix.a5;
}

static __inline__ __device__ Ix toIndex(const DIM7 sh, const DIM7 ix)
{
    DIM6 sh_ = { sh.a5, sh.a4, sh.a3, sh.a2, sh.a1, sh.a0 };
    DIM6 ix_ = { ix.a5, ix.a4, ix.a3, ix.a2, ix.a1, ix.a0 };
    return toIndex(sh_, ix_) * sh.a6 + ix.a6;
}

static __inline__ __device__ Ix toIndex(const DIM8 sh, const DIM8 ix)
{
    DIM7 sh_ = { sh.a6, sh.a5, sh.a4, sh.a3, sh.a2, sh.a1, sh.a0 };
    DIM7 ix_ = { ix.a6, ix.a5, ix.a4, ix.a3, ix.a2, ix.a1, ix.a0 };
    return toIndex(sh_, ix_) * sh.a7 + ix.a7;
}

static __inline__ __device__ Ix toIndex(const DIM9 sh, const DIM9 ix)
{
    DIM8 sh_ = { sh.a7, sh.a6, sh.a5, sh.a4, sh.a3, sh.a2, sh.a1, sh.a0 };
    DIM8 ix_ = { ix.a7, ix.a6, ix.a5, ix.a4, ix.a3, ix.a2, ix.a1, ix.a0 };
    return toIndex(sh_, ix_) * sh.a8 + ix.a8;
}


/*
 * Inverse of 'toIndex'
 */
static __inline__ __device__ DIM1 fromIndex(const DIM1 sh, const Ix ix)
{
    return ix;
}

static __inline__ __device__ DIM2 fromIndex(const DIM2 sh, const Ix ix)
{
    DIM1 sh_ = shape(sh.a0);
    DIM1 ix_ = fromIndex(sh_, ix / sh.a1);
    return shape(ix % sh.a1, ix_);
}

static __inline__ __device__ DIM3 fromIndex(const DIM3 sh, const Ix ix)
{
    DIM2 sh_ = shape(sh.a1, sh.a0);
    DIM2 ix_ = fromIndex(sh_, ix / sh.a2);
    return shape(ix % sh.a2, ix_.a1, ix_.a0);
}

static __inline__ __device__ DIM4 fromIndex(const DIM4 sh, const Ix ix)
{
    DIM3 sh_ = shape(sh.a2, sh.a1, sh.a0);
    DIM3 ix_ = fromIndex(sh_, ix / sh.a3);
    return shape(ix % sh.a3, ix_.a2, ix_.a1, ix_.a0);
}

static __inline__ __device__ DIM5 fromIndex(const DIM5 sh, const Ix ix)
{
    DIM4 sh_ = shape(sh.a3, sh.a2, sh.a1, sh.a0);
    DIM4 ix_ = fromIndex(sh_, ix / sh.a4);
    return shape(ix % sh.a4, ix_.a3, ix_.a2, ix_.a1, ix_.a0);
}

static __inline__ __device__ DIM6 fromIndex(const DIM6 sh, const Ix ix)
{
    DIM5 sh_ = shape(sh.a4, sh.a3, sh.a2, sh.a1, sh.a0);
    DIM5 ix_ = fromIndex(sh_, ix / sh.a5);
    return shape(ix % sh.a5, ix_.a4, ix_.a3, ix_.a2, ix_.a1, ix_.a0);
}

static __inline__ __device__ DIM7 fromIndex(const DIM7 sh, const Ix ix)
{
    DIM6 sh_ = shape(sh.a5, sh.a4, sh.a3, sh.a2, sh.a1, sh.a0);
    DIM6 ix_ = fromIndex(sh_, ix / sh.a6);
    return shape(ix % sh.a6, ix_.a5, ix_.a4, ix_.a3, ix_.a2, ix_.a1, ix_.a0);
}

static __inline__ __device__ DIM8 fromIndex(const DIM8 sh, const Ix ix)
{
    DIM7 sh_ = shape(sh.a6, sh.a5, sh.a4, sh.a3, sh.a2, sh.a1, sh.a0);
    DIM7 ix_ = fromIndex(sh_, ix / sh.a7);
    return shape(ix % sh.a7, ix_.a6, ix_.a5, ix_.a4, ix_.a3, ix_.a2, ix_.a1, ix_.a0);
}

static __inline__ __device__ DIM9 fromIndex(const DIM9 sh, const Ix ix)
{
    DIM8 sh_ = shape(sh.a7, sh.a6, sh.a5, sh.a4, sh.a3, sh.a2, sh.a1, sh.a0);
    DIM8 ix_ = fromIndex(sh_, ix / sh.a8);
    return shape(ix % sh.a8, ix_.a7, ix_.a6, ix_.a5, ix_.a4, ix_.a3, ix_.a2, ix_.a1, ix_.a0);
}


/*
 * Test for the magic index `ignore`
 */
static __inline__ __device__ int ignore(const DIM1 ix)
{
    return ix == -1;
}

static __inline__ __device__ int ignore(const DIM2 ix)
{
    return ix.a0 == -1 && ix.a1 == -1;
}

static __inline__ __device__ int ignore(const DIM3 ix)
{
    return ix.a0 == -1 && ix.a1 == -1 && ix.a2 == -1;
}

static __inline__ __device__ int ignore(const DIM4 ix)
{
    return ix.a0 == -1 && ix.a1 == -1 && ix.a2 == -1 && ix.a3 == -1;
}

static __inline__ __device__ int ignore(const DIM5 ix)
{
    return ix.a0 == -1 && ix.a1 == -1 && ix.a2 == -1 && ix.a3 == -1 && ix.a4 == -1;
}

static __inline__ __device__ int ignore(const DIM6 ix)
{
    return ix.a0 == -1 && ix.a1 == -1 && ix.a2 == -1 && ix.a3 == -1 && ix.a4 == -1 && ix.a5 == -1;
}

static __inline__ __device__ int ignore(const DIM7 ix)
{
    return ix.a0 == -1 && ix.a1 == -1 && ix.a2 == -1 && ix.a3 == -1 && ix.a4 == -1 && ix.a5 == -1 && ix.a6 == -1;
}

static __inline__ __device__ int ignore(const DIM8 ix)
{
    return ix.a0 == -1 && ix.a1 == -1 && ix.a2 == -1 && ix.a3 == -1 && ix.a4 == -1 && ix.a5 == -1 && ix.a6 == -1 && ix.a7 == -1;
}

static __inline__ __device__ int ignore(const DIM9 ix)
{
    return ix.a0 == -1 && ix.a1 == -1 && ix.a2 == -1 && ix.a3 == -1 && ix.a4 == -1 && ix.a5 == -1 && ix.a6 == -1 && ix.a7 == -1 && ix.a8 == -1;
}


/*
 * Destructing a shape index
 */
static __inline__ __device__ DIM1 indexTail(const DIM2 ix)
{
    return ix.a1;
}

static __inline__ __device__ DIM2 indexTail(const DIM3 ix)
{
    return shape(ix.a2, ix.a1);
}

static __inline__ __device__ DIM3 indexTail(const DIM4 ix)
{
    return shape(ix.a3, ix.a2, ix.a1);
}

static __inline__ __device__ DIM4 indexTail(const DIM5 ix)
{
    return shape(ix.a4, ix.a3, ix.a2, ix.a1);
}

static __inline__ __device__ DIM5 indexTail(const DIM6 ix)
{
    return shape(ix.a5, ix.a4, ix.a3, ix.a2, ix.a1);
}

static __inline__ __device__ DIM6 indexTail(const DIM7 ix)
{
    return shape(ix.a6, ix.a5, ix.a4, ix.a3, ix.a2, ix.a1);
}

static __inline__ __device__ DIM7 indexTail(const DIM8 ix)
{
    return shape(ix.a7, ix.a6, ix.a5, ix.a4, ix.a3, ix.a2, ix.a1);
}

static __inline__ __device__ DIM8 indexTail(const DIM9 ix)
{
    return shape(ix.a8, ix.a7, ix.a6, ix.a5, ix.a4, ix.a3, ix.a2, ix.a1);
}


#else

int dim(const Ix sh);
int size(const Ix sh);
int shape(const Ix sh);
int toIndex(const Ix sh, const Ix ix);
int fromIndex(const Ix sh, const Ix ix);

#endif  // __cplusplus
#endif  // __ACCELERATE_CUDA_SHAPE_H__

