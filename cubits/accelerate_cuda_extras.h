/* -----------------------------------------------------------------------------
 *
 * Module    : Extras
 * Copyright : (c) [2009..2010] Trevor L. McDonell
 * License   : BSD
 *
 * ---------------------------------------------------------------------------*/

#ifndef __ACCELERATE_CUDA_EXTRAS_H__
#define __ACCELERATE_CUDA_EXTRAS_H__

#include <stdio.h>
#include <stdint.h>

#include <cuda_runtime.h>

/* -----------------------------------------------------------------------------
 * Textures
 * -----------------------------------------------------------------------------
 *
 * CUDA texture definitions and access functions are defined in terms of
 * templates, and hence only available through the C++ interface. Expose some
 * dummy wrappers to enable parsing with language-c.
 *
 * FIXME: We need extra code generation support for 64-bit types
 */
#if defined(__cplusplus) && defined(__CUDACC__)

typedef texture<uint32_t, 1> TexWord;
typedef texture<uint32_t, 2> TexWord64;
typedef texture<uint32_t, 1> TexWord32;
typedef texture<uint16_t, 1> TexWord16;
typedef texture<uint8_t,  1> TexWord8;

typedef texture<int32_t, 1> TexInt;
typedef texture<int32_t, 2> TexInt64;
typedef texture<int32_t, 1> TexInt32;
typedef texture<int16_t, 1> TexInt16;
typedef texture<int8_t,  1> TexInt8;

typedef texture<float, 1> TexFloat;
typedef texture<int2,  1> TexDouble;

#else

typedef void* TexWord;
typedef void* TexWord32;
typedef void* TexWord16;
typedef void* TexWord8;
typedef void* TexInt;
typedef void* TexInt32;
typedef void* TexInt16;
typedef void* TexInt8;
typedef void* TexFloat;
typedef void* TexDouble;

void* tex1Dfetch(const void*, const int);
void* tex1D(const void*, const float);
void* tex2D(const void*, const float, const float);
void* tex3D(const void*, const float, const float, const float);

#endif

/* -----------------------------------------------------------------------------
 * Shapes
 * -------------------------------------------------------------------------- */

typedef unsigned int             Ix;
typedef Ix                       DIM1;
typedef struct { Ix a,b; }       DIM2;
typedef struct { Ix a,b,c; }     DIM3;
typedef struct { Ix a,b,c,d; }   DIM4;
typedef struct { Ix a,b,c,d,e; } DIM5;

#ifdef __cplusplus

static __inline__ __device__ int dim(DIM1 sh) { return 1; }
static __inline__ __device__ int dim(DIM2 sh) { return 2; }
static __inline__ __device__ int dim(DIM3 sh) { return 3; }
static __inline__ __device__ int dim(DIM4 sh) { return 4; }
static __inline__ __device__ int dim(DIM5 sh) { return 5; }

static __inline__ __device__ int size(DIM1 sh) { return sh; }
static __inline__ __device__ int size(DIM2 sh) { return sh.a * sh.b; }
static __inline__ __device__ int size(DIM3 sh) { return sh.a * sh.b * sh.c; }
static __inline__ __device__ int size(DIM4 sh) { return sh.a * sh.b * sh.c * sh.d; }
static __inline__ __device__ int size(DIM5 sh) { return sh.a * sh.b * sh.c * sh.d * sh.e; }

static __inline__ __device__ int index(DIM1 sh, DIM1 ix)
{
    return ix;
}

static __inline__ __device__ int index(DIM2 sh, DIM2 ix)
{
    DIM1 sh_ = sh.a;
    DIM1 ix_ = ix.a;
    return index(sh_, ix_) + ix.b * size(sh_);
}

static __inline__ __device__ int index(DIM3 sh, DIM3 ix)
{
    DIM2 sh_ = { sh.a, sh.b };
    DIM2 ix_ = { ix.a, ix.b };
    return index(sh_, ix_) + ix.c * size(sh_);
}

static __inline__ __device__ int index(DIM4 sh, DIM4 ix)
{
    DIM3 sh_ = { sh.a, sh.b, sh.c };
    DIM3 ix_ = { ix.a, ix.b, ix.c };
    return index(sh_, ix_) + ix.d * size(sh_);
}

static __inline__ __device__ int index(DIM5 sh, DIM5 ix)
{
    DIM4 sh_ = { sh.a, sh.b, sh.c, sh.d };
    DIM4 ix_ = { ix.a, ix.b, ix.c, ix.d };
    return index(sh_, ix_) + ix.e * size(sh_);
}


#else
int dim(Ix sh);
int size(Ix sh);
int index(Ix sh, Ix ix);
#endif


/* -----------------------------------------------------------------------------
 * Tuple Types
 * -------------------------------------------------------------------------- */

#endif  // __ACCELERATE_CUDA_EXTRAS_H__

// vim: filetype=cuda.c
