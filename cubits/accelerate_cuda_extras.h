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
 * We don't have a definition for `Int' or `Word', since the bitwidth of the
 * Haskell and C types may be different.
 *
 * NOTE ON 64-BIT TYPES
 *   The CUDA device uses little-endian arithmetic. We haven't accounted for the
 *   fact that this may be different on the host, for both initial data transfer
 *   and the unpacking below.
 */
#if defined(__cplusplus) && defined(__CUDACC__)

typedef texture<uint2,    1> TexWord64;
typedef texture<uint32_t, 1> TexWord32;
typedef texture<uint16_t, 1> TexWord16;
typedef texture<uint8_t,  1> TexWord8;
typedef texture<int2,     1> TexInt64;
typedef texture<int32_t,  1> TexInt32;
typedef texture<int16_t,  1> TexInt16;
typedef texture<int8_t,   1> TexInt8;
typedef texture<float,    1> TexFloat;
typedef texture<char,     1> TexCChar;

static __inline__ __device__ uint8_t  indexArray(TexWord8  t, const int x) { return tex1Dfetch(t,x); }
static __inline__ __device__ uint16_t indexArray(TexWord16 t, const int x) { return tex1Dfetch(t,x); }
static __inline__ __device__ uint32_t indexArray(TexWord32 t, const int x) { return tex1Dfetch(t,x); }
static __inline__ __device__ uint64_t indexArray(TexWord64 t, const int x)
{
  union { uint2 x; uint64_t y; } v;
  v.x = tex1Dfetch(t,x);
  return v.y;
}

static __inline__ __device__ int8_t  indexArray(TexInt8  t, const int x) { return tex1Dfetch(t,x); }
static __inline__ __device__ int16_t indexArray(TexInt16 t, const int x) { return tex1Dfetch(t,x); }
static __inline__ __device__ int32_t indexArray(TexInt32 t, const int x) { return tex1Dfetch(t,x); }
static __inline__ __device__ int64_t indexArray(TexInt64 t, const int x)
{
  union { int2 x; int64_t y; } v;
  v.x = tex1Dfetch(t,x);
  return v.y;
}

static __inline__ __device__ float indexArray(TexFloat  t, const int x) { return tex1Dfetch(t,x); }
static __inline__ __device__ char  indexArray(TexCChar  t, const int x) { return tex1Dfetch(t,x); }

#if defined(__LP64__)
typedef TexInt64  TexCLong;
typedef TexWord64 TexCULong;
#else
typedef TexInt32  TexCLong;
typedef TexWord32 TexCULong;
#endif

/*
 * Synonyms for C-types. NVCC will force Ints to be 32-bits.
 */
typedef TexInt8   TexCSChar;
typedef TexWord8  TexCUChar;
typedef TexInt16  TexCShort;
typedef TexWord16 TexCUShort;
typedef TexInt32  TexCInt;
typedef TexWord32 TexCUInt;
typedef TexInt64  TexCLLong;
typedef TexWord64 TexCULLong;
typedef TexFloat  TexCFloat;

/*
 * Doubles, only available when compiled for Compute 1.3 and greater
 */
#if !defined(CUDA_NO_SM_13_DOUBLE_INTRINSICS)
typedef texture<int2,  1> TexDouble;
typedef TexDouble         TexCDouble;

static __inline__ __device__ double indexArray(TexDouble t, const int x)
{
  int2 v = tex1Dfetch(t,x);
  return __hiloint2double(v.y,v.x);
}
#endif

#else

typedef void* TexWord64;
typedef void* TexWord32;
typedef void* TexWord16;
typedef void* TexWord8;
typedef void* TexInt64;
typedef void* TexInt32;
typedef void* TexInt16;
typedef void* TexInt8;
typedef void* TexCShort;
typedef void* TexCUShort;
typedef void* TexCInt;
typedef void* TexCUInt;
typedef void* TexCLong;
typedef void* TexCULong;
typedef void* TexCLLong;
typedef void* TexCULLong;
typedef void* TexFloat;
typedef void* TexDouble;
typedef void* TexCFloat;
typedef void* TexCDouble;
typedef void* TexCChar;
typedef void* TexCSChar;
typedef void* TexCUChar;

void* indexArray(const void*, const int);

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
