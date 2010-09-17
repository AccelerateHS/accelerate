/* -----------------------------------------------------------------------------
 *
 * Module    : Util
 * Copyright : (c) [2009..2010] Trevor L. McDonell
 * License   : BSD
 *
 * ---------------------------------------------------------------------------*/

#ifndef __ACCELERATE_CUDA_UTIL_H__
#define __ACCELERATE_CUDA_UTIL_H__

#include <math.h>
#include <cuda_runtime.h>

/*
 * Core assert function. Don't let this escape...
 */
#if defined(__CUDACC__) || !defined(__DEVICE_EMULATION__)
#define __assert(e, file, line) ((void)0)
#else
#define __assert(e, file, line) \
    ((void) fprintf (stderr, "%s:%u: failed assertion `%s'\n", file, line, e), abort())
#endif

/*
 * Test the given expression, and abort the program if it evaluates to false.
 * Only available in debug mode.
 */
#ifndef _DEBUG
#define assert(e)               ((void)0)
#else
#define assert(e)  \
    ((void) ((e) ? (void(0)) : __assert (#e, __FILE__, __LINE__)))
#endif

/*
 * Macro to insert __syncthreads() in device emulation mode
 */
#ifdef __DEVICE_EMULATION__
#define __EMUSYNC               __syncthreads()
#else
#define __EMUSYNC
#endif

/*
 * Check the return status of CUDA API calls, and abort with an appropriate
 * error string on failure.
 */
#define CUDA_SAFE_CALL_NO_SYNC(call)                                           \
    do {                                                                       \
        cudaError err = call;                                                  \
        if(cudaSuccess != err) {                                               \
            const char *str = cudaGetErrorString(err);                         \
            __assert(str, __FILE__, __LINE__);                                 \
        }                                                                      \
    } while (0)

#define CUDA_SAFE_CALL(call)                                                   \
    do {                                                                       \
        CUDA_SAFE_CALL_NO_SYNC(call);                                          \
        CUDA_SAFE_CALL_NO_SYNC(cudaThreadSynchronize());                       \
    } while (0)

#undef __assert
#endif  // __ACCELERATE_CUDA_UTIL_H__

