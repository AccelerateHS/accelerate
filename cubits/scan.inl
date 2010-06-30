/* -----------------------------------------------------------------------------
 *
 * Module    : Scan
 * Copyright : (c) [2009..2010] Trevor L. McDonell
 * License   : BSD
 *
 * The CUDPP parallel implementation of scan, hoisted out of C++ template-land
 * <http://code.google.com/p/cudpp/> (v1.1.1)
 *
 * ---------------------------------------------------------------------------*/

#define MAX_LEVEL               4       // Allows warp scan to be performed on partial warps
#define ELEMENTS_PER_THREAD     8       // Number of elements processed by each thread

#include "cudpp/globals.h"
#include "cudpp/scan_cta.cu"
#include "cudpp/scan_kernel.cu"
#include "cudpp/vector_kernel.cu"

