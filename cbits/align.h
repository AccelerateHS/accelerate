/*
 * Module      : Data.Atomic.Align
 * Copyright   : [2021] The Accelerate Team
 * License     : BSD3
 *
 * Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
 * Stability   : experimental
 * Portability : non-portable (GHC extensions)
 */

#ifndef __ACCELERATE_ALIGN_H__
#define __ACCELERATE_ALIGN_H__

/* Cache lines on x86_64 are 64-bytes. On a miss the pre-fetcher will fetch the
 * next cache line as well. Thus to prevent false sharing we need to pad hot
 * variables to 128-bytes.
 */
#define PAGE_SIZE 4096
#define CACHE_LINE_SIZE 64
#define CACHE_ALIGNED __attribute__((aligned(CACHE_LINE_SIZE)))
#define DOUBLE_CACHE_ALIGNED __attribute__((aligned(2 * CACHE_LINE_SIZE)))

#endif // __ACCELERATE_ALIGN_H__

