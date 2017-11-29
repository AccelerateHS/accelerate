/*
 * Module      : Data.Array.Accelerate.Debug.Monitoring
 * Copyright   : [2016..2017] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
 * License     : BSD3
 *
 * Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
 * Stability   : experimental
 * Portability : non-portable (GHC extensions)
 *
 * Support for runtime system monitoring
 */

#include <locale.h>
#include <stdint.h>
#include <stdio.h>
#include <time.h>


/* These monitoring counters are globals which  will be accessed from the
 * Haskell side.
 */
int64_t __active_ns_llvm_native             = 0;
int64_t __active_ns_llvm_ptx                = 0;

int64_t __current_bytes_remote              = 0;
int64_t __current_bytes_nursery             = 0;

int64_t __total_bytes_allocated_local       = 0;
int64_t __total_bytes_allocated_remote      = 0;
int64_t __total_bytes_copied_to_remote      = 0;
int64_t __total_bytes_copied_from_remote    = 0;
int64_t __total_bytes_evicted_from_remote   = 0;
int64_t __num_remote_gcs                    = 0;
int64_t __num_evictions                     = 0;

extern int32_t __dump_gc;
extern int32_t __dump_gc_stats;

#if defined(ACCELERATE_DEBUG)

double clock_gettime_elapsed_seconds(void);

/*
 * This function runs after main(), and is used to print final GC and memory
 * statistics (if enabled). This is similar to the +RTS -s option.
 */
__attribute__((destructor)) void dump_gc_stats(void)
{
  if (__dump_gc_stats) {
    struct lconv* lc = localeconv();
    char comma[]     = ",";
    char thousands[] = { 3, 0 };
    char *old_sep    = lc->thousands_sep;
    char *old_grp    = lc->grouping;

    /* elapsed wallclock time; see cbits/clock.c
     */
    double time      = clock_gettime_elapsed_seconds();

    /* set locale so that numbers are printed with a thousands separator
     */
    lc->thousands_sep = comma;
    lc->grouping      = thousands;

    printf("\n");
    printf("[%8.3f] gc: %'lld bytes allocated locally\n", time, __total_bytes_allocated_local);
    printf("[%8.3f] gc: %'lld bytes allocated on the remote device\n", time, __total_bytes_allocated_remote);
    printf("[%8.3f] gc: %'lld bytes copied to the remote device\n", time, __total_bytes_copied_to_remote);
    printf("[%8.3f] gc: %'lld bytes copied from the remote device\n", time, __total_bytes_copied_from_remote);
    printf("[%8.3f] gc: %'lld bytes evicted from the remote (%lld evictions, %lld GCs)\n", time, __total_bytes_evicted_from_remote, __num_evictions, __num_remote_gcs);

    lc->thousands_sep = old_sep;
    lc->grouping      = old_grp;
  }
}

#endif /* ACCELERATE_DEBUG */

