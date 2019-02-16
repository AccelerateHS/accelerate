/*
 * Module      : Data.Array.Accelerate.Debug.Clock
 * Copyright   : [2017..2019] The Accelerate Team
 * License     : BSD3
 *
 * Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
 * Stability   : experimental
 * Portability : non-portable (GHC extensions)
 *
 * Get the monotonic wall-clock time.
 *
 * Implementations for macOS and Win32 stolen from the clock package, which has
 * a BSD3 license.
 *
 * <https://github.com/corsis/clock/tree/0.5.1>
 */

#if defined(__MACH__) && defined(__APPLE__)
/*
 * macOS
 *
 * macOS 10.12 also has clock_gettime(), so it might be worthwhile to use that
 * when available.
 */
#include <time.h>
#include <mach/clock.h>
#include <mach/mach.h>

static void clock_darwin_gettime(clock_id_t clock, struct timespec *t)
{
    // OS X does not have clock_gettime, use clock_get_time
    // see http://stackoverflow.com/questions/11680461/monotonic-clock-on-osx
    clock_serv_t cclock;
    mach_timespec_t mts;

    host_get_clock_service(mach_host_self(), clock, &cclock);
    clock_get_time(cclock, &mts);
    mach_port_deallocate(mach_task_self(), cclock);

    t->tv_sec  = mts.tv_sec;
    t->tv_nsec = mts.tv_nsec;
}

double clock_gettime_monotonic_seconds()
{
    struct timespec t;
    clock_darwin_gettime(SYSTEM_CLOCK, &t);

    return (double) t.tv_sec + (double) t.tv_nsec * 1.0E-9;
}

#elif defined(_WIN32)
/*
 * Windows
 */
#include <windows.h>

static long ticks_to_nanos(LONGLONG subsecond_time, LONGLONG frequency)
{
    return (long)((1E9 * subsecond_time) / frequency);
}

static void clock_win32_gettime_monotonic(long long* t)
{
    LARGE_INTEGER time;
    static LARGE_INTEGER frequency;
    static int hasFreq = 0;

    QueryPerformanceCounter(&time);
    if (!hasFreq)
    {
        hasFreq = 1;
        QueryPerformanceFrequency(&frequency);
    }

    // seconds
    t[0] = time.QuadPart / frequency.QuadPart;

    // nanoseconds
    t[1] = ticks_to_nanos(time.QuadPart % frequency.QuadPart, frequency.QuadPart);
}

double clock_gettime_monotonic_seconds()
{
    long long t[2];
    clock_win32_gettime_monotonic(t);

    return (double) t[0] + (double) t[1] * 1.0E-9;
}

#else
/*
 * *nix
 */
#include <time.h>

double clock_gettime_monotonic_seconds()
{
    struct timespec t;
    clock_gettime(CLOCK_MONOTONIC, &t);

    return (double) t.tv_sec + (double) t.tv_nsec * 1.0E-9;
}

#endif /* OS */


static double __program_epoch;

double clock_gettime_elapsed_seconds(void)
{
    double now  = clock_gettime_monotonic_seconds();
    double diff = now - __program_epoch;

    return diff;
}

__attribute__((constructor)) void initialise_program_epoch(void)
{
    __program_epoch = clock_gettime_monotonic_seconds();
}

