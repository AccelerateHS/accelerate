#include "HsFFI.h"

StgInt64 hs_atomic_fetch_and_add_64(volatile StgInt64* ptr, StgInt64 val)
{
  return __sync_fetch_and_add(ptr, val);
}

StgInt64 hs_atomic_fetch_and_and_64(volatile StgInt64* ptr, StgInt64 val)
{
  return __sync_fetch_and_and(ptr, val);
}

