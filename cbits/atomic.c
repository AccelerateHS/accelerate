#include <stdint.h>

int64_t atomic_fetch_and_add_64(volatile int64_t* ptr, int64_t val)
{
  return __sync_fetch_and_add(ptr, val);
}

int64_t atomic_fetch_and_sub_64(volatile int64_t* ptr, int64_t val)
{
  return __sync_fetch_and_sub(ptr, val);
}

int64_t atomic_fetch_and_and_64(volatile int64_t* ptr, int64_t val)
{
  return __sync_fetch_and_and(ptr, val);
}

int64_t atomic_read_64(volatile int64_t* ptr)
{
  return *ptr;
}

void atomic_write_64(volatile int64_t* ptr, int64_t val)
{
  *ptr = val;
  return;
}

