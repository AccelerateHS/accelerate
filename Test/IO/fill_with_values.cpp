#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

/* Returns one if it's filled with even values starting at 0 */
template <typename T>
int is_filled_with_evens(T *p, int size) {
  T   prev   = 0;
  int result = 1; // default to true
  int i;

  if (p[0] != 0) {
    result = 0;
  }

  for (i=1; result && i < size; i++) {
      if (p[i] != prev + 2) {
          result = 0;
      }
      else {
          prev = p[i];
      }
  }

  return result;
}


#ifdef __cplusplus
extern "C" {
#endif

int32_t *one_to_ten() {
  int32_t *p = (int32_t*) malloc(sizeof(int32_t) * 10);
  int i;
  for (i=0; i<10; i++) {
    p[i] = i+1;
  }
  return p;
}

double *ten_to_one() {
  double *p = (double*) malloc(sizeof(double) * 10);
  int i;
  for (i=0; i< 10; i++) {
    p[i] = 10.0 - (double) i;
  }
  return p;
}

int32_t *n_int_32s (int n) {
  return (int32_t*) malloc(sizeof(int32_t) * n);
}

int16_t *n_int_16s(int n) {
  return (int16_t*) malloc(sizeof(int16_t) * n);
}

int64_t *n_int_64s(int n) {
  return (int64_t*) malloc(sizeof(int64_t) * n);
}

int is_filled_with_evens_16(int16_t *p, int size)
{
    return is_filled_with_evens(p, size);
}

int is_filled_with_evens_32(int32_t *p, int size)
{
    return is_filled_with_evens(p, size);
}

int is_filled_with_evens_64(int64_t *p, int size)
{
    return is_filled_with_evens(p, size);
}

#ifdef __cplusplus
}
#endif

