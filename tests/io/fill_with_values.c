#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

int *one_to_ten() {
  int *p = malloc(sizeof(int) * 10);
  int i;
  for (i=0; i<10; i++) {
    p[i] = i+1;
  }
  return p;
}

double *ten_to_one() {
  double *p = malloc(sizeof(int) * 10);
  int i;
  for (i=0; i< 10; i++) {
    p[i] = (double) 10 - i;
  }
  return p;
}

int *n_ints (int n) {
  return malloc(sizeof(int) * n);
}

int *n_int_16s(int n) {
  return malloc(sizeof(int16_t) * n);
}

int *n_int_64s(int n) {
  return malloc(sizeof(int64_t) * n);
}

/* Returns one if it's filled with even values starting at 0 */
int is_filled_with_evens(int *p, int size) {
  int i, prev = 0;
  int result = 1; // default to true
  
  if (p[0] != 0) {
    result = 0;
  }
  if (result) {
    for (i=1; i < size; i++) {
      if (p[i] != prev + 2) {
        result = 0;
        break;
      }
      prev = p[i];
    }
  }
  return result;
}

