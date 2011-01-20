#include <stdlib.h>

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
    p[i] = (double) 11 - i;
  }
  return p;
}

