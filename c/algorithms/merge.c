#include<stdio.h>
#include<stdlib.h>
#include<string.h>


void merge(int * a, int * b, const int n, const int m) {
  int i = n - 1, j = m - 1, k = n + m - 1;
  while (i >= 0 && j >= 0) {
    if (a[i] > b[j]) a[k--] = a[i--];
    else a[k--] = b[j--];
  }
  while (j >= 0) a[k--] = b[j--];
}


void print_array(const int * a, const int n) {
  printf("[ ");
  int i;
  for (i = 0; i < n; i++) {
    printf("%d, ", a[i]);
  }
  printf("]\n");
}


int array_eq(const int * a, const int * b, const int n) {
  int i;
  for (i = 0; i < n; i++) {
    if (a[i] != b[i]) return 0;
  }
  return 1;
}


void test(a, i, b, j, c, k)
  int * a;
  const int i;
  int * b;
  const int j;
  int * c;
  const int k;
{
  printf("Running test for input:\n");
  printf(" * a:        ");
  print_array(a, i);
  printf(" * b:        ");
  print_array(b, j);
  printf(" * expected: ");
  print_array(c, k);

  merge(a, b, i, j);
  printf(" * actual:   ");
  print_array(a, k);

  if (array_eq(a, c, k)){
    printf("[SUCCESS] Arrays are equal.\n");
  } else {
    printf("[FAILURE] Arrays are not equal.\n");
  }

  printf("\n");
}


int main() {
  printf("Running test cases...\n\n");

  int i = 3, j = 3, k = i + j;
  int a[6] = {1, 3, 5};
  int b[3] = {2, 4, 6};
  int c[6] = {1, 2, 3, 4, 5, 6};
  test(a, i, b, j, c, k);

  i = 5, j = 4, k = i + j;
  int d[9] = {1, 2, 4, 5, 7};
  int e[4] = {3, 6, 8, 9};
  int f[9] = {1, 2, 3, 4, 5, 6, 7, 8, 9};
  test(d, i, e, j, f, k);

  return 0;
}

