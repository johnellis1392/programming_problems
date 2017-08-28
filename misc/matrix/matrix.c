#include<stdio.h>
#include<stdlib.h>
#include<string.h>


typedef struct Vector {
  const int n;
  const int *array;
} Array;


typedef struct Matrix {
  int n;
  int m;
  int **array;
} Matrix;


Matrix * matrix_alloc(const int n, const int m) {
  Matrix * matrix = malloc(sizeof(Matrix));
  matrix->n = n;
  matrix->m = m;

  // Allocate matrix values
  int **array = malloc(sizeof(int*) * n);
  for (int i = 0; i < n; i ++) {
	int *nested_array = malloc(sizeof(int) * m);
	array[i] = nested_array;
  }

  matrix->array = array;
  return matrix;
}



Matrix * __matrix_from_array(const int input[], const int arr_len, const int n, const int m) {
  if (arr_len != n * m) {
	// Invalid dimensions for input array; return null
	return NULL;
  }

  Matrix * matrix = matrix_alloc(n, m);

  // Fill in values
  for (int i = 0; i < n; i ++) {
	for (int j = 0; j < m; i ++) {
	  int value = input[i * m + j];
	  matrix->array[i][j] = value;
	}
  }

  return matrix;
}

#define matrix_from_array(input, n, m) __matrix_from_array(input, sizeof(input) / sizeof(input[0]), n, m);



void test(const int input[]) {
  return;
}


int main() {

  int i[1][3] = {
	{ 1, 2, 3 }
  };

  test(i[0]);

  return 0;
}

