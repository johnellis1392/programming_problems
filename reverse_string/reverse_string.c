#include<stdio.h>
#include<stdlib.h>
#include<string.h>

#define DEBUG 0


void reverse_string(char * s) {
  int n = strlen(s);
  for (int i = 0; i < n / 2; i ++) {
	if (DEBUG) {
	  printf(" * Swapping characters '%c' at '%d' and at '%c' at '%d'.\n", s[i], i, s[n - i - 1], n - i - 1);
	  printf(" * New String: '%s'\n", s);
	}

	char temp = s[i];
	s[i] = s[n - i - 1];
	s[n - i - 1] = temp;
  }
}


void test(const char * s, const char * expected) {
  const int n = strlen(s);
  char * input_data = malloc(sizeof(char) * n + 1);
  input_data[n] = '\0';
  strncpy(input_data, s, n);

  reverse_string(input_data);
  if (strncmp(input_data, expected, n) != 0) {
	printf("[FAIL] Test case for string '%s' failed:\n", s);
  } else {
	printf("[SUCCESS] Test case for string '%s' passed!\n", s);
  }

  printf(" * Expected: '%s'\n", expected);
  printf(" * Actual:   '%s'\n", input_data);
  printf("\n");

  // FREE THE DATA, NINKOMPOOP!
  free(input_data);
}


int main() {

  printf("Running Tests...\n");

  test("", "");
  test("a", "a");
  test("asdf", "fdsa");
  test("asdfg", "gfdsa");
  test("aaaaaaaaaaaaaaaaaaaaaaaaab", "baaaaaaaaaaaaaaaaaaaaaaaaa");
  test("zxcvbnmasdfghjklqwertyuiop", "poiuytrewqlkjhgfdsamnbvcxz");

  return 0;
}

