#include<stdio.h>
#include<stdlib.h>
#include<string.h>


#define DEBUG 0


/**
 * Allocate new string of length n + 1, and
 * fill contents with 0's
 * TODO: Add error-checking for malloc call
 */
char * str_alloc(const int n) {
  char * s = malloc(sizeof(char) * n + 1);
  for (int i = 0; i < n; i++) {
	s[i] = '\0';
  }
  s[n] = '\0';
  return s;
}


/**
 * Shift all characters in the given string from
 * position 'i' onward to the left, and fill in
 * final character with '\0'
 */
void shift(char * s, const int i) {
  if (i < 0) return;

  if (DEBUG) {
	printf(" * Shifting characters for string '%s' at position '%d', length: '%d'\n", s, i, (int)strlen(s));
  }

  const int n = strlen(s);
  for (int j = i + 1; j < n; j++) {
	s[j - 1] = s[j];
  }
  s[n - 1] = '\0';

  if (DEBUG) {
	printf(" * String after: '%s', length: '%d'\n", s, (int)strlen(s));
  }
}


/**
 * Remove duplicate characters from a string 's'
 * TODO: Add error-checking for realloc call
 */
char * rmdup(char * s) {
  int n = strlen(s);
  int i = 0, j = 0;
  while (i < n) {
	j = i + 1;
	while (j < n) {
	  if (s[i] == s[j]) {
		shift(s, j);
		n--;
	  } else {
		j++;
	  }
	}
	i++;
  }

  s = realloc(s, sizeof(char) * n + 1);
  s[n] = '\0';
  return s;
}


/**
 * Run a test case
 * TODO: Handle possible errors from str_alloc and rmdup
 */
void test(const char * input, const char * expected) {
  const int n = strlen(input);
  // char * input_copy = (char *)malloc(sizeof(char) * n + 1);
  char * input_copy = str_alloc(n);
  strncpy(input_copy, input, n);
  input_copy = rmdup(input_copy);
  const int m = strlen(input_copy);

  if (strncmp(input_copy, expected, m) != 0) {
	printf("[FAIL]    Test case for input '%s' failed:\n", input);
  } else {
	printf("[SUCCESS] Test case for input '%s' passed!\n", input);
  }

  printf(" * Expected: '%s', length: '%d'\n", expected, (int)strlen(expected));
  printf(" * Actual:   '%s', length: '%d'\n", input_copy, (int)strlen(input_copy));
  printf("\n");
  free(input_copy);
}


int main() {

  printf("Running tests...\n\n");
  test("", "");
  test("a", "a");
  test("asdf", "asdf");
  test("asdfasdf", "asdf");
  test("asdfghjklqwertyuiopa", "asdfghjklqwertyuiop");
  test("aaaaaaaaaaaaaaaaaaaaaaaaaaa", "a");

  return 0;
}

