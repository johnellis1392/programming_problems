#include<string.h>
#include<stdio.h>
#include<stdlib.h>


int is_palindrome(const char * string) {
  const int n = strlen(string);
  int i;
  for (i = 0; i < n / 2; i++) {
    if (string[i] != string[n - i - 1]) return 0;
  }
  return 1;
}


void test(const char * input) {
  printf("Test case for input '%s':\n", input);

  const int result = is_palindrome(input);
  if (result) {
    printf(" * Is a palindrome.\n");
  } else {
    printf(" * Is not a palindrome.\n");
  }
}


int main() {
  printf("Palidrome tests:\n");
  const char * s1 = "Hello, World!\0";
  const char * s2 = "racecar\0";

  test(s1);
  test(s2);
  printf("\n");

  return 0;
}

