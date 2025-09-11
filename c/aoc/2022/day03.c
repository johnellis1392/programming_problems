#include<stdio.h>
#include<stdlib.h>

int priority(const char c) {
  if ('a' <= c && c <= 'z') {
    return c - 'a' + 1;
  } else {
    return c - 'A' + 1 + 26;
  }
}

const char find_double(const char * s, const int n) {
  int i, j;
  for (i = 0; i < n / 2; i++) {
    for (j = n / 2; j < n; j++) {
      if (s[i] == s[j]) return s[i];
    }
  }
  return '0';
}

const int day03_part1() {
  FILE * fp;
  char * line = NULL;
  size_t len = 0;
  ssize_t read;
  int s = 0;

  fp = fopen("./input.txt", "r");
  if (fp == NULL)
    exit(EXIT_FAILURE);

  while ((read = getline(&line, &len, fp)) != -1) {
    const char c = find_double(line, read);
    s += priority(c);
  }

  fclose(fp);
  if (line) free(line);
  return s;
}

const int find_triple(
  const char * s1,
  const int n1,
  const char * s2,
  const int n2,
  const char * s3,
  const int n3
) {
  int i, j, k;
  for (i = 0; i < n1; i++) {
    for (j = 0; j < n2; j++) {
      for (k = 0; k < n3; k++) {
        if (s1[i] == s2[j] && s2[j] == s3[k]) {
          return s1[i];
        }
      }
    }
  }
  return '0';
}

const int day03_part2() {
  FILE * fp;
  char * line = NULL, * line2 = NULL, * line3 = NULL;
  size_t l1 = 0, l2 = 0, l3 = 0;
  ssize_t r1, r2, r3;
  int s = 0;

  fp = fopen("./input.txt", "r");
  if (fp == NULL) exit(EXIT_FAILURE);

  while((r1 = getline(&line, &l1, fp)) != -1) {
    r2 = getline(&line2, &l2, fp);
    r3 = getline(&line3, &l3, fp);
    const char c = find_triple(line, r1, line2, r2, line3, r3);
    s += priority(c);
  }

  fclose(fp);
  if (line) free(line);
  if (line2) free(line2);
  if (line3) free(line3);
  return s;
}

int main(void) {
  const int result1 = day03_part1();
  printf("Result1 = %d\n", result1);
  const int result2 = day03_part2();
  printf("Result2 = %d\n", result2);
  exit(EXIT_SUCCESS);
}
