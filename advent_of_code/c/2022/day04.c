#include<stdio.h>
#include<stdlib.h>
#include<errno.h>
#include<fcntl.h>
#include<string.h>
#include<sys/types.h>
#include<unistd.h>

#define toint(p) ((p)-48)

// const char * input_filename = "input.test.txt";
const char * input_filename = "input.txt";

typedef struct File {
  int n;
  char *buf;
} File;

File * readfile(const char * filename) {
  File * f = NULL;
  int fd;
  off_t filesize;

  f = malloc(sizeof(File));

  if ((fd = open(filename, O_RDWR, 0666)) == -1) {
    fprintf(stderr, "Failed to open(2) '%s' %s\n", filename, strerror(errno));
    exit(EXIT_FAILURE);
  }

  if ((filesize = lseek(fd, 0, SEEK_END)) == -1) {
    fprintf(stderr, "Failed to lseek(2) %s\n", strerror(errno));
    exit(EXIT_FAILURE);
  }

  f->buf = malloc(sizeof(char) * filesize + 1);
  f->n = filesize;
  if (lseek(fd, 0, SEEK_SET) == -1) {
    fprintf(stderr, "Failed to lseek(2) %s\n", strerror(errno));
    exit(EXIT_FAILURE);
  }

  if (read(fd, f->buf, f->n) != f->n)  {
    fprintf(stderr, "Failed to read(2) %s\n", strerror(errno));
    exit(EXIT_FAILURE);
  }

  f->buf[f->n] = '\0';
  close(fd);

  return f;
}

void freefile(File * f) {
  if (f) {
    if (f->buf) free(f->buf);
    free(f);
  }
}

int calc(const int a1, const int a2, const int b1, const int b2) {
  if (a1 >= b1 && a2 <= b2) return 1;
  if (b1 >= a1 && b2 <= a2) return 1;
  return 0;
}

const int day04_part1(const char * filename) {
  File * f = readfile(filename);
  int sum = 0;
  int a1 = 0, a2 = 0, b1 = 0, b2 = 0;
  char *ptr = f->buf;

  while (*ptr) {
    if ((*ptr) == '\0') break;
    while (*ptr && *ptr != '-')  { a1 = a1 * 10 + toint(*ptr); ++ptr; }
    ++ptr;
    while (*ptr && *ptr != ',')  { a2 = a2 * 10 + toint(*ptr); ++ptr; }
    ++ptr;
    while (*ptr && *ptr != '-')  { b1 = b1 * 10 + toint(*ptr); ++ptr; }
    ++ptr;
    while (*ptr && *ptr != '\n') { b2 = b2 * 10 + toint(*ptr); ++ptr; }
    ++ptr;
    sum += calc(a1, a2, b1, b2);
    a1 = 0, a2 = 0, b1 = 0, b2 = 0;
  }

  freefile(f);
  return sum;
}

int calc2(const int a1, const int a2, const int b1, const int b2) {
  if (a1 <= b1 && b1 <= a2) return 1;
  if (a1 <= b2 && b2 <= a2) return 1;
  if (b1 <= a1 && a1 <= b2) return 1;
  if (b1 <= a2 && a2 <= b2) return 1;
  return 0;
}

const int day04_part2(const char * filename) {
  File * f = readfile(filename);
  int sum = 0;
  int a1 = 0, a2 = 0, b1 = 0, b2 = 0;
  char *ptr = f->buf;

  while (*ptr) {
    if ((*ptr) == '\0') break;
    while (*ptr && *ptr != '-')  { a1 = a1 * 10 + toint(*ptr); ++ptr; }
    ++ptr;
    while (*ptr && *ptr != ',')  { a2 = a2 * 10 + toint(*ptr); ++ptr; }
    ++ptr;
    while (*ptr && *ptr != '-')  { b1 = b1 * 10 + toint(*ptr); ++ptr; }
    ++ptr;
    while (*ptr && *ptr != '\n') { b2 = b2 * 10 + toint(*ptr); ++ptr; }
    ++ptr;
    sum += calc2(a1, a2, b1, b2);
    a1 = 0, a2 = 0, b1 = 0, b2 = 0;
  }

  freefile(f);
  return sum;
}

int main() {
  const int result1 = day04_part1(input_filename);
  const int result2 = day04_part2(input_filename);
  printf("Result1 = %d\n", result1);
  printf("Result2 = %d\n", result2);
  exit(EXIT_SUCCESS);
}
