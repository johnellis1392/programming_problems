#include<stdio.h>
#include<stdlib.h>
#include<errno.h>
#include<fcntl.h>
#include<string.h>
#include<sys/types.h>
#include<unistd.h>

#define toint(p) ((p)-48)


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

  if (read(fd, f->buf, f->n) != f->n) {
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

#define NUM_STACKS 9
#define NUM_SPACES 128
char stacks[NUM_STACKS][NUM_SPACES];
int stack_ptrs[NUM_STACKS];

void init_stacks() {
  int i, j;
  for (i = 0; i < NUM_STACKS; i++) {
    stack_ptrs[i] = 0;
    for (j = 0; j < NUM_SPACES; j++)
      stacks[i][j] = 0;
  }
}

void push(const int column, const char c) {
  stacks[column][stack_ptrs[column]] = c;
  stack_ptrs[column]++;
}

char pop(const int column) {
  char c = stacks[column][stack_ptrs[column]];
  stacks[column][stack_ptrs[column]] = 0;
  stack_ptrs[column]--;
  return c;
}

#define STR_SIZE 128
int num_strs;
char **strs;
void init_strs(const int n) {
  num_strs = n;
  strs = malloc(sizeof(char*) * n);
  int i, j;
  for (i = 0; i < n; i++) {
    strs[i] = malloc(sizeof(char) * STR_SIZE);
    for (j = 0; j < STR_SIZE; j++) strs[i][j] = 0;
  }
}

void free_strs() {
  int i;
  for (i = 0; i < num_strs; i++) 
    free(strs[i]);
  free(strs);
  num_strs = 0;
}

const char * day05_part1(const char * filename) {
  const File * f = readfile(filename);
  char *ptr = f->buf;
  char *end_header_ptr = f->buf;

  int num_lines = 0;
  while (*ptr != '\n') {
    while (*ptr != '\n') {
      ++ptr;
      ++end_header_ptr;
    }
    ++ptr;
    ++end_header_ptr;
    ++num_lines;
  }

  init_strs(num_lines);
  ptr = f->buf;

  // TODO: Finish this

  free_strs();
  return NULL;
}

const char * day05_part2(const char * filename)  {
  return NULL;
}

int main() {
  const char * input_filename = "input.test.txt";
  // const char * input_filename = "input.txt";

  const char * result1 = day05_part1(input_filename);
  printf("Result1: %s\n", result1);
  const char * result2 = day05_part2(input_filename);
  printf("Result2: %s\n", result2);

  exit(EXIT_SUCCESS);
}
