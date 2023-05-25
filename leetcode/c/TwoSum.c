#include<stdio.h>
#include<stdlib.h>

typedef struct Test {
  int *nums;
  int numsSize;
  int target;
  int *returnSize;
  int *o;
} Test;

const int numTests = 3;
const Test tests[] = {
  {
    .nums = (int[]){2, 7, 11, 15},
    .numsSize = 4,
    .target = 9,
    .returnSize = &(int){2},
    .o = (int[]){0, 1}
  },
  {
    .nums = (int[]){3, 2, 4},
    .numsSize = 3,
    .target = 6,
    .returnSize = &(int){2},
    .o = (int[]){1, 2}
  },
  {
    .nums = (int[]){3, 3},
    .numsSize = 2,
    .target = 6,
    .returnSize = &(int){2},
    .o = (int[]){0, 1}
  },
};

int * two_sum_OLD(int *nums, int numsSize, int target, int *returnSize) {
  int *o = (int *)malloc(sizeof(int) * (*returnSize));
  o[0] = 0; o[1] = 0;

  for (int i = 0; i < numsSize - 1; i++) {
    for (int j = i + 1; j < numsSize; j++) {
      if (nums[i] + nums[j] == target) {
        o[0] = i;
        o[1] = j;
        goto end;
      }
    }
  }

  end:
  return o;
}

int * two_sum(const int * nums, const int numsSize, const int target, const int * returnSize) {
  int * o = (int *)malloc(sizeof(int) * (*returnSize));
  o[0] = 0; o[1] = 0;
  int buf[128];
  int i;
  for (i = 0; i < 128; i++) buf[i] = -1;

  for (int i = 0; i < numsSize; i++) {
    if (buf[nums[i]] >= 0) {
      o[0] = buf[nums[i]];
      o[1] = i;
      break;
    } else {
      buf[target-nums[i]] = i;
    }
  }

  return o;
}

const int FALSE = 0;
const int TRUE = 1;
int equal(const int *a, const int n, const int *b, const int m) {
  if (n != m) return FALSE;
  for (int i = 0; i < n; i++)
    if (a[i] != b[i])
      return FALSE;
  return TRUE;
}

int main() {
  printf("Running...\n");
  for (int i = 0; i < numTests; i++) {
    Test t = tests[i];
    int *o = two_sum(t.nums, t.numsSize, t.target, t.returnSize);
    if (equal(o, 2, t.o, 2) == TRUE) {
      printf("Success\n");
    } else {
      printf("Failure\n");
    }
    free(o);
  }
  return 0;
}
