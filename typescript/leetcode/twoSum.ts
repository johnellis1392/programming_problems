#!/usr/bin/env ts-node

function twoSum(nums: number[], target: number): number[] {
  let n: {[_:number]: number} = {};
  for (let i = 0; i < nums.length; i++) {
    if ((target - nums[i]) in n) {
      return [n[target - nums[i]], i];
    } else {
      n[nums[i]] = i;
    }
  }
  return [];
}

class Test {
  input: {
    nums: number[],
    target: number,
  };
  expected: number[]
  constructor(nums: number[], target: number, expected: number[]) {
    this.input = { nums, target };
    this.expected = expected;
  }
};

function arrayEquals<T>(a: T[], b: T[]): boolean {
  if (a.length !== b.length) return false;
  for (let i = 0; i < a.length; i++)
    if (a[i] !== b[i]) return false;
  return true;
}

function runTest({ input: { nums, target }, expected }: Test) {
  const actual = twoSum(nums, target);
  if (arrayEquals(expected, actual)) {
    console.log(`SUCCESS`);
  } else {
    console.log(`FAILED: ${actual} !== ${expected}`);
  }
}

const tests: Test[] = [
  new Test([2, 7, 11, 15], 9, [0, 1]),
  new Test([3,2,4], 6, [1,2]),
  new Test([3,3], 6, [0,1]),
];

function runTests() {
  for (const t of tests) {
    runTest(t);
  }
}

runTests();