#!/usr/bin/env ts-node

function lengthOfLongestSubstring(s: string): number {
  let lastSeen: {[_: string]: number} = {};
  let maxLength: number = 0;
  let lastDoubleIndex: number = 0;
  for (let i = 0; i < s.length; i++) {
    let l = 0;
    if (s[i] in lastSeen) {
      lastDoubleIndex = Math.max(lastDoubleIndex, lastSeen[s[i]] + 1);
      l = i - lastDoubleIndex;
      // l = i - lastSeen[s[i]] - lastDoubleIndex;
      // lastDoubleIndex = lastSeen[s[i]] + 1;
      lastSeen[s[i]] = i;
    } else {
      lastSeen[s[i]] = i;
      l = i + 1 - lastDoubleIndex;
    }
    maxLength = Math.max(maxLength, l);
  }
  return maxLength;
}

class Test {
  input: string;
  exp: number;
  constructor(input: string, exp: number) {
    this.input = input;
    this.exp = exp;
  }
}

const tests = [
  new Test('abcabcbb', 3),
  new Test('bbbbb', 1),
  new Test('pwwkew', 3),
  new Test('abba', 2),
  new Test('dvdf', 3),
  new Test('tmmzuxt', 5),
];

function runTest({ input, exp }: Test) {
  const actual = lengthOfLongestSubstring(input);
  if (actual === exp) {
    console.log(`SUCCESS`);
  } else {
    console.error(`FAILURE: ${actual} !== ${exp}`);
  }
}

for (const t of tests) runTest(t);
