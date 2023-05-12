#!/usr/bin/env ts-node

function longestCommonPrefix(strs: string[]): string {
  const n = Math.min(...strs.map(s => s.length));
  let i = 0;
  outer:
  for (i = 0; i < n; i++) {
    let c = strs[0][i];
    for (let j = 1; j < strs.length; j++) {
      if (strs[j][i] !== c) {
        break outer;
      }
    }
  }
  return strs[0].slice(0, i);
}

class Test {
  input: string[];
  exp: string;
  constructor(input: string[], exp: string) {
    this.input = input;
    this.exp = exp;
  }
}

let tests = [
  new Test(['flower', 'flow', 'flight'], 'fl'),
  new Test(['dog', 'racecar', 'car'], ''),
]

for (const { input, exp } of tests) {
  let actual = longestCommonPrefix(input)
  if (actual === exp) {
    console.log(`SUCCESS`);
  } else {
    console.log(`FAILURE: ${actual} !== ${exp}`);
  }
}