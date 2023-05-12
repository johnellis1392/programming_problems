#!/usr/bin/env ts-node

function isValid(s: string): boolean {
  const openChars: string[] = ['(', '[', '{'];
  const charMap: {[_:string]: string} = { ')': '(', ']': '[', '}': '{' };
  let stack: string[] = [];
  for (let i = 0; i < s.length; i++) {
    if (openChars.includes(s[i])) {
      stack.push(s[i]);
    } else if (stack.length === 0) {
      return false;
    } else if (stack[stack.length-1] !== charMap[s[i]]) {
      return false;
    } else {
      stack.pop();
    }
  }
  return stack.length === 0;
}

class Test {
  input: string;
  exp: boolean;
  constructor(input: string, exp: boolean) {
    this.input = input;
    this.exp = exp;
  }
}

const tests = [
  new Test('()', true),
  new Test('()[]{}', true),
  new Test('(]', false),
]

for (const { input, exp } of tests) {
  const actual = isValid(input);
  if (actual === exp) {
    console.log('SUCCESS');
  } else {
    console.log(`FAILURE: ${actual} !== ${exp}`);
  }
}