#!/usr/bin/env ts-node

function removeDuplicates(nums: number[]): number {
  if (nums.length === 0) return 0;
  let n = nums[0];
  let k = 1;
  let i = 1;
  while (i < nums.length) {
    if (nums[i] === n) {
      nums.splice(i, 1);
    } else {
      n = nums[i];
      k++;
    }
  }
  return k;
}

console.log(removeDuplicates([1,1,2]));
console.log(removeDuplicates([0,0,1,1,1,2,2,3,3,4]));