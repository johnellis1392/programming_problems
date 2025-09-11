export const day = 1
export const year = 2015

export function parseInput(input) {
  return input.trim()
}

export function part1(input) {
  return input.split('').map(c => c === '(' ? 1 : -1)
    .reduce((r, i) => r + i, 0)
}

export function part2(input) {
  let a = input.split('').map(c => c === '(' ? 1 : -1)
  let res = 0
  for (let i = 0; i < a.length; i++) {
    res += a[i]
    if (res < 0) return i + 1
  }
  return -1
}
