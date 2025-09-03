import {
  fetchInputForDay,
  fetchInputForYear,
  readInput,
  days
} from './common/inputs.js'

async function runDay(year, day) {
  const dayString = day >= 10 ? '' + day : '0' + day
  const {
    parseInput,
    part1,
    part2,
  } = await import(`./${year}/day${dayString}.js`)
  await fetchInputForDay(year, day)
  const input = await readInput(year, day)
  console.log(`${year}, Day ${day}, Part 1: ${part1(parseInput(input))}`)
  console.log(`${year}, Day ${day}, Part 2: ${part2(parseInput(input))}`)
}

async function runYear(year) {
  fetchInputForYear(year)
  for (const day of days) {
    await runDay(year, day)
  }
}

async function main() {
  await runDay(2015, 1)
}

await main()
