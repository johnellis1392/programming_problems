import { dirname } from 'path';
import fs from 'fs';

function sessionId() {
  return fs.readFileSync('.session_id').toString()
}

function range(a, b) {
  let res = []
  for (let i = a; i <= b; i++) res.push(i)
  return res
}

export const days = range(1, 25)
export const years = range(2015, 2025)

export const inputPathFor = (year, day) => `./inputs/${year}/day${day}.input.txt`

export async function readInput(year, day) {
  const path = inputPathFor(year, day)
  return fs.readFileSync(path).toString()
}

export async function fetchInputForDay(year, day) {
  const path = inputPathFor(year, day)
  if (fs.existsSync(path)) { return }
  const res = await fetch(`https://adventofcode.com/${year}/day/${day}/input`, {
    method: 'GET',
    headers: {
      'Cookie': `session=${sessionId()}`
    }
  })
  const data = await res.text()
  fs.mkdirSync(dirname(path), { recursive: true })
  fs.writeFileSync(path, data)
}

export async function fetchInputForYear(year) {
  for (const day of days) {
    await fetchInputForDay(year, day)
  }
}

export async function fetchAllInputs() {
  for (const year of years) {
    await fetchInputForYear(year)
  }
}
