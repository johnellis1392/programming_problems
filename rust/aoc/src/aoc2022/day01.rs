use crate::common::day::Day;
use std::io::BufRead;


struct Day01;

impl Day for Day01 {
  type Input = String;
  type Output = i32;
  fn day() -> u32 { 1 }
  fn year() -> u32 { 2022 }

  fn parse_input(input: String) -> Self::Input {
    input.trim().to_string()
  }

  fn part1(input: &String) -> i32 {
    let lines = input.lines();
    let mut results: Vec<i32> = Vec::new();
    let mut values: Vec<i32> = Vec::new();

    for line in lines {
      if let Some(r) = line.parse::<i32>().ok() {
        values.push(r);
      } else {
        results.push(values.iter().sum());
        values.clear();
      }
    }

    results.push(values.iter().sum());
    results.sort();
    results.pop().expect("An error occurred while popping result list")
  }

  fn part2(input: &String) -> i32 {
    let lines = input.lines();
    let mut results: Vec<i32> = Vec::new();
    let mut values: Vec<i32> = Vec::new();

    for line in lines {
      if let Some(r) = line.parse::<i32>().ok() {
        values.push(r);
      } else {
        results.push(values.iter().sum());
        values.clear();
      }
    }

    results.push(values.iter().sum());
    results.sort();
    results.iter().skip(results.len() - 3).sum()
  }
}

pub mod tests {
  use crate::aoc2022::day01::Day01;
  use crate::common::day::Day;

  #[test]
  fn run() {
    Day01::run();
  }
}
