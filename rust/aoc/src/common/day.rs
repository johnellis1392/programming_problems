use crate::common::inputs::read_input_for_day;
use std::fmt::Display;

pub trait Day {
  type Input;
  type Output: Display;
  fn day() -> u32;
  fn year() -> u32;
  
  fn read_input() -> String {
    read_input_for_day(Self::day(), Self::year())
  }
  
  fn parse_input(input: String) -> Self::Input;

  fn part1(input: &Self::Input) -> Self::Output;

  fn part2(input: &Self::Input) -> Self::Output;
  
  fn run_part1(input: &str) -> Self::Output {
    let input = Self::parse_input(input.to_string());
    let result = Self::part1(&input);
    println!("{}, Day {}, Part 1: {}", Self::year(), Self::day(), result);
    result
  }

  fn run_part2(input: &str) -> Self::Output {
    let input = Self::parse_input(input.to_string());
    let result = Self::part2(&input);
    println!("{}, Day {}, Part 2: {}", Self::year(), Self::day(), result);
    result
  }
  
  fn run() {
    let input = Self::read_input();
    Self::run_part1(&input);
    Self::run_part2(&input);
  }
}

