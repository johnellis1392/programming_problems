use crate::common::day::Day;

struct Day01;

impl Day for Day01 {
  type Input = String;
  type Output = i32;
  fn day() -> u32 { 1 }
  fn year() -> u32 { 2015 }

  fn parse_input(input: String) -> Self::Input {
    input.trim().to_string()
  }

  fn part1(input: &String) -> i32 {
    input.chars().map(|c| match c {
      '(' => 1,
      ')' => -1,
      _ =>  panic!("found unknown character: {c}"),
    }).sum()
  }

  fn part2(input: &String) -> i32 {
    input.chars()
      .map(|c| match c {
        '(' => 1,
        ')' => -1,
        _ => panic!("found unknown character: {c}"),
      }).scan(0, |acc, i| {
      *acc = *acc + i;
      Some(*acc)
    })
      .take_while(|i| *i >= 0)
      .count() as i32 + 1
  }
}


#[allow(unused_imports)]
pub mod tests {
  use crate::common::day::Day;
  use super::{Day01};

  #[test]
  fn test_part1() {
    assert_eq!(0, Day01::run_part1("(())"));
    assert_eq!(3, Day01::run_part1("((("));
    assert_eq!(3, Day01::run_part1("))((((("));
    assert_eq!(-1, Day01::run_part1("())"));
    assert_eq!(-1, Day01::run_part1("))("));
    assert_eq!(-3, Day01::run_part1(")))"));
    assert_eq!(-3, Day01::run_part1(")())())"));
  }

  #[test]
  fn test_part2() {
    assert_eq!(1, Day01::run_part2(")"));
    assert_eq!(5, Day01::run_part2("()())"));
  }

  #[test]
  fn run() {
    Day01::run();
  }
}
