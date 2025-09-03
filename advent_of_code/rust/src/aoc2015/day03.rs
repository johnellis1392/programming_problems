use std::collections::HashSet;
use crate::common::day::Day;

#[derive(Eq, PartialEq, Copy, Clone, Hash)]
struct Point {
  x: i32,
  y: i32,
}

impl Point {
  fn north(&self) -> Point { Point { x: self.x, y: self.y + 1 } }
  fn south(&self) -> Point { Point { x: self.x, y: self.y - 1 } }
  fn east(&self) -> Point { Point { x: self.x + 1, y: self.y } }
  fn west(&self) -> Point { Point { x: self.x - 1, y: self.y } }
  fn mv(&self, c: char) -> Point {
    match c {
      '^' => self.north(),
      'v' => self.south(),
      '>' => self.east(),
      '<' => self.west(),
      _ => panic!("illegal character: {c}"),
    }
  }
}

struct Day03;

impl Day for Day03 {
  type Input = String;
  type Output = usize;

  fn day() -> u32 { 3 }
  fn year() -> u32 { 2015 }

  fn parse_input(input: String) -> Self::Input {
    input.trim().to_string()
  }


  fn part1(input: &Self::Input) -> Self::Output {
    let mut visited = HashSet::<Point>::new();
    let mut curr = Point { x: 0, y: 0 };
    visited.insert(curr);
    for c in input.chars() {
      curr = curr.mv(c);
      visited.insert(curr);
    }
    visited.len()
  }

  fn part2(input: &Self::Input) -> Self::Output {
    let mut visited = HashSet::<Point>::new();
    let mut santa = Point { x: 0, y: 0 };
    let mut robot = Point { x: 0, y: 0 };
    visited.insert(santa);
    for (i, c) in input.chars().enumerate() {
      if i % 2 == 0 {
        santa = santa.mv(c);
        visited.insert(santa);
      } else {
        robot = robot.mv(c);
        visited.insert(robot);
      }
    }
    visited.len()
  }
}

#[allow(unused_imports)]
mod tests {
  use crate::aoc2015::day03::Day03;
  use crate::common::day::Day;

  #[test]
  fn test_part1() {
    assert_eq!(2, Day03::run_part1(">"));
    assert_eq!(4, Day03::run_part1("^>v<"));
    assert_eq!(2, Day03::run_part1("^v^v^v^v^v"));
  }

  #[test]
  fn test_part2() {
    assert_eq!(3, Day03::run_part2("^v"));
    assert_eq!(3, Day03::run_part2("^>v<"));
    assert_eq!(11, Day03::run_part2("^v^v^v^v^v"));
  }
}
