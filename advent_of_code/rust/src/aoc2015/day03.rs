use std::collections::HashSet;

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

#[allow(dead_code)]
pub fn part1(input: &str) -> usize {
  let mut visited = HashSet::<Point>::new();
  let mut curr = Point { x: 0, y: 0 };
  visited.insert(curr);
  for c in input.chars() {
    curr = curr.mv(c);
    visited.insert(curr);
  }
  visited.len()
}

#[allow(dead_code)]
pub fn part2(input: &str) -> usize {
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

#[allow(dead_code)]
pub fn main() {
  let input = include_str!("../../input/aoc2015/day03.input.txt");
  let res1 = part1(input);
  println!("2015 Day 3, Part 1: {res1}");
  let res2 = part2(input);
  println!("2015 Day 3, Part 2: {res2}");
}

#[allow(unused_imports)]
mod tests {
  use super::{part1, part2};

  #[test]
  fn test_part1() {
    assert_eq!(2, part1(">"));
    assert_eq!(4, part1("^>v<"));
    assert_eq!(2, part1("^v^v^v^v^v"));
  }

  #[test]
  fn test_part2() {
    assert_eq!(3, part2("^v"));
    assert_eq!(3, part2("^>v<"));
    assert_eq!(11, part2("^v^v^v^v^v"));
  }
}
