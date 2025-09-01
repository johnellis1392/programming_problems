#[allow(dead_code)]
pub fn part1(input: &str) -> i32 {
  input.chars().map(|c| match c {
    '(' => 1,
    ')' => -1,
    _ =>  panic!("found unknown character: {c}"),
  }).sum()
}

#[allow(dead_code)]
pub fn part2(input: &str) -> i32 {
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

#[allow(dead_code)]
pub fn main() {
  let input = include_str!("../../input/aoc2015/day01.input.txt");
  let res1 = part1(input);
  println!("2015 Day 1, Part 1: {res1}");
  let res2 = part2(input);
  println!("2015 Day 1, Part 2: {res2}");
}

#[allow(unused_imports)]
pub mod tests {
  use super::{part1, part2};

  #[test]
  fn test_part1() {
    assert_eq!(0, part1("(())"));
    assert_eq!(3, part1("((("));
    assert_eq!(3, part1("))((((("));
    assert_eq!(-1, part1("())"));
    assert_eq!(-1, part1("))("));
    assert_eq!(-3, part1(")))"));
    assert_eq!(-3, part1(")())())"));
  }

  #[test]
  fn test_part2() {
    assert_eq!(1, part2(")"));
    assert_eq!(5, part2("()())"));
  }
}
