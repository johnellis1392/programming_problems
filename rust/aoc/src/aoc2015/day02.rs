use std::fmt::{Display, Formatter, Write};
use crate::common::day::Day;

#[derive(Debug)]
struct Gift(u32, u32, u32);

impl Display for Gift {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_fmt(format_args!("({}, {}, {})", self.0, self.1, self.2))
  }
}


struct Day02;

impl Day for Day02 {
  type Input = Vec<Gift>;
  type Output = u32;

  fn day() -> u32 { 2 }
  fn year() -> u32 { 2015 }

  fn parse_input(input: String) -> Self::Input {
    input.lines()
      .map(|line| line.trim())
      .map(|line| {
        let v = line.split("x").collect::<Vec<&str>>();
        match v[..] {
          [l, w, h] => Gift(
            l.parse::<u32>().unwrap(),
            w.parse::<u32>().unwrap(),
            h.parse::<u32>().unwrap()
          ),
          _ => panic!("illegal pattern: {line}"),
        }
      }).collect()
  }

  fn part1(input: &Self::Input) -> Self::Output {
    input.iter().map(|Gift(l, w, h)| {
      let sides = vec![l * w, l * h, w * h];
      let smallest_side = sides.iter().min().unwrap();
      sides.iter().map(|v| *v * 2).sum::<u32>() + smallest_side
    }).sum()
  }

  fn part2(input: &Self::Input) -> Self::Output {
    input.iter().map(|Gift(l, w, h)| -> u32 {
      let mut sides = vec![*l, *w, *h];
      sides.sort();
      2 * sides.iter().take(2).sum::<u32>() + sides.iter().product::<u32>()
    }).sum()
  }
}

#[allow(unused_imports)]
pub mod tests {
  use crate::common::day::Day;
  use super::{Day02};

  #[test]
  fn test_part1() {
    assert_eq!(58, Day02::run_part1("2x3x4"));
    assert_eq!(43, Day02::run_part1("1x1x10"));
  }

  #[test]
  fn test_part2() {
    assert_eq!(34, Day02::run_part2("2x3x4"));
    assert_eq!(14, Day02::run_part2("1x1x10"));
  }

  #[test]
  fn run() {
    Day02::run()
  }
}
