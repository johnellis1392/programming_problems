use md5;
use std::time::SystemTime;
use crate::common::day::Day;
use crate::utils::profile;

struct Day04;

impl Day for Day04 {
  type Input = String;
  type Output = u32;
  fn day() -> u32 { 4 }
  fn year() -> u32 { 2015 }

  fn parse_input(input: String) -> Self::Input {
    input.trim().to_string()
  }
  
  fn part1(input: &String) -> u32 {
    for i in 0.. {
      let s = format!("{input}{i}");
      let hash = format!("{:x}", md5::compute(s));
      if hash.starts_with("00000") {
        return i;
      }
    }
    0
  }

  fn part2(input: &String) -> u32 {
    for i in 0.. {
      let s = format!("{input}{i}");
      let hash = format!("{:x}", md5::compute(s));
      if hash.starts_with("000000") {
        return i;
      }
    }
    0
  }
}



pub mod tests {
  use crate::aoc2015::day04::Day04;
  use crate::common::day::Day;
  use crate::utils::profile;

  #[test]
  fn run() {
    let input = "iwrupvqb";
    let res1 = profile(|| Day04::run_part1(input));
    let res2 = Day04::run_part2(input);
  }
}
