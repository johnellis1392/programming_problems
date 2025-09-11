// use regex::Regex;

use crate::common::day::Day;
use crate::utils::profile;

trait Nice {
  fn is_nice(&self) -> bool;
  fn is_nicer(&self) -> bool;
}

impl Nice for String {
  fn is_nice(&self) -> bool {
    let bytes = self.as_bytes();
    let mut contains_doubles = false;
    let mut vowel_count = 0;
    let mut contains_badstr = false;

    for i in 0..self.len()-1 {
      if bytes[i] == bytes[i+1] {
        contains_doubles = true;
      }

      if matches!(bytes[i], b'a' | b'e' | b'i' | b'o' | b'u') {
        vowel_count += 1;
      }

      match [bytes[i], bytes[i+1]] {
        [b'a', b'b'] => { contains_badstr = true; break; }
        [b'c', b'd'] => { contains_badstr = true; break; }
        [b'p', b'q'] => { contains_badstr = true; break; }
        [b'x', b'y'] => { contains_badstr = true; break; }
        _ => {},
      }
    }

    if matches!(bytes[self.len()-1], b'a' | b'e' | b'i' | b'o' | b'u') {
      vowel_count += 1;
    }
  
    contains_doubles && (vowel_count >= 3) && !contains_badstr
  }

  fn is_nicer(&self) -> bool {
    let mut contains_repeat = false;
    let mut contains_triple = false;
    let bytes = self.as_bytes();

    'outer:
    for i in 0..self.len()-3 {
      for j in i+2..self.len()-1 {
        if bytes[i..i+2] == bytes[j..j+2] {
          contains_repeat = true;
          break 'outer;
        }
      }
    }

    for i in 2..self.len() {
      if bytes[i-2] == bytes[i] {
        contains_triple = true;
        break;
      }
    }

    contains_repeat && contains_triple
  }
}


struct Day05;

impl Day for Day05 {
  type Input = Vec<String>;
  type Output = usize;
  fn day() -> u32 { 5 }
  fn year() -> u32 { 2015 }

  fn parse_input(input: String) -> Vec<String> {
    input.trim()
      .lines()
      .map(|line| line.trim())
      .filter(|line| !line.is_empty())
      .map(|line| line.to_string())
      .collect()
  }
  
  fn part1(strs: &Vec<String>) -> usize {
    strs.iter().filter(|s| s.to_string().is_nice()).count()
  }

  fn part2(strs: &Vec<String>) -> usize {
    strs.iter().filter(|s| s.to_string().is_nicer()).count()
  }
}

pub mod tests {
  use super::*;

  #[test]
  fn test_part1() {
    assert_eq!(true, "ugknbfddgicrmopn".to_owned().is_nice());
    assert_eq!(true, "aaa".to_owned().is_nice());
    assert_eq!(false, "jchzalrnumimnmhp".to_owned().is_nice());
    assert_eq!(false, "haegwjzuvuyypxyu".to_owned().is_nice());
    assert_eq!(false, "dvszwmarrgswjxmb".to_owned().is_nice());
  }

  #[test]
  fn test_part2() {
    assert_eq!(true, "qjhvhtzxzqqjkmpb".to_string().is_nicer());
    assert_eq!(true, "xxyxx".to_string().is_nicer());
    assert_eq!(false, "uurcxstgmygtbstg".to_string().is_nicer());
    assert_eq!(false, "ieodomkazucvgmuy".to_string().is_nicer());
  }
  
  #[test]
  fn run() {
    Day05::run();
  }
}
