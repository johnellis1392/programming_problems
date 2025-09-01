type Gift = (u32, u32, u32);

#[allow(dead_code)]
fn parse_input(input: &str) -> Vec<Gift> {
  input.lines()
  .map(|line| line.trim())
  .map(|line| {
    let v = line.split("x").collect::<Vec<&str>>();
    match v[..] {
      [l, w, h] => (
        l.parse::<u32>().unwrap(),
        w.parse::<u32>().unwrap(),
        h.parse::<u32>().unwrap()
      ),
      _ => panic!("illegal pattern: {line}"),
    }
  }).collect()
}

#[allow(dead_code)]
pub fn part1(input: &Vec<Gift>) -> u32 {
  input.iter().map(|(l, w, h)| {
    let sides = vec![l * w, l * h, w * h];
    let smallest_side = sides.iter().min().unwrap();
    sides.iter().map(|v| *v * 2).sum::<u32>() + smallest_side
  }).sum()
}

#[allow(dead_code)]
pub fn part2(input: &Vec<Gift>) -> u32 {
  input.iter().map(|(l, w, h)| -> u32 {
    let mut sides = vec![*l, *w, *h];
    sides.sort();
    2 * sides.iter().take(2).sum::<u32>() + sides.iter().product::<u32>()
  }).sum()
}

#[allow(dead_code)]
pub fn main() {
  let input = include_str!("../../input/aoc2015/day02.input.txt");
  let gifts = parse_input(input);
  let res1 = part1(&gifts);
  println!("2015 Day 2, Part 1: {res1}");
  let res2 = part2(&gifts);
  println!("2015 Day 2, Part 2: {res2}");
}

#[allow(unused_imports)]
pub mod tests {
  use super::{part1, part2, parse_input};

  #[test]
  fn test_part1() {
    assert_eq!(58, part1(&parse_input("2x3x4")));
    assert_eq!(43, part1(&parse_input("1x1x10")));
  }

  #[test]
  fn test_part2() {
    assert_eq!(34, part2(&parse_input("2x3x4")));
    assert_eq!(14, part2(&parse_input("1x1x10")));
  }
}
