use regex::Regex;
use crate::utils::profile;

const BOUND: usize = 1000;

#[derive(Debug)]
pub enum Command {
  TurnOn((usize, usize), (usize, usize)),
  TurnOff((usize, usize), (usize, usize)),
  Toggle((usize, usize), (usize, usize)),
}
use Command::*;

pub struct Grid<T: Default + Copy> {
  // grid: [[T; BOUND]; BOUND],
  grid: Vec<Vec<T>>,
}

impl <T: Default + Copy> Grid<T> {
  fn new() -> Self {
    Grid {
      grid: vec![vec![T::default(); BOUND]; BOUND],
    }
  }

  fn eval(&mut self, from: (usize, usize), to: (usize, usize), f: fn(T) -> T) {
    let (x1, y1) = from;
    let (x2, y2) = to;
    for x in x1..=x2 {
      for y in y1..=y2 {
        self.grid[y][x] = f(self.grid[y][x])
      }
    }
  }

  fn each<F>(&self, f: &mut F) where F: FnMut(T) {
    for x in 0..BOUND {
      for y in 0..BOUND {
        f(self.grid[y][x]);
      }
    }
  }

  fn reset(&mut self) {
    for x in 0..BOUND {
      for y in 0..BOUND {
        self.grid[y][x] = T::default();
      }
    }
  }
}

pub fn parse_input(input: &str) -> Vec<Command> {
  let turn_on_re = Regex::new(r"^turn\s*on\s*(\d+),(\d+)\s*through\s*(\d+),(\d+)$").unwrap();
  let turn_off_re = Regex::new(r"^turn\s*off\s*(\d+),(\d+)\s*through\s*(\d+),(\d+)$").unwrap();
  let toggle_re = Regex::new(r"^toggle\s*(\d+),(\d+)\s*through\s*(\d+),(\d+)$").unwrap();
  input.trim()
  .split("\n")
  .map(|line| line.trim())
  .filter(|line| !line.is_empty())
  .map(|line| {
    if let Some(captures) = turn_on_re.captures(line) {
      let x1 = captures.get(1).unwrap().as_str().parse::<usize>().unwrap();
      let y1 = captures.get(2).unwrap().as_str().parse::<usize>().unwrap();
      let x2 = captures.get(3).unwrap().as_str().parse::<usize>().unwrap();
      let y2 = captures.get(4).unwrap().as_str().parse::<usize>().unwrap();
      TurnOn((x1, y1), (x2, y2))
    } else if let Some(captures) = turn_off_re.captures(line) {
      let x1 = captures.get(1).unwrap().as_str().parse::<usize>().unwrap();
      let y1 = captures.get(2).unwrap().as_str().parse::<usize>().unwrap();
      let x2 = captures.get(3).unwrap().as_str().parse::<usize>().unwrap();
      let y2 = captures.get(4).unwrap().as_str().parse::<usize>().unwrap();
      TurnOff((x1, y1), (x2, y2))
    } else if let Some(captures) = toggle_re.captures(line) {
      let x1 = captures.get(1).unwrap().as_str().parse::<usize>().unwrap();
      let y1 = captures.get(2).unwrap().as_str().parse::<usize>().unwrap();
      let x2 = captures.get(3).unwrap().as_str().parse::<usize>().unwrap();
      let y2 = captures.get(4).unwrap().as_str().parse::<usize>().unwrap();
      Toggle((x1, y1), (x2, y2))
    } else {
      panic!("Unmatched command pattern: {line}");
    }
  }).collect::<Vec<Command>>()
}

pub fn part1(grid: &mut Grid<bool>, commands: &Vec<Command>) -> u64 {
  for c in commands {
    match c {
      &TurnOn(from, to) => grid.eval(from, to, |_| true),
      &TurnOff(from, to) => grid.eval(from, to, |_| false),
      &Toggle(from, to) => grid.eval(from, to, |b| !b),
    }
  }

  let mut res = 0u64;
  grid.each(&mut |v| {
    if v { res += 1; }
  });
  res
}

pub fn part2(grid: &mut Grid<u32>, commands: &Vec<Command>) -> u64 {
  for c in commands {
    match c {
      &TurnOn(from, to) => grid.eval(from, to, |v| v + 1),
      &TurnOff(from, to) => grid.eval(from, to, |v| if v == 0 { 0 } else { v - 1 }),
      &Toggle(from, to) => grid.eval(from, to, |v| v + 2),
    }
  }
  
  let mut res = 0u64;
  grid.each(&mut |v| {
    res += v as u64;
  });
  res
}

pub fn main() {
  let input = include_str!("../../input/aoc2015/day06.input.txt");
  let commands = parse_input(input);

  let mut grid1 = Grid::<bool>::new();
  let res1 = profile(|| part1(&mut grid1, &commands));
  println!("2015 Day 6, Part 1: {res1}");

  let mut grid2 = Grid::<u32>::new();
  let res2 = profile(|| part2(&mut grid2, &commands));
  println!("2015 Day 6, Part 2: {res2}");
}

pub mod tests {
  use super::*;

  #[test]
  fn test_part1() {

  }
}
