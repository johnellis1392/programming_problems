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
use crate::common::day::Day;

pub struct Grid<T: Default + Copy> {
  grid: Vec<Vec<T>>,
}

impl<T: Default + Copy> Grid<T> {
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

  fn each<F>(&self, f: &mut F)
    where
      F: FnMut(T),
  {
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


struct Day06;

impl Day for Day06 {
  type Input = Vec<Command>;
  type Output = u64;
  fn day() -> u32 { 6 }
  fn year() -> u32 { 2015 }
  
  fn parse_input(input: String) -> Vec<Command> {
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
  
  fn part1(commands: &Vec<Command>) -> u64 {
    let mut grid: Grid<bool> = Grid::<bool>::new();
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

  fn part2(commands: &Vec<Command>) -> u64 {
    let mut grid: Grid<u32> = Grid::<u32>::new();
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
}

pub mod tests {
  use super::*;

  #[test]
  fn test_part1() {}

  #[test]
  fn run() {
    Day06::run();
  }
}
