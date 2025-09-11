use std::fs;
use std::collections::{HashSet};
use std::fmt;
use crate::common::day::Day;

// #[derive(PartialEq, Debug)]
struct Point {
  r: i64,
  c: i64,
}

impl fmt::Display for Point {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "(r={}, c={})", self.r, self.c)
  }
}

impl PartialEq for Point {
  fn eq(&self, other: &Self) -> bool {
    self.r == other.r && self.c == other.c
  }
}

impl Eq for Point {}

struct Grid {
  matrix: Vec<Vec<String>>,
  null_rows: HashSet<usize>,
  null_cols: HashSet<usize>,
  galaxies: Vec<Point>,
  expanse: u64,
}

impl Grid {
  fn new(matrix: Vec<Vec<String>>, expanse: Option<u64>) -> Grid {
    let null_rows: HashSet<usize> = matrix.iter()
      .enumerate()
      .flat_map(|(i, row)| if row.iter().all(|v| v == ".") {
        vec![i]
      } else {
        vec![]
      }).collect::<HashSet<usize>>();

    let null_cols: HashSet<usize> = matrix[0].iter()
      .enumerate()
      .flat_map(|(i, _)| if matrix.iter().all(|row| row[i] == ".") {
        vec![i]
      } else {
        vec![]
      }).collect::<HashSet<usize>>();

    let galaxies: Vec<Point> = matrix.iter()
      .enumerate()
      .flat_map(|(r, row)| {
        row.iter().enumerate().flat_map(|(c, col)| {
          if *col == "#" {
            vec![Point { r: r as i64, c: c as i64 }]
          } else {
            vec![]
          }
        }).collect::<Vec<Point>>()
      }).collect::<Vec<Point>>();

    Grid {
      matrix: matrix,
      null_rows: null_rows,
      null_cols: null_cols,
      galaxies: galaxies,
      expanse: expanse.unwrap_or(2),
    }
  }

  fn shortest_path(&self, from: &Point, to: &Point) -> u64 {
    let mut current = Point { r: from.r, c: from.c };
    let dest = Point { r: to.r, c: to.c };
    let mut steps = 0;
    while current != dest {
      let dr = to.r - current.r;
      let dc = to.c - current.c;
      if dr.abs() > dc.abs() {
        steps += if self.null_rows.contains(&(current.r as usize)) { self.expanse } else { 1 };
        current.r += dr.signum();
      } else {
        steps += if self.null_cols.contains(&(current.c as usize)) { self.expanse } else { 1 };
        current.c += dc.signum();
      }
    }
    steps
  }
}

fn read_input(input: &str) -> Vec<Vec<String>> {
  input.split("\n")
    .map(|line| line.trim())
    .filter(|line| line.len() > 0)
    .map(|line| line.chars().map(|c| c.to_string()).collect())
    .collect()
}


struct Day11;

impl Day for Day11 {
  type Input = String;
  type Output = u64;
  fn day() -> u32 { 11 }
  fn year() -> u32 { 2023 }

  fn parse_input(input: String) -> Self::Input {
    input.trim().to_string()
  }

  fn part1(input: &String) -> u64 {
    let matrix = read_input(input);
    let grid = Grid::new(matrix, None);
    let mut res = 0u64;
    let galaxies = &grid.galaxies;
    let n = galaxies.len();
    for i in 0..n - 1 {
      for j in i + 1..n {
        let d = grid.shortest_path(&galaxies[i], &galaxies[j]);
        res += d as u64;
      }
    }
    res
  }

  fn part2(input: &String) -> u64 {
    let matrix = read_input(input);
    let grid = Grid::new(matrix, Some(1000000));
    let mut res = 0u64;
    let galaxies = &grid.galaxies;
    let n = galaxies.len();
    for i in 0..n - 1 {
      for j in i + 1..n {
        let d = grid.shortest_path(&galaxies[i], &galaxies[j]);
        res += d as u64;
      }
    }
    res
  }
}


pub mod tests {
  use std::fs;
  use crate::aoc2023::day11::Day11;
  use crate::common::day::Day;

  #[test]
  fn test_part1() {
    let input =
      "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....".to_owned();

    Day11::part1(&input);
  }

  #[test]
  fn run() {
    Day11::run()
  }
}
