use std::fs;
use std::fmt;
use std::cmp::{max};
use std::ops::{Index};
use std::collections::{HashSet};
use std::cmp::{PartialEq};

#[derive(Eq, Hash, PartialEq)]
enum Direction {
  North,
  South,
  East,
  West,
}

impl fmt::Display for Direction {
  fn fmt(&self, f: &mut ::std::fmt::Formatter) -> fmt::Result {
    let s = match self {
      Direction::North => "north",
      Direction::South => "south",
      Direction::East => "east",
      Direction::West => "west",
    };
    write!(f, "{}", s)
  }
}

impl Clone for Direction {
  fn clone(&self) -> Direction {
    *self
  }
}

impl Copy for Direction { }

#[derive(Eq, Hash, PartialEq)]
struct Point {
  r: i32,
  c: i32,
}

impl Point {
  fn new(r: i32, c: i32) -> Self {
    Point { r: r, c: c }
  }

  fn north(&self) -> Self { Point::new(self.r - 1, self.c) }
  fn south(&self) -> Self { Point::new(self.r + 1, self.c) }
  fn east(&self) -> Self { Point::new(self.r, self.c + 1) }
  fn west(&self) -> Self { Point::new(self.r, self.c - 1) }

  fn mv(&self, dir: &Direction) -> Self {
    match *dir {
      Direction::North => self.north(),
      Direction::South => self.south(),
      Direction::East => self.east(),
      Direction::West => self.west(),
    }
  }
}

impl Clone for Point {
  fn clone(&self) -> Point {
    Point::new(self.r, self.c)
  }
}

impl Copy for Point { }

#[derive(Eq, Hash, PartialEq)]
struct Vector {
  p: Point,
  d: Direction,
}

impl Vector {
  fn new(p: Point, d: Direction) -> Self {
    Vector { p: p, d: d }
  }
}

impl Copy for Vector {}
impl Clone for Vector {
  fn clone(&self) -> Self {
    Vector::new(self.p, self.d)
  }
}

struct Grid {
  matrix: Vec<Vec<String>>,
}

impl Index<&Point> for Grid {
  type Output = String;
  fn index<'a>(&'a self, p: &Point) -> &'a String {
    &self.matrix[p.r as usize][p.c as usize]
  }
}

impl Grid {
  fn new(matrix: Vec<Vec<String>>) -> Grid {
    Grid {
      matrix: matrix,
    }
  }

  fn height(&self) -> i32 { self.matrix.len() as i32 }
  fn width(&self) -> i32 { self.matrix[0].len() as i32 }
  fn valid(&self, p: &Point) -> bool { 0 <= p.r && p.r < self.height() && 0 <= p.c && p.c < self.width() }

  fn mv(&self, p: &Point, dir: &Direction) -> Vec<Vector> {
    match (&self[&p][..], dir) {
      ("/", Direction::North) => vec!(Vector::new(p.east(), Direction::East)),
      ("/", Direction::South) => vec!(Vector::new(p.west(), Direction::West)),
      ("/", Direction::East) => vec!(Vector::new(p.north(), Direction::North)),
      ("/", Direction::West) => vec!(Vector::new(p.south(), Direction::South)),

      ("\\", Direction::North) => vec!(Vector::new(p.west(), Direction::West)),
      ("\\", Direction::South) => vec!(Vector::new(p.east(), Direction::East)),
      ("\\", Direction::East) => vec!(Vector::new(p.south(), Direction::South)),
      ("\\", Direction::West) => vec!(Vector::new(p.north(), Direction::North)),

      ("-", Direction::North) => vec!(Vector::new(p.east(), Direction::East), Vector::new(p.west(), Direction::West)),
      ("-", Direction::South) => vec!(Vector::new(p.east(), Direction::East), Vector::new(p.west(), Direction::West)),
      ("-", Direction::East) => vec!(Vector::new(p.east(), Direction::East)),
      ("-", Direction::West) => vec!(Vector::new(p.west(), Direction::West)),

      ("|", Direction::North) => vec!(Vector::new(p.north(), Direction::North)),
      ("|", Direction::South) => vec!(Vector::new(p.south(), Direction::South)),
      ("|", Direction::East) => vec!(Vector::new(p.north(), Direction::North), Vector::new(p.south(), Direction::South)),
      ("|", Direction::West) => vec!(Vector::new(p.north(), Direction::North), Vector::new(p.south(), Direction::South)),

      (".", _) => vec!(Vector::new(p.mv(dir), dir.clone())),
      _ => vec!(Vector::new(p.mv(dir), dir.clone())),
    }
  }
}

impl fmt::Display for Grid {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", self.matrix.iter().map(|row| row.join("")).collect::<Vec<String>>().join("\n"))
  }
}

fn read_input(input: &String) -> Grid {
  let matrix = input.trim()
    .split('\n')
    .map(|line| line.trim())
    .filter(|line| line.len() > 0)
    .map(|s| s.chars().map(|c| c.to_string()).collect::<Vec<String>>())
    .collect::<Vec<Vec<String>>>();
  Grid::new(matrix)
}

fn part1(input: &String) -> u64 {
  let grid = read_input(input);
  let origin = Point::new(0, 0);
  let original_heading = Direction::East;
  let mut visited = HashSet::<Vector>::new();
  let mut queue = Vec::<Vector>::new();
  queue.push(Vector::new(origin, original_heading));

  while !queue.is_empty() {
    let v = queue.remove(0);
    let current = &v.p;
    let heading = &v.d;
    if grid.valid(current) && !visited.contains(&v) {
      visited.insert(v);
      let mut points = grid.mv(current, heading);
      queue.append(&mut points);
    }
  }

  visited.iter().map(|v| v.p).collect::<HashSet<Point>>().len() as u64
}

fn part2(input: &String) -> u64 {
  let grid = read_input(input);
  let width = grid.width();
  let height = grid.height();
  let mut origins = Vec::<Vector>::new();
  for r in 0..height {
    origins.push(Vector::new(Point::new(r, 0), Direction::East));
    origins.push(Vector::new(Point::new(r, width - 1), Direction::West));
  }
  for c in 0..width {
    origins.push(Vector::new(Point::new(0, c), Direction::South));
    origins.push(Vector::new(Point::new(height - 1, c), Direction::North));
  }

  let mut visited = HashSet::<Vector>::new();
  let mut queue = Vec::<Vector>::new();

  let mut res = 0u64;
  for vector in origins {
    let origin = vector.p;
    let original_heading = vector.d;
    queue.push(Vector::new(origin, original_heading));

    while !queue.is_empty() {
      let v = queue.remove(0);
      let current = &v.p;
      let heading = &v.d;
      if grid.valid(current) && !visited.contains(&v) {
        visited.insert(v);
        let mut points = grid.mv(current, heading);
        queue.append(&mut points);
      }
    }

    let energy = visited.iter().map(|v| v.p).collect::<HashSet<Point>>().len() as u64;
    res = max(res, energy);

    visited.clear();
    queue.clear();
  }

  res
}

fn main() {
  let filename = "input.txt";
  let debug = false;
  let test_input = r"
    .|...\....
    |.-.\.....
    .....|-...
    ........|.
    ..........
    .........\
    ..../.\\..
    .-.-/..|..
    .|....-|.\
    ..//.|....
  ";

  let input = if debug { 
    test_input.to_owned()
  } else {
    fs::read_to_string(filename).expect("An error occurred while reading file")
  };

  println!("2023 Day 16, Part 1: {}", part1(&input));
  println!("2023 Day 16, Part 2: {}", part2(&input));
}
