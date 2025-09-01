use regex::Regex;
use std::collections::HashMap;

#[derive(Debug)]
enum Target {
  Const(u64),
  Ref(String),
}
use Target::*;

#[derive(Debug)]
enum Circuit {
  Send(Target, String),
  And(Target, Target, String),
  Or(Target, Target, String),
  LShift(Target, Target, String),
  RShift(Target, Target, String),
  Not(Target, String),
}
use Circuit::*;

impl Circuit {
  fn sink(&self) -> &String {
    match self {
      Send(_, sink) => sink,
      And(_, _, sink) => sink,
      Or(_, _, sink) => sink,
      LShift(_, _, sink) => sink,
      RShift(_, _, sink) => sink,
      Not(_, sink) => sink,
    }
  }
}

fn parse_target(s: &str) -> Target {
  match s.parse::<u64>() {
    Ok(v) => Const(v),
    Err(_) => Ref(s.to_string()),
  }
}

fn parse_input(input: &str) -> Vec<Circuit> {
  let send_re = Regex::new(r"^(\d+|[a-z]+) -> ([a-z]+)$").unwrap();
  let and_re = Regex::new(r"^(\d+|[a-z]+) AND (\d+|[a-z]+) -> ([a-z]+)$").unwrap();
  let or_re = Regex::new(r"^(\d+|[a-z]+) OR (\d+|[a-z]+) -> ([a-z]+)$").unwrap();
  let lshift_re = Regex::new(r"^(\d+|[a-z]+) LSHIFT (\d+|[a-z]+) -> ([a-z]+)$").unwrap();
  let rshift_re = Regex::new(r"^(\d+|[a-z]+) RSHIFT (\d+|[a-z]+) -> ([a-z]+)$").unwrap();
  let not_re = Regex::new(r"^NOT (\d+|[a-z]+) -> ([a-z]+)$").unwrap();

  input.lines().map(|line| line.trim()).filter(|line| !line.is_empty())
  .map(|line| {
    if let Some(c) = send_re.captures(line) {
      let source = parse_target(c.get(1).unwrap().as_str());
      let sink = c.get(2).unwrap().as_str().to_string();
      Send(source, sink)
    } else if let Some(c) = and_re.captures(line) {
      let left = parse_target(c.get(1).unwrap().as_str());
      let right = parse_target(c.get(2).unwrap().as_str());
      let sink = c.get(3).unwrap().as_str().to_string();
      And(left, right, sink)
    } else if let Some(c) = or_re.captures(line) {
      let left = parse_target(c.get(1).unwrap().as_str());
      let right = parse_target(c.get(2).unwrap().as_str());
      let sink = c.get(3).unwrap().as_str().to_string();
      Or(left, right, sink)
    } else if let Some(c) = lshift_re.captures(line) {
      let left = parse_target(c.get(1).unwrap().as_str());
      let right = parse_target(c.get(2).unwrap().as_str());
      let sink = c.get(3).unwrap().as_str().to_string();
      LShift(left, right, sink)
    } else if let Some(c) = rshift_re.captures(line) {
      let left = parse_target(c.get(1).unwrap().as_str());
      let right = parse_target(c.get(2).unwrap().as_str());
      let sink = c.get(3).unwrap().as_str().to_string();
      RShift(left, right, sink)
    } else if let Some(c) = not_re.captures(line) {
      let target = parse_target(c.get(1).unwrap().as_str());
      let sink = c.get(2).unwrap().as_str().to_string();
      Not(target, sink)
    } else {
      panic!("Unmatched pattern: {line}");
    }
  }).collect::<Vec<Circuit>>()
}

fn eval_target<'a>(
  target: &'a Target,
  circuit_map: &'a HashMap<&'a String, &'a Circuit>,
  cache: &mut HashMap<&'a String, u64>
) -> u64 {
  match target {
    Ref(s) => eval(circuit_map[&s], circuit_map, cache),
    Const(v) => *v,
  }
}

fn eval<'a>(
  circuit: &'a Circuit,
  circuit_map: &'a HashMap<&'a String, &'a Circuit>,
  cache: &mut HashMap<&'a String, u64>
) -> u64 {
  let sink = circuit.sink();
  if cache.contains_key(sink) {
    cache[sink]
  } else {
    let res = match circuit {
      Send(v, _) =>
        eval_target(v, circuit_map, cache),
      And(left, right, _) => 
        eval_target(left, circuit_map, cache) & eval_target(right, circuit_map, cache),
      Or(left, right, _) =>
        eval_target(left, circuit_map, cache) | eval_target(right, circuit_map, cache),
      LShift(left, right, _) => 
        eval_target(left, circuit_map, cache) << eval_target(right, circuit_map, cache),
      RShift(left, right, _) =>
        eval_target(left, circuit_map, cache) >> eval_target(right, circuit_map, cache),
      Not(v, _) =>
        !eval_target(v, circuit_map, cache),
    };
    cache.insert(sink, res);
    res
  }
}

fn part1(circuit_map: &HashMap<&String, &Circuit>) -> u64 {
  let mut cache = HashMap::<&String, u64>::new();
  let a: String = "a".to_string();
  
  let start = circuit_map[&a];
  eval(start, &circuit_map, &mut cache)
}

pub fn main() {
  let input = include_str!("../../input/aoc2015/day07.input.txt");
  let circuits = parse_input(input);
  let mut circuit_map = HashMap::<&String, &Circuit>::new();
  for c in &circuits { circuit_map.insert(c.sink(), c); }

  println!("Running...");
  let res1 = part1(&circuit_map);
  println!("2015 Day 7, Part 1: {res1}");

  let b = "b".to_string();
  let new_b = &Send(Const(res1), b.clone());
  circuit_map.insert(&b, new_b);
  let res2 = part1(&circuit_map);
  println!("2015 Day 7, Part 2: {res2}");
}
