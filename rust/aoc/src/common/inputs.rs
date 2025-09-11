use reqwest::*;
use std::fs;
use std::path::PathBuf;

fn days() -> Vec<u32> {
  (1..=25).collect::<Vec<u32>>()
}

fn years() -> Vec<u32> {
  (2015..=2025).collect::<Vec<u32>>()
}

fn session_id() -> String {
  // env::var("SESSION_ID").unwrap()
  fs::read_to_string(".session_id").unwrap()
}

pub fn input_path_for(day: u32, year: u32) -> PathBuf {
  PathBuf::from(&format!("./inputs/{year}/day{day}.txt"))
}

pub fn fetch_input_for_day(day: u32, year: u32) {
  let path = input_path_for(day, year);
  let client = reqwest::blocking::Client::new();
  let input = client.get(format!("https://adventofcode.com/{year}/day/{day}/input"))
    .header("Cookie", format!("session={}", session_id()))
    .send()
    .unwrap()
    .text()
    .unwrap();

  fs::create_dir_all(path.parent().unwrap()).unwrap();
  fs::write(path, input).unwrap();
}

pub fn read_input_for_day(day: u32, year: u32) -> String {
  let path = input_path_for(day, year);
  if !path.exists() {
    fetch_input_for_day(day, year);
  }
  fs::read_to_string(path).unwrap()
}

pub fn fetch_input_for_year(year: u32) {
  for day in days() {
    fetch_input_for_day(day, year);
  }
}

pub fn fetch_all_inputs() {
  for year in years() {
    fetch_input_for_year(year);
  }
}


mod tests {
  use crate::common::inputs::fetch_input_for_day;

  #[test]
  fn test1() {
    fetch_input_for_day(1, 2015);
  }
}
