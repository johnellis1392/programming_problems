use md5;
use std::time::SystemTime;

#[allow(dead_code)]
pub fn part1(input: &str) -> u32 {
  for i in 0.. {
    let s = format!("{input}{i}");
    let hash = format!("{:x}", md5::compute(s));
    if hash.starts_with("00000") {
      return i;
    }
  }
  0
}

#[allow(dead_code)]
pub fn part2(input: &str) -> u32 {
  for i in 0.. {
    let s = format!("{input}{i}");
    let hash = format!("{:x}", md5::compute(s));
    if hash.starts_with("000000") {
      return i;
    }
  }
  0
}

#[allow(dead_code)]
fn profile<F, R>(f: F) -> R where F: Fn() -> R {
  let before = SystemTime::now();
  let res = f();
  let after = before.elapsed().unwrap();
  let mut sb = String::new();
  if after.as_secs() != 0 {
    sb.push_str(&format!("{}s.", after.as_secs()));
  }
  if after.as_millis() != 0 {
    sb.push_str(&format!("{}ms.", after.as_millis() % 1_000));
  }
  sb.push_str(&format!("{}ns", after.as_nanos() % 1_000_000));
  println!("Time: {sb}");
  res
}


#[allow(dead_code)]
pub fn main() {
  let input = "iwrupvqb";
  println!("Running...");
  let res1 = profile(|| part1(input));
  println!("2015 Day 4, Part 1: {res1}");
  let res2 = part2(input);
  println!("2015 Day 4, Part 2: {res2}");
}
