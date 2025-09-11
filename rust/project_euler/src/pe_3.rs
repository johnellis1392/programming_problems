use std::cmp::max;

pub fn pe_3(x: u64) -> u64 {
  let mut n: u64 = x;
  let mut i: u64 = 2;
  let mut j: u64 = f64::sqrt(n as f64) as u64;
  'outer: loop {
    for k in i..=j {
      if n % k == 0 {
        n = n / k;
        j = f64::sqrt(n as f64) as u64;
        continue 'outer;
      }
    }
    break;
  }
  return n
}

#[cfg(test)]
pub mod tests {
  use super::*;

  #[test]
  fn test_pe_3() {
    // Factors of 13195 are 5, 7, 13, and 29
    assert_eq!(pe_3(13195), 29);
  }
}