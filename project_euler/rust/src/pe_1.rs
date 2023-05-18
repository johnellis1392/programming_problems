pub fn pe_1(x: i32) -> i32 {
  let mut sum = 0;
  for i in 1..x {
    if i % 3 == 0 || i % 5 == 0 {
      sum += i;
    }
  }
  sum
}

#[cfg(test)]
pub mod tests {
  use super::*;

  #[test]
  fn test_pe_1() {
    assert_eq!(pe_1(10), 23);
  }
}
