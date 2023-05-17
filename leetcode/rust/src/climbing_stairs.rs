
fn climb_stairs(n: i32) -> i32 {
  if n < 3 { return n; }
  let mut buf = vec![0; 1 + n as usize];
  buf[1] = 1; buf[2] = 2;
  for i in 3..=n {
    let j = i as usize;
    buf[j] = buf[j-1] + buf[j-2];
  }
  return buf[n as usize];
}


#[cfg(test)]
pub mod tests {
  use super::*;

  #[test]
  fn test1() {
    assert_eq!(climb_stairs(1), 1);
  }

  #[test]
  fn test2() {
    assert_eq!(climb_stairs(2), 2);
  }

  #[test]
  fn test3() {
    assert_eq!(climb_stairs(3), 3);
  }

  #[test]
  fn test4() {
    assert_eq!(climb_stairs(4), 5);
  }
}
