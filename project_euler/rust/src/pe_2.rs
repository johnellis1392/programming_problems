use num::bigint::*;

fn n(x: u64) -> BigUint {
  x.to_biguint().expect("Failed to convert number")
}

pub fn pe_2(x: u64) -> BigUint {
  if x == 0 {
    return n(0_u64);
  } else if x == 1 {
    return n(1_u64);
  }
  let mut sum = n(0_u64);
  let (mut i, mut j) = (0_u64, 1_u64);
  loop {
    let k = i + j;
    if k >= x { break; }
    i = j;
    j = k;
    if j % 2 == 0 { sum += n(j); }
  }
  return sum
}

#[cfg(test)]
pub mod tests {
  use super::*;

  #[test]
  fn test_pe_2() {
    let i = 90_u64;
    let j = n(44_u64);
    assert_eq!(pe_2(i), j);
  }
}