
fn plus_one(digits: Vec<i32>) -> Vec<i32> {
  let mut v: Vec<i32> = digits.clone();
  for i in (0..v.len()).rev() {
    if v[i] < 9 {
      v[i] = v[i] + 1;
      break;
    } else {
      v[i] = 0;
    }
  }
  if v[0] == 0 {
    v.insert(0, 1);
  }
  return v;
}


#[cfg(test)]
pub mod tests {
  use super::*;

  #[test]
  fn test1() { 
    let v = vec![1,2,3];
    let w = vec![1,2,4];
    assert_eq!(plus_one(v), w);
  }

  #[test]
  fn test2() { 
    let v = vec![4,3,2,1];
    let w = vec![4,3,2,2];
    assert_eq!(plus_one(v), w);
  }

  #[test]
  fn test3() { 
    let v = vec![9];
    let w = vec![1,0];
    assert_eq!(plus_one(v), w);
  }
}
