
fn search_insert(nums: Vec<i32>, target: i32) -> i32 {
  let mut i: i32 = 0;
  let mut j: i32 = nums.len() as i32;
  while i < j {
    let mid: i32 = (i + j) / 2;
    if nums[mid as usize] == target {
      return mid;
    } else if nums[mid as usize] < target {
      i = mid + 1;
    } else {
      j = mid;
    }
  }
  return i;
}


#[cfg(test)]
pub mod tests {
  use super::*;

  #[test]
  fn test1() { 
    let v = vec![1,3,5,6];
    assert_eq!(search_insert(v, 5), 2);
  }

  #[test]
  fn test2() { 
    let v = vec![1,3,5,6];
    assert_eq!(search_insert(v, 2), 1);
  }

  #[test]
  fn test3() { 
    let v = vec![1,3,5,6];
    assert_eq!(search_insert(v, 7), 4);
  }
}
