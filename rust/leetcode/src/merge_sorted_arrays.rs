
// By far the easiest solution, but not very "algorithmic"
fn _merge2(nums1: &mut Vec<i32>, m: i32, nums2: &mut Vec<i32>, n: i32) {
  nums1.resize(m as usize, 0);
  nums2.resize(n as usize, 0);
  nums1.append(nums2);
  nums1.sort();
}

fn merge(nums1: &mut Vec<i32>, m: i32, nums2: &mut Vec<i32>, n: i32) {
  let (mut i, mut j) = (m as usize, n as usize);
  while j > 0 {
    if i > 0 && nums1[i-1] > nums2[j-1] {
      nums1[i+j-1] = nums1[i-1];
      i -= 1;
    } else {
      nums1[i+j-1] = nums2[j-1];
      j -= 1;
    }
  }
}


#[cfg(test)]
pub mod tests {
  use super::*;

  #[test]
  fn test1() { 
    let mut v = vec![1,2,3,0,0,0];
    let mut w = vec![2,5,6];
    let z = vec![1,2,2,3,5,6];
    merge(&mut v, 3, &mut w, 3);
    assert_eq!(v, z);
  }

  #[test]
  fn test2() { 
    let mut v = vec![1];
    let mut w = vec![];
    let z = vec![1];
    merge(&mut v, 1, &mut w, 0);
    assert_eq!(v, z);
  }

  #[test]
  fn test3() { 
    let mut v = vec![1,4,7,0,0,0];
    let mut w = vec![2,5,6];
    let z = vec![1,2,4,5,6,7];
    merge(&mut v, 3, &mut w, 3);
    assert_eq!(v, z);
  }
}
