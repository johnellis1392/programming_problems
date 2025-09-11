
fn str_str(haystack: String, needle: String) -> i32 {
  if needle.len() == 0 || needle.len() > haystack.len() {
    return -1;
  }

  let n = haystack.len();
  let m = needle.len();
  for i in 0..n-m+1 {
    if haystack[i..i+m] == needle {
      return i as i32;
    }
  }

  return -1
}

// Another, more memory-efficient solution from LeetCode
fn _str_str2(haystack: String, needle: String) -> i32 {
  let mut step = 0;
  loop {
    if step + needle.len() > haystack.len() {
      return -1i32
    }
    let check = &haystack[step..needle.len() + step].to_string();
    if check.eq(&needle) {
      return step as i32
    }
    step += 1;
  }
}

// Another, weirder solution
fn _str_str3(haystack: String, needle: String) -> i32 {
  for (i, s) in haystack
    .chars()
    .collect::<Vec<char>>()
    .windows(needle.len())
    .enumerate()
  {
    if s.into_iter().collect::<String>() == needle {
      return i as i32;
    }
  }
  return -1;
}


#[cfg(test)]
pub mod tests {
  use super::*;

  #[test]
  fn test1() { 
    assert_eq!(str_str("sadbutsad".to_string(), "sad".to_string()), 0);
  }

  #[test]
  fn test2() { 
    assert_eq!(str_str("leetcode".to_string(), "leeto".to_string()), -1);
  }

  #[test]
  fn test3() { 
    assert_eq!(str_str("a".to_string(), "a".to_string()), 0);
  }

  #[test]
  fn test4() { 
    assert_eq!(str_str("ba".to_string(), "a".to_string()), 1);
  }
}
