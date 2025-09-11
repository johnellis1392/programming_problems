
fn length_of_last_word(s: String) -> i32 {
  let bytes: &[u8] = s.as_bytes();
  let mut stack: Vec<char> = Vec::<char>::new();
  let mut i: i32 = (bytes.len() - 1) as i32;
  while i >= 0 && bytes[i as usize].is_ascii_whitespace() { i = i - 1; }
  while i >= 0 && !bytes[i as usize].is_ascii_whitespace() {
    stack.push(bytes[i as usize] as char);
    i = i - 1;
  }
  return stack.len() as i32
}

// A clever, idiomatic solution from LeetCode
fn _length_of_last_word2(s: String) -> i32 {
  match s.trim().split_whitespace().last() {
    Some(w) => w.len() as i32,
    None => 0,
  }
}


#[cfg(test)]
pub mod tests {
  use super::*;

  #[test]
  fn test1() { 
    assert_eq!(length_of_last_word("Hello World".to_string()), 5);
  }

  #[test]
  fn test2() { 
    assert_eq!(length_of_last_word("   fly me   to   the moon  ".to_string()), 4);
  }

  #[test]
  fn test3() { 
    assert_eq!(length_of_last_word("luffy is still joyboy".to_string()), 6);
  }
}
