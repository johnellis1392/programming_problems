struct Test(&'static str, i32);

const TESTS: [Test; 3] = [
  Test("Hello World", 5),
  Test("   fly me   to   the moon  ", 4),
  Test("luffy is still joyboy", 6),
];

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
fn length_of_last_word2(s: String) -> i32 {
  match s.trim().split_whitespace().last() {
    Some(w) => w.len() as i32,
    None => 0,
  }
}

fn main() {
  println!("Running...");
  for Test(s, exp) in TESTS {
    let res = length_of_last_word(String::from(s));
    if res == exp {
      println!("Success")
    } else {
      println!("Failure: {} != {}", res, exp)
    }
  }
}
