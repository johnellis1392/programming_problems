// struct Test {
//   haystack: String,
//   needle: String,
//   output: i32,
// }

// impl Test {
//   fn new(haystack: &str, needle: &str, output: i32) -> Test {
//     Test{
//       haystack: haystack.to_owned(),
//       needle: needle.to_owned(),
//       output: output,
//     }
//   }
// }

// const tests: [Test; 4] = [
//   Test::new("sadbutsad", "sad", 0),
//   Test::new("leetcode", "leeto", -1),
//   Test::new("a", "a", 0),
//   Test::new("ba", "a", 1),
// ];

struct Test(&'static str, &'static str, i32);

const TESTS: [Test; 4] = [
  Test("sadbutsad", "sad", 0),
  Test("leetcode", "leeto", -1),
  Test("a", "a", 0),
  Test("ba", "a", 1),
];

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
    .collect<Vec<char>>()
    .windows(needle.len())
    .enumerate()
  {
    if s.into_iter().collect::<String>() == needle {
      return i as i32;
    }
  }
  return -1;
}

fn main() {
  println!("Running...");
  for Test(haystack, needle, output) in TESTS {
    let res = str_str(String::from(haystack), String::from(needle));
    if res == output {
      println!("Success");
    } else {
      println!("Failure: {} != {}", res, output);
    }
  }
}