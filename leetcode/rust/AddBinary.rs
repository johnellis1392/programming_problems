// use std::Math;
use std::cmp;

struct Test(&'static str, &'static str, &'static str);

const TESTS: &[Test] = &[
  Test("11", "1", "100"),
  Test("1010", "1011", "10101"),
  Test("0", "0", "0"),
  Test("10100000100100110110010000010101111011011001101110111111111101000000101111001110001111100001101", "110101001011101110001111100110001010100001101011101010000011011011001011101111001100000011011110011", "110111101100010011000101110110100000011101000101011001000011011000001100011110011010010011000000000"),
];

struct _Solution1 {}
impl _Solution1 {
  fn _str_to_bin(s: &String) -> u32 {
    let s2 = s.bytes().rev().map(|c| (c as u32) - ('0' as u32));
    let mut n: u32 = 0;
    for (i, c) in s2.enumerate() {
      n += c * u32::pow(2, i as u32);
    }
    return n
  }

  fn _bin_to_str(n: u32) -> String {
    if n == 0 { return "0".to_string(); }
    let mut s = String::new();
    let mut i = n;
    while i > 0 {
      s.insert_str(0, &(i % 2).to_string());
      i >>= 1;
    }
    return s
  }

  fn _add_binary(a: String, b: String) -> String {
    Self::_bin_to_str(
      (Self::_str_to_bin(&a) + Self::_str_to_bin(&b)) as u32
    )
  }
}

struct _Solution2 {}
impl _Solution2 {
  // This was another clever solution on LeetCode
  pub fn _add_binary(a: String, b: String) -> String {
    let mut a = a.as_bytes().iter().rev().map(|b| b - '0' as u8).peekable();
    let mut b = b.as_bytes().iter().rev().map(|b| b - '0' as u8).peekable();

    let mut out = Vec::with_capacity(cmp::max(a.len(), b.len()) + 1);
    let mut carry = 0;

    while carry != 0 || a.peek().is_some() || b.peek().is_some() {
      let ai = a.next().unwrap_or(0);
      let bi = b.next().unwrap_or(0);
      let sum = ai + bi + carry;
      out.push((sum & 0b01) + '0' as u8);
      carry = sum >> 1;
    }

    out.reverse();
    String::from_utf8(out).unwrap()

    // They did this; I don't know why...
    // unsafe { String::from_utf8_unchecked(out) }
  }
}

fn add_binary(a: String, b: String) -> String {
  let n = cmp::max(a.len(), b.len());
  let (mut x, mut y) = (vec![0; n], vec![0; n]);
  for (i, c) in a.chars().rev().enumerate() { x[n-i-1] = if c == '0' { 0 } else { 1 }; }
  for (i, c) in b.chars().rev().enumerate() { y[n-i-1] = if c == '0' { 0 } else { 1 }; }

  let mut c = 0;
  let mut z = vec![0; n];
  for i in 0..n {
    let j = x[n-i-1] + y[n-i-1] + c;
    z[n-i-1] = j % 2;
    c = j / 2;
  }

  if c > 0 { z.insert(0, c); }
  return z.iter().map(|c| if *c == 0 { '0' } else { '1' }).collect()
}

fn main() {
  println!("Running...");
  for Test(a, b, o) in TESTS {
    let r = add_binary(a.to_string(), b.to_string());
    if r == o.to_string() {
      println!("Success");
    } else {
      println!("Failure: \"{}\" != \"{}\"", r, o);
    }
  }
}