struct Test(&'static [i32], &'static [i32]);

const TESTS: &'static [Test] = &[
  Test(&[1,2,3], &[1,2,4]),
  Test(&[4,3,2,1], &[4,3,2,2]),
  Test(&[9], &[1,0]),
];

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

fn equals(a: &Vec<i32>, b: &Vec<i32>) -> bool {
  if a.len() != b.len() { return false; }
  for i in 0..a.len() {
    if a[i] != b[i] { return false; }
  }
  return true;
}

fn format(v: &Vec<i32>) -> String {
  let mut s = String::new();
  s.push_str("[");
  for i in 0..v.len() {
    if i != 0 { s.push_str(", "); }
    s.push_str(&v[i].to_string());
  }
  s.push_str("]");
  return s;
}

fn main() {
  println!("Running...");
  for Test(i, o) in TESTS {
    let iv = i.to_vec();
    let ov = o.to_vec();
    let r = plus_one(iv);
    if equals(&r, &ov) {
      println!("Success");
    } else {
      println!("Failure: {} != {}", format(&r), format(&ov));
    }
  }
}