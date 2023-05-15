struct Test(i32, i32);

const TESTS: &[Test] = &[
  Test(1, 1),
  Test(2, 2),
  Test(3, 3),
  Test(4, 5),
];

fn climb_stairs(n: i32) -> i32 {
  if n < 3 { return n; }
  let mut buf = vec![0; 1 + n as usize];
  buf[1] = 1; buf[2] = 2;
  for i in 3..=n {
    let j = i as usize;
    buf[j] = buf[j-1] + buf[j-2];
  }
  return buf[n as usize];
}

fn main() {
  println!("Running...");
  for Test(n, o) in TESTS {
    let r = climb_stairs(*n);
    if r == *o {
      println!("Success");
    } else {
      println!("Failure: {} != {}", r, *o);
    }
  }
}
