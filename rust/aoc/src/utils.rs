use std::time::SystemTime;

pub fn profile<F, R>(f: F) -> R where F: FnOnce() -> R {
  let before = SystemTime::now();
  let result = f();
  let after = before.elapsed().unwrap();
  let mut sb = String::new();
  if after.as_secs() != 0 {
    sb.push_str(&format!("{}s.", after.as_secs()));
  }
  if after.as_millis() != 0 {
    sb.push_str(&format!("{}ms.", after.as_millis() % 1_000));
  }
  sb.push_str(&format!("{}ns", after.as_nanos() % 1_000_000));
  println!("Time: {sb}");
  result
}
