struct Test {
  nums1: &'static [i32],
  m: i32,
  nums2: &'static [i32],
  n: i32,
  o: &'static [i32],
}

const TESTS: &'static [Test] = &[
  Test{nums1: &[1,2,3,0,0,0], m: 3, nums2: &[2,5,6], n: 3, o: &[1,2,2,3,5,6]},
  Test{nums1: &[1], m: 1, nums2: &[], n: 0, o: &[1]},
  Test{nums1: &[1,4,7,0,0,0], m: 3, nums2: &[2,5,6], n: 3, o: &[1,2,4,5,6,7]},
];

fn format(v: &Vec<i32>) -> String {
  format!("[{}]", v.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(", "))
}

fn equal(a: &Vec<i32>, b: &Vec<i32>) -> bool {
  if a.len() != b.len() { return false; }
  for i in 0..a.len() {
    if a[i] != b[i] {
      return false;
    }
  }
  return true
}

// By far the easiest solution, but not very "algorithmic"
fn merge2(nums1: &mut Vec<i32>, m: i32, nums2: &mut Vec<i32>, n: i32) {
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

fn main() {
  println!("Running...");
  for t in TESTS {
    let mut nums1 = t.nums1.to_vec();
    let mut nums2 = t.nums2.to_vec();
    merge(&mut nums1, t.m, &mut nums2, t.n);
    let ov = t.o.to_vec();
    if equal(&nums1, &ov) {
      println!("Success");
    } else {
      println!("Failure: {} != {}", format(&nums1), format(&t.o.to_vec()));
    }
  }
}