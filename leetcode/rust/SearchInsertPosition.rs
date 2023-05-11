struct Test(&'static [i32], i32, i32);

const TESTS: [Test; 3] = [
  Test(&[1,3,5,6], 5, 2),
  Test(&[1,3,5,6], 2, 1),
  Test(&[1,3,5,6], 7, 4),
];

fn search_insert(nums: Vec<i32>, target: i32) -> i32 {
  let mut i: i32 = 0;
  let mut j: i32 = nums.len() as i32;
  while i < j {
    let mid: i32 = (i + j) / 2;
    if nums[mid as usize] == target {
      return mid;
    } else if nums[mid as usize] < target {
      i = mid + 1;
    } else {
      j = mid;
    }
  }
  return i;
}

fn main() {
  println!("Running...");
  for Test(input, target, output) in TESTS {
    let v = input.to_vec();
    let res = search_insert(v, target);
    if res == output {
      println!("Success");
    } else {
      println!("Failure: {} != {}", res, output);
    }
  }
}
