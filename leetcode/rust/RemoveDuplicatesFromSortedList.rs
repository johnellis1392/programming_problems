#[derive(PartialEq, Eq, Clone, Debug)]
struct ListNode {
  pub val: i32,
  pub next: Option<Box<ListNode>>,
}

// impl ListNode {
//   #[inline]
//   fn new(val: i32) -> Self {
//     ListNode {
//       next: None,
//       val
//     }
//   }
// }

fn to_string(list: &Option<Box<ListNode>>) -> String {
  fn f(l: &Option<Box<ListNode>>) -> String {
    match l {
      None => "".to_string(),
      Some(node) => format!(", {}{}", node.val.to_string(), f(&node.next)),
    }
  }

  match list {
    None => "[]".to_string(),
    Some(node) => {
      let mut s = String::new();
      s.push_str("[");
      s.push_str(&node.val.to_string());
      s.push_str(&f(&node.next));
      s.push_str("]");
      s
    }
  }
}

fn equals(a: &Option<Box<ListNode>>, b: &Option<Box<ListNode>>) -> bool {
  match (a, b) {
    (None, None) => true,
    (Some(av), Some(bv)) if av == bv => equals(&av.next, &bv.next),
    _ => false,
  }
}

struct Test(&'static [i32], &'static [i32]);

const TESTS: &[Test] = &[
  Test(&[1,1,2], &[1,2]),
  Test(&[1,1,2,3,3], &[1,2,3]),
];

fn to_list(a: &[i32]) -> Option<Box<ListNode>> {
  match a {
    [] => None,
    [head, tail@..] => Some(Box::new(ListNode {
      val: *head,
      next: to_list(tail),
    }))
  }
}

// Another solution on LeetCode, using while loop pattern matching
fn delete_duplicates2(head: Option<Box<ListNode>>) -> Option<Box<ListNode>> {
  if head.is_none() { return None; }
  let mut h: Option<Box<ListNode>> = head;
  let mut p1: &mut Box<ListNode> = h.as_mut().unwrap();
  while let Some(p2) = p1.next.as_mut() {
    if p1.val == p2.val {
      p1.next == p2.next.take();
    } else {
      p1 = p1.next.as_mut().unwrap();
    }
  }
  h
}

fn delete_duplicates(head: Option<Box<ListNode>>) -> Option<Box<ListNode>> {
  fn f(node: Option<Box<ListNode>>, last: i32) -> Option<Box<ListNode>> {
    match node {
      None => None,
      Some(v) if v.val == last => f(v.next, last),
      Some(v) => Some(Box::new(ListNode {
        val: v.val,
        next: f(v.next, v.val),
      }))
    }
  }
  
  match head {
    None => None,
    Some(v) => Some(Box::new(ListNode {
      val: v.val,
      next: f(v.next, v.val)
    }))
  }
}

fn main() {
  println!("Running...");
  for Test(i, o) in TESTS {
    let r = delete_duplicates(to_list(i));
    let ov = to_list(o);
    if equals(&r, &ov) {
      println!("Success");
    } else {
      println!("Failure: {} != {}", to_string(&r), to_string(&ov));
      // println!("Failure: ");
    }
  }
}
