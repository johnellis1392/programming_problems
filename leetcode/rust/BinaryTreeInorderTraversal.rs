use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, PartialEq, Eq)]
pub struct TreeNode {
  pub val: i32,
  pub left: Option<Rc<RefCell<TreeNode>>>,
  pub right: Option<Rc<RefCell<TreeNode>>>,
}

impl TreeNode {
  #[inline]
  pub fn new(val: i32) -> Self {
    TreeNode {
      val,
      left: None,
      right: None,
    }
  }
}

struct Test(Option<Rc<RefCell<TreeNode>>>, Vec<i32>);

fn tests() -> Vec<Test> {
  vec![
    Test(
      Some(Rc::new(RefCell::new(TreeNode {
        val: 1,
        left: None,
        right: Some(Rc::new(RefCell::new(TreeNode {
          val: 2,
          left: Some(Rc::new(RefCell::new(TreeNode { val: 3, left: None, right: None, }))),
          right: None,
        })))
      }))),
      vec![1, 3, 2],
    ),
    Test(None, vec![],),
    Test(
      Some(Rc::new(RefCell::new(TreeNode {
        val: 1,
        left: None,
        right: None,
      }))),
      vec![1],
    )
  ]
}

fn equals(a: &Vec<i32>, b: &Vec<i32>) -> bool {
  if a.len() != b.len() { return false; }
  for i in 0..a.len() {
    if a[i] != b[i] { return false; }
  }
  return true
}

fn f(v: &Vec<i32>) -> String {
  let mut s = String::new();
  s.push_str("[");
  s.push_str(&v.iter().map(|i| i.to_string()).collect::<Vec<_>>().join(", "));
  s.push_str("]");
  return s
}

fn inorder_traversal(root: Option<Rc<RefCell<TreeNode>>>) -> Vec<i32> {
  let mut v = Vec::new();
  fn walk(node: Option<Rc<RefCell<TreeNode>>>, v: &mut Vec<i32>) {
    if let Some(r) = node {
      let n = r.borrow();
      walk(n.left.clone(), v);
      v.push(n.val);
      walk(n.right.clone(), v);
    }
  }
  walk(root, &mut v);
  v
}

fn main() {
  println!("Running...");
  for Test(i, v) in tests() {
    let r = inorder_traversal(i);
    if equals(&v, &r) {
      println!("Success");
    } else {
      println!("Failure: {} != {}", f(&r), f(&v));
    }
  }
}
