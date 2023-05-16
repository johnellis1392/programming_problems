use std::rc::Rc;
use std::cell::RefCell;
use std::cmp::max;

type Tree = Option<Rc<RefCell<TreeNode>>>;

#[derive(Debug, PartialEq, Eq)]
pub struct TreeNode {
  pub val: i32,
  pub left: Tree,
  pub right: Tree,
}

fn leaf(val: i32) -> Tree {
  Some(Rc::new(RefCell::new(TreeNode {
    val,
    left: None,
    right: None,
  })))
}

fn node(val: i32, left: Tree, right: Tree) -> Tree {
  Some(Rc::new(RefCell::new(TreeNode {
    val,
    left,
    right,
  })))
}

struct Test(Tree, i32);

fn tests() -> Vec<Test> {
  vec![
    Test(
      node(3, leaf(9), node(20, leaf(15), leaf(7))),
      3
    ),
    Test(
      node(1, None, leaf(2)),
      2
    ),
  ]
}

fn max_depth(root: Tree) -> i32 {
  match root {
    None => 0,
    Some(node) => {
      1 + max(
        max_depth(node.borrow().left.clone()),
        max_depth(node.borrow().right.clone())
      )
    }
  }
}

fn main() {
  println!("Running...");
  for Test(root, d) in tests() {
    let res = max_depth(root);
    if res == d {
      println!("Success");
    } else {
      println!("Failure: {} != {}", res, d);
    }
  }
}
