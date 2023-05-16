use std::rc::Rc;
use std::cell::RefCell;

type Tree = Option<Rc<RefCell<TreeNode>>>;

#[derive(Debug, PartialEq, Eq)]
pub struct TreeNode {
  pub val: i32,
  pub left: Tree,
  pub right: Tree,
}

struct Test(Tree, Tree, bool);

fn node(v: i32, l: Tree, r: Tree) -> Tree {
  Some(Rc::new(RefCell::new(TreeNode {
    val: v,
    left: l,
    right: r,
  })))
}

fn tests() -> Vec<Test> {
  vec![
    Test(
      node(1, node(2, None, None), node(3, None, None)),
      node(1, node(2, None, None), node(3, None, None)),
      true
    ),
    Test(
      node(1, node(2, None, None), None),
      node(1, None, node(2, None, None)),
      false
    ),
    Test(
      node(1, node(2, None, None), node(1, None, None)),
      node(1, node(1, None, None), node(2, None, None)),
      false
    ),
  ]
}

fn is_same_tree(p: Tree, q: Tree) -> bool {
  match (p, q) {
    (None, None) => true,
    (Some(pn), Some(qn)) if pn.borrow().val == qn.borrow().val => {
      is_same_tree(pn.borrow().left.clone(), qn.borrow().left.clone()) &&
      is_same_tree(pn.borrow().right.clone(), qn.borrow().right.clone())
    },
    _ => false,
  }
}

fn main() {
  println!("Running...");
  for Test(a, b, o) in tests() {
    let r = is_same_tree(a, b);
    if r == o {
      println!("Success");
    } else {
      println!("Failure: {} != {}", r, o);
    }
  }
}
