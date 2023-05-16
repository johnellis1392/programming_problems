use std::rc::Rc;
use std::cell::RefCell;

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
    right: None
  })))
}

fn node(val: i32, left: Tree, right: Tree) -> Tree {
  Some(Rc::new(RefCell::new(TreeNode {
    val,
    left,
    right
  })))
}

struct Test(Tree, bool);

fn tests() -> Vec<Test> {
  vec![
    Test(
      node(1, node(2, leaf(3), leaf(4)), node(2, leaf(4), leaf(3))),
      true
    ),
    Test(
      node(1, node(2, None, leaf(3)), node(2, None, leaf(3))),
      false
    )
  ]
}

fn is_symmetric(root: Tree) -> bool {
  fn walk(i: Tree, j: Tree) -> bool {
    match (i, j) {
      (None, None) => true,
      (Some(i), Some(j)) => {
        let (i, j) = (i.borrow(), j.borrow());
        if i.val != j.val {
          false
        } else {
          walk(i.left.clone(), j.right.clone()) && 
          walk(i.right.clone(), j.left.clone())
        }
      },
      _ => false
    }
  }

  if let Some(node) = root {
    walk(node.borrow().left.clone(), node.borrow().right.clone())
  } else {
    true
  }
}

fn main() {
  println!("Running...");
  for Test(root, o) in tests() {
    let res = is_symmetric(root);
    if res == o {
      println!("Success");
    } else {
      println!("Failure: {} != {}", res, o);
    }
  }
}
