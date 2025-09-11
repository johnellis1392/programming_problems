use std::rc::Rc;
use std::cell::RefCell;

pub type Tree = Option<Rc<RefCell<TreeNode>>>;

#[derive(Debug, PartialEq, Eq)]
pub struct TreeNode {
  pub val: i32,
  pub left: Tree,
  pub right: Tree,
}

pub fn leaf(val: i32) -> Tree {
  Some(Rc::new(RefCell::new(TreeNode {
    val,
    left: None,
    right: None,
  })))
}

pub fn node(val: i32, left: Tree, right: Tree) -> Tree {
  Some(Rc::new(RefCell::new(TreeNode {
    val,
    left,
    right,
  })))
}
