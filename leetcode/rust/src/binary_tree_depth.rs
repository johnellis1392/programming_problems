use std::cmp::max;

use crate::tree::*;

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


#[cfg(test)]
pub mod tests {
  use super::*;

  #[test]
  fn test1() {
    assert_eq!(
      max_depth(node(3, leaf(9), node(20, leaf(15), leaf(7)))),
      3
    );
  }
      
  #[test]
  fn test2() {
    assert_eq!(
      max_depth(node(1, None, leaf(2))),
      2
    );
  }
}
