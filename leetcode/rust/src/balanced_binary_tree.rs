use std::cmp::max;

use crate::tree::*;

fn depth(node: &Tree) -> i32 {
  match node {
    None => 0,
    Some(ref cell) => 1 + max(
      depth(&cell.borrow().left),
      depth(&cell.borrow().right)
    )
  }
}

fn is_balanced(root: &Tree) -> bool {
  match root {
    None => true,
    Some(ref cell) => {
      let c = cell.borrow();
      let (dl, dr) = (depth(&c.left), depth(&c.right));
      if (dl - dr).abs() > 1 {
        false
      } else {
        is_balanced(&c.left) && is_balanced(&c.right)
      }
    }
  }
}


#[cfg(test)]
pub mod tests {
  use super::*;
  
  #[test]
  fn test1() {
    assert_eq!(
      is_balanced(&node(3, leaf(9), node(20, leaf(15), leaf(7)))),
      true
    );
  }

  #[test]
  fn test2() {
    assert_eq!(
      is_balanced(&node(1, node(2, node(3, leaf(4), leaf(4)), leaf(3)), leaf(2))),
      false
    );
  }

  #[test]
  fn test3() {
    assert_eq!(
      is_balanced(&None),
      true
    );
  }
}
