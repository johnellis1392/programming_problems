use std::cmp::max;

use crate::tree::*;

fn sorted_array_to_bst(nums: Vec<i32>) -> Tree {
  fn insert(node: Tree, i: i32) -> Tree {
    match node {
      None => leaf(i),
      s@Some(ref mut n) => {
        if i < n.borrow().val {
          n.borrow_mut().left = insert(n.borrow_mut().left, i);
        } else {
          n.borrow_mut().right = insert(n.borrow_mut().right, i);
        }
        s
      }
    }
  }

  fn depth(node: Tree) -> i32 {
    match node {
      None => 0,
      Some(ref n) => 1 + max(depth(n.borrow().left), depth(n.borrow().right))
    }
  }

  fn balance(node: Tree) -> Tree {
    match node {
      None => None,
      s@Some(ref mut n) => {
        let (mut left, mut right) = (
          balance(n.borrow_mut().left.clone()),
          balance(n.borrow_mut().right.clone())
        );
        balance(left);
        balance(right);
        let (dl, dr) = (depth(left), depth(right));
        if dl - dr >= 2 {
          n.left = left.right;
          left.right = s;
          left
        } else if dr - dl >= 2 {
          n.right = right.left;
          right.left = s;
          right
        } else {
          s
        }
      }
    }
  }
}

#[cfg(test)]
pub mod tests {
  use super::*;

  #[test]
  fn test1() {
    let input = vec![1, 2, 3],
    let exp = node(2, leaf(1), leaf(3));
    assert_eq!(exp, sorted_array_to_bst(exp));
  }

  #[test]
  fn test2() {
    let input = vec![-10, -3, 0, 5, 9];
    let exp = node(0, node(-3, leaf(-10), None), node(9, leaf(5), None));
    assert_eq!(exp, sorted_array_to_bst(exp));
  }

  #[test]
  fn test3() {
    let input = vec![1, 3];
    let exp = node(3, leaf(1), None);
    assert_eq!(exp, sorted_array_to_bst(exp));
  }
}
