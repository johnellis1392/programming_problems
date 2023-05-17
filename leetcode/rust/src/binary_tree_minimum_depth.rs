use std::collections::VecDeque;
use std::cmp::min;

use crate::tree::*;


// Solution from LeetCode using a deque and graph search rather than recursion.
struct _Solution {}
impl _Solution {
  pub fn _min_depth(root: Tree) -> i32 {
    if root.is_none() { return 0; }
    let mut queue = VecDeque::new();
    queue.push_front((1, root));
    let mut depth = 0;
    while let Some((d, Some(node))) = queue.pop_front() {
      let node = node.borrow();
      depth = d;
      match (node.left.clone(), node.right.clone()) {
        (None, None) => break,
        (None, Some(r)) => queue.push_back((d + 1, Some(r))),
        (Some(l), None) => queue.push_back((d + 1, Some(l))),
        (Some(l), Some(r)) => {
          queue.push_back((d + 1, Some(l)));
          queue.push_back((d + 1, Some(r)));
        }
      }
    }
    depth
  }
}

fn min_depth(node: Tree) -> i32 {
  fn is_leaf(node: &Tree) -> bool {
    match node {
      Some(ref n) => 
        n.borrow().left.is_none() && 
        n.borrow().right.is_none(),
      _ => false,
    }
  }
  
  fn f(node: &Tree, d: i32) -> i32 {
    match node {
      None => i32::MAX,
      Some(_) if is_leaf(node) => d,
      Some(ref n) => min(
          f(&n.borrow().left, d + 1),
          f(&n.borrow().right, d + 1)
        ),
    }
  }

  match node {
    None => 0,
    _ => f(&node, 1)
  }
}


#[cfg(test)]
pub mod tests {
  use super::*;

  #[test]
  fn test1() { 
    assert_eq!(min_depth(node(3, leaf(9), node(20, leaf(15), leaf(7)))), 2);
  }

  #[test]
  fn test2() { 
    assert_eq!(min_depth(node(2, None, node(3, None, node(4, None, node(5, None, leaf(6)))))), 5);
  }
}
