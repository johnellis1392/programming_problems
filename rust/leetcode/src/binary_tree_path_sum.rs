use std::collections::VecDeque;

use crate::tree::*;


fn has_path_sum(root: Tree, target_sum: i32) -> bool {
  if root.is_none() { return false; }
  let mut queue = VecDeque::new();
  queue.push_back((root, 0));
  while let Some((Some(node), sum)) = queue.pop_front() {
    let node = node.borrow();
    match (node.left.clone(), node.right.clone()) {
      (None, None) => {
        if node.val + sum == target_sum {
          return true;
        } else {
          continue;
        }
      },
      (Some(l), None) => queue.push_back((Some(l), sum + node.val)),
      (None, Some(r)) => queue.push_back((Some(r), sum + node.val)),
      (Some(l), Some(r)) => {
        queue.push_back((Some(l), sum + node.val));
        queue.push_back((Some(r), sum + node.val));
      },
    }
  }
  false
}


#[cfg(test)]
pub mod tests {
  use super::*;

  #[test]
  fn test1() {
    assert_eq!(
      has_path_sum(node(5, node(4, node(11, leaf(7), leaf(2)), None), node(8, leaf(13), node(4, None, leaf(1)))), 22),
      true
    );
  }

  #[test]
  fn test2() {
    assert_eq!(
      has_path_sum(node(1, leaf(2), leaf(3)), 5),
      false
    );
  }

  #[test]
  fn test3() {
    assert_eq!(
      has_path_sum(node(1, leaf(2), None), 1),
      false
    );
  }
}
