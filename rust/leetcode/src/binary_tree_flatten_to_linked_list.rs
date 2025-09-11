use std::collections::VecDeque;
use crate::tree::*;

// fn flatten(root: &mut Tree) {
//   fn f(node: &mut Tree) -> (Tree, Tree) {
//     if let Some(ref n) = node {
//       let n = n.borrow();
//       match (n.left, n.right) {
//         (None, None) => (None, None),
//         (Some(left), None) => {
//           let (head, tail) = f(&mut Some(left));
//           n.left = None;
//           n.right = tail;
//           (node.clone(), tail)
//         },
//         (None, Some(right)) => {
//           let (head, tail) = f(&mut Some(right));
//           n.left = None;
//           n.right = head;
//           (node.clone(), tail)
//         },
//         (Some(left), Some(right)) => {
//           let (mut lhead, mut ltail) = f(&mut Some(left));
//           let (mut rhead, rtail) = f(&mut Some(right));
//           n.left = None;
//           n.right = lhead;
//           if let Some(mut ltail) = ltail {
//             ltail.borrow().right = rhead;
//           } else {
//             n.right = rhead;
//           }
//           (node.clone(), rtail)
//         }
//       }
//     } else {
//       (None, None)
//     }
//   }
//   let (_, _) = f(&mut root);
//   // head
// }

fn flatten(root: &mut Tree) {
  if root.is_none() { return; }
  let mut q = VecDeque::<Tree>::new();
  fn f(node: Tree, q: &mut VecDeque<Tree>) {
    if let Some(n) = node {
      q.push_back(Some(n));
      f(n.borrow().left.clone(), q);
      f(n.borrow().right.clone(), q);
    }
  }

  f(root.clone(), &mut q);
  let Some(next) = q.pop_back();
  while let Some(Some(mut node)) = q.pop_back() {
    let mut n = node.borrow();
    n.left = None;
    n.right = next;
    next = Some(node);
  }
}


#[cfg(test)]
pub mod tests {
  use super::*;

  #[test]
  fn test1() {
    let mut input = node(1, node(2, leaf(3), leaf(4)), node(5, None, leaf(6)));
    flatten(&mut input);
    assert_eq!(
      input,
      node(1, None, node(2, None, node(3, None, node(4, None, node(5, None, leaf(6))))))
    )
  }

  #[test]
  fn test2() {
    let mut input = None;
    flatten(&mut input);
    assert_eq!(input, None);
  }

  #[test]
  fn test3() {
    let mut input = leaf(0);
    flatten(&mut input);
    assert_eq!(input, leaf(0))
  }
}