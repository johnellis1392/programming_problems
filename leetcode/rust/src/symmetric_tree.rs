use crate::tree::*;

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


#[cfg(test)]
pub mod tests {
  use super::*;

  #[test]
  fn test1() {
    assert_eq!(
      is_symmetric(node(1, node(2, leaf(3), leaf(4)), node(2, leaf(4), leaf(3)))),
      true
    );
  }

  #[test]
  fn test2() {
    assert_eq!(
      is_symmetric(node(1, node(2, None, leaf(3)), node(2, None, leaf(3)))),
      false
    );
  }
}
