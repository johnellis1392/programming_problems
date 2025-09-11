use crate::tree::*;

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


#[cfg(test)]
pub mod tests {
  use super::*;

  #[test]
  fn test1() {
    assert_eq!(
      is_same_tree( 
        node(1, leaf(2), leaf(3)),
        node(1, leaf(2), leaf(3))
      ),
      true
    );
  }

  #[test]
  fn test2() {
    assert_eq!(
      is_same_tree(
        node(1, leaf(2), None),
        node(1, None, leaf(2))
      ),
      false
    );
  }

  #[test]
  fn test3() {
    assert_eq!( 
      is_same_tree( 
        node(1, leaf(2), leaf(1)),
        node(1, leaf(1), leaf(2))
      ),
      false
    );
  }
}
