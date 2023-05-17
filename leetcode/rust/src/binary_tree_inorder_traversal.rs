use crate::tree::*;


fn inorder_traversal(root: Tree) -> Vec<i32> {
  let mut v = Vec::new();
  fn walk(node: Tree, v: &mut Vec<i32>) {
    if let Some(r) = node {
      let n = r.borrow();
      walk(n.left.clone(), v);
      v.push(n.val);
      walk(n.right.clone(), v);
    }
  }
  walk(root, &mut v);
  v
}


#[cfg(test)]
pub mod tests {
  use super::*;

  #[test]
  fn test1() {
    assert_eq!(
      inorder_traversal(node(1, None, node(2, leaf(3), None))),
      vec![1, 3, 2]
    );
  }

  #[test]
  fn test2() {
    assert_eq!(inorder_traversal(None), Vec::new());
  }

  #[test]
  fn test3() {
    assert_eq!(
      inorder_traversal(leaf(1)),
      vec![1]
    )
  }
}
