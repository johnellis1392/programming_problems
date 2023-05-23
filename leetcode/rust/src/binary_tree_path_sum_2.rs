use crate::tree::*;

fn path_sum(root: Tree, target_sum: i32) -> Vec<Vec<i32>> {
  if root.is_none() { return Vec::new(); }
  let mut path_sums = Vec::new();

  fn f(
    node: Tree,
    stack: &mut Vec<i32>,
    path_sums: &mut Vec<Vec<i32>>,
    target_sum: i32
  ) {
    if let Some(n) = node {
      stack.push(n.borrow().val);
      match (n.borrow().left.clone(), n.borrow().right.clone()) {
        (None, None) => if stack.iter().sum::<i32>() == target_sum {
          path_sums.push(stack.iter().map(|x| *x).collect());
        },
        (left, right) => {
          if left.is_some() { f(left.clone(), stack, path_sums, target_sum); }
          if right.is_some() { f(right.clone(), stack, path_sums, target_sum); }
        }
      }
      stack.pop();
    }
  }

  let mut stack = Vec::<i32>::new();
  f(root, &mut stack, &mut path_sums, target_sum);
  path_sums
}

#[cfg(test)]
pub mod tests {
  use super::*;

  #[test]
  fn test1() {
    assert_eq!(
      path_sum(
        node(5, node(4, node(11, leaf(7), leaf(2)), None), node(8, leaf(13), node(4, leaf(5), leaf(1)))),
        22
      ),
      vec![vec![5,4,11,2], vec![5,8,4,5]]
    )
  }

  #[test]
  fn test2() {
    assert_eq!(
      path_sum(
        node(1, leaf(2), leaf(3)),
        5
      ),
      Vec::<Vec<i32>>::new()
    )
  }

  #[test]
  fn test3() {
    assert_eq!(
      path_sum(
        node(1, leaf(2), None),
        0
      ),
      Vec::<Vec<i32>>::new()
    )
  }
}