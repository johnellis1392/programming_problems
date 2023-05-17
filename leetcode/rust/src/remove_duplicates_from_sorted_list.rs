use crate::list::*;


// Another solution on LeetCode, using while loop pattern matching
fn _delete_duplicates2(head: Option<Box<ListNode>>) -> Option<Box<ListNode>> {
  if head.is_none() { return None; }
  let mut h: Option<Box<ListNode>> = head;
  let mut p1: &mut Box<ListNode> = h.as_mut().unwrap();
  while let Some(p2) = p1.next.as_mut() {
    if p1.val == p2.val {
      p1.next = p2.next.take();
    } else {
      p1 = p1.next.as_mut().unwrap();
    }
  }
  h
}

fn delete_duplicates(head: Option<Box<ListNode>>) -> Option<Box<ListNode>> {
  fn f(node: Option<Box<ListNode>>, last: i32) -> Option<Box<ListNode>> {
    match node {
      None => None,
      Some(v) if v.val == last => f(v.next, last),
      Some(v) => Some(Box::new(ListNode {
        val: v.val,
        next: f(v.next, v.val),
      }))
    }
  }
  
  match head {
    None => None,
    Some(v) => Some(Box::new(ListNode {
      val: v.val,
      next: f(v.next, v.val)
    }))
  }
}


#[cfg(test)]
pub mod tests {
  use super::*;

  #[test]
  fn test1() { 
    let v = slice_to_list(&[1,1,2]);
    let w = slice_to_list(&[1,2]);
    assert_eq!(delete_duplicates(v), w);
  }

  #[test]
  fn test2() { 
    let v = slice_to_list(&[1,1,2,3,3]);
    let w = slice_to_list(&[1,2,3]);
    assert_eq!(delete_duplicates(v), w);
  }
}
