type List = Option<Box<ListNode>>;

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ListNode {
  pub val: i32,
  pub next: List,
}

impl ListNode {
  #[inline]
  pub fn _new(val: i32) -> Self {
    ListNode {
      next: None,
      val
    }
  }
}

pub fn slice_to_list(a: &[i32]) -> List {
  match a {
    [] => None,
    [head, tail@..] => Some(Box::new(ListNode {
      val: *head,
      next: slice_to_list(tail),
    }))
  }
}
