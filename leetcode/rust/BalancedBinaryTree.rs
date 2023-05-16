use std::rc::Rc;
use std::cell::RefCell;
use std::cmp::max;

type Tree = Option<Rc<RefCell<TreeNode>>>;

#[derive(Debug, PartialEq, Eq)]
struct TreeNode {
  val: i32,
  left: Tree,
  right: Tree,
}

fn leaf(val: i32) -> Tree {
  Some(Rc::new(RefCell::new(TreeNode {
    val,
    left: None,
    right: None,
  })))
}

fn node(val: i32, left: Tree, right: Tree) -> Tree {
  Some(Rc::new(RefCell::new(TreeNode {
    val,
    left,
    right,
  })))
}

struct Test(Tree, bool);

fn tests() -> Vec<Test> {
  vec![
    Test(
      node(3, leaf(9), node(20, leaf(15), leaf(7))),
      true
    ),
    Test(
      node(1, node(2, node(3, leaf(4), leaf(4)), leaf(3)), leaf(2)),
      false
    ),
    Test(
      None,
      true
    )
  ]
}

// fn depth(node: Tree) -> i32 {
//   match node {
//     None => 0,
//     Some(ref cell) => 1 + max(
//       depth(cell.borrow().left.clone()),
//       depth(cell.borrow().right.clone())
//     )
//   }
// }

// fn is_balanced(root: Tree) -> bool {
//   match root {
//     None => true,
//     Some(ref cell) => {
//       let c = cell.borrow();
//       let (dl, dr) = (depth(c.left.clone()), depth(c.right.clone()));
//       if (dl - dr).abs() > 1 {
//         false
//       } else {
//         is_balanced(c.left.clone()) && is_balanced(c.right.clone())
//       }
//     }
//   }
// }

fn depth(node: &Tree) -> i32 {
  match node {
    None => 0,
    Some(ref cell) => 1 + max(
      depth(&cell.borrow().left),
      depth(&cell.borrow().right)
    )
  }
}

fn is_balanced(root: &Tree) -> bool {
  match root {
    None => true,
    Some(ref cell) => {
      let c = cell.borrow();
      let (dl, dr) = (depth(&c.left), depth(&c.right));
      if (dl - dr).abs() > 1 {
        false
      } else {
        is_balanced(&c.left) && is_balanced(&c.right)
      }
    }
  }
}

fn main() {
  println!("Running...");
  for Test(root, o) in tests() {
    let b = is_balanced(&root);
    if b == o {
      println!("Success");
    } else {
      println!("Failure: {} != {}", b, o);
    }
  }
}
