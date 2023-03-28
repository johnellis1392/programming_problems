public class AddTwoNumbers {
  public class ListNode {
    int val;
    ListNode next;
    public ListNode() {}
    public ListNode(int val) { this.val = val; }
    public ListNode(int val, ListNode next) { this.val = val; this.next = next; }
  }
  
  public static ListNode addTwoNumbers(ListNode l1, ListNode l2) {
    ListNode result = new ListNode();
    int carry = 0;
    ListNode i = l1, j = l2, k = result;
    while (i != null && j != null) {
      carry = (i.val + j.val) % 10;
      k.val = (int)((i.val + j.val) / 10);
    }
    return result;
  }
  
  public static void main(final String[] args) {
    var l1 = new ListNode(2, new ListNode(4, new ListNode(3)));
    var l2 = new ListNode(5, new ListNode(6, new ListNode(4)));
  }
}
