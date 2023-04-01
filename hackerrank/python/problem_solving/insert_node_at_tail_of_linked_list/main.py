class LinkedListNode():
  data: int
  next = None
  def __init__(self, v) -> None:
    self.data = v

class LinkedList():
  head: LinkedListNode = None
  def insert_node(self, v):
    node = LinkedListNode(v)
    if self.head is None:
      self.head = node
    else:
      i = self.head
      while i.next is not None:
        i = i.next
      i.next = node

def insertNodeAtTail(head: LinkedListNode, data: int):
  node = LinkedListNode(data)
  if head is None:
    return node
  else:
    n = head
    while n.next is not None:
      n = n.next
    n.next = node
    return head

def printAll(ll: LinkedList):
  node = ll.head
  while node is not None:
    print(node.data)
    node = node.next

if __name__ == '__main__':
  filename = './input.txt'
  with open(filename) as f:
    n = int(f.readline().rstrip())
    
    ll = LinkedList()
    for _ in range(n):
      item = int(f.readline().rstrip())
      ll.head = insertNodeAtTail(ll.head, item)
    printAll(ll)