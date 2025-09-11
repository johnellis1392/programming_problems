class SinglyLinkedListNode():
  data: int
  next = None
  def __init__(self, v) -> None:
    self.data = v
    self.next = None

class SinglyLinkedList():
  head: SinglyLinkedListNode
  def __init__(self):
    self.head = None
  
  def insert_node(self, v):
    node = SinglyLinkedListNode(v)
    i = self.head
    if i is None:
      self.head = node
    else:
      while i.next is not None:
        i = i.next
      i.next = node

def printLinkedList(node: SinglyLinkedListNode):
  i = node
  while i is not None:
    print(i.data)
    i = i.next

if __name__ == '__main__':
  filename = './input.txt'
  with open(filename) as f:
    n = int(f.readline().rstrip())
    llist = SinglyLinkedList()
    for _ in range(n):
      llist.insert_node(int(f.readline().rstrip()))
    printLinkedList(llist.head)