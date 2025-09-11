class SinglyLinkedListNode():
  data: int
  next = None
  def __init__(self, v) -> None:
    self.data = v

class LinkedList():
  head: SinglyLinkedListNode = None

def insertNodeAtHead(head, data):
  node = SinglyLinkedListNode(data)
  if head is None:
    return node
  else:
    node.next = head
    return node

def printAll(node):
  n = node
  while n is not None:
    print(n.data)
    n = n.next

if __name__ == '__main__':
  filename = './input.txt'
  with open(filename) as f:
    n = int(f.readline().rstrip())
    ll = LinkedList()
    for _ in range(n):
      v = int(f.readline().rstrip())
      ll.head = insertNodeAtHead(ll.head, v)
    printAll(ll.head)