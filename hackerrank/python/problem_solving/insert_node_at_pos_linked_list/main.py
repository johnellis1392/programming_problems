class SinglyLinkedListNode():
  data: int
  next = None
  def __init__(self, v) -> None:
    self.data = v

def append(head, v):
  node = SinglyLinkedListNode(v)
  if head is None:
    return node
  else:
    n = head
    while n.next is not None:
      n = n.next
    n.next = node
  return head

def insertNodeAtPosition(head, v, pos):
  if pos == 0:
    node = SinglyLinkedListNode(v)
    node.next = head
    return node

  i = 0
  n = head
  print('head = %d' % n.data)
  while n.next is not None and i+1 < pos:
    print(n.data)
    n = n.next
    i += 1
  print()
  node = SinglyLinkedListNode(v)
  node.next = n.next
  n.next = node
  return head
  

def printAll(node):
  n = node
  while n is not None:
    print(n.data)
    n = n.next

if __name__ == '__main__':
  filename = './input.txt'
  with open(filename) as f:
    n = int(f.readline().rstrip())
    ll = None
    for _ in range(n):
      ll = append(ll, int(f.readline().rstrip()))
    v = int(f.readline().rstrip())
    pos = int(f.readline().rstrip())
    printAll(ll)
    ll = insertNodeAtPosition(ll, v, pos)
    printAll(ll)