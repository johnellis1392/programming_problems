from datetime import datetime
from bisect import insort

CAPACITY = 100000
class Heap():
  def __init__(self):
    self.heap = [None] * CAPACITY
    self.n = 0
  
  def heapify(self, pos):
    i = pos
    while True:
      largest = i
      l = i*2+1
      r = i*2+2
      if l < self.n and self.heap[l] < self.heap[largest]:
        largest = l
      if r < self.n and self.heap[r] < self.heap[largest]:
        largest = r
      if largest != i:
        self.heap[i], self.heap[largest] = self.heap[largest], self.heap[i]
        # self.heapify(largest)
        i = largest
      else:
        break

  def insert(self, v):
    self.heap[self.n] = v
    self.n += 1
    i = self.n-1
    while i > 0 and v < self.heap[(i-1)//2]:
      temp = self.heap[(i-1)//2]
      self.heap[(i-1)//2] = self.heap[i]
      self.heap[i] = temp
      i = (i-1)//2

  def delete(self, v):
    i = 0
    while i < self.n and self.heap[i] != v:
      i += 1
    if i >= self.n:
      return

    if (self.n == 1 and heap[0] == v):
      self.heap[0] = None
      self.n -= 1
    elif i == self.n-1:
      self.heap[self.n-1] = None
      self.n -= 1
    else:
      self.heap[i] = self.heap[self.n-1]
      self.heap[self.n-1] = None
      self.n -= 1
      self.heapify(i)

  def head(self):
    if self.n == 0:
      return None
    else:
      return self.heap[0]

# def main(qs):
#   heap = []
#   for q in qs:
#     if q[0] == 1: # Insert
#       insert(heap, q[1])
#     elif q[0] == 2: # Delete
#       delete(heap, q[1])
#     else: # Print Min
#       print(heap[0])
#     # print(q, heap)

# if __name__ == '__main__':
#   # filename = './input.txt'
#   # filename = './input.2.txt'
#   # filename = './input.3.txt'
#   # filename = './input.4.txt'
#   # filename = './input.5.txt'
#   filename = './input.6.txt'
#   # starttime = datetime.now()
#   with open(filename) as f:
#     nq = int(f.readline().rstrip())
#     heap = Heap()
#     for _ in range(nq):
#       q = list(map(int, f.readline().rstrip().split()))
#       if q[0] == 1: # Insert
#         heap.insert(q[1])
#       elif q[0] == 2: # Delete
#         heap.delete(q[1])
#       else: # Print Min
#         print(heap.head())
#   endtime = datetime.now()
#   # timedelta = endtime - starttime
#   # print('Time Delta: ', timedelta)


if __name__ == '__main__':
  # filename = './input.txt'
  # filename = './input.2.txt'
  # filename = './input.3.txt'
  # filename = './input.4.txt'
  # filename = './input.5.txt'
  filename = './input.6.txt'
  starttime = datetime.now()
  with open(filename) as f:
    nq = int(f.readline().rstrip())
    heap = []
    for _ in range(nq):
      q = list(map(int, f.readline().rstrip().split()))
      if q[0] == 1: # Insert
        insort(heap, q[1])
      elif q[0] == 2: # Delete
        heap.pop(heap.index(q[1]))
      else: # Print Min
        print(heap[0])
  endtime = datetime.now()
  timedelta = endtime - starttime
  print('Time Delta: ', timedelta)