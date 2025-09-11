from bisect import insort
from collections import deque, Counter
from datetime import datetime

# def nops(q, k):
#   i, finished = 0, 0
#   while len(q) > 1 and q[0] < k:
#     a, b = q.popleft(), q.popleft()
#     c = a + b*2
#     i += 1
#     if c >= k:
#       finished += 1
#     insort(q, c)
#   if finished > 0:
#     return i
#   else:
#     return -1

def cookies(K, A):
  i = 0
  q, counter = deque(), Counter(A)
  for v in counter.keys():
    insort(q, v)
  while len(q) >= 1 and (len(counter) > 1 or counter[q[0]] > 1) and q[0] < K:
    print(K, q, counter)
    a = q.popleft()
    n = counter[a]
    if n == 1:
      i += 1
      del counter[a]
      b = q.popleft()
      m = counter[b]
      v = a+b*2
      if m == 1:
        del counter[b]
        # print('check 1')
        # insort(q, v)
      else:
        counter[b] -= 1
        q.appendleft(b)
      if v in counter:
        counter[v] += 1
      else:
        counter[v] = 1
        # print('check 2')
        insort(q, v)
    elif n % 2 == 0:
      del counter[a]
      b = a+a*2
      m = n//2
      i += m
      if b in counter:
        counter[b] += m
      else:
        counter[b] = m
        insort(q, b)
    else:
      b = a+a*2
      m = (n-1)//2
      i += m
      if b in counter:
        counter[b] += m
      else:
        counter[b] = m
        insort(q, b)
      counter[a] = 1
      q.appendleft(a)
  print(K, q, counter)
  if len(q) == 1 and q[0] < K and counter[q[0]] == 1:
    return -1
  elif len(q) == 1 and q[0] < K and counter[q[0]] > 1:
    return i+1
  else:
    return i

if __name__ == '__main__':
  # filename = './input.txt'
  filename = './input20.txt'
  # filename = './input03.txt'
  st = datetime.now()
  with open(filename) as f:
    [n, k] = list(map(int, f.readline().rstrip().split()))
    # q = deque()
    # for i in map(int, f.readline().rstrip().split()):
    #   insort(q, i)
    a = map(int, f.readline().rstrip().split())
    print(cookies(k, a))
  et = datetime.now()
  dt = et-st
  print('Time Delta: ', dt)