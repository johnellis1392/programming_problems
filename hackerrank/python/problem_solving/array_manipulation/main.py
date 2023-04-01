# import math
# import os
# import random
# import re
# import sys
from collections import defaultdict

def arrayManipulation_v1(n, queries):
  '''V1, using array splicing'''
  arr = [0] * n
  for [a, b, k] in queries:
    arr[a-1:b] = map(lambda x: x+k, arr[a-1:b])
  return max(m)

def arrayManipulation_v2(n, queries):
  '''V2, using array assignment'''
  arr = [0] * n
  m = 0
  for [a, b, k] in queries:
    print([a, b, k])
    for i in range(a, b+1):
      c = arr[i-1]+k
      arr[i-1] = c
      m = max(m, c)
  return m

def arrayManipulation_v3(n, queries):
  '''V3, using a dictionary to store points. Still too slow.'''
  d = defaultdict(int)
  m = 0
  for [a, b, k] in queries:
    for i in range(a, b+1):
      d[i] += k
      m = max(m, d[i])
  return m

def overlaps(r1, r2):
  (x1, y1), (x2, y2) = r1, r2
  return not (y2<x1 or x2>y1 or x1>y2 or y1<x2)

def contains_range(x1, y1, x2, y2):
  return (x1 <= x2) and (y2 <= y1)

def split_range(x1, y1, x2, y2):
  if x1 == x2:
    if y1 < y2:
      return [(x1, y1), (y1+1, y2)]
    else:
      return [(x1, y2), (y2+1, y1)]
  elif y1 == y2:
    if x1 < x2:
      return [(x1, x2-1), (x2, y1)]
    else:
      return [(x2, x1), (x1+1, y1)]
  elif contains_range(x1, y1, x2, y2):
    return [(x1, x2-1), (x2, y2), (y2+1, y1)]
  elif contains_range(x2, y2, x1, y1):
    return [(x2, x1-1), (x1, y1), (y1+1, y2)]
  elif (x1 <= x2 and x2 <= y1):
    return [(x1, x2-1), (x2, y1), (y1+1, y2)]
  else:
    return [(x2, x1-1), (x1, y2), (y2+1, y1)]

def arrayManipulation_v4(n, queries):
  '''
  V4, using ranges as indices in range map. The idea here is
  to have a dictionary like this:

  d[(a, b)] = v

  where (a, b) is some range, and v is the value for that range.
  The logic would need to vary to split ranges by their overlapping
  values.
  '''
  d = dict()
  m = 0
  for [a, b, k] in queries:
    s = list(filter(lambda x: overlaps(x, (a, b)), d.keys()))
    if len(s) == 0:
      d[a, b] = k
    else:
      for x, y in s:
        v = d[x, y]
        del d[x, y]
        for i, j in split_range(a, b, x, y):
          if contains_range(x, y, i, j):
            d[i, j] = v + k
            m = max(m, v+k)
          else:
            d[i, j] = v
  return m
  # return max(d.values())

def arrayManipulation(n, queries):
  arr = [0] * (n+1)
  m = 0
  for [a, b, k] in queries:
    arr[a] += k
    if b+1 <= n:
      arr[b+1] -= k
    # scan(arr)
  # Scan
  s = 0
  for i in range(0, n+1):
    s = s + arr[i]
    m = max(s, m)
  # print()
  # print(arr)
  return m

if __name__ == '__main__':
  # filename = './input.txt'
  filename = './input.2.txt'
  # filename = './input.3.txt'
  # filename = './input.4.txt'
  with open(filename) as f:
    n, m = map(int, f.readline().rstrip().split())
    queries = []
    for _ in range(m):
      queries.append(list(map(int, f.readline().rstrip().split())))
    result = arrayManipulation(n, queries)
    print(result)