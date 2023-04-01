from functools import reduce

def scan(f, l):
  if l is None or len(l) == 0:
    return None
  acc = l[0]
  yield acc
  for i in range(1, len(l)):
    acc = f(acc, l[i])
    yield acc
  return None

def main_v1(v):
  '''Doesn't work'''
  print('v = %s' % str(v))
  # w = list(map(lambda x: x[1]-x[0], zip(v[0:], v[1:])))
  w = list(map(lambda x: x[0]<x[1], zip(v[0:], v[1:])))
  print('w = %s' % str(w))
  # u = reduce(lambda x, y: x+y, w, 0)
  # print('u = %s' % str(u))
  return w

def main_v2(v):
  '''Also doesn't work'''
  if v is None:
    return False
  elif len(v) <= 2:
    return True
  flag = v[0] >= v[1]
  for i in range(2, len(v)):
    if v[i] <= v[i-1]:
      if v[i] <= v[i-2] or flag:
        return False
      else:
        flag = True
  return True

def count(v, l):
  n = 0
  for i in l:
    if i == v:
      n += 1
  return n

def main(v):
  if v is None:
    return False
  elif len(v) <= 2:
    return True
  l1 = list(map(lambda x: x[0]<x[1], zip(v[0:], v[1:])))
  flags1 = count(False, l1)
  # print('flags1 = %s, l1 = %s' % (flags1, l1))
  l2 = list(map(lambda x: x[0]<x[1], zip(v[0:], v[2:])))
  flags2 = count(False, l2)
  # print('flags2 = %s, l2 = %s' % (flags2, l2))
  return flags1 <= 1 and flags2 <= 1

if __name__ == '__main__':
  tests = [
    ([1,2,1,2], False),
    ([1,3,2,1], False),
    ([1,2,3], True),
    ([1, 1, 2, 3, 4, 4], False),
    ([1, 2, 3, 4, 3, 6], True),
  ]

  for i, exp in tests:
    out = main(i)
    if out != exp:
      print('FAILURE: input=%s, expected=%s, actual=%s' % (str(i), str(exp), str(out)))
    else:
      print('SUCCESS: input=%s, expected=%s, actual=%s' % (str(i), str(exp), str(out)))