#!/usr/bin/env python
from collections import Counter
from functools import reduce

def factorial(n):
  P = 1
  for i in range(1, n+1):
    P *= i
  return P

def choose(n, k):
  P = 1
  for i in range(n-k+1, n+1):
    P *= i
  return P

def kcombo(n, k):
  P = 1
  for i in range(n-k+1, n+1):
    P *= i
  return P // factorial(k)

def totalcombos(n):
  S = 0
  for k in range(1, n+1):
    S += choose(n, k)
  return S

def totalsum(n):
  return n * (n+1) // 2

def ncombos(n):
  return 2**n-1

def shift(s, i, j, t):
  def f(c):
    v = ord(c) - ord('a')
    v = (v + t) % 26
    return chr(v + ord('a'))
  s[i:j+1] = map(f, s[i:j+1])
  return s

def ispalindromic(items):
  odds = 0
  for _, v in items:
    if v % 2 == 1:
      if odds > 0:
        return False
      else:
        odds += 1
  return True

def num_palindromes_v1(s: str) -> int:
  Sum = 0
  counter = Counter(s)
  # print(counter)
  items = list(counter.items())
  # Sum = 0
  Sum = 1
  for i in range(len(items)):
    for j in range(i+1, len(items)+1):
      if not ispalindromic(items[i:j]):
        continue
      elif len(items[i:j]) == 1:
        Sum += ncombos(items[i][1])
      else:
        SubSum = 1
        for x in range(i, j):
          SubSum *= max(1, items[x][1]//2)
        Sum *= max(1, ncombos(SubSum))
        # num_chars_per_side = 0
        # num_dups = 0
        # for x in range(i, j):
        #   v = items[x][1]//2
        #   num_chars_per_side += v
        #   num_dups += max(0, v-1)
        #   # if v > 1:
        #   #   num_dups += 1
        # # c = choose(num_chars_per_side, num_dups)
        # print('items=%s, num_chars_per_side=%d, num_dups=%d' % (items[i:j], num_chars_per_side, num_dups))
        # # Sum += c
        # Sum += choose(num_chars_per_side, 1)
  return Sum

def num_palindromes_v2(s):
  items = [ v for _, v in Counter(s).items() ]
  product = lambda x: reduce(int.__mul__, x, 1)
  l2 = list(map(ncombos, items))
  l3 = list(map(lambda x: max(x, 1), l2))
  result = product(items)-1
  p = product(l3)
  Sum = p + result
  #   print('''- s = %s
  # - l3=%s
  # - result=%d
  # - p=%d
  # - Sum = %d ''' % (s, l3, result, p, Sum))
  print('s = %s' % s)
  return Sum
  # return product(map(lambda x: max(ncombos(x), 1), items)) \
  #   + product(items)-1

def num_palindromes(s):
  # print('s = %s' % s)
  counter = Counter(s)
  items = [ v for v in counter.values() ]
  product = lambda x: reduce(int.__mul__, x, 1)
  # n = sum(combinations) - [number of odd-numbered values]
  n = sum(max(1, ncombos(x)) for x in items) - len(items)
  m = product([ max(1, ncombos(x)) for x in items ])
  # print(' - n=%d, m=%d, counter=%s' % (n, m, counter))
  return n + m

def main(input_str, qs):
  s = list(input_str)
  for q in qs:
    if len(q) == 3: # Query type 1
      [i, j, t] = q
      s = shift(s, i, j, t)
    else: # Query type 2
      [i, j] = q
      # print('s = %s, i = %d, j = %d' % (s[i:j+1], i, j))
      print(' s=%s' % (''.join(s[i:j+1])))
      Sum = num_palindromes(s[i:j+1])
      print(Sum)
  # return Sum

if __name__ == '__main__':
  # filename = './input.txt'
  # filename = './input.2.txt'
  # s, queries = None, None
  # with open(filename) as f:
  #   [_, q] = list(map(int, f.readline().rstrip().split(' ')))
  #   s = f.readline().rstrip()
  #   queries = []
  #   for _ in range(q):
  #     query = f.readline().rstrip().split(' ')
  #     if query[0] == '1': # Query type 1
  #       queries.append([int(query[1]), int(query[2]), int(query[3])])
  #     else: # Query type 2
  #       queries.append([int(query[1]), int(query[2])])
  # main(s, queries) 

  # s = 'bbbahhfhhgf'
  # print(num_palindromes(s))
  # Should be 383

  # expected = 383
  # product = lambda x: reduce(int.__mul__, x, 1)
  # l = [4,3,2,1,1]
  # l2 = list(map(ncombos, l))
  # l3 = list(map(lambda x: max(x, 1), l2))
  # result = product(l3)
  # Sum = result + product(l)-1
  # print(Sum)
  

  product = lambda x: reduce(int.__mul__, x, 1)
  # # l = [4,3,2,1,1]
  # counter = Counter(s)
  # print(counter, len(counter))
  # l = [ncombos(x) for x in counter.values()]
  # print(l, sum(l))
  # Sum = sum(l) + product(l)-1
  # print(Sum)

  # s = "ba" # => 2
  # s = "bbbahhfhhgf" # => 383
  # s = "utaaprrq" # => 27
  # s = "zutaapfnml" # => 19
  # s = "nmlfe" # => 5
  # s = "bw" # => 2
  def f(s, exp):
    counter = Counter(s)
    a = counter.values()
    # print('s=%s, exp=%s, actual=%s' % (
    #   str.rjust(s, 12),
    #   str.rjust(str(exp), 4),
    #   str.rjust(str(total), 4)))
    ones = [x for x in a if x==1]
    numbers = [x for x in a if x>1]
    temp = 0
    total = 0
    for x in numbers:
      temp += choose(x, 2)
      total += choose(x, 2)*(sum(ones)+1)

    total = ncombos(temp)
    print(exp, total)
    # return total

  for (s, n) in [("ba", 2),
                 ("bbbahhfhhgf", 383),
                 ("utaaprrq", 27),
                 ("zutaapfnml", 19),
                 ("nmlfe", 5),
                 ("bw", 2),
                 ('aba', 2),
                 ('aaab', 9),
                 ('aaabb', 13)]:
    # print(s, n, '\t', Counter(s))
    f(s, n)