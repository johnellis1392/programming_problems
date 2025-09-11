import math

def mini_max_sum(arr):
  Sum = sum(arr)
  Min = math.inf
  Max = -math.inf
  for i in arr:
    n = Sum - i
    Min = min(Min, n)
    Max = max(Max, n)
  print('%d %d' % (Min, Max))

if __name__ == '__main__':
  filename = './input.2.txt'
  with open(filename) as f:
    arr = list(map(int, f.readline().rstrip().split()))
    mini_max_sum(arr)
