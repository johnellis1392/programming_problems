FIB_MAX=4 * 10 ** 6

def fibonacci(n=FIB_MAX):
  i, j = 0, 1
  while i < n:
    yield i
    i, j = j, i+j
  return None

def main():
  Sum = 0
  for i in fibonacci():
    if i % 2 == 0:
      Sum += i
  return Sum

if __name__ ==  '__main__':
  print('Result = %s' % str(main()))