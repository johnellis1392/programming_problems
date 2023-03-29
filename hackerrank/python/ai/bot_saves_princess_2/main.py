# import functools
# import math

# from typing import Tuple

# def adjs(r, c):
#   return [(r+1, c), (r-1, c), (r, c+1), (r, c-1)]

# def valid(n, r, c):
#   return (0<=r and r<n) and (0<=c and c<n)

# def tryMove(n, stack, grid):
#   r, c = stack[-1]
#   if grid[r][c] == 'p':
#     return [i for i in stack]
#   result = None
#   for (y, x) in adjs(r, c):
#     if not valid(n, y, x) or (y, x) in stack:
#       continue
#     stack.append((y, x))
#     res = tryMove(n, stack, grid)
#     if result is None or res is not None and len(res) < len(result):
#       result = res
#     stack.pop()
#   return result

# def nextMove(n, r, c, grid):
#   stack = list()
#   stack.append((c, r))
#   path = tryMove(n, stack, grid)
#   if not path:
#     raise Exception("Couldn't find path: stack=%s" % str(stack))
#   elif len(path) == 1:
#     raise Exception("Returned Invalid Path: stack=%s" % str(stack))
#   (r1, c1), (r2, c2)  = path[0], path[1]
#   dr, dc = r2 - r1, c2 - c1
#   move_s = None
#   if (dr, dc) == (-1, 0):
#     move_s = "UP"
#   elif (dr, dc) == (1, 0):
#     move_s = "DOWN"
#   elif (dr, dc) ==  (0, -1):
#     move_s = "LEFT"
#   elif (dr, dc) == (0, 1):
#     move_s = "RIGHT"
#   else:
#     raise Exception("Invalid Move %s" % ((dr, dc)))
#   print(move_s)

# sign = functools.partial(math.copysign, 1)

def sign(n):
  return (n<0 and -1) or (n>0 and 1) or 0

def getpos(grid):
  botpos, princesspos = None, None
  for i in range(len(grid)):
    for j in range(len(grid[i])):
      if grid[i][j] == 'p':
        princesspos = (i, j)
      elif grid[i][j] == 'm':
        botpos = (i, j)
  return botpos, princesspos

def nextMove(n, r, c, grid):
  stack = list()
  stack.append((c, r))
  botpos, princesspos = getpos(grid)

  (r1, c1), (r2, c2) = botpos, princesspos
  dr, dc = sign(r2 - r1), sign(c2 - c1)
  print((dr, dc))

  move_s = None
  if dr == -1:
    move_s = "UP"
  elif dr == 1:
    move_s = "DOWN"
  elif dc ==  -1:
    move_s = "LEFT"
  elif dc == 1:
    move_s = "RIGHT"
  else:
    raise Exception("Invalid Move %s" % ((dr, dc)))
  # print(move_s)
  return move_s

if __name__ == '__main__':
  filename = 'input.txt'
  n, x, y, grid = None, None, None, []
  with open(filename) as f:
    n = int(f.readline().strip())
    [y, x] = list(map(int, f.readline().strip().split()))
    spos = (x, y)
    for _ in range(n):
      grid.append(f.readline().strip())
  print(nextMove(n, x, y, grid))
