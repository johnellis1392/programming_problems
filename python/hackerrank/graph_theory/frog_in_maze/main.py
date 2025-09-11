from collections import deque, defaultdict

WALL = '#'
BOMB = '*'
EXIT = '%'
OPEN = 'O'

def mean(*vs):
  return float(sum(vs)) / len(vs)

def find_in_grid(grid, sigil):
  for i in range(len(grid)):
    for j in range(len(grid[i])):
      if grid[i][j] == sigil:
        return i, j
  return None

def startingpoint(grid):
  return find_in_grid(grid, 'A')

def destinationpoint(grid):
  return find_in_grid(grid, '%')

def adjs(n, m, r, c):
  def valid(i, j):
    return (0<=i and i<n) and (0<=j and j<m)
  return list(filter(lambda x: valid(*x), [(r-1,c), (r+1,c), (r,c-1), (r,c+1)]))

def main_v1(n, m, grid, warps):
  wins = 0
  losses = 0
  seen = set()
  def f(r, c):
    nonlocal wins, losses, seen
    if (r, c) in seen:
      pass
    elif grid[r][c] == WALL:
      pass
    elif all(grid[i][j] == WALL for i, j in adjs(n, m, r, c)):
      losses += 1
    elif grid[r][c] == BOMB:
      losses += 1
    elif grid[r][c] == EXIT:
      wins += 1
    elif (r, c) in warps and warps[r, c] not in seen:
      seen.add((r, c))
      f(*warps[r, c])
      seen.remove((r, c))
    else:
      moves = adjs(n, m, r, c)
      seen.add((r, c))
      for p in moves:
        f(*p)
      seen.remove((r, c))
  r, c = startingpoint(grid)
  f(r, c)
  total = wins + losses
  print('wins = %d, losses = %d, total = %d' % (wins, losses, total))
  if total == 0:
    return 0
  else:
    return float(wins) / total

def main(n, m, grid, warps):
  distance = defaultdict(int)
  paths = defaultdict(int)
  queue = deque()
  S = startingpoint(grid)
  D = destinationpoint(grid)
  queue.append(S)
  distance[S] = 0
  paths[S] = 1
  visited = set()
  while queue:
    current = queue.popleft()
    r, c = current
    for child in adjs(n, m, r, c):
      if child not in visited:
        queue.append(child)
        visited.add(child)
      if distance[child] > distance[current]+1:
        distance[child] = distance[current]+1
        paths[child] = paths[current]
      elif distance[child] == distance[current]+1:
        paths[child] = paths[child] + paths[current]
  return paths[D]

if __name__ == '__main__':
  filename = 'input.4.txt'
  # filename = 'input.3.txt'
  # filename = 'input.2.txt'
  # filename = 'input.txt'
  with open(filename) as f:
    [n, m, k] = list(map(int, f.readline().rstrip().split()))
    grid = []
    for n_itr in range(n):
      row = f.readline()
      grid.append(row)
    warps = {}
    for k_itr in range(k):
      [r1, c1, r2, c2] = list(map(lambda x: int(x)-1, f.readline().rstrip().split()))
      warps[r1, c1] = (r2, c2)
      warps[r2, c2] = (r1, c1)
    result = main(n, m, grid, warps)
    print(result)
