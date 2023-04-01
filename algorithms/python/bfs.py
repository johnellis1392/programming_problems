from collections import deque, defaultdict

def find_in_graph(grid, sigil):
  for r in range(len(grid)):
    for c in range(len(grid[r])):
      if grid[r][c] == sigil:
        return r, c
  return None

def adjs(n, m, r, c):
  return list(filter(
    lambda x: 0<=x[0] and x[0]<n and 0<=x[1] and x[1]<m,
    [(r-1, c), (r+1, c), (r, c-1), (r, c+1)]
  ))

def adj_dict(grid):
  n, m = len(grid), len(grid[0])
  d = dict()
  for r in range(n):
    for c in range(m):
      d[r, c] = adjs(n, m, r, c)
  # print('adj_dict: ', d)
  return d

def dump(distance, paths, G):
  print()
  print('Distances: ', distance)
  print()
  print('Paths: ', paths)
  print()
  print('Grid: ', [(k, list(v)) for k, v in G.items()])
  print()

def bfs_count_paths(grid):
  V = sum(map(len, grid))
  G = adj_dict(grid)
  S = find_in_graph(grid, 'a')
  D = find_in_graph(grid, 'b')
  def bfs(V, G, S, D):
    distance = defaultdict(int)
    paths = defaultdict(int)
    queue = deque()
    queue.append(S)
    distance[S] = 0
    paths[S] = 1
    visited = set()
    print('S = ', S)
    print('D = ', D)
    while queue:
      current = queue.popleft()
      print('G[current] = ', G[current])
      for child in G[current]:
        if child not in visited:
          queue.append(child)
          visited.add(child)
        if distance[child] > distance[current]+1:
          distance[child] = distance[current]+1
          paths[child] = paths[current]
        elif distance[child] == distance[current]+1:
          paths[child] = paths[child] + paths[current]
    dump(distance, paths, G)
    return paths[D]
  return bfs(V, G, S, D)


if __name__ == '__main__':
  data = ['a-', '-b']
  # data = ['a--', '---', '--b']
  bfs_count_paths(data)