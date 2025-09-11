from typing import Tuple

def parseGrid(grid: list[str]):
  '''
  Parse the grid of strings into a useable dataset.
  Returns a tuple containing:
  - bot starting position
  - princess ending position
  - dictionary containing point data
  '''
  spos, epos = None, None
  # result = {}
  for i in range(len(grid)):
    for j in range(len(grid[i])):
      if grid[i][j] == 'm':
        spos = (i, j)
      elif grid[i][j] == 'p':
        epos = (i, j)
      # else:
      #   result[i, j] = None
  return spos, epos # , result

def adjs(p: Tuple[int, int]) -> list[Tuple[int, int]]:
  return [(p[0] + 1, p[1]), (p[0] - 1, p[1]), (p[0], p[1] + 1), (p[0], p[1] - 1)]

def displayPathtoPrincess(n, grid):
  result = []
  # spos, epos, grid = parseGrid(grid)
  spos, epos = parseGrid(grid)
  y1, x1 = spos
  y2, x2 = epos
  vmovs, hmovs = y2 - y1, x2 - x1
  vmov = [(vmovs>0 and 'DOWN') or 'UP'] * abs(vmovs)
  hmov = [(hmovs<0 and 'LEFT') or 'RIGHT'] * abs(hmovs)
  result.extend(vmov)
  result.extend(hmov)
  for r in result:
    print(r)

if __name__ ==  '__main__':
  filename = 'input.txt'
  grid = [] 
  with open(filename) as f:
    m = int(f.readline().strip())
    for i in range(0, m): 
      grid.append(f.readline().strip())
  displayPathtoPrincess(m,grid)
