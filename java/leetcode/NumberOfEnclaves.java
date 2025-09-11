import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

public class NumberOfEnclaves {
  public record Test(int[][] input, int expected) {}
  public record P(int r, int c) {}

  /* ****************** */
  /* *** Solution 1 *** */
  /* ****************** */
  public List<P> adjs_v1(final P p, final int r, final int c) {
    var result = new ArrayList<P>();
    if (p.r-1 >= 0) result.add(new P(p.r-1, p.c));
    if (p.c-1 >= 0) result.add(new P(p.r, p.c-1));
    if (p.r+1 < r) result.add(new P(p.r+1, p.c));
    if (p.c+1 < c) result.add(new P(p.r, p.c+1));
    return result;
  }

  public int numEnclaves_v1(final int[][] input) {
    final int nrows = input.length, ncols = input[0].length;
    int enclaves = 0;
    var seen = new HashSet<P>();

    for (int i = 0; i < nrows; i++) {
      for (int j = 0; j < ncols; j++) {
        if (input[i][j] == 0) continue;
        var p = new P(i, j);
        if (seen.contains(p)) continue;
        seen.add(p);

        // Breadth-First Search over Island
        var q = new ArrayDeque<P>();
        var island = new HashSet<P>();
        island.add(p);
        q.add(p);
        boolean isEdgeAdjacent = false;
        while (!q.isEmpty()) {
          var p2 = q.pop();
          seen.add(p2);
          if (p2.r == 0 || p2.c == 0 || p2.r == nrows-1 || p2.c == ncols-1)
            isEdgeAdjacent = true;
          for (var p3 : this.adjs_v1(p2, nrows, ncols)) {
            if (island.contains(p3)) continue;
            else if (input[p3.r][p3.c] == 1) {
              q.add(p3);
              island.add(p3);
            }
          }
        }
        if (!isEdgeAdjacent) enclaves += island.size();
      }
    }
    return enclaves;
  }

  /* ****************** */
  /* *** Solution 2 *** */
  /* ****************** */
  // Saw this on the related solutions, and it seems much more efficient.
  // It's doing a depth-first search rather than a breadth-first search,
  // and instead of using sets and arraylists for managing state, it
  // just uses the grid itself.
  public void dfs(int[][] grid, int i, int j) {
    if (i < 0 || j < 0 || i >= grid.length || j >= grid[0].length || grid[i][j] == 0) {
      return;
    }

    grid[i][j] = 0;
    dfs(grid, i-1, j);
    dfs(grid, i+1, j);
    dfs(grid, i, j-1);
    dfs(grid, i, j+1);
  }

  public int numEnclaves(int[][] grid) {
    // iterate over edge to remove all nodes adjacent to the edge.
    for (int i = 0; i < grid.length; i++) {
      if (grid[i][0] == 1) this.dfs(grid, i, 0);
      if (grid[i][grid[i].length-1] == 1) this.dfs(grid, i, grid[i].length-1);
    }
    for (int j = 0; j < grid[0].length; j++) {
      if (grid[0][j] == 1) this.dfs(grid, 0, j);
      if (grid[grid.length-1][j] == 1) this.dfs(grid, grid.length-1, j);
    }

    int result = 0;
    for (int i = 0; i < grid.length; i++) {
      for (int j = 0; j < grid[i].length; j++) {
        if (grid[i][j] == 1) result++;
      }
    }
    return result;
  }

  public String dumps(final int[][] input) {
    var sb = new StringBuilder();
    sb.append("[");
    for (var a : input) {
      sb.append("[");
      for (var i : a) sb.append(String.format("%d, ", i));
      sb.append("], ");
    }
    sb.append("]");
    return sb.toString();
  }

  public static void main(String[] args) {
    final var tests = new Test[] {
      new Test(
        new int[][] {
          new int[] {0, 0, 0, 0},
          new int[] {1, 0, 1, 0},
          new int[] {0, 1, 1, 0},
          new int[] {0, 0, 0, 0},
        },
        3
      ),
      new Test(
        new int[][] {
          new int[] {0, 1, 1, 0},
          new int[] {0, 0, 1, 0},
          new int[] {0, 0, 1, 0},
          new int[] {0, 0, 0, 0},
        },
        0
      )
    };
    var noe = new NumberOfEnclaves();
    for (var test : tests) {
      var actual = noe.numEnclaves(test.input);
      if (actual == test.expected) {
        System.out.println("SUCCESS");
      } else {
        System.out.printf("FAILURE: input=%s, expected=%d, output=%d\n", noe.dumps(test.input), test.expected, actual);
      }
    }
  }
}
