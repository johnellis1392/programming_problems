import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class Main {
  public record Input(
    int n,
    int m,
    List<List<Integer>> edges,
    int s
  ) {}
  
  public String dumps(List<List<Integer>> list) {
    var sb = new StringBuilder();
    sb.append("[");
    for (var l : list) {
      sb.append("[");
      for (var i : l) sb.append(String.format("%d, ", i));
      sb.append("]");
    }
    sb.append("]");
    return sb.toString();
  }
  
  public List<Integer> bfs(
    int n,
    int m,
    List<List<Integer>> edges,
    int s
  )  {
    final int EDGE_WEIGHT = 6;
    // System.out.printf("n=%d, m=%d, edges=%s, s=%d\n", n, m, dumps(edges), s);
    var nodeMap = new HashMap<Integer, Integer>();
    for (var edge : edges) nodeMap.put(edge.get(0), edge.get(1));
    var result = new ArrayList<Integer>();
    for (int i = 1; i <= n; i++) {
      int node = s;
      // TODO
    }
    return result;
  }
  
  public static Optional<List<Input>> readInput(final String filename) {
    try (var r =  new BufferedReader(new FileReader(filename))) {
      var q = Integer.parseInt(r.readLine());
      List<Input> results = new ArrayList<>();
      IntStream.range(0, q).forEach(i -> {
        try {
          var sarr = Arrays.stream(r.readLine().split(" "))
            .map(Integer::parseInt)
            .collect(Collectors.toList());
          int n = sarr.get(0), m = sarr.get(1);
          List<List<Integer>> edges = new ArrayList<>();
          IntStream.range(0, m).forEach(j -> {
            try {
              edges.add(
                Stream.of(r.readLine().replaceAll("\\s+$", "").split(" "))
                  .map(Integer::parseInt)
                  .collect(Collectors.toList())
              );
            } catch (IOException  e) {
              e.printStackTrace();
              throw new RuntimeException();
            }
          });
          var s = Integer.parseInt(r.readLine());
          results.add(new Input(n, m, edges, s));
        } catch (IOException e) {
          e.printStackTrace();
          throw new RuntimeException();
        }
      });
      return Optional.of(results);
    } catch (IOException e) {
      e.printStackTrace();
      return Optional.empty();
    }
  }

  public static void dump(final List<Integer> list) {
    var sb = new StringBuilder();
    sb.append("[");
    for (var i : list) sb.append(String.format("%d, ", i));
    sb.append("]");
    System.out.println(sb.toString());
  }

  public static void main(final String[] args) {
    var m = new Main();
    readInput("./input.txt")
      .ifPresent(q -> {
        q.forEach(i -> {
          var r = m.bfs(i.n, i.m, i.edges, i.s);
          dump(r);
        });
      });
  }
}
