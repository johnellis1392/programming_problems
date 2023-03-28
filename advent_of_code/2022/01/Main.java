import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class Main {
  public static Optional<ArrayList<ArrayList<String>>> readInput(final String filename) {
    try (var r = new BufferedReader(new FileReader(filename))) {
      return Optional.of(
        new ArrayList<ArrayList<String>>() {{
          var coll = new ArrayList<String>();
          var line = r.readLine();
          while (line != null) {
            if (line.isEmpty()) {
              this.add(new ArrayList<>() {{ this.addAll(coll); }});
              coll.clear();
            } else {
              coll.add(line);
            }
            line = r.readLine();
          }
          this.add(new ArrayList<>() {{ this.addAll(coll); }});
        }}
      );
    } catch (IOException e) {
      e.printStackTrace();
      return Optional.empty();
    }
  }

  public static Optional<Integer> part1(final String filename) {
    return readInput(filename).map(
      list ->
        list.stream().map(v -> 
          v.stream()
            .map(Integer::parseInt)
            .reduce((i, j) -> i + j)
            .get()
        ).reduce(Math::max).get()
      );
  }

  public static void dumps(final Collection<Integer> c) {
    var sb = new StringBuilder();
    sb.append("[");
    for (var i : c) sb.append(String.format("%d, ", i));
    sb.append("]");
    System.out.println(sb.toString());
  }

  public static void dumps2(final ArrayList<ArrayList<String>> c) {
    var sb = new StringBuilder();
    sb.append("[");
    for (var i : c) {
      sb.append("[");
      for (var j : i) sb.append(String.format("%s, ", j));
      sb.append("], ");
    }
    sb.append("]");
    System.out.println(sb.toString());
  }

  public static Optional<Integer> part2(final String filename) {
    return readInput(filename).map(
      list ->
        list.stream().map(v ->
          v.stream()
            .map(Integer::parseInt)
            .reduce((i, j) -> i + j)
            .get()
        ).sorted(Comparator.reverseOrder())
        .limit(3)
        .reduce((i, j) -> i + j)
        .get()
    );
  }

  public static void  main(final String[] args) {
    // final var filename = "input.test.txt";
    final var filename = "input.txt";
    
    part1(filename).ifPresent(System.out::println);
    part2(filename).ifPresent(System.out::println);
  }
}
