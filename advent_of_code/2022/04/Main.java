import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Optional;
import java.util.regex.Pattern;

public class Main {
  public class Range {
    public final int start;
    public final int end;
    public Range(final int start, final int end) {
      this.start = start;
      this.end = end;
    }

    public boolean encloses(final Range r) {
      return this.start <= r.start && r.end <= this.end;
    }

    public boolean overlaps(final Range r) {
      return this.start <= r.start && r.start <= this.end 
        ||   this.start <= r.end   && r.end   <= this.end;
    }
  }

  public class Tuple<A> {
    public final A left;
    public final A right;
    public Tuple(final A left, final A right) {
      this.left = left;
      this.right = right;
    }
  }

  public Optional<Tuple<Range>> parseRanges(final String s) {
    var pattern = Pattern.compile("(\\d+)-(\\d+),(\\d+)-(\\d+)");
    var matcher = pattern.matcher(s);
    if (matcher.find()) {
      return Optional.of(
        new Tuple<> (
          new Range(
            Integer.parseInt(matcher.group(1)),
            Integer.parseInt(matcher.group(2))
          ),
          new Range(
            Integer.parseInt(matcher.group(3)),
            Integer.parseInt(matcher.group(4))
          )
        )
      );
    } else {
      return Optional.empty();
    }
  }

  public Optional<ArrayList<Tuple<Range>>> readInput(final String filename) {
    try (var r =  new BufferedReader(new FileReader(filename))) {
      var line = r.readLine();
      var result = new ArrayList<Tuple<Range>>();
      while (line !=  null) {
        var tuple = this.parseRanges(line);
        tuple.ifPresent(result::add);
        tuple.orElseThrow();
        line = r.readLine();
      }
      return Optional.of(result);
    } catch (Exception e) {
      e.printStackTrace();
      return Optional.empty(); 
    }
  }

  public void part2(final String filename) {
    readInput(filename).map(
      ranges ->
      ranges.stream()
        .filter(t -> t.left.overlaps(t.right) || t.right.overlaps(t.left))
        .count()
    ).ifPresent(System.out::println);
  }

  public void part1(final String filename) {
    readInput(filename).map(
      ranges -> 
      ranges.stream()
        .filter(t -> t.left.encloses(t.right) || t.right.encloses(t.left))
        .count()
    ).ifPresent(System.out::println);
  }

  public static void main(final String[] args) {
    // final String filename = "input.test.txt";
    final String filename = "input.txt";
    final var main = new Main();
    // main.part1(filename);
    main.part2(filename);
  }
}
