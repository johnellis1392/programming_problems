import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class Main {
  public final Integer win = 6;
  public final Integer loss = 0;
  public final Integer draw = 3;
  public final Integer rock = 1;
  public final Integer paper = 2;
  public final Integer scissors = 3;
  public enum T { A, B, C, X, Y, Z } // Type
  public enum O { W, L, D } // Outcome
  public class M { // Move
    public T o;
    public T p;
    public M(T o, T p) { this.o = o; this.p = p; }
    public Integer play() {
      if (o.equals(T.A) && p.equals(T.X)) return draw + rock;
      else if (o.equals(T.A) && p.equals(T.Y)) return win + paper;
      else if (o.equals(T.A) && p.equals(T.Z)) return loss + scissors;
      else if (o.equals(T.B) && p.equals(T.X)) return loss + rock;
      else if (o.equals(T.B) && p.equals(T.Y)) return draw + paper;
      else if (o.equals(T.B) && p.equals(T.Z)) return win + scissors;
      else if (o.equals(T.C) && p.equals(T.X)) return win + rock;
      else if (o.equals(T.C) && p.equals(T.Y)) return loss + paper;
      else if (o.equals(T.C) && p.equals(T.Z)) return draw + scissors;
      else return 0;
    }
    @Override
    public String toString() {
      return String.format("M(o=%s, p=%s)", this.o, this.p);
    }
  }

  public T move(Character c) {
    switch (c) {
      case 'A': return T.A;
      case 'B': return T.B;
      case 'C': return T.C;
      case 'X': return T.X;
      case 'Y': return T.Y;
      case 'Z': return T.Z;
      default: return null;
    }
  }

  public M parse(final String line) {
    return new Main.M(
      move(line.charAt(0)),
      move(line.charAt(2))
    );
  }

  public Optional<List<M>> readInput(final String filename) {
    try (var r =  new BufferedReader(new FileReader(filename))) {
      var result = new ArrayList<M>();
      var line = r.readLine();
      while (line != null) {
        result.add(parse(line));
        line = r.readLine();
      }
      return Optional.of(result);
    } catch (IOException e) {
      return Optional.empty();
    }
  }

  public static <A> A tap(final A a) {
    System.out.println(a);
    return a;
  }

  public void part1(final String filename) {
    readInput(filename).map(list -> {
      return list.stream()
        .map(Main::tap)
        .map(M::play)
        .map(Main::tap)
        .reduce((i, j) -> i + j)
        .map(Main::tap)
        .get();
    }).ifPresent(System.out::println);;
  }

  public static void main(final String[] args) {
    // final var filename = "input.test.txt";
    final var filename = "input.txt";
    final var m = new Main();
    m.part1(filename);
    // m.part2(filename);
  }
}
