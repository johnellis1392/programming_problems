import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.Stack;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Main {
  public record Tuple<A, B>(A left, B right) {}

  public class Headers {
    public final Integer size;
    public ArrayList<Stack<String>> headers;
    public Headers(ArrayList<Stack<String>> headers) {
      this.headers = headers;
      this.size = headers.size();
    }

    // Implementation for part 1
    public void processMove_part1(final Move move) {
      for (int i = 0; i < move.n; i++) {
        var v = this.headers.get(move.from-1).pop();
        this.headers.get(move.to-1).push(v);
      }
    }

    // Implementation for part 2
    public void processMove(final Move move) {
      var stack = new Stack<String>();
      for (int i = 0; i < move.n; i++) {
        var v = this.headers.get(move.from-1).pop();
        stack.push(v);
      }
      while (!stack.isEmpty()) {
        this.headers.get(move.to-1).push(stack.pop());
      }
    }
  }

  public record Move(
    Integer n, 
    Integer from,
    Integer to
  ) {}

  public Optional<Move> parseMove(final String s) {
    try { 
        var pattern = Pattern.compile("^move (\\d+) from (\\d+) to (\\d+)$");
        var matcher =  pattern.matcher(s);
        if (matcher.find()) {
          return Optional.of(
            new Move(
              Integer.parseInt(matcher.group(1)),
              Integer.parseInt(matcher.group(2)),
              Integer.parseInt(matcher.group(3))
            )
          );
        } else {
          return Optional.empty();
        }
    } catch (Exception e) {
      e.printStackTrace();
      return Optional.empty();
    }
  }

  public String dumps(final String[] arr) {
    var sb = new StringBuilder();
    sb.append("[");
    for (var i : arr) sb.append("\"" + i  + "\", ");
    sb.append("]");
    return sb.toString();
  }

  public Optional<Headers> processHeaders(
    Stack<String> stack
  ) {
    var headerConfig = stack.pop();
    var matches = headerConfig.split("\\s+", 0);
    if (matches.length <= 1) return Optional.empty();
    var n = matches.length - 1;

    var headers = new ArrayList<Stack<String>>();
    for (int i = 0; i < n; i++) headers.add(new Stack<String>());

    while (!stack.isEmpty()) {
      var s = stack.pop();
      for (int i = 0; i < n; i++) {
        var ss = s.substring(i*4+1, i*4+2).trim();
        if (!ss.isEmpty()) headers.get(i).push(ss);
      }
    }

    return Optional.of(new Headers(headers));
  }

  public Optional<Tuple<Headers, ArrayList<Move>>> readInput(final String filename) {
    try (var r =  new BufferedReader(new FileReader(filename))) {
      var line = r.readLine();
      var headerStack = new Stack<String>();
      // var headers = new ArrayList<Header>();
      var moves = new ArrayList<Move>();
      
      // Read Headers
      while (line != null && !line.isEmpty()) {
        headerStack.push(line);
        line = r.readLine();
      }
      var headers = this.processHeaders(headerStack).orElseThrow();

      // Read Moves
      line = r.readLine();
      while (line !=  null) {
        var p = this.parseMove(line);
        if (p.isPresent()) moves.add(p.get());
        else return Optional.empty();
        line = r.readLine();
      }
      var result = new Tuple<Headers, ArrayList<Move>>(headers, moves);
      return Optional.of(result);
    } catch (Exception e) {
      e.printStackTrace();
      return Optional.empty();
    }
  }

  public void dumps(final Tuple<Headers, ArrayList<Move>> input) {
    var sb = new StringBuilder();
    sb.append("Headers:\n");
    for (var h : input.left.headers) {
      sb.append("[");
      for (var i : h) sb.append("\"" + i + "\", ");
      sb.append("]");
    }
    sb.append("\nMoves:\n");
    for (var m : input.right) sb.append(m.toString() + "\n");
    System.out.println(sb.toString());
  }

  public void dump(final Headers headers) {
    var sb = new StringBuilder();
    sb.append("[");
    for (var h : headers.headers) {
      sb.append("[");
      for (var i : h) sb.append("\"" + i + "\", ");
      sb.append("]\n");
    }
    sb.append("]\n");
    System.out.println(sb.toString());
  }

  public void part1(final String filename) {
    readInput(filename).map(tuple -> {
      var headers = tuple.left;
      for (var move : tuple.right) {
        headers.processMove(move);
      }
      return headers;
    })
    .map(headers -> {
      var sb = new StringBuilder();
      for (var h : headers.headers) {
        sb.append(h.peek());
      }
      return sb.toString();
    })
    .ifPresent(System.out::println);
  }

  public static void  main(final String[] args) {
    final String filename = "input.txt";
    // final String filename = "input.test.txt";
    var main = new Main();
    main.part1(filename);
  }
}
