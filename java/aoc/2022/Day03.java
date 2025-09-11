import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Optional;
import java.util.Set;

public class Main {
  public static Optional<ArrayList<String>> read(final String filename) {
    try (var r =  new BufferedReader(new FileReader(filename))) {
      var result = new ArrayList<String>();
      var line = r.readLine();
      while (line != null) {
        result.add(line);
        line = r.readLine();
      }
      return Optional.of(result);
    } catch (IOException e) {
      e.printStackTrace();
      return Optional.empty();
    }
  }

  public static Integer priority(final Character c) {
    if ('a' <= c && c <= 'z') {
      return c - 'a' + 1;
    } else if ('A' <= c && c <= 'Z') {
      return c - 'A' + 27;
    } else {
      return 0;
    }
  }

  public static Integer process(final String rucksack) {
    var s1 = new HashSet<Character>() {{
      for (var c : rucksack.substring(0, rucksack.length()/2).toCharArray())
        this.add(Character.valueOf(c));
    }};

    var s2 = new HashSet<Character>() {{
      for (var c : rucksack.substring(rucksack.length()/2, rucksack.length()).toCharArray())
        this.add(Character.valueOf(c));
    }};
    
    s1.retainAll(s2);
    if (s1.isEmpty()) return 0;
    else return priority(s1.stream().findFirst().get());
  }

  public static Set<Character> toSet(final String s) {
    return new HashSet<Character>() {{
      for (var c : s.toCharArray())
        this.add(Character.valueOf(c));
    }};
  }

  public static Integer process2(final ArrayList<String> arr) {
    var set = toSet(arr.get(0));
    for (int i = 1; i < arr.size(); i++)
      set.retainAll(toSet(arr.get(i)));
    if (set.isEmpty()) return 0;
    else return priority(set.stream().findFirst().get());
  }

  public static ArrayList<ArrayList<String>> partition(
    final ArrayList<String> list,
    final Integer n
  ) {
    var result = new ArrayList<ArrayList<String>>();
    for (var i = 0; i < list.size(); i += n) {
      var temp = new ArrayList<String>();
      for (var j = 0; j < n; j++) {
        temp.add(list.get(i+j));
      }
      result.add(new ArrayList<>() {{ this.addAll(temp); }});
      temp.clear();
    }
    return result;
  }

  public static void dumps(final ArrayList<ArrayList<String>> list) {
    var sb = new StringBuilder();
    sb.append("[");
    for (var i : list) {
      sb.append("[");
      for (var j : i)  sb.append(String.format("%s, ", j));
      sb.append("], ");
    }
    sb.append("]");
    System.out.println(sb.toString());
  }

  public static void part1(final String filename) {
    read(filename).map(v -> {
      return v.stream()
        .map(Main::process)
        .reduce(Math::addExact);
    }).ifPresent(System.out::println);
  }

  public static void part2(final String filename) {
    read(filename).map(v -> {
      return partition(v, 3)
        .stream()
        .map(Main::process2)
        .reduce(Math::addExact)
        .get();
    }).ifPresent(System.out::println);;
  }

  public static void main(final String[] args) {
    // final String filename = "input.test.txt";
    final String filename = "input.txt";
    // part1(filename);
    part2(filename);
  }
}
