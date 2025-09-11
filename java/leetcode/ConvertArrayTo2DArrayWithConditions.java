import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;

public class ConvertArrayTo2DArrayWithConditions {
  public record Test(int[] input, List<List<Integer>> exp) {}

  /* ****************** */
  /* *** Solution 1 *** */
  /* ****************** */
  public void insert_v1(ArrayList<HashSet<Integer>> vs, Integer x) {
    for (var set : vs) {
      if (!set.contains(x)) {
        set.add(x);
        return;
      }
    }
    vs.add(
      new HashSet<>() {{
        this.add(x);
      }}
    );
  }

  public List<List<Integer>> findMatrix_v1(final int[] input) {
    var a = new ArrayList<HashSet<Integer>>();
    for (var i : input) this.insert_v1(a, i);
    return new ArrayList<List<Integer>>() {{
      for (var set : a) {
        this.add(new ArrayList<Integer>() {{
          this.addAll(set);
        }});
      }
    }};
  }

  /* ****************** */
  /* *** Solution 2 *** */
  /* ****************** */
  // this was another solutino that was really lightweight and
  // admittedly very clever.
  public List<List<Integer>> findMatrix(final int[] input) {
    // var ns = new HashMap<Integer, Integer>();
    final int CAP = 1000;
    var ns = new int[CAP];
    int row = 0;
    for (var i : input) {
      ns[i]++;
      row = Math.max(row, ns[i]);
    }
    var result = new ArrayList<List<Integer>>();
    for (int i = 0; i < row; i++) {
      var a = new ArrayList<Integer>();
      for (int j = 0; j < CAP; j++) {
        if (ns[j] > 0) {
          a.add(j);
          ns[j]--;
        }
      }
      result.add(a);
    }
    return result;
  }

  public static boolean equal(final List<List<Integer>> a, final List<List<Integer>> b) {
    if (a.size() != b.size()) return false;
    for (var i = 0; i < a.size(); i++) {
      if (!a.get(i).equals(b.get(i))) return false;
    }
    return true;
  }

  public static String dumps(final List<List<Integer>> input) {
    var s = new StringBuilder();
    s.append("[");
    for (var a : input) {
      s.append("[");
      for (var b : a) {
        s.append(String.format("%d, ", b));
      }
      s.append("], ");
    }
    s.append("]");
    return s.toString();
  }

  public static void main(String[] args) {
    var a = new ConvertArrayTo2DArrayWithConditions();
    var tests = new Test[] {
      new Test(
        new int[] {1,3,4,1,2,3,1},
        Arrays.asList(
          Arrays.asList(1,3,4,2),
          Arrays.asList(1,3),
          Arrays.asList(1)
        )
      ),
      new Test(
        new int[] {1,2,3,4},
        Arrays.asList(
          Arrays.asList(1,2,3,4)
        )
      )
    };
    for (var t : tests) {
      var actual = a.findMatrix(t.input);
      if (equal(t.exp, actual)) {
        System.out.println("SUCCESS");
      } else {
        System.out.printf("FAILURE: actual=%s\n", dumps(actual));
      }
    }
  }
}
