import java.util.Arrays;


public class rmdup {

    public class List<V> {
        public Node<V> head;

        public List() {
            this.head = null;
        }

        public List<V> add(V value) {
            if (this.head == null) {
                this.head = new Node(value);
            } else {
                this.head.add(value);
            }
            return this;
        }

        public int size() {
            int i = 0;
            for (Node<V> n = this.head; n != null; n = n.next) {
                i++;
            }
            return i;
        }

        public V[] toArray() {
            final int n = this.size();
            V[] result = new V[n];

            Node<V> node = this.head;
            for (int i = 0; i < n; i++, node = node.next) {
                result[i] = node.value;
            }

            return result;
        }


    public class Node<V> {
        public Node<V> next;
        public V value;

        public Node(final V value) {
            this.value = value;
            this.next = null;
        }

        public void add(V value) {
            if (this.next == null) {
                this.next = new Node<V>(value);
            } else {
                this.next.add(value);
            }
        }
    }

    }


    public static String debugString(Integer[] i) {
        int[] result = new int[i.length];
        for (int j = 0; j < i.length; j++) {
            result[j] = i[j].intValue();
        }

        return Arrays.toString(result);
    }


    public static void test(List<Integer> input, Integer[] expected) {
        Integer[] actual = rmdup(input).toArray();
        if (Arrays.equals(expected, actual)) {
            System.out.printf("[SUCCESS] Test passed for input: %s\n", debugString(input));
        } else {
            System.out.printf("[FAIL]    Test failed for input: %s\n", debugString(input));
        }

        System.out.printf(" * Expected: %s\n", debugString(expected));
        System.out.printf(" * Actual:   %s\n", debugString(actual));
        System.out.println();
    }


    public static void main(String[] args) {
        Node n1 = new List<Integer>()
            .add(1)
            .add(2)
            .add(3);
        test(n1, new Integer[] { 1, 2, 3 });

        Node n2 = new List<Integer>()
            .add(1)
            .add(1)
            .add(1);
        test(n2, new Integer[] { 1 });
    }

}
