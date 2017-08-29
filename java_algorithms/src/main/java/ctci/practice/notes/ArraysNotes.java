package ctci.practice.notes;

import java.util.Arrays;
import java.util.Random;
import java.util.ArrayList;
import java.util.List;


public class ArraysNotes {

    public static Random random = new Random();
    public static int MAX = 1e6;


    public static void arraysCopyExample1() {
        // Original Array
        int[] values = new int[] { 1, 2, 3 };

        // Copy Array
        int[] copy = Arrays.copyOf(values, 5);
    }


    public static void arraysCopyExample2() {
        ArrayList<Integer> list = new ArrayList<>();
        final int size = 20;
        for (int i = 0; i < size; i++) {
            list.add(random.nextInt(MAX));
        }

        Integer[] valuesCopy = new Integer[size];
        Integer[] values = list.toArray(valuesCopy);

        // Make copy of array, passing in Integer[] class
        Integer[] valuesCopy2 = Arrays.copyOf(values, size, values.getClass());
    }


    // Arrays Javadoc:
    // https://docs.oracle.com/javase/7/docs/api/java/util/Arrays.html
    public static void misc1() {
        int[] input = new int[] { 1, 2, 3, 4, 5 };
        Integer[] input2 = new Integer {
            new Integer(1),
            new Integer(2),
            new Integer(3),
            new Integer(4),
            new Integer(5)
        };

        Integer[] input3 = Arrays.asList(input)
            .stream()
            .map((i) -> Integer::valueOf)
            .collect(Collectors::toList);

        // Get a list of values
        final List<Integer> l1 = Arrays.asList(1, 2, 3);

        // Binary search over elements
        final int index1 = Arrays.binarySearch(input, 4);

        // Copy
        final int[] copy1 = Arrays.copyOf(input, input.length);

        // Copy of range
        final int[] copy2 = Arrays.copyOfRange(input, 1, 3);

        // DeepEquals
        final boolean equal1 = Arrays.depEquals(
            new Integer[] { new Integer(1), new Integer(2) },
            new Integer[] { new Integer(3), new Integer(4) }
        )


        // To String
        final String string1 = Arrays.deepToString(input);

    }

}
