package ctci.practice.notes;

import java.util.Collection;
import java.util.Arrays;


// Collectors Javadoc:
// https://docs.oracle.com/javase/8/docs/api/java/util/stream/Collectors.html
//
// Collection Javadoc:
// https://docs.oracle.com/javase/8/docs/api/java/util/Collection.html


public class StreamsNotes {

    public static void mapExample1() {
        final Collection<Integer> input = Arrays.asList(1, 2, 3, 4, 5);
        final Collection<String> output = input
            .stream()
            .map(Integer::toHexString)
            .collect(Collectors::toList);


        final String[] output2 = input
            .stream()
            .map(Integer::toHexString)
            .collect(Collectors::toList)
            .toArray(new String[input.length]);
    }

}
