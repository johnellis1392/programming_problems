package ctci.practice;

import junit.framework.TestCase;
import junit.framework.TestSuite;
import org.junit.Test;
import org.junit.Before;

import java.util.Random;
import ctcl.practice.util.LinkedList;
import ctcl.practice.util.Tuple;


// Read this for an introduction to JUnit:
// https://veerasundar.com/blog/2009/06/getting-started-with-junit-4-java-testing-framework/
//
// And this:
// https://github.com/junit-team/junit4/wiki/getting-started
//
// Faker for test data:
// https://www.bloco.io/blog/2015/faker-a-library-to-generate-fake-data-for-java-android
//
// Another Faker clone:
// https://github.com/DiUS/java-faker
public class rmdupTest extends TestCase {

    public final int capacity = 20;
    public final int max = 1000;
    public final Random random = new Random();

    // @Before
    public Tuple<LinkedList<Integer>, LinkedList<Integer>> randomList() {
        Integer[] integers = new Integer[capacity];
        for (int i = 0; i < capacity; i++)
            integers[i] = new Integer(this.random.nextInt(this.max));
        LinkedList<Integer> input = LinkedList.fromArray(integers);

        final int n = this.random.nextInt(this.max);
        integers = new Integer[capacity - 1];
        for (int i = 0; i < capacity - 1; i++)
            integers[i] = new Integer(this.random.nextInt(this.max));
        LinkedList<Integer> expected = LinkedList.fromArray(integers);

        return Tuple.from(input, expected);
    }


    @Test
    public void test1() {
        // LinkedList<Integer> list = this.randomList();
        Tuple<LinkedList<Integer>, LinkedList<Integer>> tuple = this.randomList();
        LinkedList<Integer> input = tuple.a;
        LinkedList<Integer> expected = tuple.b;
        final int n = this.random.nextInt(max);

        LinkedList<Integer> actual = rmdup.rmdup(input);
        assertEqual(list, expected);
    }

}
