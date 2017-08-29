package ctci.practice.util;

import junit.framework.TestCase;
import junit.framework.TestSuite;
import org.junit.Test;
// import org.junit.Before;

import java.util.Random;


public class LinkedListTest extends TestCase {

    @Test
    public void test1() {
        LinkedList<Integer> list = new LinkedList<>();
        list.add(new Integer(1));
        list.add(new Integer(2));
        list.add(new Integer(3));

        assertEqual(list.size(), 3);
        assertEqual(list.head.value, 1);
        assertEqual(list.head.next.value, 2);
        assertEqual(list.head.next.next.value, 3);
    }

}
