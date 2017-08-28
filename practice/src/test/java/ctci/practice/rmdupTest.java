package ctci.practice;

import junit.framework.TestCase;
import junit.framework.TestSuite;
import org.junit.Test;

import java.util.Random;


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

    private Integer i;

    @Before
    public void setup() {
        Random random = new Random();
        this.i = Random.randint();
    }


    @Test
    public void test1() {
        assertTrue(true);
    }

}
