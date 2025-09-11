import java.math.BigInteger;

public class LargeFactorials {
    private static BigInteger f(final int n) {
        BigInteger result = BigInteger.valueOf(1);
        for (int i = 1; i <= n; i++)
            result = result.multiply(BigInteger.valueOf(i));
        return result;
    }

    public static String Factorial(final int n) {
        return n < 0 ? null : f(n).toString();
    }

    private static void assertEquals(final String expected, final String actual) {
        System.out.printf(
            " %s == %s => %s \n",
            expected,
            actual,
            expected.equals(actual)
        );
    }

    public static void main(final String[] args) {
        assertEquals("1", Factorial(1));
        assertEquals("120", Factorial(5));
        assertEquals("362880", Factorial(9));
        assertEquals("1307674368000", Factorial(15));
    }
}
