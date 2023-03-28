
public class Rot13 {
    public static String rot13(final String message) {
        final int a = 'a';
        final int A = 'A';
        char[] cs = message.toCharArray();
        for (int i = 0; i < cs.length; i++) {
            final int c = (int)cs[i];
            if ('a' <= c && c <= 'z')
                cs[i] = (char)(a + (((c - a) + 26 - 13) % 26));
            else if ('A' <= c && c <= 'Z')
                cs[i] = (char)(A + (((c - A) + 26 - 13) % 26));
        }
        return new String(cs);
    }

    public static void main(final String[] args) {
        final String input = "Va gur ryringbef, gur rkgebireg ybbxf ng gur BGURE thl'f fubrf";
        // final String input = "EBG13 rknzcyr.";
        System.out.println(rot13(input));
    }
}
