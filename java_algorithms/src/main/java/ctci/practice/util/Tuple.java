package ctci.practice.util;

public class Tuple<A, B> {

    public final A a;
    public final B b;

    public Tuple(A a, B b) {
        this.a = a;
        this.b = b;
    }

    public static <A, B> Tuple<A, B> from(A a, B b) {
        return new Tuple<A, B>(a, b);
    }

}
