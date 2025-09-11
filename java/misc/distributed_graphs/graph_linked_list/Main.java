
public class Main {
  public interface Message { }
  public abstract class Request implements Message { }
  public abstract class Response implements Message { }

  public class CountRequest extends Request {
    public String toString() { return "CountRequest()"; }
  }
  public class CountResponse extends Response {
    final int value;
    public CountResponse(final int value) {
      this.value = value;
    }

    public String toString() {
      return String.format("CountResponse(%d)", this.value);
    }
  }

  public class Node {
    final String name;
    Node next;
    Node parent;

    public Node(final String name) {
      this.name = name;
    }
  
    public void receive(final Message message) {
      System.out.println(name + " received " + message);
      switch (message) {
        case CountRequest req -> {
          if (this.next ==  null)  {
            this.respond(new CountResponse(1));
          } else {
            this.next.receive(message);
          }
        }
        case CountResponse res -> {
          if (this.parent ==  null) {
            System.out.printf("Result = %d\n", res.value + 1);
          } else {
            this.respond(new CountResponse(res.value + 1));
          }
        }
        default -> System.out.printf("ERROR: Received unknown message type '%s'\n", message.getClass().getName());
      }
    }

    public void send(final Message message) {
      if (this.next != null)  {
        this.next.receive(message);
      }
    }

    public void respond(final Message message) {
      if (this.parent != null) {
        this.parent.receive(message);
      }
    }
  }

  public void connect(Node a, Node b) {
    b.parent = a;
    a.next = b;
  }

  public void run() {
    var a = new Node("A");
    var b = new Node("B");
    var c = new Node("C");
    var d = new Node("D");
    var e = new Node("E");
    connect(a, b);
    connect(b, c);
    connect(c, d);
    connect(d, e);
    a.receive(new CountRequest());
  }

  public static void  main(final String[] args) {
    new Main().run();
  }
}
