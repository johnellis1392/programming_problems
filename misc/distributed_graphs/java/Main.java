import java.util.HashSet;
import java.util.HashMap;

public class Main {
  public enum MessageType { REQUEST, RESPONSE, ACK }
  public enum RequestType { COUNT }
  public interface Message {
    public MessageType getMessageType();
  }

  public class AckMessage implements Message {
    public MessageType  getMessageType() { return MessageType.ACK; }
  }

  public abstract class Request implements Message {
    public abstract RequestType getRequestType();
    public MessageType getMessageType() { return MessageType.REQUEST; }
  }

  public abstract class Response<T> implements Message {
    public abstract RequestType getRequestType();
    public MessageType getMessageType() { return MessageType.RESPONSE; }
  }

  public class CountRequest extends Request {
    public RequestType getRequestType() { return RequestType.COUNT; }
  }

  public class CountResponse extends Response<Integer> {
    public final Integer value;
    public CountResponse(Integer value) { this.value = value; }
    public RequestType getRequestType() { return RequestType.COUNT; }
  }


  public class GraphNode {
    HashSet<GraphNode> adjNodes = new HashSet<>();
    HashSet<GraphNode> visited = new HashSet<>();

    final String name;
    GraphNode parent = null;
    Integer nResponses;
    Integer count;

    public GraphNode(String name) {
      this.name = name;
      this.nResponses = 0;
    }

    private void tryFinalize() {
      // Check if we're finished. If we are, send message back to parent.
      if (this.nResponses == this.adjNodes.size()) {
        System.out.printf("Finalized node '%s' with value %d\n", this.name, this.count);
        this.parent.receive(this, new CountResponse(this.count));
      }
    }

    private void handleRequest(
      final GraphNode sender,
      final Request request
    ) {
      // Check if message is from a node you've already contacted
      // if it is, skip.
      // if not, broadcast message to all child nodes.
      if (this.parent == null) {
        this.parent = sender;
        this.send(request);
      } else if (this.visited.contains(sender)) {
        this.nResponses++;
        sender.receive(this, new AckMessage());
      } else {
        // NOOP
      }
    }

    private void handleResponse(
      final GraphNode sender,
      final Response response
    ) {
      // Log message from sender.
      // Update nResponses.
      switch (response.getRequestType()) {
        case COUNT:
          this.count += ((CountResponse)response).value;
          break;
        default:
          // NOOP
          break;
      }
    }

    public void receive(
      final GraphNode sender,
      final Message message
    ) {
      System.out.printf("Received Message: %s, from sender %s\n", message.toString(), sender == null ? "null" : sender.name);
      switch (message.getMessageType()) {
        case REQUEST:
          this.handleRequest(sender, (Request)message);
          break;
        case RESPONSE:
          this.handleResponse(sender, (Response)message);
          break;
        case ACK:
          this.nResponses++;
          break;
        default:
          // Handle error
          break;
      }
      this.tryFinalize();
    }

    public void send(final Message message) {
      for (GraphNode node : this.adjNodes) {
        if (node != this.parent) {
          this.visited.add(node);
          node.receive(this, message);
        }
      }
    }
  }

  private void connect(GraphNode a, GraphNode b) {
    a.adjNodes.add(b);
    b.adjNodes.add(a);
  }

  public void run() {
    var a = new GraphNode("A");
    var b = new GraphNode("B");
    var c = new GraphNode("C");
    var d = new GraphNode("D");
    var e = new GraphNode("E");

    connect(a, b);
    connect(a, e);
    connect(b, c);
    connect(b, d);
    connect(c, d);

    a.receive(null, new CountRequest());
  }

  public static void main(final String[] args) {
    var main = new Main();
    main.run();
  }
}