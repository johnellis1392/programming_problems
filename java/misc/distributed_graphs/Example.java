import java.util.HashMap;
import java.util.HashSet;

/**
 * Logic for this set of classes taken from here:
 * https://bytethisstore.com/articles/pg/distributed-graph-traversal
 */
public class Example {
  public record GraphMessage<T, M>(
    GraphNode<T> receivedFrom,
    String messageType,
    M messageData
  ) {}

  interface IGraphMessageResponder<T, M> {
    public String getMessageType();
    public void onMessageReceived(GraphMessage<T, M> message);
  }

  public class GraphNode<T> {
    private HashSet<GraphNode<T>> children = new HashSet<GraphNode<T>>();
    private HashMap<String, IGraphMessageResponder<T, ?>> messageResponders = new HashMap<>();
    public final T value;
    public GraphNode(T value) {
      this.value = value;
    }

    public void addChild(GraphNode<T> node) {
      children.add(node);
      if (!node.hasChild(this)) {
        node.addChild(this);
      }
    }

    public boolean hasChild(GraphNode<T> node) {
      return this.children.contains(node);
    }

    public HashSet<GraphNode<T>> getChildren() {
      return this.children;
    }

    public <M> void addMessageResponder(IGraphMessageResponder<T, M> responder) {
      this.messageResponders.put(responder.getMessageType(), responder);
    }

    public <M> void receiveMessage(GraphMessage<T, M> message) {
      IGraphMessageResponder<T, M> responder = 
        (IGraphMessageResponder<T, M>) messageResponders.get(message.messageType);
      responder.onMessageReceived(message);
    }

    public <M> void sendMessage(GraphNode<T> toNode, String type, M message) {
      GraphMessage<T, Object> messageObj = new GraphMessage<T, Object>(this, type, message);
      toNode.receiveMessage(messageObj);
    }
  }

  public abstract class AbstractGraphMessageResponder<T, M> implements IGraphMessageResponder<T, M> {
    protected final GraphNode<T> associatedNode;
    AbstractGraphMessageResponder(GraphNode<T> associatedNode) {
      this.associatedNode = associatedNode;
    }
    public abstract String getMessageType();
    public abstract void onMessageReceived(GraphMessage<T, M> message);
  }

  public record GraphMessageRequestorResponderData<M>(
    String messageSubType,
    boolean isNullResponse,
    M data
  ) {}

  public abstract class AbstractGraphMessageRequestorResponder<T, M> extends AbstractGraphMessageResponder<T, GraphMessageRequestorResponderData<M>> {
    public AbstractGraphMessageRequestorResponder(GraphNode<T> associatedNode) {
      super(associatedNode);
    }

    public void onMessageReceived(GraphMessage<T, GraphMessageRequestorResponderData<M>> message) {
      if (message.messageData.messageSubType.equals("request")) {
        this.onMessageRequestReceived(message);
      } else {
        this.onMessageResponseReceived(message);
      }
    }

    protected abstract void onMessageRequestReceived(GraphMessage<T, GraphMessageRequestorResponderData<M>> message);
    protected abstract void onMessageResponseReceived(GraphMessage<T, GraphMessageRequestorResponderData<M>> message);

    protected void sendSubRequestResponse(GraphNode<T> sendTo, M data, boolean isNullResponse) {
      if (sendTo != null) {
        this.associatedNode.sendMessage(
          sendTo,
          getMessageType(),
          new GraphMessageRequestorResponderData<M>(
            "response",
            isNullResponse,
            data
          )
        );
        return;
      }
    }

    protected void sendSubRequest(GraphNode<T> sendTo, M data) {
      if (sendTo != null) {
        this.associatedNode.sendMessage(
          sendTo,
          getMessageType(),
          new GraphMessageRequestorResponderData<M>(
            "request",
            false,
            data
          )
        );
        return;
      }
    }
  }

  public record NodeCounterMessage(int count) {}

  public class NodeCounterMessageResponder<T> extends AbstractGraphMessageRequestorResponder<T, NodeCounterMessage> {
    public static final String MESSAGE_TYPE = "Node Counter";

    public String getMessageType() {
      return NodeCounterMessageResponder.MESSAGE_TYPE;
    }

    private boolean alreadyReceivedRequest = false;
    private int nodeCount = 1;
    private int numResponsesReceived = 0;
    private GraphNode<T> originalRequestor = null;

    public NodeCounterMessageResponder(GraphNode<T> associatedNode) {
      super(associatedNode);
    }

    protected void onMessageRequestReceived(
      GraphMessage<T, GraphMessageRequestorResponderData<NodeCounterMessage>> message
    ) {
      if (alreadyReceivedRequest) {
        this.sendSubRequestResponse(
          message.receivedFrom,
          new NodeCounterMessage(-1),
          true
        );
        return;
      }

      this.alreadyReceivedRequest = true;
      this.originalRequestor = message.receivedFrom;

      for (GraphNode<T> child : this.associatedNode.getChildren()) {
        if (child == message.receivedFrom) {
          this.numResponsesReceived++;
          this.checkIfConcluded();
        } else {
          this.sendSubRequest(child, new NodeCounterMessage(-1));
        }
      }

      if (this.associatedNode.getChildren().size() == 0) {
        this.sendSubRequestResponse(originalRequestor, new NodeCounterMessage(1), false);
      }
    }

    protected void onMessageResponseReceived(
      GraphMessage<T, GraphMessageRequestorResponderData<NodeCounterMessage>> message
    ) {
      this.numResponsesReceived++;
      if (!message.messageData.isNullResponse) {
        this.nodeCount += message.messageData.data.count;
      }
      this.checkIfConcluded();
    }

    private void checkIfConcluded() {
      if (numResponsesReceived == associatedNode.getChildren().size()) {
        System.out.println("Count: " + this.nodeCount);
        this.sendSubRequestResponse(this.originalRequestor, new NodeCounterMessage(this.nodeCount), false);
      }
    }
  }

  public record NodeSumMessage(int sum) {}
  public record GraphNumber(String nodeName, int nodeValue) {}

  public class NodeSumMessageResponder extends AbstractGraphMessageRequestorResponder<GraphNumber, NodeSumMessage> {
    public static final String MESSAGE_TYPE = "Node Sum";
    public String getMessageType() {
      return NodeSumMessageResponder.MESSAGE_TYPE;
    }

    private boolean alreadyReceivedRequest = false;
    private int partialSum = this.associatedNode.value.nodeValue;
    private int numResponsesReceived = 0;
    private GraphNode<GraphNumber> originalRequestor = null;

    public NodeSumMessageResponder(GraphNode<GraphNumber> associatedNode) {
      super(associatedNode);
    }

    protected void onMessageRequestReceived(
      GraphMessage<GraphNumber, GraphMessageRequestorResponderData<NodeSumMessage>> message
    ) {
      if (this.alreadyReceivedRequest) {
        this.sendSubRequestResponse(
          message.receivedFrom,
          new NodeSumMessage(-1),
          true
        );
        return;
      }

      this.alreadyReceivedRequest = true;
      this.originalRequestor = message.receivedFrom;

      for (GraphNode<GraphNumber> child : this.associatedNode.getChildren()) {
        if (child == message.receivedFrom) {
          this.numResponsesReceived++;
          this.checkIfConcluded();
        } else {
          this.sendSubRequest(child, new NodeSumMessage(-1));
        }
      }

      if (this.associatedNode.getChildren().size() == 0) {
        this.sendSubRequestResponse(
          originalRequestor,
          new NodeSumMessage(this.partialSum),
          false
        );
      }
    }

    protected void onMessageResponseReceived(
      GraphMessage<GraphNumber, GraphMessageRequestorResponderData<NodeSumMessage>> message
    ) {
      this.numResponsesReceived++;
      if (!message.messageData.isNullResponse) {
        this.partialSum += message.messageData.data.sum;
      }
      this.checkIfConcluded();
    }

    private void checkIfConcluded() {
      if (this.numResponsesReceived == associatedNode.getChildren().size()) {
        System.out.println("Sum: " + this.partialSum);
        this.sendSubRequestResponse(
          this.originalRequestor,
          new NodeSumMessage(this.partialSum),
          false
        );
      }
    }
  }

  public void run() {
    var a = new GraphNode<GraphNumber>(new GraphNumber("a", 7));
    var b = new GraphNode<GraphNumber>(new GraphNumber("b", 12));
    a.addChild(b);

    var c = new GraphNode<GraphNumber>(new GraphNumber("c", 1));
    a.addChild(c);

    var d = new GraphNode<GraphNumber>(new GraphNumber("d", 3));
    a.addChild(d);
    c.addChild(d);

    var e = new GraphNode<GraphNumber>(new GraphNumber("e", 8));
    c.addChild(e);

    var f = new GraphNode<GraphNumber>(new GraphNumber("f", 16));
    b.addChild(f);
    e.addChild(f);

    var g = new GraphNode<GraphNumber>(new GraphNumber("g", 24));
    b.addChild(g);
    d.addChild(g);

    var h = new GraphNode<GraphNumber>(new GraphNumber("h", 8));
    a.addChild(h);
    g.addChild(h);
    
    Object[] nodes = new Object[] { a, b, c, d, e, f, g, h };
    for (int i = 0; i < nodes.length; i++) {
      GraphNode<GraphNumber> node = (GraphNode<GraphNumber>)nodes[i];
      node.addMessageResponder(new NodeCounterMessageResponder<GraphNumber>(node));
      node.addMessageResponder(new NodeSumMessageResponder(node));
    }

    System.out.println("--- Nodes Count ---");
    GraphMessageRequestorResponderData<NodeCounterMessage> countMessage = 
      new GraphMessageRequestorResponderData<NodeCounterMessage>(
        "request",
        false,
        new NodeCounterMessage(0)
      );
    a.receiveMessage(
      new GraphMessage<GraphNumber, GraphMessageRequestorResponderData<NodeCounterMessage>>(
        null,
        NodeCounterMessageResponder.MESSAGE_TYPE,
        countMessage
      )
    );

    // System.out.println("--- Nodes Value Sum ---");
    // GraphMessageRequestorResponderData<NodeSumMessage> sumMessage =
    //   new GraphMessageRequestorResponderData<NodeSumMessage>(
    //     "request",
    //     false,
    //     new NodeSumMessage(0)
    //   );
    // a.receiveMessage(
    //   new GraphMessage<GraphNumber, GraphMessageRequestorResponderData<NodeSumMessage>>(
    //     null,
    //     NodeSumMessageResponder.MESSAGE_TYPE,
    //     sumMessage
    //   )
    // );
  }

  public static void  main(final String[] args) {
    var e = new Example();
    e.run();
  }
}
