import java.util.concurrent.Executors;

public class PrintInOrder {
  public class Solution1 {
    private volatile Boolean firstDone = false;
    private volatile Boolean secondDone = false;
    public Solution1() {}

    private Boolean isFirstDone() {
      synchronized (this.firstDone) {
        return this.firstDone;
      }
    }

    private Boolean isSecondDone() {
      synchronized (this.secondDone) {
        return this.secondDone;
      }
    }

    public void first(Runnable printFirst) throws InterruptedException {
      printFirst.run();
      synchronized (this.firstDone) {
        this.firstDone = true;
      }
    }

    public void second(Runnable printSecond) throws InterruptedException {
      while (!this.isFirstDone()) Thread.yield();
      printSecond.run();
      synchronized (this.secondDone) {
        this.secondDone = true;
      }
    }

    public void third(Runnable printThird) throws InterruptedException {
      while (!this.isSecondDone()) Thread.yield();
      printThird.run();
    }
  }

  public void runTest(final int[] inputs) {
    var executor = Executors.newFixedThreadPool(3);
    var s = new Solution1();
    for (var i : inputs) {
      switch (i) {
      case 1:
        executor.submit(() -> {
          try { s.first(() -> System.out.print("first")); }
          catch (InterruptedException e) { e.printStackTrace(); }
        });
        break;
      case 2:
        executor.submit(() -> {
          try { s.second(() -> System.out.print("second")); }
          catch (InterruptedException e) { e.printStackTrace(); }
        });
        break;
      case 3:
        executor.submit(() -> {
          try { s.third(() -> System.out.print("third")); }
          catch (InterruptedException e) { e.printStackTrace(); }
        });
        break;
      default:
        break;
      }
    }
    executor.shutdown();
  }

  public static void main(final String[] args) {
    try { 
      var m = new PrintInOrder();
      m.runTest(new int[] { 1, 2, 3 });
      Thread.sleep(200);
      System.out.println();
      m.runTest(new int[] { 1, 3, 2 });
      Thread.sleep(200);
      System.out.println();
    } catch (InterruptedException e) {
      e.printStackTrace();
    }
  }
}