import java.util.concurrent.Executors;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Semaphore;

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

  // LeetCode solution using CountDownLatch's
  public class Solution2 {
    private CountDownLatch waitForFirst;
    private CountDownLatch waitForSecond;
    public Solution2() {
      this.waitForFirst = new CountDownLatch(1);
      this.waitForSecond = new CountDownLatch(1);
    }

    public void first(Runnable printFirst) throws InterruptedException {
      printFirst.run();
      waitForFirst.countDown();
    }

    public void second(Runnable printSecond) throws InterruptedException {
      waitForFirst.await();
      printSecond.run();
      waitForSecond.countDown();
    }

    public void third(Runnable printThird) throws InterruptedException {
      waitForSecond.await();
      printThird.run();
    }
  }

  // Another solution using wait and notify
  public class Solution3 {
    private static int flag = 1;
    public Solution3() {}

    public synchronized void first(Runnable printFirst) throws InterruptedException {
      while (flag != 1) wait();
      flag = 2;
      printFirst.run();
      notifyAll();
    }

    public synchronized void second(Runnable printSecond) throws InterruptedException {
      while (flag != 2) wait();
      flag = 3;
      printSecond.run();
      notifyAll();
    }

    public synchronized void third(Runnable printThird) throws InterruptedException {
      while (flag != 3) wait();
      flag = 1;
      printThird.run();
      notifyAll();
    }
  }

  // Solution using Semaphores
  // I don't think the while loops here are the best solution, because
  // they'll just run infinitely, so while it's fast, I think using
  // Thread.yield() would be more efficient.
  public class Solution4 {
    Semaphore waitForFirst;
    Semaphore waitForSecond;

    public Solution4() {
      waitForFirst = new Semaphore(0);
      waitForSecond = new Semaphore(0);
    }

    public void first(Runnable printFirst) throws InterruptedException {
      printFirst.run();
      waitForFirst.release();
    }

    public void second(Runnable printSecond) throws InterruptedException {
      while (!waitForFirst.tryAcquire());
      printSecond.run();
      waitForSecond.release();
    }

    public void third(Runnable printThird) throws InterruptedException {
      while (!waitForSecond.tryAcquire());
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