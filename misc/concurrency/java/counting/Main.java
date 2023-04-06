import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.function.Supplier;
import java.util.stream.IntStream;

public class Main {
  public class Counter {
    private int value = 0;
    public void increment() {
      synchronized (this) {
        this.value++;
      }
    }

    public String toString() {
      return String.format("Counter(%d)", this.value);
    }
  }

  public void test1() {
    try {
      var c = new Counter();
      var exe = Executors.newFixedThreadPool(3);
      var t1 = exe.submit(() -> IntStream.range(0, 1000).forEach(_i -> c.increment()));
      var t2 = exe.submit(() -> IntStream.range(0, 1000).forEach(_i -> c.increment()));
      var t3 = exe.submit(() -> IntStream.range(0, 1000).forEach(_i -> c.increment()));
      exe.awaitTermination(1000, TimeUnit.MILLISECONDS);
      t1.get(); t2.get(); t3.get();
      System.out.println(c);
      exe.shutdown();
      System.out.println("Terminated Successfully");
    } catch (InterruptedException e) {
      e.printStackTrace();
      throw new RuntimeException();
    } catch (ExecutionException e) {
      e.printStackTrace();
      throw new RuntimeException();
    }
  }

  public ConcurrentMap<String, Integer> map = new ConcurrentHashMap<String, Integer>();

  public Supplier<Integer> incTask(final String key) {
    final Integer RETRY_LIMIT = 10;
    return () -> {
      try {
        int retry = 0;
        while (retry < RETRY_LIMIT) { 
          if (map.containsKey(key)) {
            break;
          } else {
            retry++;
            Thread.sleep(500);
          }
        }

        if (retry >= RETRY_LIMIT) return -1;

        IntStream.range(0, 1000).forEach(_i -> {
          var v = map.get(key);
          map.replace(key, v, v+1);
        });
        return map.get(key);
      } catch (Exception e) {
        e.printStackTrace();
        throw new RuntimeException();
      }
    };
  }

  public void run() {
    try { 
      final String key1 = "key1", key2 = "key2";

      System.out.println("Starting...");
      var f1 = CompletableFuture.supplyAsync(incTask(key1));
      var f2 = CompletableFuture.supplyAsync(incTask(key2));
      var f3 = CompletableFuture.supplyAsync(incTask(key1));
      var f4 = CompletableFuture.supplyAsync(incTask(key2));

      var f5 = CompletableFuture.runAsync(() -> {
        try { 
          Thread.sleep(500);
          this.map.put(key1, 0);
          Thread.sleep(500);
          this.map.put(key2, 0);
        } catch (InterruptedException e) {
          e.printStackTrace();
          throw new RuntimeException();
        }
      });

      CompletableFuture.allOf(f1, f2, f3, f4, f5).get();
      System.out.printf("key1 -> %d\n", this.map.get(key1));
      System.out.printf("key2 -> %d\n", this.map.get(key2));
      System.out.println("Terminated Successfully");
    } catch (Exception e) {
      e.printStackTrace();
      throw new RuntimeException();
    }
  }

  public static void main(String[] args) {
    new Main().run();
  }
}