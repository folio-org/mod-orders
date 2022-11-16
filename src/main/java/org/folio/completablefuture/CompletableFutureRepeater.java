package org.folio.completablefuture;

import java.util.function.Function;
import java.util.function.Supplier;

import io.vertx.core.Future;

public final class CompletableFutureRepeater {
  /**
   * Run <code>task</code> repeatedly until it succeeds or it has run <code>max</code> times.
   * Return the result of the succeeding task, or the last run's failure if all runs fail.
   */
  public static <T> Future<T> repeat(int max, Supplier<Future<T>> task) {
    Future<T> future = task.get();
    for (int i = 1; i < max; i++) {
      future = future.map(Future::succeededFuture)
           .onFailure(t -> task.get())
          .compose(Function.identity());
    }
    return future;
  }
}
