package org.folio.completablefuture;

import java.util.function.Supplier;

import io.vertx.core.Future;

public final class VertxFutureRepeater {

  private VertxFutureRepeater(){
  }

  /**
   * Run <code>task</code> repeatedly until it succeeds or it has run <code>max</code> times.
   * Return the result of the succeeding task, or the last run's failure if all runs fail.
   */
  public static <T> Future<T> repeat(int max, Supplier<Future<T>> task) {
    return task.get()
        .recover(e -> {
          if (max <= 1) {
            return Future.failedFuture(e);
          }
          return repeat(max - 1, task);
        });
  }
}
