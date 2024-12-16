package org.folio.orders.utils;

import io.vertx.core.Future;

import java.util.concurrent.Callable;

public class FutureUtils {

  public static Future<Void> asFuture(Runnable runnable) {
    return asFuture(() -> {
      runnable.run();
      return null;
    });
  }

  public static <T> Future<T> asFuture(Callable<T> callable) {
    try {
      return Future.succeededFuture(callable.call());
    } catch (Exception e) {
      return Future.failedFuture(e);
    }
  }

  private FutureUtils() {}

}
