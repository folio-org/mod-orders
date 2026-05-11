package org.folio.orders.utils;

import io.vertx.core.Future;
import lombok.experimental.UtilityClass;

import java.util.List;
import java.util.concurrent.Callable;

import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;

@UtilityClass
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


 public static <T> Future<List<T>> unwrap(List<Future<List<T>>> pieceFutures) {
    return collectResultsOnSuccess(pieceFutures)
      .map(lists -> lists.stream()
        .flatMap(List::stream)
        .toList());
  }
}
