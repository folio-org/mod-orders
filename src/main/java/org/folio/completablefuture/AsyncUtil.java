package org.folio.completablefuture;

import java.util.Objects;
import java.util.function.Supplier;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Promise;

public final class AsyncUtil {

  public static <T> Handler<AsyncResult<T>> asyncResultHandler(Promise<T> promise) {
    Objects.requireNonNull(promise);
    return result -> {
      if (result.succeeded()) {
        promise.complete(result.result());
      } else {
        promise.fail(result.cause());
      }
    };
  }

  public static <T> Future<T> executeBlocking(Context ctx, boolean isOrdered, Supplier<T> supplier) {
    Objects.requireNonNull(supplier);
    Promise<T> promise = Promise.promise();
    ctx.owner().executeBlocking(blockingFeature -> {
      try {
        T result = supplier.get();
        blockingFeature.complete(result);
      } catch (Exception e) {
        blockingFeature.fail(e);
      }
    }, isOrdered, asyncResultHandler(promise));
    return promise.future();
  }

  public static Future<Void> executeBlocking(Context ctx, boolean isOrdered, Runnable runnable) {
    Objects.requireNonNull(runnable);
    Promise<Void> promise = Promise.promise();
    ctx.owner().executeBlocking(blockingFeature -> {
      try {
        runnable.run();
        blockingFeature.complete(null);
      } catch (Exception e) {
        blockingFeature.fail(e);
      }
    }, isOrdered, asyncResultHandler(promise));
    return promise.future();
  }
}
