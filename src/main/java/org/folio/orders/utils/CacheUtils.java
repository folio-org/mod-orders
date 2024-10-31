package org.folio.orders.utils;

import com.github.benmanes.caffeine.cache.AsyncCache;
import com.github.benmanes.caffeine.cache.Caffeine;
import io.vertx.core.Context;
import io.vertx.core.Vertx;

import java.util.concurrent.Executor;
import java.util.concurrent.TimeUnit;

public class CacheUtils {

  public static <K, V> AsyncCache<K, V> buildAsyncCache(Vertx vertx, long cacheExpirationTime) {
    return buildAsyncCache(task -> vertx.runOnContext(v -> task.run()), cacheExpirationTime);
  }

  public static <K, V> AsyncCache<K, V> buildAsyncCache(Context context, long cacheExpirationTime) {
    return buildAsyncCache(task -> context.runOnContext(v -> task.run()), cacheExpirationTime);
  }

  private static <K, V> AsyncCache<K, V> buildAsyncCache(Executor executor, long cacheExpirationTime) {
    return Caffeine.newBuilder()
      .expireAfterWrite(cacheExpirationTime, TimeUnit.SECONDS)
      .executor(executor)
      .buildAsync();
  }

  private CacheUtils() {}

}
