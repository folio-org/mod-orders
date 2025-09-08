package org.folio.service.caches;

import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.UUID;
import java.util.concurrent.TimeUnit;

/**
 * An in-memory cache that stores IDs of cancelled import jobs.
 */
@Component
public class CancelledJobsIdsCache {

  private final Cache<UUID, Boolean> cache;

  public CancelledJobsIdsCache(
    @Value("${orders.cache.cancelled.jobs.expiration.minutes:1440}") long cacheExpirationTimeMins) {
    this.cache = Caffeine.newBuilder()
      .expireAfterWrite(cacheExpirationTimeMins, TimeUnit.MINUTES)
      .build();
  }

  /**
   * Puts the specified {@code jobId} into the cache.
   *
   * @param jobId the UUID to put into the cache
   */
  public void put(UUID jobId) {
    cache.put(jobId, Boolean.TRUE);
  }

  /**
   * Checks if the cache contains the specified {@code jobId}.
   *
   * @param jobId the job UUID to check
   * @return {@code true} if the cache contains the {@code jobId}, {@code false} otherwise
   */
  public boolean contains(UUID jobId) {
    return cache.asMap().containsKey(jobId);
  }
}
