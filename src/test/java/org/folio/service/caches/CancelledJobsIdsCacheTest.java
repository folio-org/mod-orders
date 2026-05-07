package org.folio.service.caches;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class CancelledJobsIdsCacheTest {

  public static final long CACHE_EXPIRATION_TIME_MINS = 5;

  private CancelledJobsIdsCache cache;

  @BeforeEach
  void setUp() {
    cache = new CancelledJobsIdsCache(CACHE_EXPIRATION_TIME_MINS);
  }

  @Test
  void shouldIdAddToCache() {
    String jobId = UUID.randomUUID().toString();
    cache.put(jobId);
    assertTrue(cache.contains(jobId));
  }

  @Test
  void shouldReturnFalseForNonExistentId() {
    String jobId = UUID.randomUUID().toString();
    assertFalse(cache.contains(jobId));
  }

  @Test
  void shouldThrowExceptionIfJobIdIsNull() {
    assertThrows(NullPointerException.class, () -> cache.contains(null));
  }

}
