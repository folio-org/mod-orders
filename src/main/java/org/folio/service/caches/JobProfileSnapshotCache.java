package org.folio.service.caches;

import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;

import org.apache.http.HttpStatus;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.jaxrs.model.ProfileSnapshotWrapper;
import org.folio.rest.util.OkapiConnectionParams;
import org.folio.rest.util.RestUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import com.github.benmanes.caffeine.cache.AsyncCache;

import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.Json;

import static org.folio.orders.utils.CacheUtils.buildAsyncCache;

@Component
public class JobProfileSnapshotCache {

  private static final Logger LOGGER = LogManager.getLogger();
  public static final String DATA_IMPORT_PROFILES_JOB_PROFILE_SNAPSHOTS = "/data-import-profiles/jobProfileSnapshots/";

  @Value("${orders.cache.jobprofile.expiration.seconds:3600}")
  private long cacheExpirationTime;
  private AsyncCache<String, Optional<ProfileSnapshotWrapper>> cache;

  @Autowired
  public JobProfileSnapshotCache(Vertx vertx) {
    cache = buildAsyncCache(vertx, cacheExpirationTime);
  }

  public Future<Optional<ProfileSnapshotWrapper>> get(String profileSnapshotId, OkapiConnectionParams params) {
    try {
      return Future.fromCompletionStage(cache.get(profileSnapshotId, (key, executor) -> loadJobProfileSnapshot(key, params)));
    } catch (Exception e) {
      LOGGER.warn("Error loading ProfileSnapshotWrapper by id: '{}'", profileSnapshotId, e);
      return Future.failedFuture(e);
    }
  }

  private CompletableFuture<Optional<ProfileSnapshotWrapper>> loadJobProfileSnapshot(String profileSnapshotId, OkapiConnectionParams params) {
    LOGGER.debug("Trying to load jobProfileSnapshot by id  '{}' for cache, okapi url: {}, tenantId: {}", profileSnapshotId, params.getOkapiUrl(), params.getTenantId());

    return RestUtil.doRequest(params, DATA_IMPORT_PROFILES_JOB_PROFILE_SNAPSHOTS + profileSnapshotId, HttpMethod.GET, null)
      .toCompletionStage()
      .toCompletableFuture()
      .thenCompose(httpResponse -> {
        if (httpResponse.getResponse().statusCode() == HttpStatus.SC_OK) {
          LOGGER.info("JobProfileSnapshot was loaded by id '{}'", profileSnapshotId);
          return CompletableFuture.completedFuture(Optional.of(Json.decodeValue(httpResponse.getJson().encode(), ProfileSnapshotWrapper.class)));
        } else if (httpResponse.getResponse().statusCode() == HttpStatus.SC_NOT_FOUND) {
          LOGGER.warn("JobProfileSnapshot was not found by id '{}'", profileSnapshotId);
          return CompletableFuture.completedFuture(Optional.empty());
        } else {
          String message = String.format("Error loading jobProfileSnapshot by id: '%s', status code: %s, response message: %s",
            profileSnapshotId, httpResponse.getResponse().statusCode(), httpResponse.getBody());
          LOGGER.warn(message);
          return CompletableFuture.failedFuture(new CacheLoadingException(message));
        }
      });
  }

}
