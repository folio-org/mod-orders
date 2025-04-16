package org.folio.service.caches;

import com.github.benmanes.caffeine.cache.AsyncCache;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.http.HttpMethod;
import jakarta.annotation.PostConstruct;
import org.apache.http.HttpStatus;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.util.OkapiConnectionParams;
import org.folio.rest.util.RestUtil;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.concurrent.CompletableFuture;

import static org.folio.orders.utils.CacheUtils.buildAsyncCache;

/**
 * An in-memory cache that stores total amount of imported records
 * associated with the import job ID ({@code jobExecutionId}).
 * Performs loading total amount of imported records from mod-source-record-manager module
 * if the cache does not contain value for total records amount by specified {@code jobExecutionId}.
 */
@Component
public class JobExecutionTotalRecordsCache {

  private static final Logger LOGGER = LogManager.getLogger();

  private static final String JOB_EXECUTION_URL = "/change-manager/jobExecutions/";
  private static final String JOB_PROGRESS_FIELD = "progress";
  private static final String JOB_TOTAL_RECORDS_FIELD = "total";

  private AsyncCache<String, Integer> asyncCache;

  @Value("${orders.cache.job.records.amount.expiration.seconds:3600}")
  private long cacheExpirationTime;

  @PostConstruct
  void init() {
    this.asyncCache = buildAsyncCache(Vertx.currentContext(), cacheExpirationTime);
  }

  /**
   * Returns total number of imported records by specified {@code jobExecutionId}.
   *
   * @param jobExecutionId - import job id
   * @param params         - okapi connection parameters
   * @return future with total records amount
   */
  public Future<Integer> get(String jobExecutionId, OkapiConnectionParams params) {
    try {
      return Future.fromCompletionStage(asyncCache.get(jobExecutionId, (key, executor) -> loadJobExecutionRecordsAmount(key, params)));
    } catch (Exception e) {
      LOGGER.warn("Error loading total records amount for jobExecution by id: '{}'", jobExecutionId, e);
      return Future.failedFuture(e);
    }
  }

  private CompletableFuture<Integer> loadJobExecutionRecordsAmount(String jobExecutionId, OkapiConnectionParams params) {
    LOGGER.debug("Trying to load total records amount for jobExecution by id '{}' for cache, okapi url: {}, tenantId: {}",
      jobExecutionId, params.getOkapiUrl(), params.getTenantId());

    return RestUtil.doRequest(params, JOB_EXECUTION_URL + jobExecutionId, HttpMethod.GET, null)
      .toCompletionStage()
      .toCompletableFuture()
      .thenCompose(httpResponse -> {
        if (httpResponse.getResponse().statusCode() == HttpStatus.SC_OK) {
          LOGGER.info("Total records amount was loaded by jobExecutionId '{}'", jobExecutionId);
          return CompletableFuture.completedFuture(httpResponse.getJson().getJsonObject(JOB_PROGRESS_FIELD).getInteger(JOB_TOTAL_RECORDS_FIELD));
        } else {
          String message = String.format("Error loading total records amount by jobExecutionId: '%s', status code: %s, response message: %s",
            jobExecutionId, httpResponse.getResponse().statusCode(), httpResponse.getBody());
          LOGGER.warn(message);
          return CompletableFuture.failedFuture(new CacheLoadingException(message));
        }
      });
  }
}
