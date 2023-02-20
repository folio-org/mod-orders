package org.folio.service.caches;

import com.github.benmanes.caffeine.cache.AsyncCache;
import com.github.benmanes.caffeine.cache.Caffeine;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.Json;
import org.apache.http.HttpStatus;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import org.folio.OrganizationCollection;
import org.folio.processing.mapping.defaultmapper.processor.parameters.MappingParameters;
import org.folio.rest.util.OkapiConnectionParams;
import org.folio.rest.util.RestUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;

/**
 * The class responsible for caching {@link MappingParameters}
 */
@Component
public class MappingParametersCache {
  private static final Logger LOGGER = LogManager.getLogger();
  public static final String ORGANIZATIONS = "/organizations/organizations?limit=%s";

  @Value("${orders.cache.mapping.parameters.settings.limit:1000}")
  public int settingsLimit;

  @Value("${orders.cache.mapping.parameters.expiration.seconds:3600}")
  private long cacheExpirationTime;

  private AsyncCache<String, MappingParameters> cache;

  @Autowired
  public MappingParametersCache(Vertx vertx) {
    cache = Caffeine.newBuilder()
      .expireAfterAccess(cacheExpirationTime, TimeUnit.SECONDS)
      .executor(task -> vertx.runOnContext(v -> task.run()))
      .buildAsync();
  }

  /**
   * Retrieves {@link MappingParameters} from cache by tenantId
   * @param params {@link OkapiConnectionParams} connection params
   * @return CompletableFuture with {@link MappingParameters} object
   */
  public Future<MappingParameters> get(OkapiConnectionParams params) {
    try {
      return Future.fromCompletionStage(cache.get(params.getTenantId(), (key, executor) -> loadMappingParameters(params)));
    } catch (Exception e) {
      LOGGER.warn("get:: Error loading organizations from cache, tenantId: '{}'", params.getTenantId(), e);
      return Future.failedFuture(e);
    }
  }

  /**
   * Generates {@link MappingParameters} with collection of {@link org.folio.Organization}
   * @param params {@link OkapiConnectionParams} connection params
   * @return CompletableFuture with {@link MappingParameters} object
   */
  private CompletableFuture<MappingParameters> loadMappingParameters(OkapiConnectionParams params) {
    String tenantId = params.getTenantId();
    LOGGER.debug("loadMappingParameters:: Trying to load organizations '{}' for cache, okapi url: {}, tenantId: {}",
      tenantId, params.getOkapiUrl(), params.getTenantId());

    return RestUtil.doRequest(params, String.format(ORGANIZATIONS, settingsLimit), HttpMethod.GET, null)
      .toCompletionStage()
      .toCompletableFuture()
      .thenCompose(httpResponse -> {
        if (httpResponse.getResponse().statusCode() == HttpStatus.SC_OK) {
          LOGGER.info("loadMappingParameters:: Organizations was loaded for cache, tenantId '{}'", tenantId);
          MappingParameters mappingParameters = new MappingParameters();
          OrganizationCollection organizationCollection = Json.decodeValue(httpResponse.getJson().encode(), OrganizationCollection.class);

          mappingParameters.withOrganizations(organizationCollection.getOrganizations());
          return CompletableFuture.completedFuture(mappingParameters);
        } else {
          String message = String.format("loadMappingParameters:: Error loading organizations for cache, tenantId: '%s', status code: %s, response message: %s",
            tenantId, httpResponse.getResponse().statusCode(), httpResponse.getBody());
          LOGGER.warn(message);
          return CompletableFuture.failedFuture(new CacheLoadingException(message));
        }
      });
  }
}
