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
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.util.OkapiConnectionParams;
import org.folio.rest.util.RestUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.TimeUnit;

import static java.lang.String.format;
import static org.folio.orders.utils.HelperUtils.encodeQuery;
import static org.folio.service.orders.utils.HelperUtils.collectResultsOnSuccess;

/**
 * The class responsible for caching {@link MappingParameters}
 */
@Component
public class MappingParametersCache {
  private static final Logger LOGGER = LogManager.getLogger();

  private static final String ORGANIZATIONS = "/organizations/organizations";

  private static final String SORT_BY_ID_QUERY = "(cql.allRecords=1) sortBy id";

  @Value("${orders.cache.mapping.parameters.settings.limit:5000}")
  private int settingsLimit;

  @Value("${orders.cache.mapping.parameters.expiration.seconds:3600}")
  private long cacheExpirationTime;

  private final AsyncCache<String, MappingParameters> cache;

  private final RestClient restClient;

  @Autowired
  public MappingParametersCache(Vertx vertx, RestClient restClient) {
    LOGGER.info("MappingParametersCache:: settings limit: '{}'", settingsLimit);
    cache = Caffeine.newBuilder()
      .expireAfterAccess(cacheExpirationTime, TimeUnit.SECONDS)
      .executor(task -> vertx.runOnContext(v -> task.run()))
      .buildAsync();
    this.restClient = restClient;
  }

  /**
   * Retrieves {@link MappingParameters} from cache by tenantId
   *
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
   *
   * @param params {@link OkapiConnectionParams} connection params
   * @return CompletableFuture with {@link MappingParameters} object
   */
  private CompletableFuture<MappingParameters> loadMappingParameters(OkapiConnectionParams params) {
    String tenantId = params.getTenantId();
    LOGGER.debug("loadMappingParameters:: Trying to load organizations '{}' for cache, okapi url: {}, tenantId: {}",
      tenantId, params.getOkapiUrl(), params.getTenantId());

    return RestUtil.doRequest(params, getOrganizationsSortedLimitPath(settingsLimit), HttpMethod.GET, null)
      .toCompletionStage()
      .toCompletableFuture()
      .thenCompose(httpResponse -> {
        if (httpResponse.getResponse().statusCode() == HttpStatus.SC_OK) {
          OrganizationCollection orgCollection = Json.decodeValue(httpResponse.getJson().encode(), OrganizationCollection.class);
          LOGGER.info("loadMappingParameters:: The first chunk of organizations was loaded for cache, tenantId '{}', organizations '{}', totalRecords '{}'",
            tenantId, orgCollection.getOrganizations().size(), orgCollection.getTotalRecords());

          MappingParameters mappingParameters = new MappingParameters();
          if (orgCollection.getTotalRecords() > settingsLimit) {
            return getRemainingOrganizations(params, orgCollection, mappingParameters);
          }
          mappingParameters.withOrganizations(orgCollection.getOrganizations());

          return CompletableFuture.completedFuture(mappingParameters);
        } else {
          String message = format(
            "loadMappingParameters:: Error loading chunk of organizations for cache, tenantId: '%s', status code: %s, response message: %s",
            tenantId, httpResponse.getResponse().statusCode(), httpResponse.getBody());
          LOGGER.warn(message);
          return CompletableFuture.failedFuture(new CacheLoadingException(message));
        }
      });
  }

  private CompletionStage<MappingParameters> getRemainingOrganizations(OkapiConnectionParams params,
                                                                       OrganizationCollection organizationCollection,
                                                                       MappingParameters mappingParameters) {
    return collectResultsOnSuccess(
      getOrganizationCollectionFutures(params.getHeaders(), organizationCollection.getTotalRecords()))
      .map(orgCollections -> orgCollections.stream()
        .flatMap(orgCollection -> orgCollection.getOrganizations().stream())
        .toList())
      .toCompletionStage()
      .thenCompose(organizations -> {
        LOGGER.info("getRemainingOrganizations:: Organizations were loaded for cache, tenantId '{}', organizations '{}'",
          params.getTenantId(), organizations.size());
        organizationCollection.getOrganizations().addAll(organizations);
        return CompletableFuture.completedFuture(mappingParameters.withOrganizations(organizationCollection.getOrganizations()));
      });
  }

  private List<Future<OrganizationCollection>> getOrganizationCollectionFutures(Map<String, String> headers, Integer totalRecords) {
    final int maxChunkSize = totalRecords / settingsLimit;
    int offset = settingsLimit;
    List<Future<OrganizationCollection>> organizationCollectionFutures = new ArrayList<>(maxChunkSize);
    RequestEntry requestEntry = new RequestEntry(ORGANIZATIONS).withLimit(settingsLimit).withQuery(SORT_BY_ID_QUERY);
    for (int i = 0; i < maxChunkSize; i++) {
      Future<OrganizationCollection> future = restClient.get(requestEntry.withOffset(offset), OrganizationCollection.class,
        new RequestContext(Vertx.currentContext(), headers));
      organizationCollectionFutures.add(future);
      offset += settingsLimit;
    }
    return organizationCollectionFutures;
  }

  private String getOrganizationsSortedLimitPath(int limit) {
    return format("%s?limit=%d&query=", ORGANIZATIONS, limit) + encodeQuery(SORT_BY_ID_QUERY);
  }
}
