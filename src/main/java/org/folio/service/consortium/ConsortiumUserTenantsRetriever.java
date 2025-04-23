package org.folio.service.consortium;

import com.github.benmanes.caffeine.cache.AsyncCache;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import jakarta.annotation.PostConstruct;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.springframework.beans.factory.annotation.Value;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.stream.IntStream;

import static org.folio.orders.utils.CacheUtils.buildAsyncCache;
import static org.folio.orders.utils.RequestContextUtil.createContextWithNewTenantId;
import static org.folio.orders.utils.RequestContextUtil.getUserIdFromContext;
import static org.folio.orders.utils.ResourcePathResolver.CONSORTIA_USER_TENANTS;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.service.pieces.util.UserTenantFields.COLLECTION_USER_TENANTS;
import static org.folio.service.pieces.util.UserTenantFields.TENANT_ID;
import static org.folio.service.pieces.util.UserTenantFields.USER_ID;

public class ConsortiumUserTenantsRetriever {

  private static final Logger logger = LogManager.getLogger(ConsortiumUserTenantsRetriever.class);

  private static final String CONSORTIA_USER_TENANTS_ENDPOINT = resourcesPath(CONSORTIA_USER_TENANTS);
  private static final String USER_TENANTS_CACHE_KEY_TEMPLATE = "%s.%s";

  private final RestClient restClient;
  private AsyncCache<String, List<String>> asyncCache;

  @Value("${orders.cache.consortium-user-tenants.expiration.time.seconds:60}")
  private long cacheExpirationTime;

  public ConsortiumUserTenantsRetriever(RestClient restClient) {
    this.restClient = restClient;
  }

  @PostConstruct
  void init() {
    this.asyncCache = buildAsyncCache(Vertx.currentContext(), cacheExpirationTime);
  }

  public Future<List<String>> getUserTenants(String consortiumId, String centralTenantId, RequestContext requestContext) {
    try {
      var userId = getUserIdFromContext(requestContext);
      var cacheKey = String.format(USER_TENANTS_CACHE_KEY_TEMPLATE, userId, consortiumId);
      return Future.fromCompletionStage(asyncCache.get(cacheKey, (key, executor) -> getUserTenantsFromRemote(userId, consortiumId, centralTenantId, requestContext)));
    } catch (Exception e) {
      logger.error("Error when retrieving user tenants for consortium - '{}'", consortiumId, e);
      return Future.failedFuture(e);
    }
  }

  private CompletableFuture<List<String>> getUserTenantsFromRemote(String userId, String consortiumId, String centralTenantId, RequestContext requestContext) {
    var newRequestContext = createContextWithNewTenantId(requestContext, centralTenantId);
    var requestEntry = new RequestEntry(CONSORTIA_USER_TENANTS_ENDPOINT)
      .withId(consortiumId)
      .withOffset(0)
      .withLimit(Integer.MAX_VALUE)
      .withQueryParameter(USER_ID.getValue(), userId);
    return restClient.getAsJsonObject(requestEntry, newRequestContext)
      .map(this::extractTenantIds)
      .toCompletionStage().toCompletableFuture();
  }

  private List<String> extractTenantIds(JsonObject userTenantCollection) {
    var userTenants = userTenantCollection.getJsonArray(COLLECTION_USER_TENANTS.getValue());
    return IntStream.range(0, userTenants.size())
      .mapToObj(userTenants::getJsonObject)
      .map(userTenant -> userTenant.getString(TENANT_ID.getValue()))
      .toList();
  }
}
