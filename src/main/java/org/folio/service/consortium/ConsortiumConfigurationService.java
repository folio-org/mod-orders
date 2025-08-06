package org.folio.service.consortium;

import com.github.benmanes.caffeine.cache.AsyncCache;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import jakarta.annotation.PostConstruct;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.consortium.ConsortiumConfiguration;
import org.folio.orders.utils.RequestContextUtil;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.service.settings.SettingsRetriever;
import org.folio.service.settings.util.SettingKey;
import org.springframework.beans.factory.annotation.Value;

import java.util.Optional;
import java.util.concurrent.CompletableFuture;

import static org.folio.orders.utils.CacheUtils.buildAsyncCache;
import static org.folio.orders.utils.RequestContextUtil.createContextWithNewTenantId;

public class ConsortiumConfigurationService {

  private static final Logger logger = LogManager.getLogger(ConsortiumConfigurationService.class);

  private static final String CONSORTIUM_ID_FIELD = "consortiumId";
  private static final String CENTRAL_TENANT_ID_FIELD = "centralTenantId";
  private static final String USER_TENANTS_ARRAY_IDENTIFIER = "userTenants";
  private static final String USER_TENANTS_ENDPOINT = "/user-tenants";

  private final RestClient restClient;
  private final SettingsRetriever settingsRetriever;
  private AsyncCache<String, Optional<ConsortiumConfiguration>> asyncCache;

  @Value("${orders.cache.consortium-data.expiration.time.seconds:300}")
  private long cacheExpirationTime;

  public ConsortiumConfigurationService(RestClient restClient, SettingsRetriever settingsRetriever) {
    this.restClient = restClient;
    this.settingsRetriever = settingsRetriever;
  }

  @PostConstruct
  void init() {
    this.asyncCache = buildAsyncCache(Vertx.currentContext(), cacheExpirationTime);
  }

  public Future<Optional<ConsortiumConfiguration>> getConsortiumConfiguration(RequestContext requestContext) {
    try {
      var cacheKey = TenantTool.tenantId(requestContext.getHeaders());
      return Future.fromCompletionStage(asyncCache.get(cacheKey, (key, executor) ->
        getConsortiumConfigurationFromRemote(requestContext)));
    } catch (Exception e) {
      logger.error("Error when retrieving consortium configuration", e);
      return Future.failedFuture(e);
    }
  }

  private CompletableFuture<Optional<ConsortiumConfiguration>> getConsortiumConfigurationFromRemote(RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(USER_TENANTS_ENDPOINT).withLimit(1);
    return restClient.getAsJsonObject(requestEntry, requestContext)
      .map(jsonObject -> jsonObject.getJsonArray(USER_TENANTS_ARRAY_IDENTIFIER))
      .map(userTenants -> {
        if (userTenants.isEmpty()) {
          logger.debug("getConsortiumConfigurationFromRemote:: Central tenant and consortium id not found");
          return Optional.<ConsortiumConfiguration>empty();
        }
        String consortiumId = userTenants.getJsonObject(0).getString(CONSORTIUM_ID_FIELD);
        String centralTenantId = userTenants.getJsonObject(0).getString(CENTRAL_TENANT_ID_FIELD);
        logger.info("getConsortiumConfigurationFromRemote:: Found centralTenantId: {} and consortiumId: {}", centralTenantId, consortiumId);
        return Optional.of(new ConsortiumConfiguration(centralTenantId, consortiumId));
      }).toCompletionStage().toCompletableFuture();
  }

  public Future<RequestContext> cloneRequestContextIfNeeded(RequestContext requestContext, Location location) {
    if (StringUtils.isBlank(location.getTenantId())) {
      return Future.succeededFuture(requestContext);
    }
    return getConsortiumConfiguration(requestContext)
      .map(config -> config.isEmpty() ? requestContext : RequestContextUtil.createContextWithNewTenantId(requestContext, location.getTenantId()));
  }

  public Future<RequestContext> overrideContextToCentralTenantIfNeeded(RequestContext requestContext) {
    return getConsortiumConfiguration(requestContext)
      .compose(consortiumConfiguration -> {
        if (consortiumConfiguration.isEmpty()) {
          return Future.succeededFuture(requestContext);
        }
        String tenantId = TenantTool.tenantId(requestContext.getHeaders());
        var configuration = consortiumConfiguration.get();
        logger.info("overrideContextToCentralTenantIfNeeded:: tenantId from request: {}, centralTenantId: {}",
          tenantId, configuration.centralTenantId());
        if (StringUtils.equals(tenantId, configuration.centralTenantId())) {
          return Future.succeededFuture(requestContext);
        }
        RequestContext centralContext = createContextWithNewTenantId(requestContext, configuration.centralTenantId());
        return isCentralOrderingEnabled(centralContext)
          .compose(isCentralOrderingEnabled -> Future.succeededFuture(Boolean.TRUE.equals(isCentralOrderingEnabled)
            ? centralContext : requestContext));
      });
  }

  public Future<Boolean> isCentralOrderingEnabled(RequestContext requestContext) {
    return settingsRetriever.getSettingByKey(SettingKey.CENTRAL_ORDERING_ENABLED, requestContext)
      .map(centralOrdering -> {
        logger.info("isCentralOrderingEnabled:: central ordering enabled: {}", centralOrdering);
        return centralOrdering.map(setting -> Boolean.parseBoolean((String) setting.getValue())).orElse(false);
      });
  }
}
