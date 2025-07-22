package org.folio.service.caches;

import static org.folio.orders.utils.CacheUtils.buildAsyncCache;
import static org.folio.orders.utils.ResourcePathResolver.CONFIGURATION_ENTRIES;
import static org.folio.orders.utils.ResourcePathResolver.SETTINGS_ENTRIES;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.service.settings.CommonSettingsRetriever.CONFIG_QUERY;
import static org.folio.service.settings.CommonSettingsRetriever.SETTINGS_QUERY;

import io.vertx.core.Vertx;
import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.service.UserService;
import org.folio.service.settings.CommonSettingsRetriever;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import com.github.benmanes.caffeine.cache.AsyncCache;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

import java.util.function.BiFunction;

@Log4j2
@Component
@RequiredArgsConstructor
public class CommonSettingsCache {

  private static final String UNIQUE_CACHE_KEY_PATTERN = "%s_%s_%s";

  private final CommonSettingsRetriever commonSettingsRetriever;
  private AsyncCache<String, JsonObject> configsCache;
  private AsyncCache<String, String> systemCurrencyCache;
  private AsyncCache<String, String> systemTimezoneCache;

  @Value("${orders.cache.configuration-entries.expiration.time.seconds:30}")
  private long cacheExpirationTime;

  @PostConstruct
  void init() {
    var context = Vertx.currentContext();
    this.configsCache = buildAsyncCache(context, cacheExpirationTime);
    this.systemCurrencyCache = buildAsyncCache(context, cacheExpirationTime);
    this.systemTimezoneCache = buildAsyncCache(context, cacheExpirationTime);
  }

  /**
   * Retrieves order related settings from cache by tenantId
   * @param requestContext {@link RequestContext} connection params
   * @return Future with {@link JsonObject} describing settings object
   */
  public Future<JsonObject> loadConfiguration(String module, RequestContext requestContext) {
    return loadSettingsData(requestContext, resourcesPath(CONFIGURATION_ENTRIES), CONFIG_QUERY.formatted(module),
      configsCache, commonSettingsRetriever::loadConfiguration);
  }

  public Future<String> getSystemCurrency(RequestContext requestContext) {
    return loadSettingsData(requestContext, resourcesPath(SETTINGS_ENTRIES), SETTINGS_QUERY,
      systemCurrencyCache, commonSettingsRetriever::getSystemCurrency);
  }

  public Future<String> getSystemTimeZone(RequestContext requestContext) {
    return loadSettingsData(requestContext, resourcesPath(SETTINGS_ENTRIES), SETTINGS_QUERY,
      systemTimezoneCache, commonSettingsRetriever::getSystemTimeZone);
  }

  private <T> Future<T> loadSettingsData(RequestContext requestContext,
                                         String entriesUrl, String query,
                                         AsyncCache<String, T> cache,
                                         BiFunction<RequestEntry, RequestContext, Future<T>> configExtractor) {
    var requestEntry = new RequestEntry(entriesUrl).withQuery(query).withOffset(0).withLimit(Integer.MAX_VALUE);
    var cacheKey = buildUniqueKey(requestEntry, requestContext);
    return Future.fromCompletionStage(cache.get(cacheKey, (key, executor) ->
      configExtractor.apply(requestEntry, requestContext)
        .onFailure(t -> log.error("Error loading tenant configuration, tenantId: '{}'", TenantTool.tenantId(requestContext.getHeaders()), t))
        .toCompletionStage().toCompletableFuture()));
  }

  private String buildUniqueKey(RequestEntry requestEntry, RequestContext requestContext) {
    var endpoint = requestEntry.buildEndpoint();
    var tenantId = TenantTool.tenantId(requestContext.getHeaders());
    var userId = UserService.getCurrentUserId(requestContext.getHeaders());
    return String.format(UNIQUE_CACHE_KEY_PATTERN, tenantId, userId, endpoint);
  }

}
