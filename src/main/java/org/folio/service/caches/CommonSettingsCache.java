package org.folio.service.caches;

import static org.folio.orders.utils.CacheUtils.buildAsyncCache;
import static org.folio.orders.utils.ResourcePathResolver.CONFIGURATION_ENTRIES;
import static org.folio.orders.utils.ResourcePathResolver.ORDER_SETTINGS;
import static org.folio.orders.utils.ResourcePathResolver.SETTINGS_ENTRIES;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
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

import java.util.function.BiFunction;

@Log4j2
@Component
@RequiredArgsConstructor
public class CommonSettingsCache {

  public static final String GLOBAL_SETTINGS_QUERY = "(scope==stripes-core.prefs.manage and key==tenantLocaleSettings)";
  public static final String TENANT_LOCALE_SETTINGS = "tenantLocaleSettings";
  public static final String CONFIG_QUERY = "module==%s";
  private static final String UNIQUE_CACHE_KEY_PATTERN = "%s_%s_%s";

  private final CommonSettingsRetriever commonSettingsRetriever;
  private AsyncCache<String, JsonObject> configsCache;
  private AsyncCache<String, JsonObject> settingsCache;
  private AsyncCache<String, String> systemCurrencyCache;
  private AsyncCache<String, String> systemTimezoneCache;

  @Value("${orders.cache.configuration-entries.expiration.time.seconds:30}")
  private long cacheExpirationTime;
  @Value("${orders.cache.configuration-entries.bypass-cache:false}")
  private boolean byPassCache;

  @PostConstruct
  void init() {
    var context = Vertx.currentContext();
    this.configsCache = buildAsyncCache(context, cacheExpirationTime);
    this.settingsCache = buildAsyncCache(context, cacheExpirationTime);
    this.systemCurrencyCache = buildAsyncCache(context, cacheExpirationTime);
    this.systemTimezoneCache = buildAsyncCache(context, cacheExpirationTime);
  }

  public Future<JsonObject> loadConfigurations(String module, RequestContext requestContext) {
    return cacheData(resourcesPath(CONFIGURATION_ENTRIES), CONFIG_QUERY.formatted(module), configsCache,
      commonSettingsRetriever::loadConfigurations, requestContext);
  }

  public Future<JsonObject> loadSettings(RequestContext requestContext) {
    return cacheData(resourcesPath(ORDER_SETTINGS), null, settingsCache,
      commonSettingsRetriever::getLocalSettings, requestContext);
  }

  public Future<String> getSystemCurrency(RequestContext requestContext) {
    return cacheData(resourcesPath(SETTINGS_ENTRIES), GLOBAL_SETTINGS_QUERY, systemCurrencyCache,
      commonSettingsRetriever::getSystemCurrency, requestContext);
  }

  public Future<String> getSystemTimeZone(RequestContext requestContext) {
    return cacheData(resourcesPath(SETTINGS_ENTRIES), GLOBAL_SETTINGS_QUERY, systemTimezoneCache,
      commonSettingsRetriever::getSystemTimeZone, requestContext);
  }

  private <T> Future<T> cacheData(String url, String query, AsyncCache<String, T> cache,
                                  BiFunction<RequestEntry, RequestContext, Future<T>> configExtractor,
                                  RequestContext requestContext) {
    var requestEntry = new RequestEntry(url).withQuery(query).withOffset(0).withLimit(Integer.MAX_VALUE);
    var cacheKey = buildUniqueKey(requestEntry, requestContext);
    log.debug("loadSettingsData:: Loading setting data, url: '{}', query: '{}', bypass-cache mode: '{}'", url, query, byPassCache);
    if (byPassCache) {
      return extractData(configExtractor, requestContext, requestEntry);
    }
    return Future.fromCompletionStage(cache.get(cacheKey, (key, executor) ->
      extractData(configExtractor, requestContext, requestEntry)
        .toCompletionStage().toCompletableFuture()));
  }

  private <T> Future<T> extractData(BiFunction<RequestEntry, RequestContext, Future<T>> extractor,
                                    RequestContext requestContext, RequestEntry requestEntry) {
    var tenantId = TenantTool.tenantId(requestContext.getHeaders());
    return extractor.apply(requestEntry, requestContext)
      .onFailure(t -> log.error("Error loading configuration, tenantId: '{}'", tenantId, t));
  }

  private String buildUniqueKey(RequestEntry requestEntry, RequestContext requestContext) {
    var endpoint = requestEntry.buildEndpoint();
    var tenantId = TenantTool.tenantId(requestContext.getHeaders());
    var userId = UserService.getCurrentUserId(requestContext.getHeaders());
    return String.format(UNIQUE_CACHE_KEY_PATTERN, tenantId, userId, endpoint);
  }
}
