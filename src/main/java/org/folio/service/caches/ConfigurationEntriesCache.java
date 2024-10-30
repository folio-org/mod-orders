package org.folio.service.caches;

import static org.folio.orders.utils.CacheUtils.buildAsyncCache;
import static org.folio.orders.utils.HelperUtils.SYSTEM_CONFIG_MODULE_NAME;
import static org.folio.service.configuration.ConfigurationEntriesService.CONFIG_QUERY;
import static org.folio.service.configuration.ConfigurationEntriesService.TENANT_CONFIGURATION_ENTRIES;

import io.vertx.core.Vertx;
import lombok.extern.log4j.Log4j2;
import org.folio.orders.utils.HelperUtils.BiFunctionReturningFuture;
import org.folio.processing.mapping.defaultmapper.processor.parameters.MappingParameters;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.service.UserService;
import org.folio.service.configuration.ConfigurationEntriesService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import com.github.benmanes.caffeine.cache.AsyncCache;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

@Log4j2
@Component
public class ConfigurationEntriesCache {

  private static final String UNIQUE_CACHE_KEY_PATTERN = "%s_%s_%s";
  @Value("${orders.cache.configuration-entries.expiration.time.seconds:30}")
  private long cacheExpirationTime;

  private final AsyncCache<String, JsonObject> configsCache;
  private final AsyncCache<String, String> systemCurrencyCache;
  private final AsyncCache<String, String> systemTimezoneCache;
  private final ConfigurationEntriesService configurationEntriesService;


  @Autowired
  public ConfigurationEntriesCache(ConfigurationEntriesService configurationEntriesService) {
    this.configurationEntriesService = configurationEntriesService;
    var context = Vertx.currentContext();
    configsCache = buildAsyncCache(context, cacheExpirationTime);
    systemCurrencyCache = buildAsyncCache(context, cacheExpirationTime);
    systemTimezoneCache = buildAsyncCache(context, cacheExpirationTime);
  }

  /**
   * Retrieves {@link MappingParameters} from cache by tenantId
   * @param requestContext {@link RequestContext} connection params
   * @return CompletableFuture with {@link String} ISBN UUID value
   */
  public Future<JsonObject> loadConfiguration(String module, RequestContext requestContext) {
    return loadConfigurationData(module, requestContext, configsCache, configurationEntriesService::loadConfiguration);
  }

  public Future<String> getSystemCurrency(RequestContext requestContext) {
    return loadConfigurationData(SYSTEM_CONFIG_MODULE_NAME, requestContext, systemCurrencyCache, configurationEntriesService::getSystemCurrency);
  }

  public Future<String> getSystemTimeZone(RequestContext requestContext) {
    return loadConfigurationData(SYSTEM_CONFIG_MODULE_NAME, requestContext, systemTimezoneCache, configurationEntriesService::getSystemTimeZone);
  }

  private <T> Future<T> loadConfigurationData(String module, RequestContext requestContext, AsyncCache<String, T> cache,
                                              BiFunctionReturningFuture<RequestEntry, RequestContext, T> configExtractor) {
    try {
      var requestEntry = new RequestEntry(TENANT_CONFIGURATION_ENTRIES)
        .withQuery(String.format(CONFIG_QUERY, module))
        .withOffset(0)
        .withLimit(Integer.MAX_VALUE);
      var cacheKey = buildUniqueKey(requestEntry, requestContext);
      return Future.fromCompletionStage(cache.get(cacheKey, (key, executor) ->
        configExtractor.apply(requestEntry, requestContext)
          .toCompletionStage().toCompletableFuture()));
    } catch (Exception e) {
      log.error("loadConfigurationData:: Error loading tenant configuration from cache, tenantId: '{}'", TenantTool.tenantId(requestContext.getHeaders()), e);
      return Future.failedFuture(e);
    }
  }

  private String buildUniqueKey(RequestEntry requestEntry, RequestContext requestContext) {
    var endpoint = requestEntry.buildEndpoint();
    var tenantId = TenantTool.tenantId(requestContext.getHeaders());
    var userId = UserService.getCurrentUserId(requestContext.getHeaders());
    return String.format(UNIQUE_CACHE_KEY_PATTERN, tenantId, userId, endpoint);
  }

}
