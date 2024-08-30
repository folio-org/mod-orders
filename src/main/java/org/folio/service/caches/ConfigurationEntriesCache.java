package org.folio.service.caches;

import static org.folio.orders.utils.HelperUtils.SYSTEM_CONFIG_MODULE_NAME;
import static org.folio.service.configuration.ConfigurationEntriesService.CONFIG_QUERY;
import static org.folio.service.configuration.ConfigurationEntriesService.TENANT_CONFIGURATION_ENTRIES;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.processing.mapping.defaultmapper.processor.parameters.MappingParameters;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.service.UserService;
import org.folio.service.configuration.ConfigurationEntriesService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.github.benmanes.caffeine.cache.AsyncCache;
import com.github.benmanes.caffeine.cache.Caffeine;

import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;

@Component
public class ConfigurationEntriesCache {
  private static final Logger log = LogManager.getLogger();

  private final AsyncCache<String, JsonObject> configsCache;
  private final AsyncCache<String, String> systemCurrencyCache;
  private static final String UNIQUE_CACHE_KEY_PATTERN = "%s_%s_%s";
  private final ConfigurationEntriesService configurationEntriesService;
  @Autowired
  public ConfigurationEntriesCache(ConfigurationEntriesService configurationEntriesService) {
    this.configurationEntriesService = configurationEntriesService;
    configsCache = Caffeine.newBuilder()
      .expireAfterWrite(30, TimeUnit.SECONDS)
      .executor(task -> Vertx.currentContext()
        .runOnContext(v -> task.run()))
      .buildAsync();

    systemCurrencyCache = Caffeine.newBuilder()
      .expireAfterWrite(30, TimeUnit.SECONDS)
      .executor(task -> Vertx.currentContext()
        .runOnContext(v -> task.run()))
      .buildAsync();
  }

  /**
   * Retrieves {@link MappingParameters} from cache by tenantId
   * @param requestContext {@link RequestContext} connection params
   * @return CompletableFuture with {@link String} ISBN UUID value
   */
  public Future<JsonObject> loadConfiguration(String module, RequestContext requestContext) {
    try {
      RequestEntry requestEntry = new RequestEntry(TENANT_CONFIGURATION_ENTRIES)
        .withQuery(String.format(CONFIG_QUERY, module))
        .withOffset(0)
        .withLimit(Integer.MAX_VALUE);

      var cacheKey = buildUniqueKey(requestEntry, requestContext);

      return Future.fromCompletionStage(configsCache.get(cacheKey, (key, executor) -> loadConfiguration(requestEntry, requestContext)));
    } catch (Exception e) {
      log.error("loadConfiguration:: Error loading tenant configuration from cache, tenantId: '{}'", TenantTool.tenantId(requestContext.getHeaders()), e);
      return Future.failedFuture(e);
    }
  }

  private CompletableFuture<JsonObject> loadConfiguration(RequestEntry requestEntry, RequestContext requestContext) {
    return configurationEntriesService.loadConfiguration(requestEntry, requestContext)
      .toCompletionStage()
      .toCompletableFuture();
  }

  private String buildUniqueKey(RequestEntry requestEntry, RequestContext requestContext) {
    var endpoint = requestEntry.buildEndpoint();
    var tenantId = TenantTool.tenantId(requestContext.getHeaders());
    var userId = UserService.getCurrentUserId(requestContext.getHeaders());
    return String.format(UNIQUE_CACHE_KEY_PATTERN, tenantId, userId, endpoint);
  }

  public Future<String> getSystemCurrency(RequestContext requestContext) {

    try {
      RequestEntry requestEntry = new RequestEntry(TENANT_CONFIGURATION_ENTRIES)
        .withQuery(String.format(CONFIG_QUERY, SYSTEM_CONFIG_MODULE_NAME))
        .withOffset(0)
        .withLimit(Integer.MAX_VALUE);

      var cacheKey = buildUniqueKey(requestEntry, requestContext);

      return Future.fromCompletionStage(systemCurrencyCache.get(cacheKey, (key, executor) -> getSystemCurrency(requestEntry, requestContext)));
    } catch (Exception e) {
      log.warn("get:: Error loading system currency from cache, tenantId: '{}'", TenantTool.tenantId(requestContext.getHeaders()), e);
      return Future.failedFuture(e);
    }
  }
  private CompletableFuture<String> getSystemCurrency(RequestEntry requestEntry, RequestContext requestContext) {
    return configurationEntriesService.getSystemCurrency(requestEntry, requestContext)
      .toCompletionStage()
      .toCompletableFuture();
  }
}
