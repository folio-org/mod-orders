package org.folio.service.caches;

import java.util.concurrent.CompletableFuture;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.service.UserService;
import org.folio.service.inventory.InventoryService;
import org.springframework.stereotype.Component;

import com.github.benmanes.caffeine.cache.AsyncCache;

import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;

import static org.folio.orders.utils.CacheUtils.buildAsyncCache;

@Component
public class InventoryCache {
  private static final Logger log = LogManager.getLogger();

  private final AsyncCache<String, JsonObject> asyncJsonCache;
  private static final String UNIQUE_CACHE_KEY_PATTERN = "%s_%s_%s";
  private final InventoryService inventoryService;

  public InventoryCache(InventoryService inventoryService) {
    this.inventoryService = inventoryService;
    var context = Vertx.currentContext();
    asyncJsonCache = buildAsyncCache(context, 30);
  }

  public Future<JsonObject> getEntryId(String entryType, String entryTypeValue, RequestContext requestContext) {
    try {
      var tenantId = TenantTool.tenantId(requestContext.getHeaders());
      var userId = UserService.getCurrentUserId(requestContext.getHeaders());
      var cacheKey = String.format(UNIQUE_CACHE_KEY_PATTERN, tenantId, userId, entryType);

      return Future.fromCompletionStage(asyncJsonCache.get(cacheKey, (key, executor) -> getEntryTypeId(entryType, entryTypeValue, requestContext)));
    } catch (Exception e) {
      log.error("getEntryId:: Error loading inventory entry from cache: '{}'", entryType, e);
      return Future.failedFuture(e);
    }
  }

  private CompletableFuture<JsonObject> getEntryTypeId(String entryType, String entryTypeValue, RequestContext requestContext) {
    return inventoryService.getEntryTypeId(entryType, entryTypeValue, requestContext)
      .toCompletionStage()
      .toCompletableFuture();
  }
}
