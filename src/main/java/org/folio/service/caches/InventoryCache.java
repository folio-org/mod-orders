package org.folio.service.caches;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.service.UserService;
import org.folio.service.inventory.InventoryService;
import org.springframework.stereotype.Component;

import com.github.benmanes.caffeine.cache.AsyncCache;
import com.github.benmanes.caffeine.cache.Caffeine;

import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;

@Component
public class InventoryCache {
  private static final Logger log = LogManager.getLogger();

  private final AsyncCache<String, String> asyncCache;
  private final AsyncCache<String, JsonObject> asyncJsonCache;
  private static final String UNIQUE_CACHE_KEY_PATTERN = "%s_%s_%s";
  private final InventoryService inventoryService;

  public InventoryCache(InventoryService inventoryService) {
    this.inventoryService = inventoryService;
    asyncCache = Caffeine.newBuilder()
      .expireAfterWrite(30, TimeUnit.SECONDS)
      .executor(task -> Vertx.currentContext()
        .runOnContext(v -> task.run()))
      .buildAsync();

    asyncJsonCache = Caffeine.newBuilder()
      .expireAfterWrite(30, TimeUnit.SECONDS)
      .executor(task -> Vertx.currentContext()
        .runOnContext(v -> task.run()))
      .buildAsync();
  }

  /**
   * Retrieves {@link String} from cache by tenantId
   * @param requestContext {@link RequestContext} connection params
   * @return CompletableFuture with {@link String} ISBN UUID value
   */
  public Future<String> getProductTypeUuid(RequestContext requestContext) {
    try {
      RequestEntry requestEntry = new RequestEntry("/identifier-types").withLimit(1)
        .withQuery("name==ISBN");
      var cacheKey = buildUniqueKey(requestEntry, requestContext);

      return Future
        .fromCompletionStage(asyncCache.get(cacheKey, (key, executor) -> getProductTypeUuidByIsbn(requestContext, requestEntry)));
    } catch (Exception e) {
      log.error("get:: Error loading identifier types from cache, tenantId: '{}'", TenantTool.tenantId(requestContext.getHeaders()), e);
      return Future.failedFuture(e);
    }
  }


  public Future<String> convertToISBN13(String isbn, RequestContext requestContext) {
    try {
      String convertEndpoint = String.format("/isbn/convertTo13?isbn=%s", isbn);
      RequestEntry requestEntry = new RequestEntry(convertEndpoint);
      var cacheKey = buildUniqueKey(requestEntry, requestContext);
      return Future.fromCompletionStage(asyncCache.get(cacheKey, (key, executor) -> convertToISBN13(isbn, requestEntry, requestContext)));
    } catch (Exception e) {
      log.error("get:: Error normilizing isbn value: '{}'", isbn, e);
      return Future.failedFuture(e);
    }
  }

  public Future<JsonObject> getEntryId(String entryType, String entryTypeValue, RequestContext requestContext) {
    try {
      var tenantId = TenantTool.tenantId(requestContext.getHeaders());
      var userId = UserService.getCurrentUserId(requestContext.getHeaders());
      var cacheKey = String.format(UNIQUE_CACHE_KEY_PATTERN, tenantId, userId, entryType);

      return Future.fromCompletionStage(asyncJsonCache.get(cacheKey, (key, executor) -> getEntryTypeId(entryType, entryTypeValue, requestContext)));
    } catch (Exception e) {
      log.error("get:: Error loading inventory entry from cache: '{}'", entryType, e);
      return Future.failedFuture(e);
    }
  }

  private CompletableFuture<String> getProductTypeUuidByIsbn(RequestContext requestContext, RequestEntry requestEntry) {
    return inventoryService.getProductTypeUuidByIsbn(requestEntry.buildEndpoint(), requestContext)
      .toCompletionStage()
      .toCompletableFuture();
  }

  private CompletableFuture<JsonObject> getEntryTypeId(String entryType, String entryTypeValue, RequestContext requestContext) {
    return inventoryService.getEntryTypeId(entryType, entryTypeValue, requestContext)
      .toCompletionStage()
      .toCompletableFuture();
  }

  private CompletableFuture<String> convertToISBN13(String isbn, RequestEntry requestEntry, RequestContext requestContext) {
    return inventoryService.convertToISBN13(isbn, requestEntry.buildEndpoint(), requestContext)
      .toCompletionStage()
      .toCompletableFuture();
  }

  private String buildUniqueKey(RequestEntry requestEntry, RequestContext requestContext) {
    var endpoint = requestEntry.buildEndpoint();
    var tenantId = TenantTool.tenantId(requestContext.getHeaders());
    var userId = UserService.getCurrentUserId(requestContext.getHeaders());
    return String.format(UNIQUE_CACHE_KEY_PATTERN, tenantId, userId, endpoint);
  }
}
