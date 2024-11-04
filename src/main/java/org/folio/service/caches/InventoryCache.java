package org.folio.service.caches;

import java.util.concurrent.CompletableFuture;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.core.exceptions.NoInventoryRecordException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
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

  private final AsyncCache<String, String> asyncCache;
  private final AsyncCache<String, JsonObject> asyncJsonCache;
  // https://github.com/folio-org/mod-inventory-storage/blob/v27.1.0/reference-data/identifier-types/isbn.json
  private static final String ISBN_PRODUCT_TYPE_NAME = "ISBN";
  private static final String ISBN_PRODUCT_TYPE_ID = "8261054f-be78-422d-bd51-4ed9f33c3422";
  // https://github.com/folio-org/mod-inventory-storage/blob/v27.1.0/reference-data/identifier-types/InvalidIsbn.json
  private static final String INVALID_ISBN_PRODUCT_TYPE_NAME = "Invalid ISBN";
  private static final String INVALID_ISBN_PRODUCT_TYPE_ID = "fcca2643-406a-482a-b760-7a7f8aec640e";
  private static final String UNIQUE_CACHE_KEY_PATTERN = "%s_%s_%s";
  private final InventoryService inventoryService;

  public InventoryCache(InventoryService inventoryService) {
    this.inventoryService = inventoryService;
    var context = Vertx.currentContext();
    asyncCache = buildAsyncCache(context, 30);
    asyncJsonCache = buildAsyncCache(context, 30);
  }

  public Future<String> getISBNProductTypeId(RequestContext requestContext) {
    return getProductTypeUuid(ISBN_PRODUCT_TYPE_NAME, ISBN_PRODUCT_TYPE_ID, requestContext);
  }

  public Future<String> getInvalidISBNProductTypeId(RequestContext requestContext) {
    return getProductTypeUuid(INVALID_ISBN_PRODUCT_TYPE_NAME, INVALID_ISBN_PRODUCT_TYPE_ID, requestContext);
  }

  /**
   * Retrieves {@link String} from cache by tenantId
   * @param productType {@link String} product type name
   * @param productId {@link String} product type id if name not found (might be translated)
   * @param requestContext {@link RequestContext} connection params
   * @return CompletableFuture with {@link String} product type UUID value
   */
  public Future<String> getProductTypeUuid(String productType, String productId, RequestContext requestContext) {

    try {
      RequestEntry requestEntry = new RequestEntry("/identifier-types").withLimit(1)
        .withQuery(String.format("name==%s", productType));
      var cacheKey = buildUniqueKey(requestEntry, requestContext);

      return Future
        .fromCompletionStage(asyncCache.get(cacheKey, (key, executor)
            -> getProductTypeUuid(requestContext, requestEntry, productId)));
    } catch (Exception e) {
      log.error("getProductTypeUuid:: Error loading identifier types from cache, tenantId: '{}'", TenantTool.tenantId(requestContext.getHeaders()), e);
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
      log.error("convertToISBN13:: Error normalizing isbn value: '{}'", isbn, e);
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
      log.error("getEntryId:: Error loading inventory entry from cache: '{}'", entryType, e);
      return Future.failedFuture(e);
    }
  }

  private CompletableFuture<String> getProductTypeUuid(RequestContext requestContext, RequestEntry requestEntry,
      String fallbackUuid) {
    return inventoryService.getProductTypeUuid(requestEntry.buildEndpoint(), requestContext)
        .recover(e -> {
          if (e.getCause() instanceof NoInventoryRecordException) {
            return Future.succeededFuture(fallbackUuid);
          }
          return Future.failedFuture(e);
        })
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
