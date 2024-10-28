package org.folio.service.orders.lines.update;

import static org.folio.rest.core.exceptions.ErrorCodes.INSTANCE_INVALID_PRODUCT_ID_ERROR;
import static org.folio.service.inventory.InventoryInstanceManager.INSTANCE_TITLE;
import static org.folio.service.inventory.InventoryUtils.INSTANCE_RECORDS_BY_ID_ENDPOINT;
import static org.folio.service.inventory.InventoryUtils.INVENTORY_LOOKUP_ENDPOINTS;
import static org.folio.service.orders.utils.ProductIdUtils.buildSetOfProductIds;
import static org.folio.service.orders.utils.ProductIdUtils.isISBN;
import static org.folio.service.orders.utils.ProductIdUtils.removeISBNDuplicates;
import static org.folio.service.orders.utils.ProductIdUtils.extractProductId;
import static org.folio.service.orders.utils.ProductIdUtils.extractQualifier;

import java.util.List;
import java.util.Objects;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.orders.lines.update.OrderLineUpdateInstanceHolder;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.acq.model.StoragePatchOrderLineRequest;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.CreateInventoryType;
import org.folio.rest.jaxrs.model.Details;
import org.folio.rest.jaxrs.model.PatchOrderLineRequest;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.ProductId;
import org.folio.service.caches.InventoryCache;
import org.folio.service.inventory.InventoryInstanceManager;
import org.folio.service.inventory.InventoryUtils;
import org.folio.service.orders.PurchaseOrderLineService;

import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.json.JsonObject;
import org.folio.orders.utils.HelperUtils;


public class OrderLinePatchOperationService {
  private static final Logger logger = LogManager.getLogger(OrderLinePatchOperationService.class);

  private static final String ENDPOINT = "/orders-storage/po-lines";
  private static final String BY_ID_ENDPOINT = ENDPOINT + "/{id}";

  private final RestClient restClient;

  private final OrderLineUpdateInstanceStrategyResolver orderLineUpdateInstanceStrategyResolver;

  private final PurchaseOrderLineService purchaseOrderLineService;

  private final InventoryCache inventoryCache;
  private final InventoryInstanceManager inventoryInstanceManager;

  public OrderLinePatchOperationService(RestClient restClient,
                                        OrderLineUpdateInstanceStrategyResolver orderLineUpdateInstanceStrategyResolver,
                                        PurchaseOrderLineService purchaseOrderLineService,
                                        InventoryCache inventoryCache,
                                        InventoryInstanceManager inventoryInstanceManager) {
    this.restClient = restClient;
    this.orderLineUpdateInstanceStrategyResolver = orderLineUpdateInstanceStrategyResolver;
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.inventoryCache = inventoryCache;
    this.inventoryInstanceManager = inventoryInstanceManager;
  }

  public Future<Void> patch(String lineId, PatchOrderLineRequest request, RequestContext requestContext) {
    String newInstanceId = request.getReplaceInstanceRef().getNewInstanceId();
    return inventoryInstanceManager.createShadowInstanceIfNeeded(newInstanceId, requestContext)
      .compose(v -> patchOrderLine(request, lineId, requestContext))
      .compose(v -> updateInventoryInstanceInformation(request, lineId, requestContext));
  }

  private Future<Void> patchOrderLine(PatchOrderLineRequest request, String lineId, RequestContext requestContext) {
    return purchaseOrderLineService.getOrderLineById(lineId, requestContext)
      .compose(poLine -> {
        OrderLineUpdateInstanceHolder orderLineUpdateInstanceHolder = new OrderLineUpdateInstanceHolder()
          .withPathOrderLineRequest(request)
          .withStoragePoLine(poLine);

        return handleUpdateInstance(orderLineUpdateInstanceHolder, requestContext)
          .compose(v -> sendPatchOrderLineRequest(orderLineUpdateInstanceHolder, lineId, requestContext));
      })
      .onFailure(t ->
        logger.error("Error when sending patch request to order lines endpoint for lineId {}", lineId, t)
      );
  }

  public Future<Void> handleUpdateInstance(OrderLineUpdateInstanceHolder holder, RequestContext requestContext) {
    return updateInstanceForPhysical(holder, requestContext)
      .compose(v -> updateInstanceForEresource(holder, requestContext));
  }

  private Future<Void> updateInstanceForPhysical(OrderLineUpdateInstanceHolder holder, RequestContext requestContext) {
    var physical = PoLineCommonUtil.getPhysical(holder.getStoragePoLine());
    if (physical == null) {
      return Future.succeededFuture();
    }
    return orderLineUpdateInstanceStrategyResolver.resolve(
      CreateInventoryType.fromValue(physical.getCreateInventory().value())).updateInstance(holder, requestContext);
  }

  private Future<Void> updateInstanceForEresource(OrderLineUpdateInstanceHolder holder, RequestContext requestContext) {
    var eresource = PoLineCommonUtil.getEresource(holder.getStoragePoLine());
    if (eresource == null) {
      return Future.succeededFuture();
    }
    return orderLineUpdateInstanceStrategyResolver.resolve(
      CreateInventoryType.fromValue(eresource.getCreateInventory().value())).updateInstance(holder, requestContext);
  }

  private Future<Void> sendPatchOrderLineRequest(OrderLineUpdateInstanceHolder orderLineUpdateInstanceHolder, String lineId,
                                                 RequestContext requestContext) {
    StoragePatchOrderLineRequest storagePatchOrderLineRequest = orderLineUpdateInstanceHolder.getStoragePatchOrderLineRequest();
    if (Objects.nonNull(storagePatchOrderLineRequest)) {
      RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(lineId);
      return restClient.patch(requestEntry, storagePatchOrderLineRequest, requestContext);
    }
    return Future.succeededFuture();
  }

  private Future<Void> updateInventoryInstanceInformation(PatchOrderLineRequest request, String lineId, RequestContext requestContext) {
    Promise<Void> promise = Promise.promise();

    String newInstanceId = request.getReplaceInstanceRef().getNewInstanceId();
    purchaseOrderLineService.getOrderLineById(lineId, requestContext)
      .map(poLine -> {
        RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(INSTANCE_RECORDS_BY_ID_ENDPOINT)).withId(newInstanceId);
        return restClient.getAsJsonObject(requestEntry, requestContext)
          .compose(instanceRecord -> updatePoLineWithInstanceRecordInfo(instanceRecord, poLine, requestContext))
          .compose(validatedPoLine -> purchaseOrderLineService.saveOrderLine(validatedPoLine, requestContext))
          .onSuccess(v -> promise.complete())
          .onFailure(promise::fail);
      })
      .onFailure(t -> {
        logger.error("Error when updating retrieving instance record from inventory-storage request to by instanceId {}, poLineId {}", newInstanceId, lineId);
        promise.fail(t);
      });

    return promise.future();
  }

  private Future<PoLine> updatePoLineWithInstanceRecordInfo(JsonObject lookupObj, PoLine poLine, RequestContext requestContext) {
    Promise<PoLine> promise = Promise.promise();

    poLine.setTitleOrPackage(lookupObj.getString(INSTANCE_TITLE));
    poLine.setPublisher(InventoryUtils.getPublisher(lookupObj));
    poLine.setPublicationDate(InventoryUtils.getPublicationDate(lookupObj));
    poLine.setContributors(InventoryUtils.getContributors(lookupObj));

    inventoryCache.getISBNProductTypeId(requestContext)
      .compose(isbnTypeId -> {
        List<ProductId> productIds = InventoryUtils.getProductIds(lookupObj);
        Set<String> setOfProductIds = buildSetOfProductIds(productIds, isbnTypeId);
        return HelperUtils.executeWithSemaphores(setOfProductIds,
            productId -> inventoryCache.convertToISBN13(extractProductId(productId), requestContext)
              .map(normalizedId -> Map.entry(productId, normalizedId)), requestContext)
          .map(result -> result
            .stream()
            .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue)))
          .map(productIdsMap -> {
            // update productids with normalized values
            productIds.stream()
              .filter(productId -> isISBN(isbnTypeId, productId))
              .forEach(productId -> {
                productId.withQualifier(extractQualifier(productId.getProductId()));
                productId.setProductId(productIdsMap.get(productId.getProductId()));
              });
            return null;
          })
          .onSuccess(v -> {
            if (Objects.isNull(poLine.getDetails())) {
              poLine.setDetails(new Details());
            }
            poLine.getDetails().setProductIds(removeISBNDuplicates(productIds, isbnTypeId));
            promise.complete(poLine);
          })
          .onFailure(t -> {
            logger.error("Failed update poLine {} with instance", poLine.getId(), t);
            promise.fail(new HttpException(400, INSTANCE_INVALID_PRODUCT_ID_ERROR.toError()));
          })
          .mapEmpty();
      });
    return promise.future();
  }

}
