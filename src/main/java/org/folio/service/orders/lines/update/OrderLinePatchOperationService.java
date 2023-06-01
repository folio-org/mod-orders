package org.folio.service.orders.lines.update;

import static org.folio.service.inventory.InventoryManager.CONTRIBUTOR_NAME;
import static org.folio.service.inventory.InventoryManager.CONTRIBUTOR_NAME_TYPE_ID;
import static org.folio.service.inventory.InventoryManager.INSTANCE_CONTRIBUTORS;
import static org.folio.service.inventory.InventoryManager.INSTANCE_DATE_OF_PUBLICATION;
import static org.folio.service.inventory.InventoryManager.INSTANCE_IDENTIFIERS;
import static org.folio.service.inventory.InventoryManager.INSTANCE_IDENTIFIER_TYPE_ID;
import static org.folio.service.inventory.InventoryManager.INSTANCE_IDENTIFIER_TYPE_VALUE;
import static org.folio.service.inventory.InventoryManager.INSTANCE_PUBLICATION;
import static org.folio.service.inventory.InventoryManager.INSTANCE_PUBLISHER;
import static org.folio.service.inventory.InventoryManager.INSTANCE_RECORDS_BY_ID_ENDPOINT;
import static org.folio.service.inventory.InventoryManager.INSTANCE_TITLE;
import static org.folio.service.inventory.InventoryManager.INVENTORY_LOOKUP_ENDPOINTS;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apache.commons.lang3.tuple.MutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.orders.lines.update.OrderLineUpdateInstanceHolder;
import org.folio.rest.acq.model.StoragePatchOrderLineRequest;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.Contributor;
import org.folio.rest.jaxrs.model.PatchOrderLineRequest;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.ProductId;
import org.folio.service.caches.InventoryCache;
import org.folio.service.orders.PurchaseOrderLineService;

import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;


public class OrderLinePatchOperationService {
  private static final Logger logger = LogManager.getLogger(OrderLinePatchOperationService.class);

  private static final String ENDPOINT = "/orders-storage/po-lines";
  private static final String BY_ID_ENDPOINT = ENDPOINT + "/{id}";

  private final RestClient restClient;

  private final OrderLinePatchOperationHandlerResolver orderLinePatchOperationHandlerResolver;

  private final PurchaseOrderLineService purchaseOrderLineService;

  private final InventoryCache inventoryCache;

  public OrderLinePatchOperationService(RestClient restClient, OrderLinePatchOperationHandlerResolver orderLinePatchOperationHandlerResolver,
                                        PurchaseOrderLineService purchaseOrderLineService, InventoryCache inventoryCache) {
    this.restClient = restClient;
    this.orderLinePatchOperationHandlerResolver = orderLinePatchOperationHandlerResolver;
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.inventoryCache = inventoryCache;
  }

  public Future<Void> patch(String lineId, PatchOrderLineRequest request, RequestContext requestContext) {
    return patchOrderLine(request, lineId, requestContext)
      .compose(v -> updateInventoryInstanceInformation(request, lineId, requestContext));
  }

  private Future<Void> patchOrderLine(PatchOrderLineRequest request, String lineId, RequestContext requestContext) {
    Promise<Void> promise = Promise.promise();

    purchaseOrderLineService.getOrderLineById(lineId, requestContext)
      .map(poLine -> {
        OrderLineUpdateInstanceHolder orderLineUpdateInstanceHolder = new OrderLineUpdateInstanceHolder()
          .withPathOrderLineRequest(request)
          .withStoragePoLine(poLine);

        PatchOperationHandler patchOperationHandler = orderLinePatchOperationHandlerResolver.resolve(request.getOperation());
        patchOperationHandler.handle(orderLineUpdateInstanceHolder, requestContext)
          .compose(v -> sendPatchOrderLineRequest(orderLineUpdateInstanceHolder, lineId, requestContext))
          .onSuccess(v -> promise.complete())
          .onFailure(promise::fail);
        return null;
      })
      .onFailure(t -> {
        logger.error("Error when sending patch request to order lines endpoint for lineId {}", lineId);
        promise.fail(t);
      });

    return promise.future();
  }

  private Future<Void> updateInventoryInstanceInformation(PatchOrderLineRequest request, String lineId, RequestContext requestContext) {
    Promise<Void> promise = Promise.promise();

    String newInstanceId = request.getReplaceInstanceRef().getNewInstanceId();
    purchaseOrderLineService.getOrderLineById(lineId, requestContext)
      .map(poLine -> {
        RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(INSTANCE_RECORDS_BY_ID_ENDPOINT)).withId(newInstanceId);
        return restClient.getAsJsonObject(requestEntry, requestContext)
          .map(instanceRecord -> updatePoLineWithInstanceRecordInfo(instanceRecord, poLine))
          .compose(updatedPoLine -> validateAndNormalizeIfISBN(updatedPoLine, requestContext))
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

  private Future<Void> sendPatchOrderLineRequest(OrderLineUpdateInstanceHolder orderLineUpdateInstanceHolder, String lineId,
      RequestContext requestContext) {
    StoragePatchOrderLineRequest storagePatchOrderLineRequest = orderLineUpdateInstanceHolder.getStoragePatchOrderLineRequest();
    if (Objects.nonNull(storagePatchOrderLineRequest)) {
      RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(lineId);
      return restClient.patch(requestEntry, storagePatchOrderLineRequest, requestContext);
    }
    return Future.succeededFuture();
  }

  private PoLine updatePoLineWithInstanceRecordInfo(JsonObject lookupObj, PoLine poLine) {
    Pair<String, String> instancePublication = Optional.ofNullable(lookupObj.getJsonArray(INSTANCE_PUBLICATION)).orElse(new JsonArray())
      .stream()
      .map(JsonObject.class::cast)
      .findFirst()
      .map(publication -> Pair.of(publication.getString(INSTANCE_PUBLISHER), publication.getString(INSTANCE_DATE_OF_PUBLICATION)))
      .orElse(new MutablePair<>());

    List<Contributor> contributors = Optional.ofNullable(lookupObj.getJsonArray(INSTANCE_CONTRIBUTORS))
      .orElse(new JsonArray())
      .stream()
      .map(JsonObject.class::cast)
      .map(jsonObject -> new Contributor()
        .withContributor(jsonObject.getString(CONTRIBUTOR_NAME))
        .withContributorNameTypeId(jsonObject.getString(CONTRIBUTOR_NAME_TYPE_ID)))
      .collect(Collectors.toList());

    List<ProductId> productIds = Optional.ofNullable(lookupObj.getJsonArray(INSTANCE_IDENTIFIERS))
      .orElse(new JsonArray())
      .stream()
      .map(JsonObject.class::cast)
      .map(jsonObject -> new ProductId()
        .withProductId(jsonObject.getString(INSTANCE_IDENTIFIER_TYPE_VALUE))
        .withProductIdType(jsonObject.getString(INSTANCE_IDENTIFIER_TYPE_ID)))
      .collect(Collectors.toList());

    poLine.setTitleOrPackage(lookupObj.getString(INSTANCE_TITLE));
    poLine.setPublisher(instancePublication.getLeft());
    poLine.setPublicationDate(instancePublication.getRight());
    poLine.setContributors(contributors);
    poLine.getDetails().setProductIds(productIds);

    return poLine;
  }

  private Future<PoLine> validateAndNormalizeIfISBN(PoLine poLine, RequestContext requestContext) {
    return inventoryCache.getProductTypeUuid(requestContext)
      .map(isbnTypeId -> {
        if (Objects.equals(poLine.getDetails().getProductIds().get(0).getProductIdType(), isbnTypeId)) {
          return inventoryCache.convertToISBN13(extractProductId(poLine.getDetails().getProductIds().get(0).getProductId()), requestContext)
            .map(normalizedISBN -> {
              poLine.getDetails().getProductIds().get(0).setQualifier(extractQualifier(poLine.getDetails().getProductIds().get(0).getProductId()));
              poLine.getDetails().getProductIds().get(0).setProductId(normalizedISBN);
              return poLine;
            }).result();
        }
        return poLine;
      });
  }

  private String extractProductId(String value) {
    if(value == null || !value.contains(" ")){
      return value;
    }
    return value.substring(0, value.indexOf(" "));
  }

  private String extractQualifier(String value) {
    if(value == null || !value.contains(" ")){
      return null;
    }
    return value.substring(value.indexOf(" ") + 1);
  }
}
