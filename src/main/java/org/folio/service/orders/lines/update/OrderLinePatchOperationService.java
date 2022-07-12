package org.folio.service.orders.lines.update;

import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
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
import org.folio.service.orders.PurchaseOrderLineService;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import static org.folio.service.inventory.InventoryManager.INSTANCE_TITLE;
import static org.folio.service.inventory.InventoryManager.CONTRIBUTOR_NAME;
import static org.folio.service.inventory.InventoryManager.INSTANCE_PUBLISHER;
import static org.folio.service.inventory.InventoryManager.INSTANCE_PUBLICATION;
import static org.folio.service.inventory.InventoryManager.INSTANCE_IDENTIFIERS;
import static org.folio.service.inventory.InventoryManager.INSTANCE_CONTRIBUTORS;
import static org.folio.service.inventory.InventoryManager.CONTRIBUTOR_NAME_TYPE_ID;
import static org.folio.service.inventory.InventoryManager.INVENTORY_LOOKUP_ENDPOINTS;
import static org.folio.service.inventory.InventoryManager.INSTANCE_IDENTIFIER_TYPE_ID;
import static org.folio.service.inventory.InventoryManager.INSTANCE_DATE_OF_PUBLICATION;
import static org.folio.service.inventory.InventoryManager.INSTANCE_IDENTIFIER_TYPE_VALUE;
import static org.folio.service.inventory.InventoryManager.INSTANCE_RECORDS_BY_ID_ENDPOINT;


public class OrderLinePatchOperationService {
  private static final Logger logger = LogManager.getLogger(OrderLinePatchOperationService.class);

  private static final String ENDPOINT = "/orders-storage/po-lines";
  private static final String BY_ID_ENDPOINT = ENDPOINT + "/{id}";

  private final RestClient restClient;

  private final OrderLinePatchOperationHandlerResolver orderLinePatchOperationHandlerResolver;

  private final PurchaseOrderLineService purchaseOrderLineService;

  public OrderLinePatchOperationService(RestClient restClient, OrderLinePatchOperationHandlerResolver orderLinePatchOperationHandlerResolver,
      PurchaseOrderLineService purchaseOrderLineService) {
    this.restClient = restClient;
    this.orderLinePatchOperationHandlerResolver = orderLinePatchOperationHandlerResolver;
    this.purchaseOrderLineService = purchaseOrderLineService;
  }

  public CompletableFuture<Void> patch(String lineId, PatchOrderLineRequest request, RequestContext requestContext) {
    return patchOrderLine(request, lineId, requestContext)
      .thenCompose(v -> updateInventoryInstanceInformation(request, lineId, requestContext));
  }

  private CompletableFuture<Void> patchOrderLine(PatchOrderLineRequest request,
                                                 String lineId, RequestContext requestContext) {
    CompletableFuture<Void> future = new CompletableFuture<>();

    purchaseOrderLineService.getOrderLineById(lineId, requestContext)
      .thenAccept(poLine -> {
        OrderLineUpdateInstanceHolder orderLineUpdateInstanceHolder = new OrderLineUpdateInstanceHolder()
          .withPathOrderLineRequest(request)
          .withStoragePoLine(poLine);

        PatchOperationHandler patchOperationHandler = orderLinePatchOperationHandlerResolver.resolve(request.getOperation());
        patchOperationHandler.handle(orderLineUpdateInstanceHolder, requestContext)
          .thenCompose(v -> sendPatchOrderLineRequest(orderLineUpdateInstanceHolder, lineId, requestContext))
          .thenAccept(v -> future.complete(null))
          .exceptionally(t -> {
            future.completeExceptionally(t);
            return null;
          });
      }).exceptionally(t -> {
        logger.error("Error when sending patch request to order lines endpoint for lineId {}", lineId);
        future.completeExceptionally(t);
        return null;
      });

    return future;
  }

  private CompletableFuture<Void> updateInventoryInstanceInformation(PatchOrderLineRequest request,
                                                                     String lineId, RequestContext requestContext) {
    CompletableFuture<Void> future = new CompletableFuture<>();

    String newInstanceId = request.getReplaceInstanceRef().getNewInstanceId();
    purchaseOrderLineService.getOrderLineById(lineId, requestContext)
      .thenApply(poLine -> {
        RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(INSTANCE_RECORDS_BY_ID_ENDPOINT)).withId(newInstanceId);
        return restClient.getAsJsonObject(requestEntry, requestContext)
          .thenApply(instanceRecord -> updatePoLineWithInstanceRecordInfo(instanceRecord, poLine))
          .thenCompose(updatedPoLine -> purchaseOrderLineService.saveOrderLine(updatedPoLine, requestContext))
          .thenAccept(v -> future.complete(null))
          .exceptionally(t -> {
            future.completeExceptionally(t);
            return null;
          });
      }).exceptionally(t -> {
        logger.error("Error when updating retrieving instance record from inventory-storage request to by instanceId {}, poLineId {}", newInstanceId, lineId);
        future.completeExceptionally(t);
        return null;
      });

    return future;
  }

  private CompletableFuture<Void> sendPatchOrderLineRequest(OrderLineUpdateInstanceHolder orderLineUpdateInstanceHolder,
                                                            String lineId, RequestContext requestContext) {
    StoragePatchOrderLineRequest storagePatchOrderLineRequest = orderLineUpdateInstanceHolder.getStoragePatchOrderLineRequest();
    if (Objects.nonNull(storagePatchOrderLineRequest)) {
      RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(lineId);
      return restClient.patch(requestEntry, storagePatchOrderLineRequest, requestContext);
    }
    return CompletableFuture.completedFuture(null);
  }

  private PoLine updatePoLineWithInstanceRecordInfo(JsonObject lookupObj, PoLine poLine) {
    Optional.ofNullable(lookupObj.getJsonArray(INSTANCE_PUBLICATION)).orElse(new JsonArray())
      .stream()
      .map(JsonObject.class::cast)
      .findFirst()
      .ifPresentOrElse(publication -> {
        poLine.setPublisher(publication.getString(INSTANCE_PUBLISHER));
        poLine.setPublicationDate(publication.getString(INSTANCE_DATE_OF_PUBLICATION));
      }, () -> {
        poLine.setPublisher(null);
        poLine.setPublicationDate(null);
      });

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
    poLine.setContributors(contributors);
    poLine.getDetails().setProductIds(productIds);

    return poLine;
  }

}
