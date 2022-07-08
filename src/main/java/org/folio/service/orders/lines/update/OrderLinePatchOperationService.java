package org.folio.service.orders.lines.update;

import io.vertx.core.json.JsonObject;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.assertj.core.util.IterableUtil;
import org.assertj.core.util.Lists;
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
    return patchOrderLine(request, lineId, requestContext).thenCompose(v -> {
      if(request.getOperation() == PatchOrderLineRequest.Operation.REPLACE_INSTANCE_REF) {
        return updateInventoryInstanceInformation(request, lineId, requestContext);
      } else {
        return null;
      }
    });
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
          .thenApply(instanceRecord -> updatePoLineInstanceRecordInfo(instanceRecord, poLine))
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

  private PoLine updatePoLineInstanceRecordInfo(JsonObject lookupObj, PoLine poLine) {
    poLine.setTitleOrPackage(lookupObj.getString(INSTANCE_TITLE));

    if(!IterableUtil.isNullOrEmpty(lookupObj.getJsonArray(INSTANCE_PUBLICATION))) {
      JsonObject publications = lookupObj.getJsonArray(INSTANCE_PUBLICATION).getJsonObject(0);
      poLine.setPublisher(publications.getString(INSTANCE_PUBLISHER));
      poLine.setPublicationDate(publications.getString(INSTANCE_DATE_OF_PUBLICATION));
    } else {
      poLine.setPublisher(null);
      poLine.setPublicationDate(null);
    }

    if(!IterableUtil.isNullOrEmpty(lookupObj.getJsonArray(INSTANCE_CONTRIBUTORS))) {
      List<Contributor> contributors = lookupObj.getJsonArray(INSTANCE_CONTRIBUTORS).stream()
        .map(JsonObject.class::cast)
        .map(jsonObject -> new Contributor()
          .withContributor(jsonObject.getString(CONTRIBUTOR_NAME))
          .withContributorNameTypeId(jsonObject.getString(CONTRIBUTOR_NAME_TYPE_ID)))
        .collect(Collectors.toList());
      poLine.setContributors(contributors);
    } else {
      poLine.setContributors(Lists.emptyList());
    }

    if(!IterableUtil.isNullOrEmpty(lookupObj.getJsonArray(INSTANCE_IDENTIFIERS))) {
      List<ProductId> productIds = lookupObj.getJsonArray(INSTANCE_IDENTIFIERS).stream()
        .map(JsonObject.class::cast)
        .map(jsonObject -> new ProductId()
          .withProductId(jsonObject.getString(INSTANCE_IDENTIFIER_TYPE_VALUE))
          .withProductIdType(jsonObject.getString(INSTANCE_IDENTIFIER_TYPE_ID)))
        .collect(Collectors.toList());
      poLine.getDetails().setProductIds(productIds);
    } else {
      poLine.getDetails().setProductIds(Lists.emptyList());
    }

    return poLine;
  }

}
