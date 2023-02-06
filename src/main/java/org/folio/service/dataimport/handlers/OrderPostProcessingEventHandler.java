package org.folio.service.dataimport.handlers;

import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.json.Json;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.DataImportEventPayload;
import org.folio.MappingProfile;
import org.folio.dbschema.ObjectMapperTool;
import org.folio.helper.PurchaseOrderHelper;
import org.folio.orders.utils.HelperUtils;
import org.folio.processing.events.services.handler.EventHandler;
import org.folio.processing.exceptions.EventProcessingException;
import org.folio.rest.RestConstants;
import org.folio.rest.RestVerticle;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.EntityType;
import org.folio.service.dataimport.PoLineImportProgressService;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import static org.apache.commons.lang3.StringUtils.isBlank;
import static org.folio.DataImportEventTypes.DI_ORDER_CREATED;
import static org.folio.rest.jaxrs.model.EntityType.ORDER;
import static org.folio.rest.jaxrs.model.ProfileSnapshotWrapper.ContentType.MAPPING_PROFILE;

@Component
public class OrderPostProcessingEventHandler implements EventHandler {
  private static final Logger LOGGER = LogManager.getLogger();
  private static final String PAYLOAD_HAS_NO_DATA_MSG =
    "Failed to handle event payload, cause event payload context does not contain ORDER_LINE data";

  private static final String ORDER_LINES_KEY = "ORDER_LINES";

  private final PurchaseOrderHelper purchaseOrderHelper;
  private final PurchaseOrderStorageService purchaseOrderStorageService;
  private final Context vertxContext;
  private final PoLineImportProgressService poLineImportProgressService;

  @Autowired
  public OrderPostProcessingEventHandler(PurchaseOrderHelper purchaseOrderHelper,
                                         PurchaseOrderStorageService purchaseOrderStorageService, Context vertxContext,
                                         PoLineImportProgressService poLineImportProgressService) {
    this.purchaseOrderHelper = purchaseOrderHelper;
    this.purchaseOrderStorageService = purchaseOrderStorageService;
    this.vertxContext = vertxContext;
    this.poLineImportProgressService = poLineImportProgressService;
  }

  @Override
  public CompletableFuture<DataImportEventPayload> handle(DataImportEventPayload dataImportEventPayload) {
    CompletableFuture<DataImportEventPayload> future = new CompletableFuture<>();
    dataImportEventPayload.setEventType(DI_ORDER_CREATED.value());
    HashMap<String, String> payloadContext = dataImportEventPayload.getContext();
    if (payloadContext == null || isBlank(payloadContext.get(ORDER_LINES_KEY))) {
      LOGGER.warn("handle:: {}", PAYLOAD_HAS_NO_DATA_MSG);
      return CompletableFuture.failedFuture(new EventProcessingException(PAYLOAD_HAS_NO_DATA_MSG));
    }

    Map<String, String> okapiHeaders = extractOkapiHeaders(dataImportEventPayload);
    RequestContext requestContext = new RequestContext(vertxContext, okapiHeaders);
    CompositePoLine poLine = Json.decodeValue(payloadContext.get(ORDER_LINES_KEY), CompositePoLine.class);

    poLineImportProgressService.isPoLinesImported(poLine.getPurchaseOrderId(), dataImportEventPayload.getTenant())
      .compose(isLinesImported -> Boolean.TRUE.equals(isLinesImported)
        ? purchaseOrderStorageService.getPurchaseOrderByIdAsJson(poLine.getPurchaseOrderId(), requestContext)
          .map(HelperUtils::convertToCompositePurchaseOrder)
          .map(order -> order.withWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN))
          .compose(order -> purchaseOrderHelper.updateOrder(order, false, requestContext))
        : Future.succeededFuture())
      .onComplete(ar -> {
        if (ar.failed()) {
          LOGGER.error("handle:: Error during processing order to Open status", ar.cause());
          future.completeExceptionally(ar.cause());
          return;
        }
        future.complete(dataImportEventPayload);
      });

    return future;
  }

  private Map<String, String> extractOkapiHeaders(DataImportEventPayload dataImportEventPayload) {
    Map<String, String> headers = new HashMap<>();
    headers.put(RestVerticle.OKAPI_HEADER_TENANT, dataImportEventPayload.getTenant());
    headers.put(RestVerticle.OKAPI_HEADER_TOKEN, dataImportEventPayload.getToken());
    headers.put(RestConstants.OKAPI_URL, dataImportEventPayload.getOkapiUrl());

    String permissionsHeader = dataImportEventPayload.getContext().get(CreateOrderEventHandler.PERMISSIONS_KEY);
    if (StringUtils.isNotBlank(permissionsHeader)) {
      headers.put(CreateOrderEventHandler.OKAPI_PERMISSIONS_HEADER, permissionsHeader);
    }
    String userId = dataImportEventPayload.getContext().get(CreateOrderEventHandler.USER_ID_KEY);
    if (StringUtils.isNotBlank(userId)) {
      headers.put(RestVerticle.OKAPI_USERID_HEADER, userId);
    }
    return headers;
  }

  @Override
  public boolean isEligible(DataImportEventPayload dataImportEventPayload) {
    if (dataImportEventPayload.getCurrentNode() != null && MAPPING_PROFILE == dataImportEventPayload.getCurrentNode().getContentType()) {
      MappingProfile mappingProfile = ObjectMapperTool.getMapper()
        .convertValue(dataImportEventPayload.getCurrentNode().getContent(), MappingProfile.class);

      return mappingProfile.getExistingRecordType() == ORDER
        || mappingProfile.getExistingRecordType() == EntityType.INSTANCE
        || mappingProfile.getExistingRecordType() == EntityType.HOLDINGS
        || mappingProfile.getExistingRecordType() == EntityType.ITEM;
    }
    return false;
  }
}
