package org.folio.service.dataimport.handlers;

import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.json.Json;
import io.vertx.core.json.JsonObject;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.ActionProfile;
import org.folio.DataImportEventPayload;
import org.folio.helper.PurchaseOrderHelper;
import org.folio.helper.PurchaseOrderLineHelper;
import org.folio.processing.events.services.handler.EventHandler;
import org.folio.processing.exceptions.EventProcessingException;
import org.folio.processing.mapping.MappingManager;
import org.folio.processing.mapping.mapper.MappingContext;
import org.folio.rest.RestConstants;
import org.folio.rest.RestVerticle;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import static org.apache.commons.lang3.StringUtils.isBlank;
import static org.folio.ActionProfile.Action.CREATE;
import static org.folio.ActionProfile.FolioRecord.MARC_BIBLIOGRAPHIC;
import static org.folio.ActionProfile.FolioRecord.ORDER;
import static org.folio.rest.jaxrs.model.ProfileSnapshotWrapper.ContentType.ACTION_PROFILE;

@Component
public class CreateOrderEventHandler implements EventHandler {

  private static final Logger LOGGER = LogManager.getLogger();
  private static final String PAYLOAD_HAS_NO_DATA_MSG = "Failed to handle event payload, cause event payload context does not contain MARC_BIBLIOGRAPHIC data";

  private static final String ORDER_FIELD = "po";
  private static final String PO_LINES_FIELD = "poLine";
  private static final String ORDER_LINES_KEY = "ORDER_LINES";
  private PurchaseOrderHelper purchaseOrderHelper;
  private PurchaseOrderLineHelper poLineHelper;

  @Autowired
  public CreateOrderEventHandler(PurchaseOrderHelper purchaseOrderHelper, PurchaseOrderLineHelper poLineHelper) {
    this.purchaseOrderHelper = purchaseOrderHelper;
    this.poLineHelper = poLineHelper;
  }

  @Override
  public CompletableFuture<DataImportEventPayload> handle(DataImportEventPayload dataImportEventPayload) {
    CompletableFuture<DataImportEventPayload> future = new CompletableFuture<>();
    dataImportEventPayload.setEventType("DI_ORDER_CREATED");

    HashMap<String, String> payloadContext = dataImportEventPayload.getContext();
    if (payloadContext == null || isBlank(payloadContext.get(MARC_BIBLIOGRAPHIC.value()))) {
      LOGGER.error(PAYLOAD_HAS_NO_DATA_MSG);
      return CompletableFuture.failedFuture(new EventProcessingException(PAYLOAD_HAS_NO_DATA_MSG));
    }

    Map<String, String> okapiHeaders = extractOkapiHeaders(dataImportEventPayload);
    MappingManager.map(dataImportEventPayload, new MappingContext());
    prepareMappingResult(dataImportEventPayload);
    saveOrder(dataImportEventPayload, okapiHeaders)
      .compose(v -> saveOrderLines(dataImportEventPayload, okapiHeaders))
      .onComplete(ar -> {
        if (ar.failed()) {
          LOGGER.error("Error during order creation", ar.cause());
          future.completeExceptionally(ar.cause());
          return;
        }
        future.complete(dataImportEventPayload);
      });

    return future;
  }

    private Map<String, String> extractOkapiHeaders(DataImportEventPayload dataImportEventPayload) {
    return Map.of(RestVerticle.OKAPI_HEADER_TENANT, dataImportEventPayload.getTenant(),
      RestVerticle.OKAPI_HEADER_TOKEN, dataImportEventPayload.getToken(),
      RestConstants.OKAPI_URL, dataImportEventPayload.getOkapiUrl());
  }

  private Future<Void> saveOrder(DataImportEventPayload dataImportEventPayload, Map<String, String> okapiHeaders) {
    RequestContext requestContext = new RequestContext(Vertx.currentContext(), okapiHeaders);
    CompositePurchaseOrder orderToSave = Json.decodeValue(dataImportEventPayload.getContext().get(ORDER.value()), CompositePurchaseOrder.class);

    return purchaseOrderHelper.validateOrder(orderToSave, null, requestContext).compose(errors -> {
      if (CollectionUtils.isNotEmpty(errors)) {
        return Future.failedFuture(new EventProcessingException(errors.toString())); //todo: prepare error msg
      }

      return purchaseOrderHelper.createPurchaseOrder(orderToSave, requestContext)
        .onFailure(e -> LOGGER.warn("Error during creation order in the storage", e))
        .mapEmpty();
    });
  }

  private Future<CompositePoLine> saveOrderLines(DataImportEventPayload dataImportEventPayload, Map<String, String> okapiHeaders) {
    RequestContext requestContext = new RequestContext(Vertx.currentContext(), okapiHeaders);
    CompositePoLine poLine = Json.decodeValue(dataImportEventPayload.getContext().get(ORDER_LINES_KEY), CompositePoLine.class);

    return poLineHelper.createPoLine(poLine, requestContext);
  }

  private void prepareMappingResult(DataImportEventPayload dataImportEventPayload) {
    JsonObject mappingResult = new JsonObject(dataImportEventPayload.getContext().get(ORDER.value()));
    JsonObject orderJson = mappingResult.getJsonObject(ORDER_FIELD);
    JsonObject poLineJson = mappingResult.getJsonObject(PO_LINES_FIELD);
    dataImportEventPayload.getContext().put(ORDER.value(), orderJson.encode());
    dataImportEventPayload.getContext().put(ORDER_LINES_KEY, poLineJson.encode());
  }

  @Override
  public boolean isEligible(DataImportEventPayload dataImportEventPayload) {
    if (dataImportEventPayload.getCurrentNode() != null && ACTION_PROFILE == dataImportEventPayload.getCurrentNode().getContentType()) {
      ActionProfile actionProfile = JsonObject.mapFrom(dataImportEventPayload.getCurrentNode().getContent()).mapTo(ActionProfile.class);
      return actionProfile.getAction() == CREATE && actionProfile.getFolioRecord() == ORDER;
    }
    return false;
  }
}
