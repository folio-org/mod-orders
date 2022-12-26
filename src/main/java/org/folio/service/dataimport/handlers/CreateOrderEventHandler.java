package org.folio.service.dataimport.handlers;

import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.json.Json;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.ActionProfile;
import org.folio.DataImportEventPayload;
import org.folio.DataImportEventTypes;
import org.folio.helper.PurchaseOrderHelper;
import org.folio.helper.PurchaseOrderLineHelper;
import org.folio.kafka.exception.DuplicateEventException;
import org.folio.processing.events.services.handler.EventHandler;
import org.folio.processing.exceptions.EventProcessingException;
import org.folio.processing.mapping.MappingManager;
import org.folio.processing.mapping.mapper.MappingContext;
import org.folio.rest.RestConstants;
import org.folio.rest.RestVerticle;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.EntityType;
import org.folio.service.configuration.ConfigurationEntriesService;
import org.folio.service.dataimport.IdStorageService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

import static org.apache.commons.lang3.StringUtils.isBlank;
import static org.folio.ActionProfile.Action.CREATE;
import static org.folio.ActionProfile.FolioRecord.MARC_BIBLIOGRAPHIC;
import static org.folio.ActionProfile.FolioRecord.ORDER;
import static org.folio.orders.utils.HelperUtils.ORDER_CONFIG_MODULE_NAME;
import static org.folio.rest.jaxrs.model.ProfileSnapshotWrapper.ContentType.ACTION_PROFILE;

@Component
public class CreateOrderEventHandler implements EventHandler {

  private static final Logger LOGGER = LogManager.getLogger();
  private static final String PAYLOAD_HAS_NO_DATA_MSG = "Failed to handle event payload, cause event payload context does not contain MARC_BIBLIOGRAPHIC data";

  private static final String ORDER_FIELD = "po";
  private static final String PO_LINES_FIELD = "poLine";
  public static final String MAPPING_RESULT_FIELD = "order";
  private static final String INSTANCE_ID_FIELD = "id";
  private static final String ORDER_LINES_KEY = "ORDER_LINES";
  private static final String RECORD_ID_HEADER = "recordId";
  public static final String ID_UNIQUENESS_ERROR_MSG = "duplicate key value violates unique constraint";

  private final PurchaseOrderHelper purchaseOrderHelper;
  private final PurchaseOrderLineHelper poLineHelper;
  private final ConfigurationEntriesService configurationEntriesService;
  private final IdStorageService idStorageService;

  @Autowired
  public CreateOrderEventHandler(PurchaseOrderHelper purchaseOrderHelper, PurchaseOrderLineHelper poLineHelper,
                                 ConfigurationEntriesService configurationEntriesService, IdStorageService idStorageService) {
    this.purchaseOrderHelper = purchaseOrderHelper;
    this.poLineHelper = poLineHelper;
    this.configurationEntriesService = configurationEntriesService;
    this.idStorageService = idStorageService;
  }

  @Override
  public CompletableFuture<DataImportEventPayload> handle(DataImportEventPayload dataImportEventPayload) {
    CompletableFuture<DataImportEventPayload> future = new CompletableFuture<>();
    dataImportEventPayload.setEventType("DI_ORDER_CREATED"); //todo:

    HashMap<String, String> payloadContext = dataImportEventPayload.getContext();
    if (payloadContext == null || isBlank(payloadContext.get(MARC_BIBLIOGRAPHIC.value()))) {
      LOGGER.error(PAYLOAD_HAS_NO_DATA_MSG);
      return CompletableFuture.failedFuture(new EventProcessingException(PAYLOAD_HAS_NO_DATA_MSG));
    }

    Map<String, String> okapiHeaders = extractOkapiHeaders(dataImportEventPayload);
    String sourceRecordId = dataImportEventPayload.getContext().get(RECORD_ID_HEADER);
    prepareEventPayloadForMapping(dataImportEventPayload);
    MappingManager.map(dataImportEventPayload, new MappingContext());
    prepareMappingResult(dataImportEventPayload);

    idStorageService.store(sourceRecordId, UUID.randomUUID().toString(), dataImportEventPayload.getTenant())
      .compose(orderId -> saveOrder(dataImportEventPayload, orderId, okapiHeaders))
      .compose(savedOrder -> saveOrderLines(savedOrder.getId(), dataImportEventPayload, okapiHeaders))
      .onComplete(ar -> {
        if (ar.failed() && !(ar.cause() instanceof DuplicateEventException)) {
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

  private Future<CompositePurchaseOrder> saveOrder(DataImportEventPayload dataImportEventPayload, String orderId, Map<String, String> okapiHeaders) {
    RequestContext requestContext = new RequestContext(Vertx.currentContext(), okapiHeaders);
    CompositePurchaseOrder orderToSave = Json.decodeValue(dataImportEventPayload.getContext().get(ORDER.value()), CompositePurchaseOrder.class);
    orderToSave.setId(orderId);
    orderToSave.setOrderType(CompositePurchaseOrder.OrderType.ONE_TIME); // todo: workaround for mapping profile

    return configurationEntriesService.loadConfiguration(ORDER_CONFIG_MODULE_NAME, requestContext)
      .compose(tenantConfig -> purchaseOrderHelper.validateOrder(orderToSave, tenantConfig, requestContext))
      .compose(errors -> {
        if (CollectionUtils.isNotEmpty(errors)) {
          return Future.failedFuture(new EventProcessingException(errors.toString())); //todo: prepare error msg
        }
        return purchaseOrderHelper.createPurchaseOrder(orderToSave, requestContext)
          .recover(e -> {
            if (e instanceof HttpException) {
              String message = ((HttpException) e).getError().getMessage();
              if (message.contains(ID_UNIQUENESS_ERROR_MSG)) {
                LOGGER.debug("Failed to create order with existing id: '{}' due to duplicated event. Ignoring event processing", orderId);
                return Future.failedFuture(new DuplicateEventException(message));
              }
            }
            LOGGER.warn("Error during creation order in the storage", e);
            return Future.failedFuture(e);
          });
      });
  }

  private Future<CompositePoLine> saveOrderLines(String orderId, DataImportEventPayload dataImportEventPayload, Map<String, String> okapiHeaders) {
    RequestContext requestContext = new RequestContext(Vertx.currentContext(), okapiHeaders);
    CompositePoLine poLine = Json.decodeValue(dataImportEventPayload.getContext().get(ORDER_LINES_KEY), CompositePoLine.class);
    poLine.setPurchaseOrderId(orderId);
    poLine.setSource(CompositePoLine.Source.MARC);

    if (dataImportEventPayload.getContext().containsKey(EntityType.INSTANCE.value())) {
      JsonObject instanceJson = new JsonObject(dataImportEventPayload.getContext().get(EntityType.INSTANCE.value()));
      poLine.setInstanceId(instanceJson.getString(INSTANCE_ID_FIELD));
    }

    return poLineHelper.createPoLine(poLine, requestContext);
  }

  private void prepareEventPayloadForMapping(DataImportEventPayload dataImportEventPayload) {
    dataImportEventPayload.getEventsChain().add(dataImportEventPayload.getEventType());
    dataImportEventPayload.setCurrentNode(dataImportEventPayload.getCurrentNode().getChildSnapshotWrappers().get(0));
    dataImportEventPayload.getContext().put(ORDER.value(), new JsonObject().encode());
  }

  private void prepareMappingResult(DataImportEventPayload dataImportEventPayload) {
    JsonObject mappingResult = new JsonObject(dataImportEventPayload.getContext().get(ORDER.value()));
    JsonObject orderJson = mappingResult.getJsonObject(MAPPING_RESULT_FIELD).getJsonObject(ORDER_FIELD);
    JsonObject poLineJson = mappingResult.getJsonObject(MAPPING_RESULT_FIELD).getJsonObject(PO_LINES_FIELD);
    dataImportEventPayload.getContext().put(ORDER.value(), orderJson.encode());

    // todo: workaround:
    orderJson.put("workflowStatus", orderJson.getString("poStatus"));
    orderJson.put("orderType", "One-Time");
    orderJson.remove("poStatus");
    if (orderJson.getString("acqUnitIds") != null) {
      orderJson.put("acqUnitIds", new JsonArray(List.of(orderJson.getString("acqUnitIds"))));
    }
    dataImportEventPayload.getContext().put(ORDER.value(), orderJson.encode());
    poLineJson.put("titleOrPackage", poLineJson.getString("title"));
    poLineJson.remove("title");
    poLineJson.getJsonObject("eresource").put("activated", false);
    poLineJson.getJsonObject("eresource").remove("activationStatus");
    poLineJson.remove("useExchangeRate");
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
