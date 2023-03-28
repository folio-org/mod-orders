package org.folio.service.dataimport.handlers;

import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.json.Json;
import io.vertx.core.json.JsonObject;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.DataImportEventPayload;
import org.folio.MappingProfile;
import org.folio.dbschema.ObjectMapperTool;
import org.folio.helper.PurchaseOrderHelper;
import org.folio.orders.utils.HelperUtils;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.processing.events.services.handler.EventHandler;
import org.folio.processing.exceptions.EventProcessingException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.EntityType;
import org.folio.rest.jaxrs.model.Location;
import org.folio.service.dataimport.PoLineImportProgressService;
import org.folio.service.dataimport.utils.DataImportUtils;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.function.BiConsumer;

import static org.apache.commons.lang3.StringUtils.isBlank;
import static org.folio.DataImportEventTypes.DI_ORDER_CREATED;
import static org.folio.rest.jaxrs.model.EntityType.HOLDINGS;
import static org.folio.rest.jaxrs.model.EntityType.INSTANCE;
import static org.folio.rest.jaxrs.model.EntityType.ITEM;
import static org.folio.rest.jaxrs.model.EntityType.ORDER;
import static org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat.PHYSICAL_RESOURCE;
import static org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE;
import static org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat.P_E_MIX;
import static org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat.OTHER;
import static org.folio.rest.jaxrs.model.ProfileSnapshotWrapper.ContentType.MAPPING_PROFILE;

@Component
public class OrderPostProcessingEventHandler implements EventHandler {
  private static final Logger LOGGER = LogManager.getLogger();
  private static final String PAYLOAD_HAS_NO_DATA_MSG =
    "Failed to handle event payload, cause event payload context does not contain ORDER_LINE data";

  private static final String PO_LINE_KEY = "PO_LINE";
  private static final String ID_FIELD = "id";
  private static final String MATERIAL_TYPE_ID = "materialTypeId";
  private static final String PERMANENT_LOCATION_ID = "permanentLocationId";

  private final PurchaseOrderHelper purchaseOrderHelper;
  private final PurchaseOrderStorageService purchaseOrderStorageService;
  private final Context vertxContext;
  private final PoLineImportProgressService poLineImportProgressService;
  private final PurchaseOrderLineService purchaseOrderLineService;
  private final HashMap<CompositePoLine.OrderFormat, BiConsumer<CompositePoLine, String>> orderFormatToAdjustingMaterialType = new HashMap<>() {{
    put(PHYSICAL_RESOURCE, (CompositePoLine pol, String materialType) -> pol.getPhysical().setMaterialType(materialType));
    put(ELECTRONIC_RESOURCE, (CompositePoLine pol, String materialType) -> pol.getEresource().setMaterialType(materialType));
    put(OTHER, (CompositePoLine pol, String materialType) -> pol.getPhysical().setMaterialType(materialType));
    put(P_E_MIX, (CompositePoLine pol, String materialType) -> {
      pol.getPhysical().setMaterialType(materialType);
      pol.getEresource().setMaterialType(materialType);
    });
  }};

  private final HashMap<CompositePoLine.OrderFormat, Location> orderFormatToLocation = new HashMap<>() {{
    put(PHYSICAL_RESOURCE, new Location().withQuantityPhysical(1));
    put(ELECTRONIC_RESOURCE, new Location().withQuantityElectronic(1));
    put(OTHER, new Location().withQuantityPhysical(1));
    put(P_E_MIX, new Location().withQuantityPhysical(1).withQuantityElectronic(1));
  }};

  @Autowired
  public OrderPostProcessingEventHandler(PurchaseOrderHelper purchaseOrderHelper,
                                         PurchaseOrderStorageService purchaseOrderStorageService, Context vertxContext,
                                         PoLineImportProgressService poLineImportProgressService,
                                         PurchaseOrderLineService purchaseOrderLineService) {
    this.purchaseOrderHelper = purchaseOrderHelper;
    this.purchaseOrderStorageService = purchaseOrderStorageService;
    this.vertxContext = vertxContext;
    this.poLineImportProgressService = poLineImportProgressService;
    this.purchaseOrderLineService = purchaseOrderLineService;
  }

  @Override
  public CompletableFuture<DataImportEventPayload> handle(DataImportEventPayload dataImportEventPayload) {
    LOGGER.debug("handle:: jobExecutionId {}", dataImportEventPayload.getJobExecutionId());
    CompletableFuture<DataImportEventPayload> future = new CompletableFuture<>();
    dataImportEventPayload.setEventType(DI_ORDER_CREATED.value());
    HashMap<String, String> payloadContext = dataImportEventPayload.getContext();
    if (payloadContext == null || isBlank(payloadContext.get(PO_LINE_KEY))) {
      LOGGER.warn("handle:: {}, jobExecutionId {}", PAYLOAD_HAS_NO_DATA_MSG, dataImportEventPayload.getJobExecutionId());
      return CompletableFuture.failedFuture(new EventProcessingException(PAYLOAD_HAS_NO_DATA_MSG));
    }

    Map<String, String> okapiHeaders = DataImportUtils.extractOkapiHeaders(dataImportEventPayload);
    RequestContext requestContext = new RequestContext(vertxContext, okapiHeaders);
    CompositePoLine poLine = Json.decodeValue(payloadContext.get(PO_LINE_KEY), CompositePoLine.class);
    adjustLocationAndMaterialType(poLine, payloadContext);

    LOGGER.info("handle:: jobExecutionId {}, poLineId {}, orderId {}", dataImportEventPayload.getJobExecutionId(), poLine.getId(), poLine.getPurchaseOrderId());
    ensurePoLineWithInstanceId(poLine, dataImportEventPayload, requestContext)
      .compose(v -> poLineImportProgressService.trackProcessedPoLine(poLine.getPurchaseOrderId(), dataImportEventPayload.getTenant()))
      .compose(poLinesImported -> Boolean.TRUE.equals(poLinesImported)
        ? openOrder(poLine, requestContext, dataImportEventPayload.getJobExecutionId())
        : Future.succeededFuture())
      .onComplete(ar -> {
        if (ar.failed()) {
          LOGGER.error("handle:: Error during processing order to Open status, jobExecutionId: {}", dataImportEventPayload.getJobExecutionId(), ar.cause());
          future.completeExceptionally(ar.cause());
          return;
        }
        future.complete(dataImportEventPayload);
      });

    return future;
  }

  private void adjustLocationAndMaterialType(CompositePoLine poLine, HashMap<String, String> payloadContext) {
    if (payloadContext.get(HOLDINGS.value()) != null) {
      JsonObject holdingsAsJson = new JsonObject(payloadContext.get(HOLDINGS.value()));
      String permanentLocationId = holdingsAsJson.getString(PERMANENT_LOCATION_ID);
      List<Location> locations = poLine.getLocations();
      if (locations.size() > 0) {
        locations.stream().findFirst().ifPresent(location -> location.setLocationId(permanentLocationId));
      } else {
        locations.add(orderFormatToLocation.get(poLine.getOrderFormat()).withLocationId(permanentLocationId));
      }
    }

    if (payloadContext.get(ITEM.value()) != null) {
      JsonObject itemAsJson = new JsonObject(payloadContext.get(ITEM.value()));
      String materialType = itemAsJson.getString(MATERIAL_TYPE_ID);
      orderFormatToAdjustingMaterialType.get(poLine.getOrderFormat()).accept(poLine, materialType);
    }
  }

  private Future<Void> openOrder(CompositePoLine poLine, RequestContext requestContext, String jobExecutionId) {
    LOGGER.info("All poLines for order were processed, initializing order opening, orderId: {}, poLineNumber: {}, jobExecutionId: {} ",
      poLine.getPurchaseOrderId(), poLine.getPoLineNumber(), jobExecutionId);

    return purchaseOrderStorageService.getPurchaseOrderByIdAsJson(poLine.getPurchaseOrderId(), requestContext)
      .map(HelperUtils::convertToCompositePurchaseOrder)
      .map(order -> order.withWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN))
      .compose(order -> purchaseOrderHelper.updateOrder(order, false, requestContext));
  }

  private Future<Void> ensurePoLineWithInstanceId(CompositePoLine poLine, DataImportEventPayload dataImportEventPayload,
                                                  RequestContext requestContext) {
    if (PoLineCommonUtil.isInventoryUpdateNotRequired(poLine)) {
      LOGGER.debug("ensurePoLineWithInstanceId:: Skipping instanceId filling because poLine does not require inventory entities creation,  jobExecutionId: {}, poLineNumber: {}",
        dataImportEventPayload.getJobExecutionId(), poLine.getPoLineNumber());
      return Future.succeededFuture();
    }

    String instanceAsString = dataImportEventPayload.getContext().get(INSTANCE.value());
    if (StringUtils.isNotEmpty(instanceAsString)) {
      JsonObject instanceJson = new JsonObject(instanceAsString);
      poLine.setInstanceId(instanceJson.getString(ID_FIELD));
      return purchaseOrderLineService.saveOrderLine(poLine, requestContext).mapEmpty();
    }
    return Future.failedFuture("Failed to handle event payload, cause event payload context does not contain INSTANCE data");
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
