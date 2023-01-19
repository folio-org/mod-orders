package org.folio.service.dataimport.handlers;

import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.Vertx;
import io.vertx.core.json.Json;
import io.vertx.core.json.JsonObject;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.ActionProfile;
import org.folio.DataImportEventPayload;
import org.folio.MappingProfile;
import org.folio.dbschema.ObjectMapperTool;
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
import org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus;
import org.folio.rest.jaxrs.model.EntityType;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.ProfileSnapshotWrapper;
import org.folio.rest.util.OkapiConnectionParams;
import org.folio.service.caches.JobProfileSnapshotCache;
import org.folio.service.configuration.ConfigurationEntriesService;
import org.folio.service.dataimport.IdStorageService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.time.LocalDate;
import java.time.Period;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import static java.lang.String.format;
import static org.apache.commons.lang3.StringUtils.isBlank;
import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.folio.ActionProfile.Action.CREATE;
import static org.folio.ActionProfile.FolioRecord.HOLDINGS;
import static org.folio.ActionProfile.FolioRecord.INSTANCE;
import static org.folio.ActionProfile.FolioRecord.ITEM;
import static org.folio.ActionProfile.FolioRecord.MARC_BIBLIOGRAPHIC;
import static org.folio.ActionProfile.FolioRecord.ORDER;
import static org.folio.DataImportEventTypes.DI_COMPLETED;
import static org.folio.DataImportEventTypes.DI_ORDER_CREATED;
import static org.folio.DataImportEventTypes.DI_ORDER_CREATED_READY_FOR_POST_PROCESSING;
import static org.folio.DataImportEventTypes.DI_PENDING_ORDER_CREATED;
import static org.folio.orders.utils.HelperUtils.ORDER_CONFIG_MODULE_NAME;
import static org.folio.orders.utils.HelperUtils.PO_LINES_LIMIT_PROPERTY;
import static org.folio.rest.jaxrs.model.ProfileSnapshotWrapper.ContentType.ACTION_PROFILE;

@Component
public class CreateOrderEventHandler implements EventHandler {

  private static final Logger LOGGER = LogManager.getLogger();
  private static final String PAYLOAD_HAS_NO_DATA_MSG =
    "Failed to handle event payload, cause event payload context does not contain MARC_BIBLIOGRAPHIC data";

  public static final String PERMISSIONS_KEY = "OKAPI_PERMISSIONS";
  public static final String USER_ID_KEY = "USER_ID";
  public static final String OKAPI_PERMISSIONS_HEADER = "X-Okapi-Permissions";
  static final String POL_LIMIT_RULE_NAME = "overridePoLinesLimit";
  private static final String ORDER_FIELD = "po";
  private static final String PO_LINES_FIELD = "poLine";
  private static final String MAPPING_RESULT_FIELD = "order";
  private static final String INSTANCE_ID_FIELD = "id";
  private static final String ORDER_STATUS_FIELD = "workflowStatus";
  private static final String POL_ACTIVATION_DUE_FIELD = "activationDue";
  private static final String POL_ERESOURCE_FIELD = "eresource";
  private static final String POL_PHYSICAL_FIELD = "physical";
  private static final String POL_CREATE_INVENTORY_FIELD = "createInventory";
  private static final String ORDER_LINES_KEY = "ORDER_LINES";
  private static final String RECORD_ID_HEADER = "recordId";
  private static final String JOB_PROFILE_SNAPSHOT_ID_KEY = "JOB_PROFILE_SNAPSHOT_ID";
  private static final String ID_UNIQUENESS_ERROR_MSG = "duplicate key value violates unique constraint";
  private static final String PROFILE_SNAPSHOT_NOT_FOUND_MSG = "JobProfileSnapshot was not found by profileSnapshotId '%s'";
  private static final String POST_PROCESSING = "POST_PROCESSING";
  private static final String WORKFLOW_STATUS_PATH = "order.po.workflowStatus";

  private final PurchaseOrderHelper purchaseOrderHelper;
  private final PurchaseOrderLineHelper poLineHelper;
  private final ConfigurationEntriesService configurationEntriesService;
  private final IdStorageService idStorageService;
  private final JobProfileSnapshotCache jobProfileSnapshotCache;

  @Autowired
  public CreateOrderEventHandler(PurchaseOrderHelper purchaseOrderHelper, PurchaseOrderLineHelper poLineHelper,
                                 ConfigurationEntriesService configurationEntriesService, IdStorageService idStorageService,
                                 JobProfileSnapshotCache jobProfileSnapshotCache) {
    this.purchaseOrderHelper = purchaseOrderHelper;
    this.poLineHelper = poLineHelper;
    this.configurationEntriesService = configurationEntriesService;
    this.idStorageService = idStorageService;
    this.jobProfileSnapshotCache = jobProfileSnapshotCache;
  }

  @Override
  public CompletableFuture<DataImportEventPayload> handle(DataImportEventPayload dataImportEventPayload) {
    CompletableFuture<DataImportEventPayload> future = new CompletableFuture<>();
    dataImportEventPayload.getEventsChain().add(dataImportEventPayload.getEventType());
    dataImportEventPayload.setEventType(DI_ORDER_CREATED.value());
    HashMap<String, String> payloadContext = dataImportEventPayload.getContext();
    if (payloadContext == null || isBlank(payloadContext.get(MARC_BIBLIOGRAPHIC.value()))) {
      LOGGER.warn("handle:: {}", PAYLOAD_HAS_NO_DATA_MSG);
      return CompletableFuture.failedFuture(new EventProcessingException(PAYLOAD_HAS_NO_DATA_MSG));
    }

    Map<String, String> okapiHeaders = extractOkapiHeaders(dataImportEventPayload);
    String sourceRecordId = dataImportEventPayload.getContext().get(RECORD_ID_HEADER);
    Optional<Integer> poLinesLimitOptional = extractPoLinesLimit(dataImportEventPayload);
    prepareEventPayloadForMapping(dataImportEventPayload);
    MappingManager.map(dataImportEventPayload, new MappingContext());

    RequestContext requestContext = new RequestContext(Vertx.currentContext(), okapiHeaders);
    Future<JsonObject> tenantConfigFuture = configurationEntriesService.loadConfiguration(ORDER_CONFIG_MODULE_NAME, requestContext);

    tenantConfigFuture
      .onSuccess(tenantConfig -> overridePoLinesLimit(tenantConfig, poLinesLimitOptional))
      .compose(v -> prepareMappingResult(dataImportEventPayload))
      .compose(v -> setApprovedFalseIfUserNotHaveApprovalPermission(dataImportEventPayload, tenantConfigFuture.result(), requestContext))
      .compose(v -> idStorageService.store(sourceRecordId, UUID.randomUUID().toString(), dataImportEventPayload.getTenant()))
      .compose(orderId -> saveOrder(dataImportEventPayload, orderId, tenantConfigFuture.result(), requestContext))
      .compose(savedOrder -> saveOrderLines(savedOrder.getId(), dataImportEventPayload, tenantConfigFuture.result(), requestContext))
      .compose(v -> adjustEventType(dataImportEventPayload, okapiHeaders))
      .onComplete(ar -> {
        if (ar.failed()) {
          LOGGER.error("handle:: Error during order or order line creation", ar.cause());
          future.completeExceptionally(ar.cause());
          return;
        }
        future.complete(dataImportEventPayload);
      });

    return future;
  }

  private Future<Object> setApprovedFalseIfUserNotHaveApprovalPermission(DataImportEventPayload dataImportEventPayload, JsonObject tenantConfig, RequestContext requestContext) {
    CompositePurchaseOrder order = Json.decodeValue(dataImportEventPayload.getContext().get(ORDER.value()), CompositePurchaseOrder.class);
    Boolean isApproved = order.getApproved();
    boolean isApprovalRequired = PurchaseOrderHelper.isApprovalRequiredConfiguration(tenantConfig);
    boolean isUserNotHaveApprovalPermission = PurchaseOrderHelper.isUserNotHaveApprovePermission(requestContext);

    if (isApprovalRequired && isApproved && isUserNotHaveApprovalPermission) {
      order.setApproved(false);
      dataImportEventPayload.getContext().put(ORDER.value(), Json.encode(order));
    }
    return Future.succeededFuture();
  }

  private Future<Void> adjustEventType(DataImportEventPayload dataImportEventPayload, Map<String, String> okapiHeaders) {
    CompositePurchaseOrder order = Json.decodeValue(dataImportEventPayload.getContext().get(ORDER.value()), CompositePurchaseOrder.class);
    WorkflowStatus workflowStatus = extractWorkflowStatus(dataImportEventPayload);

    Promise<Void> promise = Promise.promise();

    if (workflowStatus.equals(WorkflowStatus.PENDING)) {
      LOGGER.debug(format("adjustEventType:: set event type DI_COMPLETED for jobExecutionId %s", dataImportEventPayload.getJobExecutionId()));
      return Future.succeededFuture();
    }

    Boolean isApproved = order.getApproved();

    if (workflowStatus.equals(WorkflowStatus.OPEN)) {
      if (!isApproved) {
        LOGGER.debug(format("adjustEventType:: set event type DI_COMPLETED for jobExecutionId %s", dataImportEventPayload.getJobExecutionId()));
        return Future.succeededFuture();
      }

      String profileSnapshotId = dataImportEventPayload.getContext().get(JOB_PROFILE_SNAPSHOT_ID_KEY);

      Map<String, String> okapiHeadersLowerCaseKeys = okapiHeaders.entrySet().stream().collect(Collectors.toMap(
        key -> key.getKey().toLowerCase(Locale.ROOT),
        value -> value.getValue()
      ));
      OkapiConnectionParams okapiParams = new OkapiConnectionParams(okapiHeadersLowerCaseKeys, Vertx.vertx());

      jobProfileSnapshotCache.get(profileSnapshotId, okapiParams)
        .toCompletionStage()
        .thenCompose(snapshotOptional -> snapshotOptional
          .map(profileSnapshot -> setEventTypeForOpenOrder(dataImportEventPayload, profileSnapshot))
          .orElse(CompletableFuture.failedFuture((new EventProcessingException(format(PROFILE_SNAPSHOT_NOT_FOUND_MSG, profileSnapshotId))))))
        .whenComplete((processed, throwable) -> {
          if (throwable != null) {
            promise.fail(throwable);
            LOGGER.error(throwable.getMessage());
          } else {
            promise.complete();
            LOGGER.debug(format("adjustEventType:: Job profile snapshot with id '%s' was retrieved from cache", profileSnapshotId));
          }
        });
    }

    return promise.future();
  }

  private WorkflowStatus extractWorkflowStatus(DataImportEventPayload dataImportEventPayload) {
    ProfileSnapshotWrapper mappingProfileWrapper = dataImportEventPayload.getCurrentNode();
    MappingProfile mappingProfile = ObjectMapperTool.getMapper().convertValue(mappingProfileWrapper.getContent(), MappingProfile.class);

    return mappingProfile.getMappingDetails().getMappingFields().stream()
      .filter(mappingRule -> WORKFLOW_STATUS_PATH.equals(mappingRule.getPath()))
      .map(mappingRule -> Json.decodeValue(mappingRule.getValue(), WorkflowStatus.class))
      .findFirst().orElse(WorkflowStatus.PENDING);
  }

  private CompletableFuture<Void> setEventTypeForOpenOrder(DataImportEventPayload dataImportEventPayload, ProfileSnapshotWrapper jobProfileSnapshotWrapper) {
    List<ProfileSnapshotWrapper> actionProfiles = jobProfileSnapshotWrapper
      .getChildSnapshotWrappers()
      .stream()
      .filter(e -> e.getContentType() == ProfileSnapshotWrapper.ContentType.ACTION_PROFILE)
      .collect(Collectors.toList());

    if (!actionProfiles.isEmpty() && checkIfCurrentProfileIsTheLastOne(dataImportEventPayload, actionProfiles)) {
      LOGGER.debug(format("setEventTypeForOpenOrder:: set event type DI_ORDER_CREATED_READY_FOR_POST_PROCESSING for jobExecutionId %s", dataImportEventPayload.getJobExecutionId()));
      dataImportEventPayload.setEventType(DI_ORDER_CREATED_READY_FOR_POST_PROCESSING.value());
      dataImportEventPayload.getContext().put(POST_PROCESSING, "true");
    } else {
      LOGGER.debug(format("setEventTypeForOpenOrder:: set event type DI_ORDER_CREATED for jobExecutionId %s", dataImportEventPayload.getJobExecutionId()));
      dataImportEventPayload.setEventType(DI_PENDING_ORDER_CREATED.value());
    }

    return CompletableFuture.completedFuture(null);
  }

  private static boolean checkIfCurrentProfileIsTheLastOne(DataImportEventPayload eventPayload, List<ProfileSnapshotWrapper> actionProfiles) {
    String currentMappingProfileId = eventPayload.getCurrentNode().getProfileId();
    ProfileSnapshotWrapper lastActionProfile = actionProfiles.get(actionProfiles.size() - 1);
    List<ProfileSnapshotWrapper> childSnapshotWrappers = lastActionProfile.getChildSnapshotWrappers();
    String mappingProfileId = org.apache.commons.lang.StringUtils.EMPTY;

    if (childSnapshotWrappers != null && childSnapshotWrappers.size() > 0
      && childSnapshotWrappers.get(0) != null && Objects.equals(childSnapshotWrappers.get(0).getContentType().value(), "MAPPING_PROFILE")) {
      mappingProfileId = childSnapshotWrappers.get(0).getProfileId();
    }

    return mappingProfileId.equals(currentMappingProfileId);
  }

  private Map<String, String> extractOkapiHeaders(DataImportEventPayload dataImportEventPayload) {
    Map<String, String> headers = new HashMap<>();
    headers.put(RestVerticle.OKAPI_HEADER_TENANT, dataImportEventPayload.getTenant());
    headers.put(RestVerticle.OKAPI_HEADER_TOKEN, dataImportEventPayload.getToken());
    headers.put(RestConstants.OKAPI_URL, dataImportEventPayload.getOkapiUrl());

    String permissionsHeader = dataImportEventPayload.getContext().get(PERMISSIONS_KEY);
    if (StringUtils.isNotBlank(permissionsHeader)) {
      headers.put(OKAPI_PERMISSIONS_HEADER, permissionsHeader);
    }
    String userId = dataImportEventPayload.getContext().get(USER_ID_KEY);
    if (StringUtils.isNotBlank(userId)) {
      headers.put(RestVerticle.OKAPI_USERID_HEADER, userId);
    }
    return headers;
  }

  private Future<CompositePurchaseOrder> saveOrder(DataImportEventPayload dataImportEventPayload, String orderId,
                                                   JsonObject tenantConfig, RequestContext requestContext) {
    CompositePurchaseOrder orderToSave = Json.decodeValue(dataImportEventPayload.getContext().get(ORDER.value()), CompositePurchaseOrder.class);
    orderToSave.setId(orderId);
    orderToSave.setOrderType(CompositePurchaseOrder.OrderType.ONE_TIME); // todo: workaround for mapping profile
    // in this handler a purchase order always is created in PENDING status despite the status that is set during mapping
    orderToSave.setWorkflowStatus(WorkflowStatus.PENDING);

    return purchaseOrderHelper.validateOrder(orderToSave, tenantConfig, requestContext)
      .compose(errors -> {
        if (CollectionUtils.isNotEmpty(errors)) {
          return Future.failedFuture(new EventProcessingException(errors.toString()));
        }
        return purchaseOrderHelper.createPurchaseOrder(orderToSave, tenantConfig, requestContext)
          .onComplete(v -> dataImportEventPayload.getContext().put(ORDER.value(), Json.encode(orderToSave)))
          .recover(e -> {
            if (e instanceof HttpException) {
              String message = ((HttpException) e).getError().getMessage();
              if (message.contains(ID_UNIQUENESS_ERROR_MSG)) {
                LOGGER.debug("saveOrder:: Failed to create order with existing id: '{}' due to duplicated event. Ignoring event processing", orderId);
                return Future.failedFuture(new DuplicateEventException(message));
              }
            }
            LOGGER.warn("saveOrder:: Error during creation order in the storage", e);
            return Future.failedFuture(e);
          });
      });
  }

  private Future<CompositePoLine> saveOrderLines(String orderId, DataImportEventPayload dataImportEventPayload,
                                                 JsonObject tenantConfig, RequestContext requestContext) {
    CompositePoLine poLine = Json.decodeValue(dataImportEventPayload.getContext().get(ORDER_LINES_KEY), CompositePoLine.class);
    poLine.setPurchaseOrderId(orderId);
    poLine.setSource(CompositePoLine.Source.MARC);

    if (dataImportEventPayload.getContext().containsKey(EntityType.INSTANCE.value())) {
      JsonObject instanceJson = new JsonObject(dataImportEventPayload.getContext().get(EntityType.INSTANCE.value()));
      poLine.setInstanceId(instanceJson.getString(INSTANCE_ID_FIELD));
    }

    return poLineHelper.createPoLine(poLine, tenantConfig, requestContext)
      .onComplete(ar -> dataImportEventPayload.getContext().put(ORDER_LINES_KEY, Json.encode(poLine)));
  }

  private Optional<Integer> extractPoLinesLimit(DataImportEventPayload dataImportEventPayload) {
    ProfileSnapshotWrapper mappingProfileWrapper = dataImportEventPayload.getCurrentNode().getChildSnapshotWrappers().get(0);
    MappingProfile mappingProfile = ObjectMapperTool.getMapper().convertValue(mappingProfileWrapper.getContent(), MappingProfile.class);

    Optional<Integer> limitOptional = mappingProfile.getMappingDetails().getMappingFields().stream()
      .filter(mappingRule -> POL_LIMIT_RULE_NAME.equals(mappingRule.getName()) && isNotBlank(mappingRule.getValue()))
      .peek(mappingRule -> mappingRule.setEnabled("false"))
      .map(mappingRule -> Integer.parseInt(StringUtils.unwrap(mappingRule.getValue(), '"')))
      .findFirst();

    if (limitOptional.isPresent()) {
      mappingProfileWrapper.setContent(ObjectMapperTool.getMapper().convertValue(mappingProfile, Map.class));
    }
    return limitOptional;
  }

  private void prepareEventPayloadForMapping(DataImportEventPayload dataImportEventPayload) {
    dataImportEventPayload.setCurrentNode(dataImportEventPayload.getCurrentNode().getChildSnapshotWrappers().get(0));
    dataImportEventPayload.getContext().put(ORDER.value(), new JsonObject().encode());
  }

  private Future<Void> prepareMappingResult(DataImportEventPayload dataImportEventPayload) {
    JsonObject mappingResult = new JsonObject(dataImportEventPayload.getContext().get(ORDER.value()));
    JsonObject orderJson = mappingResult.getJsonObject(MAPPING_RESULT_FIELD).getJsonObject(ORDER_FIELD);
    JsonObject poLineJson = mappingResult.getJsonObject(MAPPING_RESULT_FIELD).getJsonObject(PO_LINES_FIELD);
    calculateActivationDue(poLineJson);
    dataImportEventPayload.getContext().put(ORDER.value(), orderJson.encode());

    if (WorkflowStatus.OPEN.value().equals(orderJson.getString(ORDER_STATUS_FIELD))
      && (poLineJson.getJsonObject(POL_ERESOURCE_FIELD) != null || poLineJson.getJsonObject(POL_PHYSICAL_FIELD) != null)) {
      return overrideCreateInventoryField(poLineJson, dataImportEventPayload)
        .onComplete(v -> dataImportEventPayload.getContext().put(ORDER_LINES_KEY, poLineJson.encode()));
    }

    // todo: workaround:
//    orderJson.put("workflowStatus", orderJson.getString("poStatus"));
//    orderJson.put("orderType", "One-Time");
//    orderJson.remove("poStatus");
//    if (orderJson.getString("acqUnitIds") != null) {
//      orderJson.put("acqUnitIds", new JsonArray(List.of(orderJson.getString("acqUnitIds"))));
//    }
    dataImportEventPayload.getContext().put(ORDER.value(), orderJson.encode());
//    poLineJson.put("titleOrPackage", poLineJson.getString("title"));
//    poLineJson.remove("title");
//    poLineJson.getJsonObject("eresource").put("activated", false);
//    poLineJson.getJsonObject("eresource").remove("activationStatus");
//    poLineJson.remove("useExchangeRate");
    dataImportEventPayload.getContext().put(ORDER_LINES_KEY, poLineJson.encode());
    return Future.succeededFuture();
  }

  private void calculateActivationDue(JsonObject poLineJson) {
    JsonObject eresourceJson = poLineJson.getJsonObject(POL_ERESOURCE_FIELD);
    if (eresourceJson != null && eresourceJson.getString(POL_ACTIVATION_DUE_FIELD) != null) {
      String activationDueValue = eresourceJson.getString(POL_ACTIVATION_DUE_FIELD);
      LocalDate activationDate = LocalDate.parse(activationDueValue);
      LocalDate orderCreationDate = LocalDate.now(ZoneId.of(ZoneOffset.UTC.getId()));

      int activationDue = activationDate.isAfter(orderCreationDate)
        ? Period.between(orderCreationDate, activationDate).getDays()
        : 1;
      poLineJson.getJsonObject(POL_ERESOURCE_FIELD).put(POL_ACTIVATION_DUE_FIELD, activationDue);
    }
  }

  private Future<Void> overrideCreateInventoryField(JsonObject poLineJson, DataImportEventPayload dataImportEventPayload) {
    String profileSnapshotId = dataImportEventPayload.getContext().get(JOB_PROFILE_SNAPSHOT_ID_KEY);
    Map<String, String> headers = extractOkapiHeaders(dataImportEventPayload);
    OkapiConnectionParams okapiParams = new OkapiConnectionParams(headers, Vertx.vertx());

    return jobProfileSnapshotCache.get(profileSnapshotId, okapiParams)
      .compose(snapshotOptional -> snapshotOptional
        .map(profileSnapshot -> Future.succeededFuture(populateCreateInventoryField(poLineJson, profileSnapshot)))
        .orElse(Future.failedFuture((String.format(PROFILE_SNAPSHOT_NOT_FOUND_MSG, profileSnapshotId))))
      );
  }

  private Void populateCreateInventoryField(JsonObject poLineJson, ProfileSnapshotWrapper profileSnapshot) {
    Set<ActionProfile.FolioRecord> inventoryTypes = profileSnapshot.getChildSnapshotWrappers().stream()
      .filter(childWrapper -> ACTION_PROFILE.equals(childWrapper.getContentType()))
      .map(actionWrapper -> ObjectMapperTool.getMapper().convertValue((actionWrapper.getContent()), ActionProfile.class))
      .filter(actionProfile -> actionProfile.getFolioRecord().equals(INSTANCE)
        || actionProfile.getFolioRecord().equals(HOLDINGS)
        || actionProfile.getFolioRecord().equals(ITEM))
      .map(ActionProfile::getFolioRecord)
      .collect(Collectors.toCollection(() -> EnumSet.noneOf(ActionProfile.FolioRecord.class)));

    Eresource.CreateInventory createInventoryFieldValue;
    if (inventoryTypes.contains(INSTANCE) && inventoryTypes.contains(HOLDINGS) && inventoryTypes.contains(ITEM)) {
      createInventoryFieldValue = Eresource.CreateInventory.INSTANCE_HOLDING_ITEM;
    } else if (inventoryTypes.contains(INSTANCE) && inventoryTypes.contains(HOLDINGS)) {
      createInventoryFieldValue = Eresource.CreateInventory.INSTANCE_HOLDING;
    } else if (inventoryTypes.contains(INSTANCE)) {
      createInventoryFieldValue = Eresource.CreateInventory.INSTANCE;
    } else {
      createInventoryFieldValue = Eresource.CreateInventory.NONE;
    }

    if (poLineJson.getJsonObject(POL_ERESOURCE_FIELD) != null) {
      poLineJson.getJsonObject(POL_ERESOURCE_FIELD).put(POL_CREATE_INVENTORY_FIELD, createInventoryFieldValue.value());
    } else if (poLineJson.getJsonObject(POL_PHYSICAL_FIELD) != null) {
      poLineJson.getJsonObject(POL_PHYSICAL_FIELD).put(POL_CREATE_INVENTORY_FIELD, createInventoryFieldValue.value());
    }
    return null;
  }

  private void overridePoLinesLimit(JsonObject tenantConfig, Optional<Integer> poLinesLimitOptional) {
    poLinesLimitOptional.ifPresent(poLinesLimit -> tenantConfig.put(PO_LINES_LIMIT_PROPERTY, poLinesLimit));
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
