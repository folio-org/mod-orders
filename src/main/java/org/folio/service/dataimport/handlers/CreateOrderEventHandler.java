package org.folio.service.dataimport.handlers;

import static java.lang.Boolean.FALSE;
import static java.lang.String.format;
import static org.apache.commons.lang3.StringUtils.isBlank;
import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.apache.commons.lang3.StringUtils.isNotEmpty;
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
import static org.folio.service.orders.OrderValidationService.isApprovalRequiredConfiguration;
import static org.folio.orders.utils.HelperUtils.DEFAULT_POLINE_LIMIT;
import static org.folio.orders.utils.HelperUtils.ORDER_CONFIG_MODULE_NAME;
import static org.folio.orders.utils.HelperUtils.PO_LINES_LIMIT_PROPERTY;
import static org.folio.orders.utils.PermissionsUtil.userDoesNotHaveApprovePermission;
import static org.folio.rest.jaxrs.model.PoLine.OrderFormat.ELECTRONIC_RESOURCE;
import static org.folio.rest.jaxrs.model.PoLine.OrderFormat.OTHER;
import static org.folio.rest.jaxrs.model.PoLine.OrderFormat.PHYSICAL_RESOURCE;
import static org.folio.rest.jaxrs.model.PoLine.OrderFormat.P_E_MIX;
import static org.folio.rest.jaxrs.model.Eresource.CreateInventory.NONE;
import static org.folio.rest.jaxrs.model.ProfileType.ACTION_PROFILE;
import static org.folio.rest.jaxrs.model.ProfileType.MAPPING_PROFILE;

import java.time.LocalDate;
import java.time.Period;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.IterableUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.ActionProfile;
import org.folio.DataImportEventPayload;
import org.folio.Fund;
import org.folio.MappingProfile;
import org.folio.Record;
import org.folio.dbschema.ObjectMapperTool;
import org.folio.helper.PurchaseOrderHelper;
import org.folio.helper.PurchaseOrderLineHelper;
import org.folio.kafka.exception.DuplicateEventException;
import org.folio.processing.events.services.handler.EventHandler;
import org.folio.processing.exceptions.EventProcessingException;
import org.folio.processing.mapping.MappingManager;
import org.folio.processing.mapping.defaultmapper.processor.parameters.MappingParameters;
import org.folio.processing.mapping.mapper.MappingContext;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus;
import org.folio.rest.jaxrs.model.EntityType;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.ProfileSnapshotWrapper;
import org.folio.rest.util.OkapiConnectionParams;
import org.folio.service.caches.CommonSettingsCache;
import org.folio.service.caches.JobExecutionTotalRecordsCache;
import org.folio.service.caches.JobProfileSnapshotCache;
import org.folio.service.caches.MappingParametersCache;
import org.folio.service.dataimport.IdStorageService;
import org.folio.service.dataimport.PoLineImportProgressService;
import org.folio.service.dataimport.SequentialOrderIdService;
import org.folio.service.dataimport.utils.DataImportUtils;
import org.folio.service.orders.OrderValidationService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import io.vertx.circuitbreaker.CircuitBreaker;
import io.vertx.circuitbreaker.CircuitBreakerOptions;
import io.vertx.circuitbreaker.RetryPolicy;
import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.Vertx;
import io.vertx.core.json.Json;
import io.vertx.core.json.JsonObject;

@Component
public class CreateOrderEventHandler implements EventHandler {

  private static final Logger LOGGER = LogManager.getLogger();
  private static final String PAYLOAD_HAS_NO_DATA_MSG =
    "Failed to handle event payload, cause event payload context does not contain MARC_BIBLIOGRAPHIC data";

  public static final String USER_ID_KEY = "USER_ID";
  public static final String OKAPI_PERMISSIONS_HEADER = "X-Okapi-Permissions";
  static final String POL_LIMIT_RULE_NAME = "overridePoLinesLimit";
  private static final String ORDER_FIELD = "po";
  private static final String PO_LINES_FIELD = "poLine";
  private static final String MAPPING_RESULT_FIELD = "order";
  private static final String INSTANCE_ID_FIELD = "id";
  private static final String ORDER_STATUS_FIELD = "workflowStatus";
  private static final String ORDER_FORMAT_FIELD = "orderFormat";
  private static final String POL_ACTIVATION_DUE_FIELD = "activationDue";
  private static final String POL_ERESOURCE_FIELD = "eresource";
  private static final String POL_PHYSICAL_FIELD = "physical";
  private static final String POL_CREATE_INVENTORY_FIELD = "createInventory";
  private static final String POL_FUND_DISTRIBUTION_FIELD = "fundDistribution";
  private static final String POL_FUND_CODE_FIELD = "code";
  private static final String POL_FUND_ID_FIELD = "fundId";
  private static final String PO_LINE_KEY = "PO_LINE";
  private static final String RECORD_ID_HEADER = "recordId";
  private static final String JOB_PROFILE_SNAPSHOT_ID_KEY = "JOB_PROFILE_SNAPSHOT_ID";
  private static final String ID_UNIQUENESS_ERROR_MSG = "duplicate key value violates unique constraint";
  private static final String PROFILE_SNAPSHOT_NOT_FOUND_MSG = "JobProfileSnapshot was not found by profileSnapshotId '%s'";
  private static final String POST_PROCESSING = "POST_PROCESSING";
  private static final String WORKFLOW_STATUS_PATH = "order.po.workflowStatus";
  private static final String PO_LINE_ORDER_ID_KEY = "purchaseOrderId";
  private static final String DEFAULT_PO_LINES_LIMIT = "1";
  private static final char LEFT_BRACKET = '(';
  private static final char RIGHT_BRACKET = ')';

  private final PurchaseOrderHelper purchaseOrderHelper;
  private final PurchaseOrderLineHelper poLineHelper;
  private final CommonSettingsCache commonSettingsCache;
  private final IdStorageService idStorageService;
  private final JobProfileSnapshotCache jobProfileSnapshotCache;
  private final SequentialOrderIdService sequentialOrderIdService;
  private final PoLineImportProgressService poLineImportProgressService;
  private final OrderValidationService orderValidationService;
  private final JobExecutionTotalRecordsCache jobExecutionTotalRecordsCache;
  private final RetryPolicy retryPolicy = (failure, retryCount) -> 1000L;
  private final Vertx vertx;
  private final CircuitBreaker circuitBreaker;
  private final MappingParametersCache mappingParametersCache;

  @Autowired
  public CreateOrderEventHandler(PurchaseOrderHelper purchaseOrderHelper, PurchaseOrderLineHelper poLineHelper,
                                 CommonSettingsCache commonSettingsCache, IdStorageService idStorageService,
                                 JobProfileSnapshotCache jobProfileSnapshotCache,
                                 SequentialOrderIdService sequentialOrderIdService,
                                 PoLineImportProgressService poLineImportProgressService,
                                 JobExecutionTotalRecordsCache jobExecutionTotalRecordsCache,
                                 Vertx vertx,
                                 MappingParametersCache mappingParametersCache,
                                 OrderValidationService orderValidationService) {
    this.purchaseOrderHelper = purchaseOrderHelper;
    this.poLineHelper = poLineHelper;
    this.commonSettingsCache = commonSettingsCache;
    this.idStorageService = idStorageService;
    this.jobProfileSnapshotCache = jobProfileSnapshotCache;
    this.sequentialOrderIdService = sequentialOrderIdService;
    this.mappingParametersCache = mappingParametersCache;
    this.poLineImportProgressService = poLineImportProgressService;
    this.jobExecutionTotalRecordsCache = jobExecutionTotalRecordsCache;
    this.vertx = vertx;
    this.orderValidationService = orderValidationService;
    circuitBreaker = CircuitBreaker.create("order line circuit breaker", vertx,
      new CircuitBreakerOptions()
        .setMaxRetries(5)       // number of retries
        .setMaxFailures(5)      // number of failure before opening the circuit
        .setTimeout(2000)       // consider a failure if the operation does not succeed in time
    ).retryPolicy(retryPolicy); // retry every second
  }

  @Override
  public CompletableFuture<DataImportEventPayload> handle(DataImportEventPayload dataImportEventPayload) {
    LOGGER.info("handle:: jobExecutionId {}", dataImportEventPayload.getJobExecutionId());
    CompletableFuture<DataImportEventPayload> future = new CompletableFuture<>();
    dataImportEventPayload.getEventsChain().add(dataImportEventPayload.getEventType());
    dataImportEventPayload.setEventType(DI_ORDER_CREATED.value());
    HashMap<String, String> payloadContext = dataImportEventPayload.getContext();
    if (payloadContext == null || isBlank(payloadContext.get(MARC_BIBLIOGRAPHIC.value()))) {
      LOGGER.warn("handle:: {}, jobExecutionId {}", PAYLOAD_HAS_NO_DATA_MSG, dataImportEventPayload.getJobExecutionId());
      return CompletableFuture.failedFuture(new EventProcessingException(PAYLOAD_HAS_NO_DATA_MSG));
    }

    Map<String, String> okapiHeaders = DataImportUtils.extractOkapiHeaders(dataImportEventPayload);
    OkapiConnectionParams okapiParams = getOkapiConnectionParams(okapiHeaders, vertx);
    String sourceRecordId = dataImportEventPayload.getContext().get(RECORD_ID_HEADER);
    idStorageService.store(sourceRecordId, dataImportEventPayload.getTenant())
      .onComplete(result -> {
        if (result.failed()) {
          future.completeExceptionally(result.cause());
        } else {
          Optional<Integer> poLinesLimitOptional = extractPoLinesLimit(dataImportEventPayload);

          RequestContext requestContext = new RequestContext(Vertx.currentContext(), okapiHeaders);
          Future<JsonObject> tenantConfigFuture = commonSettingsCache.loadConfiguration(ORDER_CONFIG_MODULE_NAME, requestContext);
          String temporaryOrderIdForANewOrder = UUID.randomUUID().toString();

          tenantConfigFuture
            .onSuccess(tenantConfig -> overridePoLinesLimit(tenantConfig, poLinesLimitOptional))
            .compose(tenantConfig -> {
              Promise<MappingParameters> promise = Promise.promise();
              prepareEventPayloadForMapping(dataImportEventPayload);
              mappingParametersCache.get(okapiParams)
                .onSuccess(mappingParameters -> {
                  MappingManager.map(dataImportEventPayload, new MappingContext().withMappingParameters(mappingParameters));
                  promise.complete(mappingParameters);
                })
                .onFailure(promise::fail);
              return promise.future();
            })
            .compose(mappingParameters -> prepareMappingResult(dataImportEventPayload, mappingParameters))
            .compose(v -> setApprovedFalseIfUserNotHaveApprovalPermission(dataImportEventPayload, tenantConfigFuture.result(), requestContext))
            .compose(tenantConfig -> generateSequentialOrderId(dataImportEventPayload, tenantConfigFuture.result(), temporaryOrderIdForANewOrder))
            .compose(generatedOrderId -> {
              if (isEmpty(generatedOrderId)) {
                String errorMessage = format("handle:: generatedOrderId is null, jobExecutionId %s", dataImportEventPayload.getJobExecutionId());
                LOGGER.error(errorMessage);
                return Future.failedFuture(new EventProcessingException(errorMessage));
              }
              LOGGER.info("handle:: generatedOrderId = {}, jobExecutionId {}", generatedOrderId, dataImportEventPayload.getJobExecutionId());
              if (temporaryOrderIdForANewOrder.equals(generatedOrderId)) {
                LOGGER.info("handle:: new order with id: {} should be created for jobExecutionId: {}",
                  generatedOrderId, dataImportEventPayload.getJobExecutionId());
                return saveOrder(dataImportEventPayload, generatedOrderId, tenantConfigFuture.result(), requestContext)
                  .map(CompositePurchaseOrder::getId);
              }
              return Future.succeededFuture(generatedOrderId);
            })
            .compose(orderId -> checkIfOrderSaved(orderId, requestContext, dataImportEventPayload.getJobExecutionId(), vertx))
            .compose(orderId -> saveOrderLines(orderId, dataImportEventPayload, tenantConfigFuture.result(), requestContext))
            .compose(v -> adjustEventType(dataImportEventPayload, tenantConfigFuture.result(), okapiParams, requestContext))
            .onComplete(ar -> {
              if (ar.failed()) {
                LOGGER.error("handle:: Error during order or order line creation for jobExecutionId: {}",
                  dataImportEventPayload.getJobExecutionId(), ar.cause());
                clearOrderIdInPoLineEntityIfNecessary(dataImportEventPayload);
                future.completeExceptionally(ar.cause());
                return;
              }
              future.complete(dataImportEventPayload);
            });
        }
      });
    return future;
  }

  private Future<String> checkIfOrderSaved(String orderId, RequestContext requestContext, String jobExecutionId, Vertx vertx) {
    LOGGER.info("checkIfOrderSaved:: orderId: {}, jobExecutionId: {}", orderId, jobExecutionId);
    if (isEmpty(orderId)) {
      String errorMessage = format("checkIfOrderSaved:: orderId is null, jobExecutionId %s", jobExecutionId);
      LOGGER.error(errorMessage);
      return Future.failedFuture(new EventProcessingException(errorMessage));
    }
    Promise<String> finalPromise = Promise.promise();
    vertx.setTimer(1000L, timerId ->
      circuitBreaker.execute(promise -> {
        purchaseOrderHelper.getPurchaseOrderById(orderId, requestContext)
          .onSuccess(purchaseOrder -> {
            LOGGER.info("checkIfOrderSaved:: Order with orderId: {} for jobExecutionId: {} exists.", orderId, jobExecutionId);
            promise.complete(purchaseOrder.getId());
          })
          .onFailure(e -> {
            LOGGER.warn("checkIfOrderSaved:: Retry to get order {} for jobExecutionId: {}", orderId, jobExecutionId, e);
            promise.fail(e);
          });
      }).onComplete(ar -> {
        if (ar.succeeded())
          finalPromise.complete(orderId);
        else {
          LOGGER.error("checkIfOrderSaved:: The error happened getting order {} for jobExecutionId: {}", orderId, jobExecutionId, ar.cause());
          finalPromise.fail(ar.cause());
        }
      }));
    return finalPromise.future();
  }

  private void clearOrderIdInPoLineEntityIfNecessary(DataImportEventPayload dataImportEventPayload) {
    String poLineAsString = dataImportEventPayload.getContext().get(PO_LINE_KEY);
    if (isNotBlank(poLineAsString)) {
      JsonObject poLineAsJson = new JsonObject(poLineAsString);
      poLineAsJson.remove(PO_LINE_ORDER_ID_KEY);
      dataImportEventPayload.getContext().put(PO_LINE_KEY, Json.encode(poLineAsJson));
    }
  }

  private Future<Object> setApprovedFalseIfUserNotHaveApprovalPermission(DataImportEventPayload dataImportEventPayload, JsonObject tenantConfig,
                                                                         RequestContext requestContext) {
    CompositePurchaseOrder order = Json.decodeValue(dataImportEventPayload.getContext().get(ORDER.value()), CompositePurchaseOrder.class);
    Boolean isApproved = order.getApproved();
    boolean isApprovalRequired = isApprovalRequiredConfiguration(tenantConfig);
    boolean isUserNotHaveApprovalPermission = userDoesNotHaveApprovePermission(requestContext);

    if (isApprovalRequired && Boolean.TRUE.equals(isApproved) && isUserNotHaveApprovalPermission) {
      order.setApproved(false);
      dataImportEventPayload.getContext().put(ORDER.value(), Json.encode(order));
    }
    return Future.succeededFuture();
  }

  private Future<Void> adjustEventType(DataImportEventPayload dataImportEventPayload, JsonObject tenantConfig,
                                       OkapiConnectionParams okapiParams,
                                       RequestContext requestContext) {
    LOGGER.debug("adjustEventType:: jobExecutionId: {}", dataImportEventPayload.getJobExecutionId());
    WorkflowStatus workflowStatus = extractWorkflowStatus(dataImportEventPayload);

    Promise<Void> promise = Promise.promise();

    if (workflowStatus.equals(WorkflowStatus.PENDING)) {
      setDiCompletedEvent(dataImportEventPayload);
      return Future.succeededFuture();
    }

    boolean isApprovalRequired = isApprovalRequiredConfiguration(tenantConfig);
    boolean isUserNotHaveApprovalPermission = userDoesNotHaveApprovePermission(requestContext);

    if (workflowStatus.equals(WorkflowStatus.OPEN)) {
      if (isApprovalRequired && isUserNotHaveApprovalPermission) {
        setDiCompletedEvent(dataImportEventPayload);
        return Future.succeededFuture();
      }

      String profileSnapshotId = dataImportEventPayload.getContext().get(JOB_PROFILE_SNAPSHOT_ID_KEY);

      jobProfileSnapshotCache.get(profileSnapshotId, okapiParams)
        .toCompletionStage()
        .thenCompose(snapshotOptional -> snapshotOptional
          .map(profileSnapshot -> setEventTypeForOpenOrder(dataImportEventPayload, profileSnapshot))
          .orElse(CompletableFuture.failedFuture((new EventProcessingException(format(PROFILE_SNAPSHOT_NOT_FOUND_MSG, profileSnapshotId))))))
        .whenComplete((processed, throwable) -> {
          if (throwable != null) {
            promise.fail(throwable);
            LOGGER.error("adjustEventType:: jobExecutionId: {}", dataImportEventPayload.getJobExecutionId(), throwable);
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

  private void setDiCompletedEvent(DataImportEventPayload dataImportEventPayload) {
    LOGGER.debug("setDiCompletedEvent:: set event type DI_COMPLETED for jobExecutionId {}", dataImportEventPayload.getJobExecutionId());
    dataImportEventPayload.getEventsChain().add(dataImportEventPayload.getEventType());
    dataImportEventPayload.setEventType(DI_COMPLETED.value());
    dataImportEventPayload.getContext().put(POST_PROCESSING, "true");
  }

  private CompletableFuture<Void> setEventTypeForOpenOrder(DataImportEventPayload dataImportEventPayload, ProfileSnapshotWrapper jobProfileSnapshotWrapper) {
    List<ProfileSnapshotWrapper> actionProfiles = jobProfileSnapshotWrapper
      .getChildSnapshotWrappers()
      .stream()
      .filter(e -> e.getContentType() == ACTION_PROFILE)
      .collect(Collectors.toList());

    if (!actionProfiles.isEmpty() && checkIfCurrentProfileIsTheLastOne(dataImportEventPayload, actionProfiles)) {
      LOGGER.debug("setEventTypeForOpenOrder:: set event type DI_ORDER_CREATED_READY_FOR_POST_PROCESSING for jobExecutionId {}", dataImportEventPayload.getJobExecutionId());
      dataImportEventPayload.setEventType(DI_ORDER_CREATED_READY_FOR_POST_PROCESSING.value());
      dataImportEventPayload.getContext().put(POST_PROCESSING, "true");
    } else {
      LOGGER.debug("setEventTypeForOpenOrder:: set event type DI_PENDING_ORDER_CREATED for jobExecutionId {}", dataImportEventPayload.getJobExecutionId());
      dataImportEventPayload.setEventType(DI_PENDING_ORDER_CREATED.value());
    }

    return CompletableFuture.completedFuture(null);
  }

  private static boolean checkIfCurrentProfileIsTheLastOne(DataImportEventPayload eventPayload, List<ProfileSnapshotWrapper> actionProfiles) {
    String currentMappingProfileId = eventPayload.getCurrentNode().getProfileId();
    ProfileSnapshotWrapper lastActionProfile = actionProfiles.get(actionProfiles.size() - 1);
    List<ProfileSnapshotWrapper> childSnapshotWrappers = lastActionProfile.getChildSnapshotWrappers();
    String mappingProfileId = org.apache.commons.lang.StringUtils.EMPTY;

    if (childSnapshotWrappers != null && !childSnapshotWrappers.isEmpty()
      && childSnapshotWrappers.get(0) != null && Objects.equals(childSnapshotWrappers.get(0).getContentType(), MAPPING_PROFILE)) {
      mappingProfileId = childSnapshotWrappers.get(0).getProfileId();
    }

    return mappingProfileId.equals(currentMappingProfileId);
  }

  private Future<String> generateSequentialOrderId(DataImportEventPayload dataImportEventPayload, JsonObject tenantConfig, String orderId) {
    LOGGER.info("generateSequentialOrderId:: jobExecutionId: {}, newOrderId: {}, poLinesLimit: {} ",
      dataImportEventPayload.getJobExecutionId(), orderId, tenantConfig.getString(PO_LINES_LIMIT_PROPERTY));

    Integer poLinesLimit = Integer.valueOf(Optional.ofNullable(tenantConfig.getString(PO_LINES_LIMIT_PROPERTY)).orElse(DEFAULT_POLINE_LIMIT));
    Record parsedMarcBibRecord = new JsonObject(dataImportEventPayload.getContext().get(MARC_BIBLIOGRAPHIC.value())).mapTo(Record.class);
    if (parsedMarcBibRecord.getOrder() == null) {
      return Future.failedFuture(new IllegalArgumentException(
        String.format("generateSequentialOrderId:: Order parameter is missing. jobExecutionId: %s", dataImportEventPayload.getJobExecutionId())));
    }

    int sequentialNo = parsedMarcBibRecord.getOrder() / poLinesLimit;
    return sequentialOrderIdService.store(dataImportEventPayload.getJobExecutionId(), sequentialNo, orderId, dataImportEventPayload.getTenant());
  }

  private Future<CompositePurchaseOrder> saveOrder(DataImportEventPayload dataImportEventPayload, String orderId,
                                                   JsonObject tenantConfig, RequestContext requestContext) {
    LOGGER.info("saveOrder:: jobExecutionId: {}, orderId: {} ", dataImportEventPayload.getJobExecutionId(), orderId);
    CompositePurchaseOrder orderToSave = Json.decodeValue(dataImportEventPayload.getContext().get(ORDER.value()), CompositePurchaseOrder.class);
    orderToSave.setId(orderId);
    // in this handler a purchase order always is created in PENDING status despite the status that is set during mapping
    orderToSave.setWorkflowStatus(WorkflowStatus.PENDING);
    if (orderToSave.getVendor() == null) {
      LOGGER.warn("saveOrder:: jobExecutionId: '{}', recordId: '{}', orderId: '{}' vendor for the order has not been mapped",
        dataImportEventPayload.getJobExecutionId(), dataImportEventPayload.getContext().get(RECORD_ID_HEADER), orderId);
    }

    return orderValidationService.validateOrderForPost(orderToSave, tenantConfig, requestContext)
      .compose(errors -> {
        if (CollectionUtils.isNotEmpty(errors)) {
          return Future.failedFuture(new EventProcessingException(Json.encode(errors)));
        }
        return purchaseOrderHelper.createPurchaseOrder(orderToSave, tenantConfig, requestContext)
          .compose(order -> savePoLinesAmountPerOrder(orderId, dataImportEventPayload, tenantConfig).map(order))
          .onComplete(v -> dataImportEventPayload.getContext().put(ORDER.value(), Json.encode(orderToSave)))
          .recover(e -> {
            if (e instanceof HttpException httpException) {
              String message = httpException.getError().getMessage();
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

  private Future<Void> savePoLinesAmountPerOrder(String orderId, DataImportEventPayload eventPayload, JsonObject tenantConfig) {
    Map<String, String> headers = DataImportUtils.extractOkapiHeaders(eventPayload);
    OkapiConnectionParams params = new OkapiConnectionParams(headers, vertx);

    return jobExecutionTotalRecordsCache.get(eventPayload.getJobExecutionId(), params)
      .map(totalRecords -> determinePoLinesAmountPerOrder(eventPayload, totalRecords, tenantConfig))
      .compose(orderPolAmount -> poLineImportProgressService.savePoLinesAmountPerOrder(orderId, orderPolAmount, eventPayload.getTenant()));
  }

  private int determinePoLinesAmountPerOrder(DataImportEventPayload dataImportEventPayload, int totalRecordsAmount, JsonObject tenantConfig) {
    int poLinesLimit = Integer.parseInt(tenantConfig.getString(PO_LINES_LIMIT_PROPERTY, DEFAULT_PO_LINES_LIMIT));
    if (totalRecordsAmount % poLinesLimit == 0) {
      return poLinesLimit;
    }

    Record sourceRecord = Json.decodeValue(dataImportEventPayload.getContext().get(MARC_BIBLIOGRAPHIC.value()), Record.class);
    int fullOrdersAmount = totalRecordsAmount / poLinesLimit;
    int sequenceOrderNumber = sourceRecord.getOrder() / poLinesLimit + 1;
    return sequenceOrderNumber <= fullOrdersAmount ? poLinesLimit : totalRecordsAmount % poLinesLimit;
  }

  private Future<PoLine> saveOrderLines(String orderId, DataImportEventPayload dataImportEventPayload,
                                                 JsonObject tenantConfig, RequestContext requestContext) {
    LOGGER.info("saveOrderLines:: jobExecutionId: {}, orderId: {} ", dataImportEventPayload.getJobExecutionId(), orderId);
    PoLine poLine = Json.decodeValue(dataImportEventPayload.getContext().get(PO_LINE_KEY), PoLine.class);
    poLine.setPurchaseOrderId(orderId);
    poLine.setSource(PoLine.Source.MARC);

    if (dataImportEventPayload.getContext().containsKey(EntityType.INSTANCE.value())) {
      JsonObject instanceJson = new JsonObject(dataImportEventPayload.getContext().get(EntityType.INSTANCE.value()));
      poLine.setInstanceId(instanceJson.getString(INSTANCE_ID_FIELD));
    }

    return poLineHelper.createPoLine(poLine, tenantConfig, requestContext)
      .recover(e -> poLineImportProgressService.trackProcessedPoLine(orderId, dataImportEventPayload.getTenant())
        .onFailure(trackingError -> LOGGER.warn("saveOrderLines:: Failed to track processed PO line by orderId: {}, jobExecutionId: {}", orderId, dataImportEventPayload.getJobExecutionId(), trackingError))
        .transform(v -> Future.failedFuture(e)))
      .onComplete(ar -> dataImportEventPayload.getContext().put(PO_LINE_KEY, Json.encode(poLine)));
  }

  /**
   * Extracts po lines limit from mapping profile which is contained in the specified {@code dataImportEventPayload}
   * and marks poLine limit mapping rule as not enabled, if one exists,
   * to prevent the mapping rule from being used for po line mapping.
   *
   * @param dataImportEventPayload containing a mapping profile
   * @return Optional of po lines limit
   */
  private Optional<Integer> extractPoLinesLimit(DataImportEventPayload dataImportEventPayload) {
    ProfileSnapshotWrapper mappingProfileWrapper = dataImportEventPayload.getCurrentNode().getChildSnapshotWrappers().get(0);
    MappingProfile mappingProfile = ObjectMapperTool.getMapper().convertValue(mappingProfileWrapper.getContent(), MappingProfile.class);

    Optional<Integer> limitOptional = mappingProfile.getMappingDetails().getMappingFields().stream()
      .filter(mappingRule -> POL_LIMIT_RULE_NAME.equals(mappingRule.getName()) && isNotBlank(mappingRule.getValue()))
      .map(mappingRule -> mappingRule.withEnabled(FALSE.toString()))
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

  private Future<Void> prepareMappingResult(DataImportEventPayload dataImportEventPayload, MappingParameters mappingParameters) {
    JsonObject mappingResult = new JsonObject(dataImportEventPayload.getContext().get(ORDER.value()));
    JsonObject orderJson = mappingResult.getJsonObject(MAPPING_RESULT_FIELD).getJsonObject(ORDER_FIELD);
    JsonObject poLineJson = mappingResult.getJsonObject(MAPPING_RESULT_FIELD).getJsonObject(PO_LINES_FIELD);
    calculateActivationDue(poLineJson);
    ensureFundCode(poLineJson, mappingParameters);
    dataImportEventPayload.getContext().put(ORDER.value(), orderJson.encode());

    if (WorkflowStatus.OPEN.value().equals(orderJson.getString(ORDER_STATUS_FIELD))) {
      return overrideCreateInventoryField(poLineJson, dataImportEventPayload)
        .onComplete(v -> dataImportEventPayload.getContext().put(PO_LINE_KEY, poLineJson.encode()));
    }

    dataImportEventPayload.getContext().put(PO_LINE_KEY, poLineJson.encode());
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

  private void ensureFundCode(JsonObject poLineJson, MappingParameters mappingParameters) {
    if (!IterableUtils.isEmpty(poLineJson.getJsonArray(POL_FUND_DISTRIBUTION_FIELD))) {
      poLineJson.getJsonArray(POL_FUND_DISTRIBUTION_FIELD).stream()
        .map(JsonObject.class::cast)
        .filter(fundDistributionJson -> isNotEmpty(fundDistributionJson.getString(POL_FUND_ID_FIELD)))
        .forEach(fundDistribution -> fundDistribution.put(POL_FUND_CODE_FIELD, determineFundCode(fundDistribution, mappingParameters)));
    }
  }

  private String determineFundCode(JsonObject fundDistribution, MappingParameters mappingParameters) {
    String fundId = fundDistribution.getString(POL_FUND_ID_FIELD);
    Optional<Fund> fund = mappingParameters.getFunds().stream().filter(f -> fundId.equals(f.getId())).findFirst();
    return fund.map(Fund::getCode).orElse(null);
  }

  private Future<Void> overrideCreateInventoryField(JsonObject poLineJson, DataImportEventPayload dataImportEventPayload) {
    String profileSnapshotId = dataImportEventPayload.getContext().get(JOB_PROFILE_SNAPSHOT_ID_KEY);
    Map<String, String> headers = DataImportUtils.extractOkapiHeaders(dataImportEventPayload);
    OkapiConnectionParams okapiParams = new OkapiConnectionParams(headers, vertx);

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
      createInventoryFieldValue = NONE;
    }

    if (PHYSICAL_RESOURCE.value().equals(poLineJson.getString(ORDER_FORMAT_FIELD))
      || OTHER.value().equals(poLineJson.getString(ORDER_FORMAT_FIELD))) {
      setCreateInventoryValue(poLineJson, POL_PHYSICAL_FIELD, createInventoryFieldValue.value());
    } else if (ELECTRONIC_RESOURCE.value().equals(poLineJson.getString(ORDER_FORMAT_FIELD))) {
      setCreateInventoryValue(poLineJson, POL_ERESOURCE_FIELD, createInventoryFieldValue.value());
    } else if (P_E_MIX.value().equals(poLineJson.getString(ORDER_FORMAT_FIELD))) {
      setCreateInventoryValue(poLineJson, POL_PHYSICAL_FIELD, createInventoryFieldValue.value());
      setCreateInventoryValue(poLineJson, POL_ERESOURCE_FIELD, createInventoryFieldValue.value());
    }
    return null;
  }

  private void setCreateInventoryValue(JsonObject poLineJson, String resourceDetailsFieldName, String createInventoryFieldValue) {
    if (poLineJson.getJsonObject(resourceDetailsFieldName) == null) {
      poLineJson.put(resourceDetailsFieldName, new JsonObject());
    }
    poLineJson.getJsonObject(resourceDetailsFieldName).put(POL_CREATE_INVENTORY_FIELD, createInventoryFieldValue);
  }


  private void overridePoLinesLimit(JsonObject tenantConfig, Optional<Integer> poLinesLimitOptional) {
    poLinesLimitOptional.ifPresent(poLinesLimit -> tenantConfig.put(PO_LINES_LIMIT_PROPERTY, poLinesLimit));
  }

  private OkapiConnectionParams getOkapiConnectionParams(Map<String, String> okapiHeaders, Vertx vertx) {
    Map<String, String> okapiHeadersLowerCaseKeys = okapiHeaders.entrySet().stream().collect(Collectors.toMap(
      key -> key.getKey().toLowerCase(Locale.ROOT),
      Map.Entry::getValue
    ));
    return new OkapiConnectionParams(okapiHeadersLowerCaseKeys, vertx);
  }

  @Override
  public boolean isEligible(DataImportEventPayload dataImportEventPayload) {
    if (dataImportEventPayload.getCurrentNode() != null && ACTION_PROFILE == dataImportEventPayload.getCurrentNode().getContentType()) {
      ActionProfile actionProfile = ObjectMapperTool.getMapper()
        .convertValue(dataImportEventPayload.getCurrentNode().getContent(), ActionProfile.class);
      return actionProfile.getAction() == CREATE && actionProfile.getFolioRecord() == ORDER;
    }
    return false;
  }
}
