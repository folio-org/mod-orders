package org.folio.service.dataimport.handlers;

import io.vertx.core.CompositeFuture;
import io.vertx.core.Future;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.Json;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.unit.Async;
import io.vertx.ext.unit.TestContext;
import org.apache.kafka.clients.producer.ProducerRecord;
import org.folio.ActionProfile;
import org.folio.DataImportEventPayload;
import org.folio.JobProfile;
import org.folio.MappingProfile;
import org.folio.TestConfig;
import org.folio.di.DiAbstractRestTest;
import org.folio.rest.RestConstants;
import org.folio.rest.impl.MockServer;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Event;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PieceCollection;
import org.folio.rest.jaxrs.model.ProfileSnapshotWrapper;
import org.folio.rest.jaxrs.model.Title;
import org.folio.service.dataimport.PoLineImportProgressService;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import static org.folio.ActionProfile.Action.CREATE;
import static org.folio.DataImportEventTypes.DI_COMPLETED;
import static org.folio.DataImportEventTypes.DI_ERROR;
import static org.folio.DataImportEventTypes.DI_ORDER_CREATED;
import static org.folio.DataImportEventTypes.DI_ORDER_CREATED_READY_FOR_POST_PROCESSING;
import static org.folio.TestConfig.closeMockServer;
import static org.folio.helper.FinanceInteractionsTestHelper.verifyEncumbrancesOnPoUpdate;
import static org.folio.orders.utils.ResourcePathResolver.PIECES_STORAGE_BATCH;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.TITLES;
import static org.folio.rest.RestVerticle.OKAPI_USERID_HEADER;
import static org.folio.rest.impl.MockServer.ITEM_RECORDS;
import static org.folio.rest.impl.MockServer.addMockEntry;
import static org.folio.rest.impl.MockServer.getCreatedHoldings;
import static org.folio.rest.impl.MockServer.getCreatedInstances;
import static org.folio.rest.impl.MockServer.getCreatedItems;
import static org.folio.rest.impl.MockServer.getCreatedPiecesBatch;
import static org.folio.rest.impl.MockServer.getPurchaseOrderUpdates;
import static org.folio.rest.impl.TitlesApiTest.SAMPLE_TITLE_ID;
import static org.folio.rest.jaxrs.model.EntityType.HOLDINGS;
import static org.folio.rest.jaxrs.model.EntityType.INSTANCE;
import static org.folio.rest.jaxrs.model.EntityType.ITEM;
import static org.folio.rest.jaxrs.model.EntityType.MARC_BIBLIOGRAPHIC;
import static org.folio.rest.jaxrs.model.EntityType.ORDER;
import static org.folio.rest.jaxrs.model.Piece.Format.OTHER;
import static org.folio.rest.jaxrs.model.ProfileType.ACTION_PROFILE;
import static org.folio.rest.jaxrs.model.ProfileType.JOB_PROFILE;
import static org.folio.rest.jaxrs.model.ProfileType.MAPPING_PROFILE;
import static org.folio.service.dataimport.handlers.CreateOrderEventHandler.OKAPI_PERMISSIONS_HEADER;
import static org.folio.service.inventory.InventoryItemManager.ID;
import static org.folio.service.inventory.InventoryItemManager.ITEM_HOLDINGS_RECORD_ID;
import static org.folio.service.inventory.InventoryItemManager.ITEM_MATERIAL_TYPE_ID;
import static org.folio.service.inventory.InventoryItemManager.ITEM_PURCHASE_ORDER_LINE_IDENTIFIER;
import static org.folio.service.inventory.InventoryManagerTest.OLD_LOCATION_ID;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class OrderPostProcessingEventHandlerTest extends DiAbstractRestTest {

  private static final String JOB_PROFILE_SNAPSHOT_ID_KEY = "JOB_PROFILE_SNAPSHOT_ID";
  private static final String PO_LINE_KEY = "PO_LINE";
  private static final String JOB_PROFILE_SNAPSHOTS_MOCK = "jobProfileSnapshots";
  private static final String OKAPI_URL = "http://localhost:" + TestConfig.mockPort;
  private static final String ID_FIELD = "id";
  private static final String ELECTRONIC_RESOURCE_MATERIAL_TYPE_ID = "615b8413-82d5-4203-aa6e-e37984cb5ac3";
  private static final String HOLDINGS_ID = "65cb2bf0-d4c2-4886-8ad0-b76f1ba75d63";
  private static final String ITEM_ID = "86481a22-633e-4b97-8061-0dc5fdaaeabb";
  private static final String INSTANCE_ID = "5294d737-a04b-4158-857a-3f3c555bcc60";

  private final JobProfile jobProfile = new JobProfile()
    .withId(UUID.randomUUID().toString())
    .withName("Create order")
    .withDataType(JobProfile.DataType.MARC);

  private final ActionProfile actionProfile = new ActionProfile()
    .withId(UUID.randomUUID().toString())
    .withAction(CREATE)
    .withFolioRecord(ActionProfile.FolioRecord.ORDER);

  private final MappingProfile mappingProfile = new MappingProfile()
    .withId(UUID.randomUUID().toString())
    .withIncomingRecordType(MARC_BIBLIOGRAPHIC)
    .withExistingRecordType(ORDER);

  private CompositePurchaseOrder order;
  private PoLine poLine;

  @BeforeClass
  public static void setupClass() throws ExecutionException, InterruptedException, TimeoutException {
    TestConfig.startMockServer();
  }

  @AfterClass
  public static void tearDownClass() {
    closeMockServer();
  }

  @Before
  public void setup() {
    MockServer.release();
    order = new CompositePurchaseOrder()
      .withId(UUID.randomUUID().toString())
      .withPoNumber("10000");

    poLine = new PoLine()
      .withId(UUID.randomUUID().toString())
      .withTitleOrPackage("poLine for data-import")
      .withPurchaseOrderId(order.getId())
      .withPoLineNumber("10000-1")
      .withSource(PoLine.Source.MARC)
      .withOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE)
      .withEresource(new Eresource()
        .withMaterialType(ELECTRONIC_RESOURCE_MATERIAL_TYPE_ID)
        .withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING_ITEM))
      .withLocations(List.of(new Location()
        .withLocationId(OLD_LOCATION_ID)
        .withQuantityElectronic(1)))
      .withCost(new Cost().withCurrency("USD").withQuantityElectronic(1))
      .withFundDistribution(List.of(new FundDistribution()
        .withFundId("fb7b70f1-b898-4924-a991-0e4b6312bb5f")
        .withValue(100d)));
  }

  @Test
  public void shouldOpenOrderAndUseExistingInstanceHoldingsItemWhenAllPoLinesProcessed(TestContext context) throws InterruptedException {
    // given
    JsonObject itemJson = new JsonObject()
      .put(ID, ITEM_ID)
      .put(ITEM_HOLDINGS_RECORD_ID, HOLDINGS_ID)
      .put(ITEM_PURCHASE_ORDER_LINE_IDENTIFIER, poLine.getId())
      .put(ITEM_MATERIAL_TYPE_ID, poLine.getEresource().getMaterialType());

    JsonObject instanceJson = new JsonObject().put(ID_FIELD, INSTANCE_ID);
    PoLine mockPoLine = JsonObject.mapFrom(poLine).mapTo(PoLine.class);
    mockPoLine.setInstanceId(instanceJson.getString(ID_FIELD));

    ProfileSnapshotWrapper profileSnapshotWrapper = buildProfileSnapshotWrapper(jobProfile, actionProfile, mappingProfile);
    addMockEntry(JOB_PROFILE_SNAPSHOTS_MOCK, profileSnapshotWrapper);
    addMockEntry(PURCHASE_ORDER_STORAGE, order);
    addMockEntry(PO_LINES_STORAGE, mockPoLine);
    addMockEntry(ITEM_RECORDS, itemJson);
    createPieceAndTitle(mockPoLine);

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withCurrentNode(profileSnapshotWrapper.getChildSnapshotWrappers().get(0).getChildSnapshotWrappers().get(0))
      .withEventType(DI_ORDER_CREATED_READY_FOR_POST_PROCESSING.value())
      .withTenant(TENANT_ID)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN)
      .withContext(new HashMap<>() {{
        put(JOB_PROFILE_SNAPSHOT_ID_KEY, profileSnapshotWrapper.getId());
        put(INSTANCE.value(), instanceJson.encodePrettily());
        put(ORDER.value(), Json.encodePrettily(order));
        put(PO_LINE_KEY, Json.encodePrettily(poLine));
      }});

    CompletableFuture<Object> future = new CompletableFuture<>();
    PoLineImportProgressService polProgressService = getBeanFromSpringContext(vertx, PoLineImportProgressService.class);
    polProgressService.savePoLinesAmountPerOrder(order.getId(), 2, TENANT_ID)
      .compose(v -> polProgressService.trackProcessedPoLine(order.getId(), TENANT_ID))
      .onComplete(context.asyncAssertSuccess(v -> future.complete(null)));

    ProducerRecord<String, String> request = prepareKafkaRequest(dataImportEventPayload);
    future.join();

    // when
    send(request);

    // then
    DataImportEventPayload eventPayload = observeEvent(DI_COMPLETED.value());
    assertEquals(DI_ORDER_CREATED.value(), eventPayload.getEventsChain().get(eventPayload.getEventsChain().size() - 1));
    verifyPoLine(eventPayload);

    // verifies that request was performed to update po line with instance id
    List<JsonObject> updatedPoLines = MockServer.getRqRsEntries(HttpMethod.PUT, PO_LINES_STORAGE);
    PoLine updatedPoLine = updatedPoLines.get(0).mapTo(PoLine.class);
    assertEquals(instanceJson.getString(ID_FIELD), updatedPoLine.getInstanceId());

    assertNull(getCreatedInstances());
    assertNull(getCreatedHoldings());
    assertNull(getCreatedItems());

    List<JsonObject> createdPieces = getCreatedPiecesBatch();
    assertEquals(1, createdPieces.size());
    Piece piece = createdPieces.get(0).mapTo(PieceCollection.class).getPieces().get(0);
    assertEquals(poLine.getId(), piece.getPoLineId());
    assertEquals(itemJson.getString(ID), piece.getItemId());

    List<JsonObject> ordersResp = getPurchaseOrderUpdates();
    assertFalse(ordersResp.isEmpty());
    CompositePurchaseOrder openedOrder = ordersResp.get(0).mapTo(CompositePurchaseOrder.class);
    assertEquals(order.getId(), openedOrder.getId());
    assertEquals(WorkflowStatus.OPEN, openedOrder.getWorkflowStatus());

    verifyEncumbrancesOnPoUpdate(order.withPoLines(List.of(poLine)));
  }

  @Test
  public void shouldNotUpdateOrderStatusToOpenWhenNotAllPoLinesProcessed(TestContext context) throws InterruptedException {
    // given
    JsonObject instanceJson = new JsonObject().put(ID_FIELD, INSTANCE_ID);

    ProfileSnapshotWrapper profileSnapshotWrapper = buildProfileSnapshotWrapper(jobProfile, actionProfile, mappingProfile);
    addMockEntry(JOB_PROFILE_SNAPSHOTS_MOCK, profileSnapshotWrapper);

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withCurrentNode(profileSnapshotWrapper.getChildSnapshotWrappers().get(0).getChildSnapshotWrappers().get(0))
      .withEventType(DI_ORDER_CREATED_READY_FOR_POST_PROCESSING.value())
      .withTenant(TENANT_ID)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN)
      .withContext(new HashMap<>() {{
        put(JOB_PROFILE_SNAPSHOT_ID_KEY, profileSnapshotWrapper.getId());
        put(INSTANCE.value(), instanceJson.encodePrettily());
        put(PO_LINE_KEY, Json.encodePrettily(poLine));
      }});

    CompletableFuture<Void> future = new CompletableFuture<>();
    PoLineImportProgressService polProgressService = getBeanFromSpringContext(vertx, PoLineImportProgressService.class);
    polProgressService.savePoLinesAmountPerOrder(order.getId(), 3, TENANT_ID)
      .compose(v -> polProgressService.trackProcessedPoLine(order.getId(), TENANT_ID))
      .onComplete(context.asyncAssertSuccess(v -> future.complete(null)));

    ProducerRecord<String, String> request = prepareKafkaRequest(dataImportEventPayload);
    future.join();

    // when
    send(request);

    // then
    DataImportEventPayload eventPayload = observeEvent(DI_COMPLETED.value());
    assertEquals(DI_ORDER_CREATED.value(), eventPayload.getEventsChain().get(eventPayload.getEventsChain().size() - 1));
    verifyPoLine(eventPayload);
    assertNull(getPurchaseOrderUpdates());
  }

  @Test
  public void shouldOpenOrderWhenAllPoLinesProcessedAndInventoryCreationIsNotRequired(TestContext context) throws InterruptedException {
    // given
    poLine.setInstanceId(null);
    poLine.getEresource().setCreateInventory(Eresource.CreateInventory.NONE);
    PoLine mockPoLine = JsonObject.mapFrom(poLine).mapTo(PoLine.class);

    ProfileSnapshotWrapper profileSnapshotWrapper = buildProfileSnapshotWrapper(jobProfile, actionProfile, mappingProfile);
    addMockEntry(JOB_PROFILE_SNAPSHOTS_MOCK, profileSnapshotWrapper);
    addMockEntry(PURCHASE_ORDER_STORAGE, order);
    addMockEntry(PO_LINES_STORAGE, mockPoLine);
    createPieceAndTitle(poLine);

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withCurrentNode(profileSnapshotWrapper.getChildSnapshotWrappers().get(0).getChildSnapshotWrappers().get(0))
      .withEventType(DI_ORDER_CREATED_READY_FOR_POST_PROCESSING.value())
      .withTenant(TENANT_ID)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN)
      .withContext(new HashMap<>() {{
        put(JOB_PROFILE_SNAPSHOT_ID_KEY, profileSnapshotWrapper.getId());
        put(ORDER.value(), Json.encodePrettily(order));
        put(PO_LINE_KEY, Json.encodePrettily(poLine));
      }});

    CompletableFuture<Object> future = new CompletableFuture<>();
    PoLineImportProgressService polProgressService = getBeanFromSpringContext(vertx, PoLineImportProgressService.class);
    polProgressService.savePoLinesAmountPerOrder(order.getId(), 1, TENANT_ID)
      .compose(v -> polProgressService.trackProcessedPoLine(order.getId(), TENANT_ID))
      .onComplete(context.asyncAssertSuccess(v -> future.complete(null)));

    ProducerRecord<String, String> request = prepareKafkaRequest(dataImportEventPayload);
    future.join();

    // when
    send(request);

    // then
    DataImportEventPayload eventPayload = observeEvent(DI_COMPLETED.value());
    assertEquals(DI_ORDER_CREATED.value(), eventPayload.getEventsChain().get(eventPayload.getEventsChain().size() - 1));
    verifyPoLine(eventPayload);

    List<JsonObject> updatedPoLines = MockServer.getRqRsEntries(HttpMethod.PUT, PO_LINES_STORAGE);
    PoLine updatedPoLine = updatedPoLines.get(0).mapTo(PoLine.class);
    assertNull(updatedPoLine.getInstanceId());

    assertNull(getCreatedInstances());
    assertNull(getCreatedHoldings());
    assertNull(getCreatedItems());

    List<JsonObject> ordersResp = getPurchaseOrderUpdates();
    assertFalse(ordersResp.isEmpty());
    CompositePurchaseOrder openedOrder = ordersResp.get(0).mapTo(CompositePurchaseOrder.class);
    assertEquals(order.getId(), openedOrder.getId());
    assertEquals(WorkflowStatus.OPEN, openedOrder.getWorkflowStatus());

    verifyEncumbrancesOnPoUpdate(order.withPoLines(List.of(poLine)));
  }

  @Test
  public void shouldOpenOrderOnceWhenHandlingMultipleEventsAndAllPoLinesProcessed(TestContext context) {
    // given
    Async async = context.async();
    poLine.setInstanceId(null);
    poLine.getEresource().setCreateInventory(Eresource.CreateInventory.NONE);
    PoLine mockPoLine = JsonObject.mapFrom(poLine).mapTo(PoLine.class);

    ProfileSnapshotWrapper profileSnapshotWrapper = buildProfileSnapshotWrapper(jobProfile, actionProfile, mappingProfile);
    addMockEntry(JOB_PROFILE_SNAPSHOTS_MOCK, profileSnapshotWrapper);
    addMockEntry(PURCHASE_ORDER_STORAGE, order);
    addMockEntry(PO_LINES_STORAGE, mockPoLine);
    createPieceAndTitle(mockPoLine);

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withCurrentNode(profileSnapshotWrapper.getChildSnapshotWrappers().get(0).getChildSnapshotWrappers().get(0))
      .withEventType(DI_ORDER_CREATED_READY_FOR_POST_PROCESSING.value())
      .withTenant(TENANT_ID)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN)
      .withContext(new HashMap<>() {{
        put(JOB_PROFILE_SNAPSHOT_ID_KEY, profileSnapshotWrapper.getId());
        put(ORDER.value(), Json.encodePrettily(order));
        put(PO_LINE_KEY, Json.encodePrettily(poLine));
      }});

    PoLineImportProgressService polProgressService = getBeanFromSpringContext(vertx, PoLineImportProgressService.class);
    OrderPostProcessingEventHandler orderPostProcessingHandler = getBeanFromSpringContext(vertx, OrderPostProcessingEventHandler.class);

    // when
    Future<CompositeFuture> future = polProgressService.savePoLinesAmountPerOrder(order.getId(), 2, TENANT_ID)
      .compose(v -> CompositeFuture.join(
        Future.fromCompletionStage(orderPostProcessingHandler.handle(dataImportEventPayload)),
        Future.fromCompletionStage(orderPostProcessingHandler.handle(dataImportEventPayload))
      ));

    // then
    future.onComplete(ar -> {
      context.assertTrue(ar.succeeded());
      List<JsonObject> ordersResp = getPurchaseOrderUpdates();

      // verifies that the order opening process was triggered only one time
      // by checking that request to update the order with status "Open" was performed once
      context.assertEquals(1, ordersResp.size());
      CompositePurchaseOrder openedOrder = ordersResp.get(0).mapTo(CompositePurchaseOrder.class);
      context.assertEquals(order.getId(), openedOrder.getId());
      context.assertEquals(WorkflowStatus.OPEN, openedOrder.getWorkflowStatus());
      async.complete();
    });
  }

  @Test
  public void shouldPublishDiErrorEventWhenHasNoPoLine() throws InterruptedException {
    // given
    ProfileSnapshotWrapper profileSnapshotWrapper = buildProfileSnapshotWrapper(jobProfile, actionProfile, mappingProfile);
    addMockEntry(JOB_PROFILE_SNAPSHOTS_MOCK, profileSnapshotWrapper);

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withCurrentNode(profileSnapshotWrapper.getChildSnapshotWrappers().get(0).getChildSnapshotWrappers().get(0))
      .withEventType(DI_ORDER_CREATED_READY_FOR_POST_PROCESSING.value())
      .withTenant(TENANT_ID)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN)
      .withContext(new HashMap<>() {{
        put(JOB_PROFILE_SNAPSHOT_ID_KEY, profileSnapshotWrapper.getId());
      }});

    assertNull(dataImportEventPayload.getContext().get(PO_LINE_KEY));
    ProducerRecord<String, String> request = prepareKafkaRequest(dataImportEventPayload);

    // when
    send(request);

    // then
    DataImportEventPayload eventPayload = observeEvent(DI_ERROR.value());
    assertEquals(DI_ORDER_CREATED.value(), eventPayload.getEventsChain().get(eventPayload.getEventsChain().size() - 1));
  }

  @Test
  public void shouldPublishDiErrorEventWhenHasNoInstanceAndInventoryRecordsCreationIsRequired() throws InterruptedException {
    // given
    ProfileSnapshotWrapper profileSnapshotWrapper = buildProfileSnapshotWrapper(jobProfile, actionProfile, mappingProfile);
    addMockEntry(JOB_PROFILE_SNAPSHOTS_MOCK, profileSnapshotWrapper);

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withCurrentNode(profileSnapshotWrapper.getChildSnapshotWrappers().get(0).getChildSnapshotWrappers().get(0))
      .withEventType(DI_ORDER_CREATED_READY_FOR_POST_PROCESSING.value())
      .withTenant(TENANT_ID)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN)
      .withContext(new HashMap<>() {{
        put(JOB_PROFILE_SNAPSHOT_ID_KEY, profileSnapshotWrapper.getId());
        put(PO_LINE_KEY, Json.encodePrettily(poLine));
      }});

    assertNotEquals(Eresource.CreateInventory.NONE, poLine.getEresource().getCreateInventory());
    ProducerRecord<String, String> request = prepareKafkaRequest(dataImportEventPayload);

    // when
    send(request);

    // then
    DataImportEventPayload eventPayload = observeEvent(DI_ERROR.value());
    assertEquals(DI_ORDER_CREATED.value(), eventPayload.getEventsChain().get(eventPayload.getEventsChain().size() - 1));
  }

  @Test
  public void shouldReturnTrueWhenCurrentNodeIsMappingProfileWithExistingRecordTypeOrder() {
    shouldReturnTrueWhenHandlerIsEligibleForPayload(mappingProfile);
  }

  @Test
  public void shouldReturnTrueWhenCurrentNodeIsMappingProfileWithExistingRecordTypeInstance() {
    shouldReturnTrueWhenHandlerIsEligibleForPayload(new MappingProfile().withExistingRecordType(INSTANCE));
  }

  @Test
  public void shouldReturnTrueWhenCurrentNodeIsMappingProfileWithExistingRecordTypeHoldings() {
    shouldReturnTrueWhenHandlerIsEligibleForPayload(new MappingProfile().withExistingRecordType(HOLDINGS));
  }

  @Test
  public void shouldReturnTrueWhenCurrentNodeIsMappingProfileWithExistingRecordTypeItem() {
    shouldReturnTrueWhenHandlerIsEligibleForPayload(new MappingProfile().withExistingRecordType(ITEM));
  }

  public void shouldReturnTrueWhenHandlerIsEligibleForPayload(MappingProfile mappingProfile) {
    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withCurrentNode(new ProfileSnapshotWrapper()
        .withContentType(MAPPING_PROFILE)
        .withContent(JsonObject.mapFrom(mappingProfile).getMap()));

    OrderPostProcessingEventHandler createOrderHandler = getBeanFromSpringContext(vertx, OrderPostProcessingEventHandler.class);
    assertTrue(createOrderHandler.isEligible(dataImportEventPayload));
  }

  @Test
  public void shouldReturnFalseWhenHandlerIsNotEligibleForCurrentNode() {
    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withCurrentNode(new ProfileSnapshotWrapper()
        .withContentType(ACTION_PROFILE)
        .withContent(JsonObject.mapFrom(actionProfile).getMap()));

    OrderPostProcessingEventHandler createOrderHandler = getBeanFromSpringContext(vertx, OrderPostProcessingEventHandler.class);
    assertFalse(createOrderHandler.isEligible(dataImportEventPayload));
  }

  private ProfileSnapshotWrapper buildProfileSnapshotWrapper(JobProfile jobProfile, ActionProfile actionProfile,
                                                             MappingProfile mappingProfile) {
    return new ProfileSnapshotWrapper()
      .withId(UUID.randomUUID().toString())
      .withProfileId(jobProfile.getId())
      .withContentType(JOB_PROFILE)
      .withContent(JsonObject.mapFrom(jobProfile).getMap())
      .withChildSnapshotWrappers(Collections.singletonList(
        new ProfileSnapshotWrapper()
          .withId(UUID.randomUUID().toString())
          .withProfileId(actionProfile.getId())
          .withContentType(ACTION_PROFILE)
          .withContent(JsonObject.mapFrom(actionProfile).getMap())
          .withChildSnapshotWrappers(Collections.singletonList(
            new ProfileSnapshotWrapper()
              .withId(UUID.randomUUID().toString())
              .withProfileId(mappingProfile.getId())
              .withContentType(MAPPING_PROFILE)
              .withContent(JsonObject.mapFrom(mappingProfile).getMap())))));
  }

  private ProducerRecord<String, String> prepareKafkaRequest(DataImportEventPayload payload) {
    String topic = formatToKafkaTopicName(payload.getEventType());
    Event event = new Event().withEventPayload(Json.encode(payload));
    ProducerRecord<String, String> producerRecord = new ProducerRecord<>(topic, "test-key", Json.encode(event));
    addHeader(producerRecord, RestConstants.OKAPI_URL, payload.getOkapiUrl());
    addHeader(producerRecord, OKAPI_PERMISSIONS_HEADER, payload.getContext().getOrDefault(OKAPI_PERMISSIONS_HEADER, ""));
    addHeader(producerRecord, OKAPI_USERID_HEADER, payload.getContext().getOrDefault(OKAPI_USERID_HEADER, ""));
    return producerRecord;
  }

  private DataImportEventPayload observeEvent(String eventType) throws InterruptedException {
    String topicToObserve = formatToKafkaTopicName(eventType);
    var value = observeTopic(topicToObserve);
    Event obtainedEvent = Json.decodeValue(value, Event.class);
    return Json.decodeValue(obtainedEvent.getEventPayload(), DataImportEventPayload.class);
  }

  private void verifyPoLine(DataImportEventPayload eventPayload) {
    assertNotNull(eventPayload.getContext().get(PO_LINE_KEY));
    PoLine poLine = Json.decodeValue(eventPayload.getContext().get(PO_LINE_KEY), PoLine.class);
    assertNotNull(poLine.getId());
    assertNotNull(poLine.getTitleOrPackage());
    assertNotNull(poLine.getPurchaseOrderId());
  }

  private void createPieceAndTitle(PoLine poLine) {
    Title title = new Title().withId(SAMPLE_TITLE_ID)
      .withTitle(poLine.getTitleOrPackage()).withPoLineId(poLine.getId())
      .withPoLineNumber(poLine.getPoLineNumber())
      .withInstanceId(poLine.getInstanceId());
    var piece = new Piece().withPoLineId(poLine.getId()).withFormat(OTHER)
      .withReceivingStatus(Piece.ReceivingStatus.EXPECTED)
      .withTitleId(title.getId());
    var pieceCollection = new PieceCollection().withPieces(List.of(piece));
    addMockEntry(TITLES, JsonObject.mapFrom(title));
    addMockEntry(PIECES_STORAGE_BATCH, pieceCollection);
  }
}
