package org.folio.service.dataimport.handlers;

import io.vertx.core.Future;
import io.vertx.core.json.Json;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.unit.Async;
import io.vertx.ext.unit.TestContext;
import io.vertx.ext.unit.junit.VertxUnitRunner;
import net.mguenther.kafka.junit.KeyValue;
import net.mguenther.kafka.junit.ObserveKeyValues;
import net.mguenther.kafka.junit.SendKeyValues;
import org.apache.kafka.clients.consumer.ConsumerConfig;
import org.folio.ActionProfile;
import org.folio.DataImportEventPayload;
import org.folio.JobProfile;
import org.folio.MappingProfile;
import org.folio.ParsedRecord;
import org.folio.Record;
import org.folio.TestConfig;
import org.folio.di.DiAbstractRestTest;
import org.folio.kafka.KafkaTopicNameHelper;
import org.folio.kafka.exception.DuplicateEventException;
import org.folio.orders.utils.AcqDesiredPermissions;
import org.folio.rest.RestConstants;
import org.folio.rest.client.TenantClient;
import org.folio.rest.impl.MockServer;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.EntityType;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Event;
import org.folio.rest.jaxrs.model.MappingDetail;
import org.folio.rest.jaxrs.model.MappingRule;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.ProfileSnapshotWrapper;
import org.folio.service.dataimport.PoLineImportProgressService;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.jupiter.api.Assertions;
import org.junit.runner.RunWith;

import java.time.LocalDate;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import static java.nio.charset.StandardCharsets.UTF_8;
import static org.apache.commons.lang.StringUtils.isEmpty;
import static java.time.ZoneOffset.UTC;
import static org.folio.ActionProfile.Action.CREATE;
import static org.folio.ActionProfile.FolioRecord.HOLDINGS;
import static org.folio.ActionProfile.FolioRecord.INSTANCE;
import static org.folio.DataImportEventTypes.DI_COMPLETED;
import static org.folio.DataImportEventTypes.DI_ERROR;
import static org.folio.DataImportEventTypes.DI_INVENTORY_INSTANCE_MATCHED;
import static org.folio.DataImportEventTypes.DI_MARC_BIB_FOR_ORDER_CREATED;
import static org.folio.DataImportEventTypes.DI_ORDER_CREATED;
import static org.folio.DataImportEventTypes.DI_ORDER_CREATED_READY_FOR_POST_PROCESSING;
import static org.folio.DataImportEventTypes.DI_PENDING_ORDER_CREATED;
import static org.folio.TestConfig.closeMockServer;
import static org.folio.kafka.KafkaTopicNameHelper.getDefaultNameSpace;
import static org.folio.orders.utils.HelperUtils.CONFIG_NAME;
import static org.folio.orders.utils.HelperUtils.PO_LINES_LIMIT_PROPERTY;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES_STORAGE;
import static org.folio.rest.RestVerticle.OKAPI_USERID_HEADER;
import static org.folio.rest.impl.MockServer.CONFIGS;
import static org.folio.rest.impl.MockServer.JOB_EXECUTIONS;
import static org.folio.rest.impl.MockServer.addMockEntry;
import static org.folio.rest.jaxrs.model.EntityType.MARC_BIBLIOGRAPHIC;
import static org.folio.rest.jaxrs.model.EntityType.ORDER;
import static org.folio.rest.jaxrs.model.ProfileSnapshotWrapper.ContentType.ACTION_PROFILE;
import static org.folio.rest.jaxrs.model.ProfileSnapshotWrapper.ContentType.JOB_PROFILE;
import static org.folio.rest.jaxrs.model.ProfileSnapshotWrapper.ContentType.MAPPING_PROFILE;
import static org.folio.service.dataimport.handlers.CreateOrderEventHandler.OKAPI_PERMISSIONS_HEADER;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

@RunWith(VertxUnitRunner.class)
public class CreateOrderEventHandlerTest extends DiAbstractRestTest {

  private static final String PARSED_CONTENT = "{\"leader\":\"01314nam  22003851a 4500\",\"fields\":[{\"001\":\"ybp7406411\"},{\"245\":{\"ind1\":\"1\",\"ind2\":\"0\",\"subfields\":[{\"a\":\"titleValue\"}]}},{\"336\":{\"ind1\":\"1\",\"ind2\":\"0\",\"subfields\":[{\"b\":\"b6698d38-149f-11ec-82a8-0242ac130003\"}]}},{\"780\":{\"ind1\":\"0\",\"ind2\":\"0\",\"subfields\":[{\"t\":\"Houston oil directory\"}]}},{\"785\":{\"ind1\":\"0\",\"ind2\":\"0\",\"subfields\":[{\"t\":\"SAIS review of international affairs\"},{\"x\":\"1945-4724\"}]}},{\"500\":{\"ind1\":\" \",\"ind2\":\" \",\"subfields\":[{\"a\":\"Adaptation of Xi xiang ji by Wang Shifu.\"}]}},{\"520\":{\"ind1\":\" \",\"ind2\":\" \",\"subfields\":[{\"a\":\"Ben shu miao shu le cui ying ying he zhang sheng wei zheng qu hun yin zi you li jin qu zhe jian xin zhi hou, zhong cheng juan shu de ai qing gu shi. jie lu le bao ban hun yin he feng jian li jiao de zui e.\"}]}}]}";
  private static final String JOB_PROFILE_SNAPSHOT_ID_KEY = "JOB_PROFILE_SNAPSHOT_ID";
  private static final String PO_LINE_KEY = "PO_LINE";
  private static final String GROUP_ID = "test-consumers-group";
  private static final String RECORD_ID_HEADER = "recordId";
  private static final String ID_FIELD = "id";
  private static final String JOB_PROFILE_SNAPSHOTS_MOCK = "jobProfileSnapshots";
  private static final String TENANT_APPROVAL_REQUIRED = "test_diku_limit_1";
  private static final String OKAPI_URL = "http://localhost:" + TestConfig.mockPort;
  private static final String USER_ID = "6bece55a-831c-4197-bed1-coff1e00b7d8";
  private static final String PO_LINE_ORDER_ID_KEY = "purchaseOrderId";

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
    .withExistingRecordType(ORDER)
    .withMappingDetails(new MappingDetail()
      .withMappingFields(new ArrayList<>(List.of(
        new MappingRule().withPath("order.po.workflowStatus").withValue("\"Pending\"").withEnabled("true"),
        new MappingRule().withPath("order.po.orderType").withValue("\"One-Time\"").withEnabled("true"),
        new MappingRule().withPath("order.po.vendor").withValue("\"e0fb5df2-cdf1-11e8-a8d5-f2801f1b9fd1\"").withEnabled("true"),
        new MappingRule().withPath("order.po.approved").withValue("\"true\"").withEnabled("true"),
        new MappingRule().withPath("order.poLine.titleOrPackage").withValue("245$a").withEnabled("true"),
        new MappingRule().withPath("order.poLine.cost.currency").withValue("\"USD\"").withEnabled("true"),
        new MappingRule().withPath("order.poLine.orderFormat").withValue("\"Physical Resource\"").withEnabled("true"),
        new MappingRule().withPath("order.poLine.checkinItems").withValue("\"true\"").withEnabled("true"),
        new MappingRule().withPath("order.poLine.acquisitionMethod").withValue("\"Purchase\"").withEnabled("true")
          .withAcceptedValues(new HashMap<>(Map.of(
            "df26d81b-9d63-4ff8-bf41-49bf75cfa70e", "Purchase"
          )))))));

  private final MappingProfile openOrderMappingProfile = new MappingProfile()
    .withId(UUID.randomUUID().toString())
    .withIncomingRecordType(MARC_BIBLIOGRAPHIC)
    .withExistingRecordType(ORDER)
    .withMappingDetails(new MappingDetail()
      .withMappingFields(new ArrayList<>(List.of(
        new MappingRule().withPath("order.po.workflowStatus").withValue("\"Open\"").withEnabled("true"),
        new MappingRule().withPath("order.po.orderType").withValue("\"One-Time\"").withEnabled("true"),
        new MappingRule().withPath("order.po.vendor").withValue("\"e0fb5df2-cdf1-11e8-a8d5-f2801f1b9fd1\"").withEnabled("true"),
        new MappingRule().withPath("order.po.approved").withValue("\"true\"").withEnabled("true"),
        new MappingRule().withPath("order.po.poLinesLimit").withValue("7").withEnabled("true").withName("poLinesLimit"),
        new MappingRule().withPath("order.poLine.titleOrPackage").withValue("245$a").withEnabled("true"),
        new MappingRule().withPath("order.poLine.cost.currency").withValue("\"USD\"").withEnabled("true"),
        new MappingRule().withPath("order.poLine.orderFormat").withValue("\"Physical Resource\"").withEnabled("true"),
        new MappingRule().withPath("order.poLine.checkinItems").withValue("\"true\"").withEnabled("true"),
        new MappingRule().withPath("order.poLine.acquisitionMethod").withValue("\"Purchase\"").withEnabled("true")
          .withAcceptedValues(new HashMap<>(Map.of(
            "df26d81b-9d63-4ff8-bf41-49bf75cfa70e", "Purchase"
          )))))));

  private final MappingProfile openOrderMappingProfileApprovedFalse = new MappingProfile()
    .withId(UUID.randomUUID().toString())
    .withIncomingRecordType(MARC_BIBLIOGRAPHIC)
    .withExistingRecordType(ORDER)
    .withMappingDetails(new MappingDetail()
      .withMappingFields(new ArrayList<>(List.of(
        new MappingRule().withPath("order.po.workflowStatus").withValue("\"Open\"").withEnabled("true"),
        new MappingRule().withPath("order.po.orderType").withValue("\"One-Time\"").withEnabled("true"),
        new MappingRule().withPath("order.po.vendor").withValue("\"e0fb5df2-cdf1-11e8-a8d5-f2801f1b9fd1\"").withEnabled("true"),
        new MappingRule().withPath("order.po.approved").withValue("\"false\"").withEnabled("true"),
        new MappingRule().withPath("order.po.poLinesLimit").withValue("7").withEnabled("true").withName("poLinesLimit"),
        new MappingRule().withPath("order.poLine.titleOrPackage").withValue("245$a").withEnabled("true"),
        new MappingRule().withPath("order.poLine.cost.currency").withValue("\"USD\"").withEnabled("true"),
        new MappingRule().withPath("order.poLine.orderFormat").withValue("\"Physical Resource\"").withEnabled("true"),
        new MappingRule().withPath("order.poLine.checkinItems").withValue("\"true\"").withEnabled("true"),
        new MappingRule().withPath("order.poLine.acquisitionMethod").withValue("\"Purchase\"").withEnabled("true")
          .withAcceptedValues(new HashMap<>(Map.of(
            "df26d81b-9d63-4ff8-bf41-49bf75cfa70e", "Purchase"
          )))))));

  private Record record;
  private JsonObject jobExecutionJson;

  @BeforeClass
  public static void setupClass(final TestContext testContext) throws ExecutionException, InterruptedException, TimeoutException {
    TestConfig.startMockServer();
    String okapiUrl = "http://localhost:" + port;
    TenantClient tenantClient = new TenantClient(okapiUrl, TENANT_APPROVAL_REQUIRED, TOKEN);
    postTenant(testContext, testContext.async(), tenantClient);
  }

  @AfterClass
  public static void after() {
    closeMockServer();
  }

  @Before
  public void setup() {
    MockServer.release();
    record = getRecord(0);

    jobExecutionJson = new JsonObject()
      .put(ID_FIELD, UUID.randomUUID().toString())
      .put("progress", new JsonObject().put("total", 1));

    addMockEntry(JOB_EXECUTIONS, jobExecutionJson);
  }

  private Record getRecord(Integer ordinalNumber) {
    return new Record().withId(UUID.randomUUID().toString())
      .withOrder(ordinalNumber)
      .withParsedRecord(new ParsedRecord().withContent(PARSED_CONTENT));
  }

  @Test
  public void shouldCreatePendingOrderAndPublishDiCompletedEvent() throws InterruptedException {
    // given
    ProfileSnapshotWrapper profileSnapshotWrapper = buildProfileSnapshotWrapper(jobProfile, actionProfile, mappingProfile);
    addMockEntry(JOB_PROFILE_SNAPSHOTS_MOCK, profileSnapshotWrapper);

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withCurrentNode(profileSnapshotWrapper.getChildSnapshotWrappers().get(0))
      .withEventType(DI_MARC_BIB_FOR_ORDER_CREATED.value())
      .withJobExecutionId(jobExecutionJson.getString(ID_FIELD))
      .withTenant(TENANT_ID)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN)
      .withContext(new HashMap<>() {{
        put(MARC_BIBLIOGRAPHIC.value(), Json.encode(record));
        put(JOB_PROFILE_SNAPSHOT_ID_KEY, profileSnapshotWrapper.getId());
      }});

    SendKeyValues<String, String> request = prepareKafkaRequest(dataImportEventPayload);

    // when
    kafkaCluster.send(request);

    // then
    DataImportEventPayload eventPayload = observeEvent(DI_COMPLETED.value());
    assertEquals(DI_ORDER_CREATED.value(), eventPayload.getEventsChain().get(eventPayload.getEventsChain().size() - 1));
    verifyOrder(eventPayload);
    verifyPoLine(eventPayload);
  }

  @Test
  public void shouldCreatePendingOrderAndPublishDiCompletedEventWhenActionProfileIsNotTheLastOne() throws InterruptedException {
    // given
    ProfileSnapshotWrapper profileSnapshotWrapper =
      new ProfileSnapshotWrapper()
        .withId(UUID.randomUUID().toString())
        .withProfileId(jobProfile.getId())
        .withContentType(JOB_PROFILE)
        .withContent(JsonObject.mapFrom(jobProfile).getMap())
        .withChildSnapshotWrappers(List.of(
          new ProfileSnapshotWrapper()
            .withId(UUID.randomUUID().toString())
            .withContentType(ACTION_PROFILE)
            .withOrder(0)
            .withProfileId(actionProfile.getId())
            .withContent(JsonObject.mapFrom(actionProfile).getMap())
            .withChildSnapshotWrappers(Collections.singletonList(
              new ProfileSnapshotWrapper()
                .withId(UUID.randomUUID().toString())
                .withProfileId(mappingProfile.getId())
                .withContentType(MAPPING_PROFILE)
                .withContent(JsonObject.mapFrom(mappingProfile).getMap()))),
          new ProfileSnapshotWrapper()
            .withId(UUID.randomUUID().toString())
            .withContentType(ACTION_PROFILE)
            .withOrder(1)
            .withProfileId(UUID.randomUUID().toString())
            .withContent(JsonObject.mapFrom(new ActionProfile().withFolioRecord(ActionProfile.FolioRecord.INSTANCE)))
            .withChildSnapshotWrappers(Collections.singletonList(
              new ProfileSnapshotWrapper()
                .withId(UUID.randomUUID().toString())
                .withProfileId(UUID.randomUUID().toString())
                .withContentType(MAPPING_PROFILE)
                .withContent(JsonObject.mapFrom(new MappingProfile()
                  .withIncomingRecordType(MARC_BIBLIOGRAPHIC).withExistingRecordType(EntityType.INSTANCE)))))));
    addMockEntry(JOB_PROFILE_SNAPSHOTS_MOCK, profileSnapshotWrapper);

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withCurrentNode(profileSnapshotWrapper.getChildSnapshotWrappers().get(0))
      .withEventType(DI_MARC_BIB_FOR_ORDER_CREATED.value())
      .withJobExecutionId(jobExecutionJson.getString(ID_FIELD))
      .withTenant(TENANT_ID)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN)
      .withContext(new HashMap<>() {{
        put(MARC_BIBLIOGRAPHIC.value(), Json.encode(record));
        put(JOB_PROFILE_SNAPSHOT_ID_KEY, profileSnapshotWrapper.getId());
      }});

    SendKeyValues<String, String> request = prepareKafkaRequest(dataImportEventPayload);

    // when
    kafkaCluster.send(request);

    // then
    DataImportEventPayload eventPayload = observeEvent(DI_COMPLETED.value());
    assertEquals(DI_ORDER_CREATED.value(), eventPayload.getEventsChain().get(eventPayload.getEventsChain().size() - 1));
    verifyOrder(eventPayload);
    verifyPoLine(eventPayload);
  }

  @Test
  public void shouldCreatePendingOrderAndPublishDiCompletedEventAndSetApprovedFalseWhenApprovalIsRequiredAndUserHaveNotPermission() throws InterruptedException {
    // given
    ProfileSnapshotWrapper profileSnapshotWrapper = buildProfileSnapshotWrapper(jobProfile, actionProfile, mappingProfile);
    addMockEntry(JOB_PROFILE_SNAPSHOTS_MOCK, profileSnapshotWrapper);

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withCurrentNode(profileSnapshotWrapper.getChildSnapshotWrappers().get(0))
      .withEventType(DI_MARC_BIB_FOR_ORDER_CREATED.value())
      .withJobExecutionId(jobExecutionJson.getString(ID_FIELD))
      .withTenant(TENANT_APPROVAL_REQUIRED)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN)
      .withContext(new HashMap<>() {{
        put(MARC_BIBLIOGRAPHIC.value(), Json.encode(record));
        put(JOB_PROFILE_SNAPSHOT_ID_KEY, profileSnapshotWrapper.getId());
      }});

    SendKeyValues<String, String> request = prepareKafkaRequest(dataImportEventPayload);

    // when
    kafkaCluster.send(request);

    // then
    String topicToObserve = KafkaTopicNameHelper.formatTopicName(KAFKA_ENV_VALUE, getDefaultNameSpace(),
      TENANT_APPROVAL_REQUIRED, DI_COMPLETED.value());

    List<KeyValue<String, String>> observedRecords = kafkaCluster.observe(ObserveKeyValues.on(topicToObserve, 1)
      .with(ConsumerConfig.GROUP_ID_CONFIG, GROUP_ID)
      .observeFor(30, TimeUnit.SECONDS)
      .build());

    Event obtainedEvent = Json.decodeValue(observedRecords.get(0).getValue(), Event.class);
    DataImportEventPayload eventPayload = Json.decodeValue(obtainedEvent.getEventPayload(), DataImportEventPayload.class);
    assertEquals(DI_ORDER_CREATED.value(), eventPayload.getEventsChain().get(eventPayload.getEventsChain().size() - 1));

    CompositePurchaseOrder order = Json.decodeValue(eventPayload.getContext().get(ActionProfile.FolioRecord.ORDER.value()), CompositePurchaseOrder.class);
    assertFalse(order.getApproved());

    verifyOrder(eventPayload);
    verifyPoLine(eventPayload);
  }

  @Test
  public void shouldCreatePendingOrderAndPublishDiReadyForPostProcessingEventWhenOpenStatusSpecified() throws InterruptedException {
    // given
    ProfileSnapshotWrapper profileSnapshotWrapper = buildProfileSnapshotWrapper(jobProfile, actionProfile, openOrderMappingProfile);
    addMockEntry(JOB_PROFILE_SNAPSHOTS_MOCK, profileSnapshotWrapper);

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withEventType(DI_MARC_BIB_FOR_ORDER_CREATED.value())
      .withJobExecutionId(jobExecutionJson.getString(ID_FIELD))
      .withTenant(TENANT_ID)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN)
      .withContext(new HashMap<>() {{
        put(MARC_BIBLIOGRAPHIC.value(), Json.encode(record));
        put(JOB_PROFILE_SNAPSHOT_ID_KEY, profileSnapshotWrapper.getId());
      }});

    SendKeyValues<String, String> request = prepareKafkaRequest(dataImportEventPayload);

    // when
    kafkaCluster.send(request);

    // then
    DataImportEventPayload eventPayload = observeEvent(DI_ORDER_CREATED_READY_FOR_POST_PROCESSING.value());
    assertEquals(DI_ORDER_CREATED_READY_FOR_POST_PROCESSING.value(), eventPayload.getEventType());

    assertNotNull(eventPayload.getContext().get(ORDER.value()));
    CompositePurchaseOrder createdOrder = Json.decodeValue(eventPayload.getContext().get(ORDER.value()), CompositePurchaseOrder.class);
    assertNotNull(createdOrder.getId());
    assertEquals(CompositePurchaseOrder.WorkflowStatus.PENDING, createdOrder.getWorkflowStatus());
    assertNotNull(createdOrder.getVendor());
    assertEquals(CompositePurchaseOrder.OrderType.ONE_TIME, createdOrder.getOrderType());
  }

  @Test
  public void shouldCreatePendingOrderAndPublishDiPendingOrderCreatedEventWhenOpenStatusSpecifiedAndActionProfileIsNotTheLastOne() throws InterruptedException {
    // given
    ProfileSnapshotWrapper profileSnapshotWrapper =
      new ProfileSnapshotWrapper()
        .withId(UUID.randomUUID().toString())
        .withProfileId(jobProfile.getId())
        .withContentType(JOB_PROFILE)
        .withContent(JsonObject.mapFrom(jobProfile).getMap())
        .withChildSnapshotWrappers(List.of(
          new ProfileSnapshotWrapper()
            .withId(UUID.randomUUID().toString())
            .withContentType(ACTION_PROFILE)
            .withOrder(0)
            .withProfileId(actionProfile.getId())
            .withContent(JsonObject.mapFrom(actionProfile).getMap())
            .withChildSnapshotWrappers(Collections.singletonList(
              new ProfileSnapshotWrapper()
                .withId(UUID.randomUUID().toString())
                .withProfileId(openOrderMappingProfile.getId())
                .withContentType(MAPPING_PROFILE)
                .withContent(JsonObject.mapFrom(openOrderMappingProfile).getMap()))),
          new ProfileSnapshotWrapper()
            .withId(UUID.randomUUID().toString())
            .withContentType(ACTION_PROFILE)
            .withOrder(1)
            .withProfileId(UUID.randomUUID().toString())
            .withContent(JsonObject.mapFrom(new ActionProfile().withFolioRecord(ActionProfile.FolioRecord.INSTANCE)))
            .withChildSnapshotWrappers(Collections.singletonList(
              new ProfileSnapshotWrapper()
                .withId(UUID.randomUUID().toString())
                .withProfileId(UUID.randomUUID().toString())
                .withContentType(MAPPING_PROFILE)
                .withContent(JsonObject.mapFrom(new MappingProfile()
                  .withIncomingRecordType(MARC_BIBLIOGRAPHIC).withExistingRecordType(EntityType.INSTANCE)))))));

    addMockEntry(JOB_PROFILE_SNAPSHOTS_MOCK, profileSnapshotWrapper);

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withEventType(DI_MARC_BIB_FOR_ORDER_CREATED.value())
      .withJobExecutionId(jobExecutionJson.getString(ID_FIELD))
      .withTenant(TENANT_ID)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN)
      .withContext(new HashMap<>() {{
        put(MARC_BIBLIOGRAPHIC.value(), Json.encode(record));
        put(JOB_PROFILE_SNAPSHOT_ID_KEY, profileSnapshotWrapper.getId());
      }});

    SendKeyValues<String, String> request = prepareKafkaRequest(dataImportEventPayload);

    // when
    kafkaCluster.send(request);

    // then
    DataImportEventPayload eventPayload = observeEvent(DI_PENDING_ORDER_CREATED.value());
    assertEquals(DI_PENDING_ORDER_CREATED.value(), eventPayload.getEventType());

    assertNotNull(eventPayload.getContext().get(ORDER.value()));
    CompositePurchaseOrder createdOrder = Json.decodeValue(eventPayload.getContext().get(ORDER.value()), CompositePurchaseOrder.class);
    assertNotNull(createdOrder.getId());
    assertEquals(CompositePurchaseOrder.WorkflowStatus.PENDING, createdOrder.getWorkflowStatus());
    assertNotNull(createdOrder.getVendor());
    assertEquals(CompositePurchaseOrder.OrderType.ONE_TIME, createdOrder.getOrderType());
  }

  @Test
  public void shouldCreatePendingOrderAndPublishDiReadyForPostProcessingEventWhenOpenStatusSpecifiedAndApprovalIsNotRequiredAndApprovedFalse() throws InterruptedException {
    // given
    ProfileSnapshotWrapper profileSnapshotWrapper = buildProfileSnapshotWrapper(jobProfile, actionProfile, openOrderMappingProfileApprovedFalse);
    addMockEntry(JOB_PROFILE_SNAPSHOTS_MOCK, profileSnapshotWrapper);

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withEventType(DI_MARC_BIB_FOR_ORDER_CREATED.value())
      .withJobExecutionId(jobExecutionJson.getString(ID_FIELD))
      .withTenant(TENANT_ID)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN)
      .withContext(new HashMap<>() {{
        put(MARC_BIBLIOGRAPHIC.value(), Json.encode(record));
        put(JOB_PROFILE_SNAPSHOT_ID_KEY, profileSnapshotWrapper.getId());
      }});

    SendKeyValues<String, String> request = prepareKafkaRequest(dataImportEventPayload);

    // when
    kafkaCluster.send(request);

    // then
    DataImportEventPayload eventPayload = observeEvent(DI_ORDER_CREATED_READY_FOR_POST_PROCESSING.value());
    assertEquals(DI_ORDER_CREATED_READY_FOR_POST_PROCESSING.value(), eventPayload.getEventType());

    assertNotNull(eventPayload.getContext().get(ORDER.value()));
    CompositePurchaseOrder createdOrder = Json.decodeValue(eventPayload.getContext().get(ORDER.value()), CompositePurchaseOrder.class);
    assertNotNull(createdOrder.getId());
    assertEquals(CompositePurchaseOrder.WorkflowStatus.PENDING, createdOrder.getWorkflowStatus());
    assertNotNull(createdOrder.getVendor());
    assertEquals(CompositePurchaseOrder.OrderType.ONE_TIME, createdOrder.getOrderType());
  }

  @Test
  public void shouldCreatePendingOrderAndPublishDiReadyForPostProcessingEventWhenOpenStatusSpecifiedAndApprovalIsRequiredAndApprovedFalse() throws InterruptedException {
    // given
    ProfileSnapshotWrapper profileSnapshotWrapper = buildProfileSnapshotWrapper(jobProfile, actionProfile, openOrderMappingProfileApprovedFalse);
    addMockEntry(JOB_PROFILE_SNAPSHOTS_MOCK, profileSnapshotWrapper);

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withEventType(DI_MARC_BIB_FOR_ORDER_CREATED.value())
      .withJobExecutionId(jobExecutionJson.getString(ID_FIELD))
      .withTenant(TENANT_APPROVAL_REQUIRED)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN)
      .withContext(new HashMap<>() {{
        put(MARC_BIBLIOGRAPHIC.value(), Json.encode(record));
        put(JOB_PROFILE_SNAPSHOT_ID_KEY, profileSnapshotWrapper.getId());
        put(OKAPI_PERMISSIONS_HEADER, JsonArray.of("orders.item.approve").encode());
      }});

    SendKeyValues<String, String> request = prepareKafkaRequest(dataImportEventPayload);

    // when
    kafkaCluster.send(request);

    // then
    String topicToObserve = KafkaTopicNameHelper.formatTopicName(KAFKA_ENV_VALUE, getDefaultNameSpace(),
      TENANT_APPROVAL_REQUIRED, DI_ORDER_CREATED_READY_FOR_POST_PROCESSING.value());

    List<KeyValue<String, String>> observedRecords = kafkaCluster.observe(ObserveKeyValues.on(topicToObserve, 1)
      .with(ConsumerConfig.GROUP_ID_CONFIG, GROUP_ID)
      .observeFor(30, TimeUnit.SECONDS)
      .build());

    Event obtainedEvent = Json.decodeValue(observedRecords.get(0).getValue(), Event.class);
    DataImportEventPayload eventPayload = Json.decodeValue(obtainedEvent.getEventPayload(), DataImportEventPayload.class);
    assertEquals(DI_ORDER_CREATED_READY_FOR_POST_PROCESSING.value(), eventPayload.getEventType());

    assertNotNull(eventPayload.getContext().get(ORDER.value()));
    CompositePurchaseOrder createdOrder = Json.decodeValue(eventPayload.getContext().get(ORDER.value()), CompositePurchaseOrder.class);
    assertNotNull(createdOrder.getId());
    assertEquals(CompositePurchaseOrder.WorkflowStatus.PENDING, createdOrder.getWorkflowStatus());
    assertNotNull(createdOrder.getVendor());
    assertEquals(CompositePurchaseOrder.OrderType.ONE_TIME, createdOrder.getOrderType());
  }

  @Test
  public void shouldCreatePendingOrderAndPublishDiCompletedEventWhenOpenStatusSpecifiedAndUserHaveNotPermission() throws InterruptedException {
    // given
    ProfileSnapshotWrapper profileSnapshotWrapper = buildProfileSnapshotWrapper(jobProfile, actionProfile, openOrderMappingProfile);
    addMockEntry(JOB_PROFILE_SNAPSHOTS_MOCK, profileSnapshotWrapper);

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withEventType(DI_MARC_BIB_FOR_ORDER_CREATED.value())
      .withJobExecutionId(jobExecutionJson.getString(ID_FIELD))
      .withTenant(TENANT_APPROVAL_REQUIRED)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN)
      .withContext(new HashMap<>() {{
        put(MARC_BIBLIOGRAPHIC.value(), Json.encode(record));
        put(JOB_PROFILE_SNAPSHOT_ID_KEY, profileSnapshotWrapper.getId());
      }});

    SendKeyValues<String, String> request = prepareKafkaRequest(dataImportEventPayload);

    // when
    kafkaCluster.send(request);

    // then
    String topicToObserve = KafkaTopicNameHelper.formatTopicName(KAFKA_ENV_VALUE, getDefaultNameSpace(), TENANT_APPROVAL_REQUIRED, DI_COMPLETED.value());

    List<KeyValue<String, String>> observedRecords = kafkaCluster.observe(ObserveKeyValues.on(topicToObserve, 1)
      .with(ConsumerConfig.GROUP_ID_CONFIG, GROUP_ID)
      .observeFor(30, TimeUnit.SECONDS)
      .build());

    Event obtainedEvent = Json.decodeValue(observedRecords.get(0).getValue(), Event.class);
    DataImportEventPayload eventPayload = Json.decodeValue(obtainedEvent.getEventPayload(), DataImportEventPayload.class);
    assertEquals(DI_ORDER_CREATED.value(), eventPayload.getEventsChain().get(eventPayload.getEventsChain().size() - 1));

    CompositePurchaseOrder order = Json.decodeValue(eventPayload.getContext().get(ActionProfile.FolioRecord.ORDER.value()), CompositePurchaseOrder.class);
    assertEquals(order.getApproved(), Boolean.FALSE);

    verifyOrder(eventPayload);
    verifyPoLine(eventPayload);
  }

  @Test
  public void shouldCreatePendingOrderAndPublishDiCompletedEventWhenOpenStatusSpecifiedAndUserHaveNotPermissionAndActionProfileNotTheLastOne() throws InterruptedException {
    // given
    ProfileSnapshotWrapper profileSnapshotWrapper =
      new ProfileSnapshotWrapper()
        .withId(UUID.randomUUID().toString())
        .withProfileId(jobProfile.getId())
        .withContentType(JOB_PROFILE)
        .withContent(JsonObject.mapFrom(jobProfile).getMap())
        .withChildSnapshotWrappers(List.of(
          new ProfileSnapshotWrapper()
            .withId(UUID.randomUUID().toString())
            .withContentType(ACTION_PROFILE)
            .withOrder(0)
            .withProfileId(actionProfile.getId())
            .withContent(JsonObject.mapFrom(actionProfile).getMap())
            .withChildSnapshotWrappers(Collections.singletonList(
              new ProfileSnapshotWrapper()
                .withId(UUID.randomUUID().toString())
                .withProfileId(openOrderMappingProfile.getId())
                .withContentType(MAPPING_PROFILE)
                .withContent(JsonObject.mapFrom(openOrderMappingProfile).getMap()))),
          new ProfileSnapshotWrapper()
            .withId(UUID.randomUUID().toString())
            .withContentType(ACTION_PROFILE)
            .withOrder(1)
            .withProfileId(UUID.randomUUID().toString())
            .withContent(JsonObject.mapFrom(new ActionProfile().withFolioRecord(ActionProfile.FolioRecord.INSTANCE)))
            .withChildSnapshotWrappers(Collections.singletonList(
              new ProfileSnapshotWrapper()
                .withId(UUID.randomUUID().toString())
                .withProfileId(UUID.randomUUID().toString())
                .withContentType(MAPPING_PROFILE)
                .withContent(JsonObject.mapFrom(new MappingProfile()
                  .withIncomingRecordType(MARC_BIBLIOGRAPHIC).withExistingRecordType(EntityType.INSTANCE)))))));

    addMockEntry(JOB_PROFILE_SNAPSHOTS_MOCK, profileSnapshotWrapper);

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withJobExecutionId(jobExecutionJson.getString(ID_FIELD))
      .withEventType(DI_MARC_BIB_FOR_ORDER_CREATED.value())
      .withTenant(TENANT_APPROVAL_REQUIRED)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN)
      .withContext(new HashMap<>() {{
        put(MARC_BIBLIOGRAPHIC.value(), Json.encode(record));
        put(JOB_PROFILE_SNAPSHOT_ID_KEY, profileSnapshotWrapper.getId());
      }});

    SendKeyValues<String, String> request = prepareKafkaRequest(dataImportEventPayload);

    // when
    kafkaCluster.send(request);

    // then
    String topicToObserve = KafkaTopicNameHelper.formatTopicName(KAFKA_ENV_VALUE, getDefaultNameSpace(), TENANT_APPROVAL_REQUIRED, DI_COMPLETED.value());

    List<KeyValue<String, String>> observedRecords = kafkaCluster.observe(ObserveKeyValues.on(topicToObserve, 1)
      .with(ConsumerConfig.GROUP_ID_CONFIG, GROUP_ID)
      .observeFor(30, TimeUnit.SECONDS)
      .build());

    Event obtainedEvent = Json.decodeValue(observedRecords.get(0).getValue(), Event.class);
    DataImportEventPayload eventPayload = Json.decodeValue(obtainedEvent.getEventPayload(), DataImportEventPayload.class);
    assertEquals(DI_ORDER_CREATED.value(), eventPayload.getEventsChain().get(eventPayload.getEventsChain().size() - 1));
    verifyOrder(eventPayload);
    verifyPoLine(eventPayload);
  }

  @Test
  public void shouldCreatePendingOrderAndPublishDiReadyForPostProcessingEventWhenOpenStatusSpecifiedAndUserHavePermission() throws InterruptedException {
    // given
    ProfileSnapshotWrapper profileSnapshotWrapper = buildProfileSnapshotWrapper(jobProfile, actionProfile, openOrderMappingProfile);
    addMockEntry(JOB_PROFILE_SNAPSHOTS_MOCK, profileSnapshotWrapper);

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withJobExecutionId(jobExecutionJson.getString(ID_FIELD))
      .withEventType(DI_MARC_BIB_FOR_ORDER_CREATED.value())
      .withTenant(TENANT_APPROVAL_REQUIRED)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN)
      .withContext(new HashMap<>() {{
        put(MARC_BIBLIOGRAPHIC.value(), Json.encode(record));
        put(JOB_PROFILE_SNAPSHOT_ID_KEY, profileSnapshotWrapper.getId());
        put(OKAPI_PERMISSIONS_HEADER, JsonArray.of("orders.item.approve").encode());
      }});

    SendKeyValues<String, String> request = prepareKafkaRequest(dataImportEventPayload);

    // when
    kafkaCluster.send(request);

    // then
    String topicToObserve = KafkaTopicNameHelper.formatTopicName(KAFKA_ENV_VALUE, getDefaultNameSpace(), TENANT_APPROVAL_REQUIRED,
      DI_ORDER_CREATED_READY_FOR_POST_PROCESSING.value());

    List<KeyValue<String, String>> observedRecords = kafkaCluster.observe(ObserveKeyValues.on(topicToObserve, 1)
      .with(ConsumerConfig.GROUP_ID_CONFIG, GROUP_ID)
      .observeFor(30, TimeUnit.SECONDS)
      .build());

    Event obtainedEvent = Json.decodeValue(observedRecords.get(0).getValue(), Event.class);
    DataImportEventPayload eventPayload = Json.decodeValue(obtainedEvent.getEventPayload(), DataImportEventPayload.class);

    assertEquals(DI_ORDER_CREATED_READY_FOR_POST_PROCESSING.value(), eventPayload.getEventType());

    assertNotNull(eventPayload.getContext().get(ORDER.value()));
    CompositePurchaseOrder createdOrder = Json.decodeValue(eventPayload.getContext().get(ORDER.value()), CompositePurchaseOrder.class);
    assertNotNull(createdOrder.getId());
    assertEquals(CompositePurchaseOrder.WorkflowStatus.PENDING, createdOrder.getWorkflowStatus());
    assertNotNull(createdOrder.getVendor());
    assertEquals(CompositePurchaseOrder.OrderType.ONE_TIME, createdOrder.getOrderType());
  }

  @Test
  public void shouldCreateOrderAndPoLineWithPopulatedInstanceIdWhenPayloadContainsInventoryInstance() throws InterruptedException {
    //given
    ProfileSnapshotWrapper profileSnapshotWrapper = buildProfileSnapshotWrapper(jobProfile, actionProfile, openOrderMappingProfile);
    addMockEntry(JOB_PROFILE_SNAPSHOTS_MOCK, profileSnapshotWrapper);
    JsonObject instanceJson = new JsonObject().put(ID_FIELD, UUID.randomUUID().toString());

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withJobExecutionId(jobExecutionJson.getString(ID_FIELD))
      .withEventType(DI_INVENTORY_INSTANCE_MATCHED.value())
      .withTenant(TENANT_ID)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN)
      .withContext(new HashMap<>() {{
        put(MARC_BIBLIOGRAPHIC.value(), Json.encode(record));
        put(EntityType.INSTANCE.value(), instanceJson.encode());
        put(JOB_PROFILE_SNAPSHOT_ID_KEY, profileSnapshotWrapper.getId());
      }});

    SendKeyValues<String, String> request = prepareKafkaRequest(dataImportEventPayload);

    // when
    kafkaCluster.send(request);

    // then
    DataImportEventPayload eventPayload = observeEvent(DI_ORDER_CREATED_READY_FOR_POST_PROCESSING.value());
    verifyOrder(eventPayload);
    CompositePoLine createdPoLine = verifyPoLine(eventPayload);
    assertEquals(instanceJson.getString(ID_FIELD), createdPoLine.getInstanceId());
  }

  @Test
  public void shouldPublishDiErrorEventWhenHasNoSourceRecord() throws InterruptedException {
    // given
    ProfileSnapshotWrapper profileSnapshotWrapper = buildProfileSnapshotWrapper(jobProfile, actionProfile, mappingProfile);
    addMockEntry(JOB_PROFILE_SNAPSHOTS_MOCK, profileSnapshotWrapper);

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withJobExecutionId(jobExecutionJson.getString(ID_FIELD))
      .withEventType(DI_MARC_BIB_FOR_ORDER_CREATED.value())
      .withTenant(TENANT_ID)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN)
      .withContext(new HashMap<>() {{
        put(JOB_PROFILE_SNAPSHOT_ID_KEY, profileSnapshotWrapper.getId());
      }});

    Assert.assertFalse(dataImportEventPayload.getContext().containsKey(MARC_BIBLIOGRAPHIC.value()));
    SendKeyValues<String, String> request = prepareKafkaRequest(dataImportEventPayload);

    // when
    kafkaCluster.send(request);

    // then
    DataImportEventPayload eventPayload = observeEvent(DI_ERROR.value());
    assertEquals(DI_ORDER_CREATED.value(), eventPayload.getEventsChain().get(eventPayload.getEventsChain().size() - 1));
    assertEquals(DI_ERROR.value(), eventPayload.getEventType());
  }

  @Test
  public void shouldCreatePoLineAndCalculateActivationDueFieldWhenActivationDueSpecified() throws InterruptedException {
    // given
    int expectedActivationDue = 3;
    String activationDueValue = LocalDate.now(ZoneId.of(UTC.getId())).plusDays(expectedActivationDue).toString();
    MappingRule activationDueRule = new MappingRule()
      .withPath("order.poLine.eresource.activationDue")
      .withValue(String.format("\"%s\"", activationDueValue))
      .withEnabled("true");

    mappingProfile.getMappingDetails().getMappingFields().add(activationDueRule);
    ProfileSnapshotWrapper profileSnapshotWrapper = buildProfileSnapshotWrapper(jobProfile, actionProfile, mappingProfile);
    addMockEntry(JOB_PROFILE_SNAPSHOTS_MOCK, profileSnapshotWrapper);

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withJobExecutionId(jobExecutionJson.getString(ID_FIELD))
      .withEventType(DI_MARC_BIB_FOR_ORDER_CREATED.value())
      .withTenant(TENANT_ID)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN)
      .withContext(new HashMap<>() {{
        put(MARC_BIBLIOGRAPHIC.value(), Json.encode(record));
        put(JOB_PROFILE_SNAPSHOT_ID_KEY, profileSnapshotWrapper.getId());
      }});

    SendKeyValues<String, String> request = prepareKafkaRequest(dataImportEventPayload);

    // when
    kafkaCluster.send(request);

    // then
    DataImportEventPayload eventPayload = observeEvent(DI_COMPLETED.value());
    verifyOrder(eventPayload);
    CompositePoLine createdPoLine = verifyPoLine(eventPayload);
    assertEquals(expectedActivationDue, createdPoLine.getEresource().getActivationDue());
  }

  @Test
  public void shouldCreatePoLineAndCalculateActivationDueFieldWhenActivationDueSpecifiedAsTodayExpression() throws InterruptedException {
    // given
    int expectedActivationDue = 1;
    MappingRule activationDueRule = new MappingRule()
      .withPath("order.poLine.eresource.activationDue")
      .withValue("###TODAY###")
      .withEnabled("true");

    mappingProfile.getMappingDetails().getMappingFields().add(activationDueRule);
    ProfileSnapshotWrapper profileSnapshotWrapper = buildProfileSnapshotWrapper(jobProfile, actionProfile, mappingProfile);
    addMockEntry(JOB_PROFILE_SNAPSHOTS_MOCK, profileSnapshotWrapper);

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withJobExecutionId(jobExecutionJson.getString(ID_FIELD))
      .withEventType(DI_MARC_BIB_FOR_ORDER_CREATED.value())
      .withTenant(TENANT_ID)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN)
      .withContext(new HashMap<>() {{
        put(MARC_BIBLIOGRAPHIC.value(), Json.encode(record));
        put(JOB_PROFILE_SNAPSHOT_ID_KEY, profileSnapshotWrapper.getId());
      }});

    SendKeyValues<String, String> request = prepareKafkaRequest(dataImportEventPayload);

    // when
    kafkaCluster.send(request);

    // then
    DataImportEventPayload eventPayload = observeEvent(DI_COMPLETED.value());
    assertEquals(DI_ORDER_CREATED.value(), eventPayload.getEventsChain().get(eventPayload.getEventsChain().size() - 1));
    verifyOrder(eventPayload);
    verifyPoLine(eventPayload);
    CompositePoLine createdPoLine = verifyPoLine(eventPayload);
    assertEquals(expectedActivationDue, createdPoLine.getEresource().getActivationDue());
  }

  @Test
  public void shouldOverrideConfiguredPoLinesLimitWhenPoLinesLimitSpecifiedAtMappingProfile() throws InterruptedException {
    // given
    JsonObject polLimitConfig = new JsonObject()
      .put("configName", PO_LINES_LIMIT_PROPERTY)
      .put("value", "1");

    MappingRule poLineLimitRule = new MappingRule()
      .withName(CreateOrderEventHandler.POL_LIMIT_RULE_NAME)
      .withPath("order.po.overridePoLinesLimit")
      .withValue("\"2\"")
      .withEnabled("true");

    mappingProfile.getMappingDetails().getMappingFields().add(poLineLimitRule);
    ProfileSnapshotWrapper profileSnapshotWrapper = buildProfileSnapshotWrapper(jobProfile, actionProfile, mappingProfile);
    addMockEntry(JOB_PROFILE_SNAPSHOTS_MOCK, profileSnapshotWrapper);
    addMockEntry(CONFIGS, polLimitConfig);
    addMockEntry(PO_LINES_STORAGE, new CompositePoLine().withTitleOrPackage("Mocked poLine for data-import"));

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withJobExecutionId(jobExecutionJson.getString(ID_FIELD))
      .withEventType(DI_MARC_BIB_FOR_ORDER_CREATED.value())
      .withTenant(TENANT_ID)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN)
      .withContext(new HashMap<>() {{
        put(MARC_BIBLIOGRAPHIC.value(), Json.encode(record));
        put(JOB_PROFILE_SNAPSHOT_ID_KEY, profileSnapshotWrapper.getId());
      }});

    SendKeyValues<String, String> request = prepareKafkaRequest(dataImportEventPayload);

    // when
    kafkaCluster.send(request);

    // then
    DataImportEventPayload eventPayload = observeEvent(DI_COMPLETED.value());
    assertEquals(DI_ORDER_CREATED.value(), eventPayload.getEventsChain().get(eventPayload.getEventsChain().size() - 1));
    verifyOrder(eventPayload);
    verifyPoLine(eventPayload);
  }

  @Test
  public void shouldOverrideElectronicPoLineCreateInventoryFieldAccordingToProfileSnapshot() throws InterruptedException {
    // given
    MappingRule createInventoryRule = new MappingRule()
      .withPath("order.poLine.eresource.createInventory")
      .withValue("\"Instance, Holding, Item\"")
      .withEnabled("true");

    openOrderMappingProfile.getMappingDetails().getMappingFields().add(createInventoryRule);
    ProfileSnapshotWrapper profileSnapshotWrapper = new ProfileSnapshotWrapper()
      .withId(UUID.randomUUID().toString())
      .withProfileId(jobProfile.getId())
      .withContentType(JOB_PROFILE)
      .withContent(JsonObject.mapFrom(jobProfile).getMap())
      .withChildSnapshotWrappers(List.of(
        new ProfileSnapshotWrapper()
          .withId(UUID.randomUUID().toString())
          .withProfileId(actionProfile.getId())
          .withOrder(0)
          .withContentType(ACTION_PROFILE)
          .withContent(JsonObject.mapFrom(actionProfile).getMap())
          .withChildSnapshotWrappers(Collections.singletonList(
            new ProfileSnapshotWrapper()
              .withId(UUID.randomUUID().toString())
              .withProfileId(mappingProfile.getId())
              .withContentType(MAPPING_PROFILE)
              .withContent(JsonObject.mapFrom(openOrderMappingProfile).getMap()))),
        new ProfileSnapshotWrapper()
          .withId(UUID.randomUUID().toString())
          .withProfileId(actionProfile.getId())
          .withOrder(1)
          .withContentType(ACTION_PROFILE)
          .withContent(JsonObject.mapFrom(new ActionProfile()
            .withName("Create instance")
            .withFolioRecord(INSTANCE)).getMap())
      ));
    addMockEntry(JOB_PROFILE_SNAPSHOTS_MOCK, profileSnapshotWrapper);

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withJobExecutionId(jobExecutionJson.getString(ID_FIELD))
      .withEventType(DI_MARC_BIB_FOR_ORDER_CREATED.value())
      .withTenant(TENANT_ID)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN)
      .withContext(new HashMap<>() {{
        put(MARC_BIBLIOGRAPHIC.value(), Json.encode(record));
        put(JOB_PROFILE_SNAPSHOT_ID_KEY, profileSnapshotWrapper.getId());
      }});

    SendKeyValues<String, String> request = prepareKafkaRequest(dataImportEventPayload);

    // when
    kafkaCluster.send(request);

    // then
    DataImportEventPayload eventPayload = observeEvent(DI_PENDING_ORDER_CREATED.value());
    assertEquals(DI_PENDING_ORDER_CREATED.value(), eventPayload.getEventType());
    verifyOrder(eventPayload);
    CompositePoLine poLine = verifyPoLine(eventPayload);
    assertNotNull(poLine.getEresource());
    assertEquals(Eresource.CreateInventory.INSTANCE, poLine.getEresource().getCreateInventory());
  }

  @Test
  public void shouldOverridePhysicalPoLineCreateInventoryFieldAccordingToProfileSnapshot() throws InterruptedException {
    // given
    MappingRule createInventoryRule = new MappingRule()
      .withPath("order.poLine.physical.createInventory")
      .withValue("\"Instance\"")
      .withEnabled("true");

    openOrderMappingProfile.getMappingDetails().getMappingFields().add(createInventoryRule);
    ProfileSnapshotWrapper profileSnapshotWrapper = new ProfileSnapshotWrapper()
      .withId(UUID.randomUUID().toString())
      .withProfileId(jobProfile.getId())
      .withContentType(JOB_PROFILE)
      .withContent(JsonObject.mapFrom(jobProfile).getMap())
      .withChildSnapshotWrappers(List.of(
        new ProfileSnapshotWrapper()
          .withId(UUID.randomUUID().toString())
          .withProfileId(actionProfile.getId())
          .withContentType(ACTION_PROFILE)
          .withOrder(0)
          .withContent(JsonObject.mapFrom(actionProfile).getMap())
          .withChildSnapshotWrappers(Collections.singletonList(
            new ProfileSnapshotWrapper()
              .withId(UUID.randomUUID().toString())
              .withProfileId(mappingProfile.getId())
              .withContentType(MAPPING_PROFILE)
              .withContent(JsonObject.mapFrom(openOrderMappingProfile).getMap()))),
        new ProfileSnapshotWrapper()
          .withId(UUID.randomUUID().toString())
          .withProfileId(actionProfile.getId())
          .withContentType(ACTION_PROFILE)
          .withOrder(1)
          .withContent(JsonObject.mapFrom(new ActionProfile()
            .withName("Create instance")
            .withFolioRecord(INSTANCE)).getMap()),
        new ProfileSnapshotWrapper()
          .withId(UUID.randomUUID().toString())
          .withProfileId(actionProfile.getId())
          .withContentType(ACTION_PROFILE)
          .withOrder(2)
          .withContent(JsonObject.mapFrom(new ActionProfile()
            .withName("Create holdings")
            .withFolioRecord(HOLDINGS)).getMap())
      ));
    addMockEntry(JOB_PROFILE_SNAPSHOTS_MOCK, profileSnapshotWrapper);

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withJobExecutionId(jobExecutionJson.getString(ID_FIELD))
      .withEventType(DI_MARC_BIB_FOR_ORDER_CREATED.value())
      .withTenant(TENANT_ID)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN)
      .withContext(new HashMap<>() {{
        put(MARC_BIBLIOGRAPHIC.value(), Json.encode(record));
        put(JOB_PROFILE_SNAPSHOT_ID_KEY, profileSnapshotWrapper.getId());
      }});

    SendKeyValues<String, String> request = prepareKafkaRequest(dataImportEventPayload);

    // when
    kafkaCluster.send(request);

    // then
    DataImportEventPayload eventPayload = observeEvent(DI_PENDING_ORDER_CREATED.value());
    assertEquals(DI_PENDING_ORDER_CREATED.value(), eventPayload.getEventType());
    verifyOrder(eventPayload);
    CompositePoLine poLine = verifyPoLine(eventPayload);
    assertNotNull(poLine.getPhysical());
    assertEquals(Physical.CreateInventory.INSTANCE_HOLDING, poLine.getPhysical().getCreateInventory());
  }

  @Test
  public void shouldCreateOrderWithSpecifiedAcquisitionUnitWhenUserHasUnitsAssignmentsPermission() throws InterruptedException {
    // given
    String expectedAcqUnitId = "0e9525aa-d123-4e4d-9f7e-1b302a97eb90";
    String acqUnitName = "Not protected";
    MappingRule acqUnitIdsRule = new MappingRule()
      .withAcceptedValues(new HashMap<>(Map.of(expectedAcqUnitId, acqUnitName)))
      .withPath("order.po.acqUnitIds[]")
      .withValue(String.format("\"%s\"", acqUnitName))
      .withEnabled("true");

    mappingProfile.getMappingDetails().getMappingFields().add(acqUnitIdsRule);
    ProfileSnapshotWrapper profileSnapshotWrapper = buildProfileSnapshotWrapper(jobProfile, actionProfile, mappingProfile);
    addMockEntry(JOB_PROFILE_SNAPSHOTS_MOCK, profileSnapshotWrapper);

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withJobExecutionId(jobExecutionJson.getString(ID_FIELD))
      .withEventType(DI_MARC_BIB_FOR_ORDER_CREATED.value())
      .withTenant(TENANT_ID)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN)
      .withContext(new HashMap<>() {{
        put(MARC_BIBLIOGRAPHIC.value(), Json.encode(record));
        put(OKAPI_PERMISSIONS_HEADER, JsonArray.of(AcqDesiredPermissions.ASSIGN.getPermission()).encode());
        put(OKAPI_USERID_HEADER, USER_ID);
        put(JOB_PROFILE_SNAPSHOT_ID_KEY, profileSnapshotWrapper.getId());
      }});

    SendKeyValues<String, String> request = prepareKafkaRequest(dataImportEventPayload);

    // when
    kafkaCluster.send(request);

    // then
    DataImportEventPayload eventPayload = observeEvent(DI_COMPLETED.value());
    assertEquals(DI_ORDER_CREATED.value(), eventPayload.getEventsChain().get(eventPayload.getEventsChain().size() - 1));
    CompositePurchaseOrder order = verifyOrder(eventPayload);
    MatcherAssert.assertThat(order.getAcqUnitIds(), Matchers.hasItem(expectedAcqUnitId));
  }

  @Test
  public void shouldPublishDiErrorWhenMappedOrderIsInvalid() throws InterruptedException {
    // given
    // reproduces invalid order case where: order.orderType == One-Time && order.ongoing != null
    MappingRule isSubscriptionRule = new MappingRule()
      .withPath("order.po.ongoing.isSubscription")
      .withValue("\"true\"")
      .withEnabled("true");

    mappingProfile.getMappingDetails().getMappingFields().add(isSubscriptionRule);
    ProfileSnapshotWrapper profileSnapshotWrapper = buildProfileSnapshotWrapper(jobProfile, actionProfile, mappingProfile);
    addMockEntry(JOB_PROFILE_SNAPSHOTS_MOCK, profileSnapshotWrapper);

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withJobExecutionId(jobExecutionJson.getString(ID_FIELD))
      .withEventType(DI_MARC_BIB_FOR_ORDER_CREATED.value())
      .withTenant(TENANT_ID)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN)
      .withContext(new HashMap<>() {{
        put(MARC_BIBLIOGRAPHIC.value(), Json.encode(record));
        put(JOB_PROFILE_SNAPSHOT_ID_KEY, profileSnapshotWrapper.getId());
      }});

    SendKeyValues<String, String> request = prepareKafkaRequest(dataImportEventPayload);

    // when
    kafkaCluster.send(request);

    // then
    DataImportEventPayload eventPayload = observeEvent(DI_ERROR.value());
    assertEquals(DI_ORDER_CREATED.value(), eventPayload.getEventsChain().get(eventPayload.getEventsChain().size() - 1));
    assertNotNull(eventPayload.getContext().get(ORDER.value()));
    assertDoesNotThrow(() -> Json.decodeValue(eventPayload.getContext().get(ORDER.value()), CompositePurchaseOrder.class));
    assertNotNull(eventPayload.getContext().get(PO_LINE_KEY));
    assertDoesNotThrow(() -> Json.decodeValue(eventPayload.getContext().get(PO_LINE_KEY), CompositePoLine.class));
  }

  @Test
  public void shouldPublishDiErrorEventAndClearOrderIdInPoLineWhenPoLineIsInvalid() throws InterruptedException {
    // given
    MappingRule isSubscriptionRule = new MappingRule()
      .withPath("order.poLine.cost.quantityPhysical")
      .withValue("\"-25\"")
      .withEnabled("true");

    mappingProfile.getMappingDetails().getMappingFields().add(isSubscriptionRule);
    ProfileSnapshotWrapper profileSnapshotWrapper = buildProfileSnapshotWrapper(jobProfile, actionProfile, mappingProfile);
    addMockEntry(JOB_PROFILE_SNAPSHOTS_MOCK, profileSnapshotWrapper);

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withJobExecutionId(jobExecutionJson.getString(ID_FIELD))
      .withCurrentNode(profileSnapshotWrapper.getChildSnapshotWrappers().get(0))
      .withEventType(DI_MARC_BIB_FOR_ORDER_CREATED.value())
      .withTenant(TENANT_ID)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN)
      .withContext(new HashMap<>() {{
        put(MARC_BIBLIOGRAPHIC.value(), Json.encode(record));
        put(JOB_PROFILE_SNAPSHOT_ID_KEY, profileSnapshotWrapper.getId());
      }});

    SendKeyValues<String, String> request = prepareKafkaRequest(dataImportEventPayload);

    // when
    kafkaCluster.send(request);

    // then
    DataImportEventPayload eventPayload = observeEvent(DI_ERROR.value());
    assertEquals(DI_ORDER_CREATED.value(), eventPayload.getEventsChain().get(eventPayload.getEventsChain().size() - 1));
    JsonObject poLineJsonObject = new JsonObject(eventPayload.getContext().get(PO_LINE_KEY));
    assertNull(poLineJsonObject.getString(PO_LINE_ORDER_ID_KEY));
  }

  @Test
  public void shouldIncrementPoLinesProgressWhenFailedToCreatePoLineAndPublishDiCompletedEvent(TestContext context) throws InterruptedException {
    // given
    // reproduces invalid order line case when physical quantity and Locations physical quantity do not match
    MappingRule physicalQuantityRule = new MappingRule()
      .withPath("order.poLine.cost.quantityPhysical")
      .withValue("\"7\"")
      .withEnabled("true");

    mappingProfile.getMappingDetails().getMappingFields().add(physicalQuantityRule);
    ProfileSnapshotWrapper profileSnapshotWrapper = buildProfileSnapshotWrapper(jobProfile, actionProfile, mappingProfile);
    addMockEntry(JOB_PROFILE_SNAPSHOTS_MOCK, profileSnapshotWrapper);

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withCurrentNode(profileSnapshotWrapper.getChildSnapshotWrappers().get(0))
      .withJobExecutionId(jobExecutionJson.getString(ID_FIELD))
      .withEventType(DI_MARC_BIB_FOR_ORDER_CREATED.value())
      .withTenant(TENANT_ID)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN)
      .withContext(new HashMap<>() {{
        put(MARC_BIBLIOGRAPHIC.value(), Json.encode(record));
        put(JOB_PROFILE_SNAPSHOT_ID_KEY, profileSnapshotWrapper.getId());
      }});

    SendKeyValues<String, String> request = prepareKafkaRequest(dataImportEventPayload);

    // when
    kafkaCluster.send(request);

    // then
    DataImportEventPayload eventPayload = observeEvent(DI_ERROR.value());
    assertEquals(DI_ORDER_CREATED.value(), eventPayload.getEventsChain().get(eventPayload.getEventsChain().size() - 1));
    assertNotNull(eventPayload.getContext().get(ORDER.value()));
    CompositePurchaseOrder order = Json.decodeValue(eventPayload.getContext().get(ORDER.value()), CompositePurchaseOrder.class);
    assertNotNull(order.getId());

    Async async = context.async();
    PoLineImportProgressService poLineImportProgressService = getBeanFromSpringContext(vertx, PoLineImportProgressService.class);
    poLineImportProgressService.poLinesProcessed(order.getId(), TENANT_ID)
      .onComplete(ar -> {
        context.assertTrue(ar.succeeded());
        context.assertTrue(ar.result());
        async.complete();
      });
  }

  @Test
  public void shouldReturnFailedByDuplicateEventExceptionFutureWhenOrderIdDuplicationErrorOccurs(TestContext context) {
    Async async = context.async();
    ProfileSnapshotWrapper profileSnapshotWrapper = buildProfileSnapshotWrapper(jobProfile, actionProfile, mappingProfile);
    addMockEntry(JOB_PROFILE_SNAPSHOTS_MOCK, profileSnapshotWrapper);

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withCurrentNode(profileSnapshotWrapper.getChildSnapshotWrappers().get(0))
      .withJobExecutionId(jobExecutionJson.getString(ID_FIELD))
      .withEventType(DI_MARC_BIB_FOR_ORDER_CREATED.value())
      .withTenant(TENANT_ID)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN)
      .withContext(new HashMap<>() {{
        put(MARC_BIBLIOGRAPHIC.value(), Json.encode(record));
        put(JOB_PROFILE_SNAPSHOT_ID_KEY, profileSnapshotWrapper.getId());
        put(CreateOrderEventHandler.USER_ID_KEY, MockServer.ORDER_ID_DUPLICATION_ERROR_USER_ID);
        put(RECORD_ID_HEADER, record.getId());
      }});

    CreateOrderEventHandler createOrderHandler = getBeanFromSpringContext(vertx, CreateOrderEventHandler.class);
    vertx.runOnContext(event -> Future.fromCompletionStage(createOrderHandler.handle(dataImportEventPayload))
      .onComplete(context.asyncAssertFailure(th -> {
        context.assertTrue(th instanceof DuplicateEventException);
        async.complete();
      })));
  }


  @Test
  public void shouldCreateNumberOfOrdersRelatedToPoLinesLimit() throws InterruptedException {
    // given
    Integer poLimit = 2;
    Integer recordsNumber = 3;
    MappingRule poLineLimitRule = new MappingRule()
      .withName(CreateOrderEventHandler.POL_LIMIT_RULE_NAME)
      .withPath("order.po.overridePoLinesLimit")
      .withValue("\"" + poLimit + "\"")
      .withEnabled("true");

    Set<String> ordersIds = new HashSet<>();
    Set<String> orderLinesIds = new HashSet<>();

    mappingProfile.getMappingDetails().getMappingFields().add(poLineLimitRule);
    ProfileSnapshotWrapper profileSnapshotWrapper = buildProfileSnapshotWrapper(jobProfile, actionProfile, mappingProfile);
    addMockEntry(JOB_PROFILE_SNAPSHOTS_MOCK, profileSnapshotWrapper);

    JsonObject polLimitConfig = new JsonObject().put(CONFIG_NAME, PO_LINES_LIMIT_PROPERTY).put("value", poLimit);
    addMockEntry(CONFIGS, polLimitConfig);

    // when
    DataImportEventPayload dataImportEventPayload;
    for (int i = 0; i < recordsNumber; i++) {
      record = getRecord(i);
      dataImportEventPayload = new DataImportEventPayload()
        .withJobExecutionId(jobExecutionJson.getString(ID_FIELD))
        .withEventType(DI_MARC_BIB_FOR_ORDER_CREATED.value())
        .withTenant(TENANT_ID)
        .withOkapiUrl(OKAPI_URL)
        .withToken(TOKEN)
        .withContext(new HashMap<>() {{
          put(MARC_BIBLIOGRAPHIC.value(), Json.encode(record));
          put(JOB_PROFILE_SNAPSHOT_ID_KEY, profileSnapshotWrapper.getId());
          put(RECORD_ID_HEADER, record.getId());
        }});

      kafkaCluster.send(prepareKafkaRequest(dataImportEventPayload));

      dataImportEventPayload = observeEvent(DI_COMPLETED.value());
      ordersIds.add(getPurchaseOrderId(dataImportEventPayload));
      orderLinesIds.add(verifyPoLine(dataImportEventPayload).getId());

      //clean MockServer storage after 1st order complete (filled by 2 poLInes)
      if (i == 1) {
        MockServer.release();
        addMockEntry(JOB_PROFILE_SNAPSHOTS_MOCK, profileSnapshotWrapper);
        addMockEntry(CONFIGS, polLimitConfig);
        addMockEntry(JOB_EXECUTIONS, jobExecutionJson);
      }
    }

    // then
    Assertions.assertEquals(ordersIds.size(), poLimit);
    Assertions.assertEquals(orderLinesIds.size(), recordsNumber);
  }


  @Test
  public void shouldReturnTrueWhenHandlerIsEligibleForActionProfile() {
    mappingProfile.getMappingDetails().getMappingFields().add(new MappingRule().withName("test"));
    ProfileSnapshotWrapper profileSnapshotWrapper = buildProfileSnapshotWrapper(jobProfile, actionProfile, mappingProfile);
    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withCurrentNode(profileSnapshotWrapper.getChildSnapshotWrappers().get(0));

    CreateOrderEventHandler createOrderHandler = getBeanFromSpringContext(vertx, CreateOrderEventHandler.class);
    Assertions.assertTrue(createOrderHandler.isEligible(dataImportEventPayload));
  }

  @Test
  public void shouldReturnFalseWhenHandlerIsNotEligibleForActionProfile() {
    ActionProfile actionProfile = new ActionProfile()
      .withName("Create instance")
      .withAction(ActionProfile.Action.CREATE)
      .withFolioRecord(INSTANCE);

    ProfileSnapshotWrapper actionProfileWrapper = new ProfileSnapshotWrapper()
      .withProfileId(jobProfile.getId())
      .withContentType(ACTION_PROFILE)
      .withContent(actionProfile);

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withCurrentNode(actionProfileWrapper);

    CreateOrderEventHandler createOrderHandler = getBeanFromSpringContext(vertx, CreateOrderEventHandler.class);
    Assertions.assertFalse(createOrderHandler.isEligible(dataImportEventPayload));
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

  private SendKeyValues<String, String> prepareKafkaRequest(DataImportEventPayload payload) {
    Event event = new Event().withEventPayload(Json.encode(payload));
    KeyValue<String, String> kafkaRecord = new KeyValue<>("test-key", Json.encode(event));
    if (!isEmpty(payload.getContext().get(ActionProfile.FolioRecord.MARC_BIBLIOGRAPHIC.value()))) {
      Record record = new JsonObject(payload.getContext().get(MARC_BIBLIOGRAPHIC.value())).mapTo(Record.class);
      kafkaRecord.addHeader(RECORD_ID_HEADER, record.getId(), UTF_8);
    } else {
      kafkaRecord.addHeader(RECORD_ID_HEADER, record.getId(), UTF_8);
    }
    kafkaRecord.addHeader(RestConstants.OKAPI_URL, payload.getOkapiUrl(), UTF_8);
    kafkaRecord.addHeader(OKAPI_PERMISSIONS_HEADER, payload.getContext().getOrDefault(OKAPI_PERMISSIONS_HEADER, ""), UTF_8);
    kafkaRecord.addHeader(OKAPI_USERID_HEADER, payload.getContext().getOrDefault(OKAPI_USERID_HEADER, ""), UTF_8);
    String topic = formatToKafkaTopicName(payload.getEventType());
    return SendKeyValues.to(topic, Collections.singletonList(kafkaRecord)).useDefaults();
  }

  private DataImportEventPayload observeEvent(String eventType) throws InterruptedException {
    String topicToObserve = formatToKafkaTopicName(eventType);
    List<KeyValue<String, String>> observedRecords = null;
    try {
      observedRecords = kafkaCluster.observe(ObserveKeyValues.on(topicToObserve, 1)
        .with(ConsumerConfig.GROUP_ID_CONFIG, GROUP_ID)
        .observeFor(30, TimeUnit.SECONDS)
        .build());
    } catch (AssertionError assertionError) {
      String topicErrorToObserve = formatToKafkaTopicName("DI_ERROR");
      observedRecords = kafkaCluster.observe(ObserveKeyValues.on(topicErrorToObserve, 1)
        .with(ConsumerConfig.GROUP_ID_CONFIG, GROUP_ID)
        .observeFor(15, TimeUnit.SECONDS)
        .build());
      String error = getCompositePurchaseOrder(observedRecords.get(0).getValue()).getContext().get("ERROR");
      throw new AssertionError(error);
    }

    assertEquals(record.getId(), new String(observedRecords.get(0).getHeaders().lastHeader(RECORD_ID_HEADER).value(), UTF_8));
    Event obtainedEvent = Json.decodeValue(observedRecords.get(0).getValue(), Event.class);
    return Json.decodeValue(obtainedEvent.getEventPayload(), DataImportEventPayload.class);
  }

  private DataImportEventPayload getCompositePurchaseOrder(String value) {
    Event obtainedEvent = Json.decodeValue(value, Event.class);
    return Json.decodeValue(obtainedEvent.getEventPayload(), DataImportEventPayload.class);
  }

  private String getPurchaseOrderId(DataImportEventPayload eventPayload) {
    return Json.decodeValue(eventPayload.getContext().get(PO_LINE_KEY), CompositePoLine.class).getPurchaseOrderId();
  }

  private CompositePurchaseOrder verifyOrder(DataImportEventPayload eventPayload) {
    assertNotNull(eventPayload.getContext().get(ORDER.value()));
    CompositePurchaseOrder createdOrder = Json.decodeValue(eventPayload.getContext().get(ORDER.value()), CompositePurchaseOrder.class);
    assertNotNull(createdOrder.getId());
    assertEquals(CompositePurchaseOrder.WorkflowStatus.PENDING, createdOrder.getWorkflowStatus());
    assertNotNull(createdOrder.getVendor());
    assertEquals(CompositePurchaseOrder.OrderType.ONE_TIME, createdOrder.getOrderType());
    return createdOrder;
  }

  private CompositePoLine verifyPoLine(DataImportEventPayload eventPayload) {
    assertNotNull(eventPayload.getContext().get(PO_LINE_KEY));
    CompositePoLine poLine = Json.decodeValue(eventPayload.getContext().get(PO_LINE_KEY), CompositePoLine.class);
    assertNotNull(poLine.getId());
    assertNotNull(poLine.getTitleOrPackage());
    assertNotNull(poLine.getPurchaseOrderId());
    assertEquals(CompositePoLine.Source.MARC, poLine.getSource());
    assertEquals(CompositePoLine.OrderFormat.PHYSICAL_RESOURCE, poLine.getOrderFormat());
    assertTrue(poLine.getCheckinItems());
    assertNotNull(poLine.getAcquisitionMethod());
    return poLine;
  }
}
