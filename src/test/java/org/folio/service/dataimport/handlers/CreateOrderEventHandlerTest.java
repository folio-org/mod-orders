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
import org.folio.AcquisitionsUnit;
import org.folio.ActionProfile;
import org.folio.DataImportEventPayload;
import org.folio.DataImportEventTypes;
import org.folio.Fund;
import org.folio.JobProfile;
import org.folio.MappingProfile;
import org.folio.Organization;
import org.folio.ParsedRecord;
import org.folio.Record;
import org.folio.TestConfig;
import org.folio.di.DiAbstractRestTest;
import org.folio.kafka.KafkaTopicNameHelper;
import org.folio.kafka.exception.DuplicateEventException;
import org.folio.orders.utils.AcqDesiredPermissions;
import org.folio.processing.events.EventManager;
import org.folio.rest.RestConstants;
import org.folio.rest.client.TenantClient;
import org.folio.rest.impl.MockServer;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.EntityType;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Event;
import org.folio.rest.jaxrs.model.MappingDetail;
import org.folio.rest.jaxrs.model.MappingRule;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.ProfileSnapshotWrapper;
import org.folio.rest.jaxrs.model.RepeatableSubfieldMapping;
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
import static org.folio.ActionProfile.FolioRecord.ITEM;
import static org.folio.DataImportEventTypes.DI_COMPLETED;
import static org.folio.DataImportEventTypes.DI_ERROR;
import static org.folio.DataImportEventTypes.DI_INCOMING_MARC_BIB_FOR_ORDER_PARSED;
import static org.folio.DataImportEventTypes.DI_INVENTORY_INSTANCE_MATCHED;
import static org.folio.DataImportEventTypes.DI_ORDER_CREATED;
import static org.folio.DataImportEventTypes.DI_ORDER_CREATED_READY_FOR_POST_PROCESSING;
import static org.folio.DataImportEventTypes.DI_PENDING_ORDER_CREATED;
import static org.folio.TestConfig.closeMockServer;
import static org.folio.kafka.KafkaTopicNameHelper.getDefaultNameSpace;
import static org.folio.orders.utils.HelperUtils.CONFIG_NAME;
import static org.folio.orders.utils.HelperUtils.PO_LINES_LIMIT_PROPERTY;
import static org.folio.orders.utils.ResourcePathResolver.ACQUISITIONS_UNITS;
import static org.folio.orders.utils.ResourcePathResolver.FUNDS;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES_STORAGE;
import static org.folio.rest.RestVerticle.OKAPI_USERID_HEADER;
import static org.folio.rest.impl.MockServer.CONFIGS;
import static org.folio.rest.impl.MockServer.JOB_EXECUTIONS;
import static org.folio.rest.impl.MockServer.addMockEntry;
import static org.folio.rest.jaxrs.model.PoLine.OrderFormat.ELECTRONIC_RESOURCE;
import static org.folio.rest.jaxrs.model.PoLine.OrderFormat.OTHER;
import static org.folio.rest.jaxrs.model.PoLine.OrderFormat.PHYSICAL_RESOURCE;
import static org.folio.rest.jaxrs.model.PoLine.OrderFormat.P_E_MIX;
import static org.folio.rest.jaxrs.model.EntityType.MARC_BIBLIOGRAPHIC;
import static org.folio.rest.jaxrs.model.EntityType.ORDER;
import static org.folio.rest.jaxrs.model.FundDistribution.DistributionType.PERCENTAGE;
import static org.folio.rest.jaxrs.model.MappingRule.RepeatableFieldAction.EXTEND_EXISTING;
import static org.folio.rest.jaxrs.model.ProfileType.ACTION_PROFILE;
import static org.folio.rest.jaxrs.model.ProfileType.JOB_PROFILE;
import static org.folio.rest.jaxrs.model.ProfileType.MAPPING_PROFILE;
import static org.folio.service.dataimport.handlers.CreateOrderEventHandler.OKAPI_PERMISSIONS_HEADER;
import static org.folio.service.orders.AcquisitionsUnitsServiceTest.USER_ID_ASSIGNED_TO_ACQ_UNITS;
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
  private static final String ORGANIZATIONS_MOCK = "organizations";
  private static final String TENANT_APPROVAL_REQUIRED = "test_diku_limit_1";
  private static final String OKAPI_URL = "http://localhost:" + TestConfig.mockPort;
  private static final String USER_ID = "6bece55a-831c-4197-bed1-coff1e00b7d8";
  private static final String PO_LINE_ORDER_ID_KEY = "purchaseOrderId";
  private static final String SYSTEM_USER_ENABLED = "SYSTEM_USER_ENABLED";

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
        new MappingRule().withName("vendor").withPath("order.po.vendor").withValue("\"e0fb5df2-cdf1-11e8-a8d5-f2801f1b9fd1\"").withEnabled("true"),
        new MappingRule().withPath("order.po.approved").withValue("\"true\"").withEnabled("true"),
        new MappingRule().withPath("order.po.poNumberPrefix").withValue("\"pref\"").withEnabled("true"),
        new MappingRule().withPath("order.po.poNumberSuffix").withValue("\"suf\"").withEnabled("true"),
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
        new MappingRule().withName("vendor").withPath("order.po.vendor").withValue("\"e0fb5df2-cdf1-11e8-a8d5-f2801f1b9fd1\"").withEnabled("true"),
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
        new MappingRule().withName("vendor").withPath("order.po.vendor").withValue("\"e0fb5df2-cdf1-11e8-a8d5-f2801f1b9fd1\"").withEnabled("true"),
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
  private final Organization organization = new Organization()
    .withId("e0fb5df2-cdf1-11e8-a8d5-f2801f1b9fd1").withName("OrgName").withCode("OrgCode");

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
    registerCreateOrderEventHandler();
    record = getRecord(0);

    jobExecutionJson = new JsonObject()
      .put(ID_FIELD, UUID.randomUUID().toString())
      .put("progress", new JsonObject().put("total", 1));

    addMockEntry(JOB_EXECUTIONS, jobExecutionJson);
    addMockEntry(ORGANIZATIONS_MOCK, organization);
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
      .withEventType(DI_INCOMING_MARC_BIB_FOR_ORDER_PARSED.value())
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

    CompositePurchaseOrder createdOrder = Json.decodeValue(eventPayload.getContext().get(ORDER.value()), CompositePurchaseOrder.class);
    assertTrue(createdOrder.getPoNumber().contains("pref"));
    assertTrue(createdOrder.getPoNumber().contains("suf"));
  }

  @Test
  public void shouldCreatePendingOrderAndPublishDiCompletedEventWithSystemUser() throws InterruptedException {
    // given
    System.setProperty(SYSTEM_USER_ENABLED, "false");
    ProfileSnapshotWrapper profileSnapshotWrapper = buildProfileSnapshotWrapper(jobProfile, actionProfile, mappingProfile);
    addMockEntry(JOB_PROFILE_SNAPSHOTS_MOCK, profileSnapshotWrapper);

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withCurrentNode(profileSnapshotWrapper.getChildSnapshotWrappers().get(0))
      .withEventType(DI_INCOMING_MARC_BIB_FOR_ORDER_PARSED.value())
      .withJobExecutionId(jobExecutionJson.getString(ID_FIELD))
      .withTenant(TENANT_ID)
      .withToken(null)
      .withOkapiUrl(OKAPI_URL)
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

    CompositePurchaseOrder createdOrder = Json.decodeValue(eventPayload.getContext().get(ORDER.value()), CompositePurchaseOrder.class);
    assertTrue(createdOrder.getPoNumber().contains("pref"));
    assertTrue(createdOrder.getPoNumber().contains("suf"));
    System.clearProperty(SYSTEM_USER_ENABLED);
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
      .withEventType(DI_INCOMING_MARC_BIB_FOR_ORDER_PARSED.value())
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
      .withEventType(DI_INCOMING_MARC_BIB_FOR_ORDER_PARSED.value())
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
      .withEventType(DI_INCOMING_MARC_BIB_FOR_ORDER_PARSED.value())
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
      .withEventType(DI_INCOMING_MARC_BIB_FOR_ORDER_PARSED.value())
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
      .withEventType(DI_INCOMING_MARC_BIB_FOR_ORDER_PARSED.value())
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
      .withEventType(DI_INCOMING_MARC_BIB_FOR_ORDER_PARSED.value())
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
      .withEventType(DI_INCOMING_MARC_BIB_FOR_ORDER_PARSED.value())
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
    assertEquals(Boolean.FALSE, order.getApproved());

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
      .withEventType(DI_INCOMING_MARC_BIB_FOR_ORDER_PARSED.value())
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
      .withEventType(DI_INCOMING_MARC_BIB_FOR_ORDER_PARSED.value())
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
    PoLine createdPoLine = verifyPoLine(eventPayload);
    assertEquals(instanceJson.getString(ID_FIELD), createdPoLine.getInstanceId());
  }

  @Test
  public void shouldPublishDiErrorEventWhenHasNoSourceRecord() throws InterruptedException {
    // given
    ProfileSnapshotWrapper profileSnapshotWrapper = buildProfileSnapshotWrapper(jobProfile, actionProfile, mappingProfile);
    addMockEntry(JOB_PROFILE_SNAPSHOTS_MOCK, profileSnapshotWrapper);

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withJobExecutionId(jobExecutionJson.getString(ID_FIELD))
      .withEventType(DI_INCOMING_MARC_BIB_FOR_ORDER_PARSED.value())
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
      .withEventType(DI_INCOMING_MARC_BIB_FOR_ORDER_PARSED.value())
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
    PoLine createdPoLine = verifyPoLine(eventPayload);
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
      .withEventType(DI_INCOMING_MARC_BIB_FOR_ORDER_PARSED.value())
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
    PoLine createdPoLine = verifyPoLine(eventPayload);
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
    addMockEntry(PO_LINES_STORAGE, new PoLine().withTitleOrPackage("Mocked poLine for data-import"));

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withJobExecutionId(jobExecutionJson.getString(ID_FIELD))
      .withEventType(DI_INCOMING_MARC_BIB_FOR_ORDER_PARSED.value())
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
  public void shouldSetPhysicalPoLineCreateInventoryFieldBasedOnActionProfilesIfOrderFormatPhysicalAndInstanceActionExists() throws InterruptedException {
    List<ActionProfile> nextInventoryActionProfiles = List.of(new ActionProfile().withFolioRecord(INSTANCE));

    DataImportEventPayload eventPayload = importPoLineWithCreateInventoryFieldBasedOnActionsAndOrderFormat(nextInventoryActionProfiles, PHYSICAL_RESOURCE);

    PoLine poLine = Json.decodeValue(eventPayload.getContext().get(PO_LINE_KEY), PoLine.class);
    assertNotNull(poLine.getPhysical());
    assertEquals(Physical.CreateInventory.INSTANCE, poLine.getPhysical().getCreateInventory());
  }

  @Test
  public void shouldSetPhysicalPoLineCreateInventoryFieldBasedOnActionProfilesIfOrderFormatPhysicalAndInstanceHoldingActionsExist() throws InterruptedException {
    List<ActionProfile> nextInventoryActionProfiles = List.of(
      new ActionProfile().withFolioRecord(INSTANCE),
      new ActionProfile().withFolioRecord(HOLDINGS));

    DataImportEventPayload eventPayload = importPoLineWithCreateInventoryFieldBasedOnActionsAndOrderFormat(nextInventoryActionProfiles, PHYSICAL_RESOURCE);

    PoLine poLine = Json.decodeValue(eventPayload.getContext().get(PO_LINE_KEY), PoLine.class);
    assertNotNull(poLine.getPhysical());
    assertEquals(Physical.CreateInventory.INSTANCE_HOLDING, poLine.getPhysical().getCreateInventory());
  }

  @Test
  public void shouldSetPhysicalPoLineCreateInventoryFieldBasedOnActionProfilesIfOrderFormatPhysicalAndInstanceHoldingItemActionsExist() throws InterruptedException {
    List<ActionProfile> nextInventoryActionProfiles = List.of(
      new ActionProfile().withFolioRecord(INSTANCE),
      new ActionProfile().withFolioRecord(HOLDINGS),
      new ActionProfile().withFolioRecord(ITEM));

    DataImportEventPayload eventPayload = importPoLineWithCreateInventoryFieldBasedOnActionsAndOrderFormat(nextInventoryActionProfiles, PHYSICAL_RESOURCE);

    PoLine poLine = Json.decodeValue(eventPayload.getContext().get(PO_LINE_KEY), PoLine.class);
    assertNotNull(poLine.getPhysical());
    assertEquals(Physical.CreateInventory.INSTANCE_HOLDING_ITEM, poLine.getPhysical().getCreateInventory());
  }

  @Test
  public void shouldSetPhysicalPoLineCreateInventoryFieldBasedOnActionProfilesIfOrderFormatPhysicalAndHaveNoInventoryActionsProfiles() throws InterruptedException {
    DataImportEventPayload eventPayload = importPoLineWithCreateInventoryFieldBasedOnActionsAndOrderFormat(List.of(), PHYSICAL_RESOURCE);

    PoLine poLine = Json.decodeValue(eventPayload.getContext().get(PO_LINE_KEY), PoLine.class);
    assertNotNull(poLine.getPhysical());
    assertEquals(Physical.CreateInventory.NONE, poLine.getPhysical().getCreateInventory());
  }

  @Test
  public void shouldSetElectronicPoLineCreateInventoryFieldBasedOnActionProfilesIfOrderFormatElectronicAndInstanceActionExists() throws InterruptedException {
    List<ActionProfile> inventoryActionProfiles = List.of(new ActionProfile().withFolioRecord(INSTANCE));

    DataImportEventPayload eventPayload = importPoLineWithCreateInventoryFieldBasedOnActionsAndOrderFormat(inventoryActionProfiles, ELECTRONIC_RESOURCE);

    PoLine poLine = Json.decodeValue(eventPayload.getContext().get(PO_LINE_KEY), PoLine.class);
    assertNotNull(poLine.getEresource());
    assertEquals(Eresource.CreateInventory.INSTANCE, poLine.getEresource().getCreateInventory());
  }

  @Test
  public void shouldSetElectronicPoLineCreateInventoryFieldBasedOnActionProfilesIfOrderFormatElectronicAndInstanceHoldingActionsExist() throws InterruptedException {
    List<ActionProfile> nextInventoryActionProfiles = List.of(
      new ActionProfile().withFolioRecord(INSTANCE),
      new ActionProfile().withFolioRecord(HOLDINGS));

    DataImportEventPayload eventPayload = importPoLineWithCreateInventoryFieldBasedOnActionsAndOrderFormat(nextInventoryActionProfiles, ELECTRONIC_RESOURCE);

    PoLine poLine = Json.decodeValue(eventPayload.getContext().get(PO_LINE_KEY), PoLine.class);
    assertNotNull(poLine.getEresource());
    assertEquals(Eresource.CreateInventory.INSTANCE_HOLDING, poLine.getEresource().getCreateInventory());
  }

  @Test
  public void shouldSetElectronicPoLineCreateInventoryFieldBasedOnActionProfilesIfOrderFormatElectronicAndInstanceHoldingItemActionsExist() throws InterruptedException {
    List<ActionProfile> nextInventoryActionProfiles = List.of(
      new ActionProfile().withFolioRecord(INSTANCE),
      new ActionProfile().withFolioRecord(HOLDINGS),
      new ActionProfile().withFolioRecord(ITEM));

    DataImportEventPayload eventPayload = importPoLineWithCreateInventoryFieldBasedOnActionsAndOrderFormat(nextInventoryActionProfiles, ELECTRONIC_RESOURCE);

    PoLine poLine = Json.decodeValue(eventPayload.getContext().get(PO_LINE_KEY), PoLine.class);
    assertNotNull(poLine.getEresource());
    assertEquals(Eresource.CreateInventory.INSTANCE_HOLDING_ITEM, poLine.getEresource().getCreateInventory());
  }

  @Test
  public void shouldSetElectronicPoLineCreateInventoryFieldBasedOnActionProfilesIfOrderFormatElectronicAndHaveNoInventoryActionProfiles() throws InterruptedException {
    DataImportEventPayload eventPayload = importPoLineWithCreateInventoryFieldBasedOnActionsAndOrderFormat(List.of(), ELECTRONIC_RESOURCE);

    PoLine poLine = Json.decodeValue(eventPayload.getContext().get(PO_LINE_KEY), PoLine.class);
    assertNotNull(poLine.getEresource());
    assertEquals(Eresource.CreateInventory.NONE, poLine.getEresource().getCreateInventory());
  }

  @Test
  public void shouldSetPhysicalPoLineCreateInventoryFieldBasedOnActionProfilesIfOrderFormatOtherAndInstanceActionExists() throws InterruptedException {
    List<ActionProfile> nextInventoryActionProfiles = List.of(new ActionProfile().withFolioRecord(INSTANCE));

    DataImportEventPayload eventPayload = importPoLineWithCreateInventoryFieldBasedOnActionsAndOrderFormat(nextInventoryActionProfiles, OTHER);

    PoLine poLine = Json.decodeValue(eventPayload.getContext().get(PO_LINE_KEY), PoLine.class);
    assertNotNull(poLine.getPhysical());
    assertEquals(Physical.CreateInventory.INSTANCE, poLine.getPhysical().getCreateInventory());
  }

  @Test
  public void shouldSetPhysicalPoLineCreateInventoryFieldBasedOnActionProfilesIfOrderFormatOtherAndInstanceHoldingActionsExist() throws InterruptedException {
    List<ActionProfile> nextInventoryActionProfiles = List.of(
      new ActionProfile().withFolioRecord(INSTANCE),
      new ActionProfile().withFolioRecord(HOLDINGS));

    DataImportEventPayload eventPayload = importPoLineWithCreateInventoryFieldBasedOnActionsAndOrderFormat(nextInventoryActionProfiles, OTHER);

    PoLine poLine = Json.decodeValue(eventPayload.getContext().get(PO_LINE_KEY), PoLine.class);
    assertNotNull(poLine.getPhysical());
    assertEquals(Physical.CreateInventory.INSTANCE_HOLDING, poLine.getPhysical().getCreateInventory());
  }

  @Test
  public void shouldSetPhysicalPoLineCreateInventoryFieldBasedOnActionProfilesIfOrderFormatOtherAndInstanceHoldingItemActionsExist() throws InterruptedException {
    List<ActionProfile> nextInventoryActionProfiles = List.of(
      new ActionProfile().withFolioRecord(INSTANCE),
      new ActionProfile().withFolioRecord(HOLDINGS),
      new ActionProfile().withFolioRecord(ITEM));

    DataImportEventPayload eventPayload = importPoLineWithCreateInventoryFieldBasedOnActionsAndOrderFormat(nextInventoryActionProfiles, OTHER);

    PoLine poLine = Json.decodeValue(eventPayload.getContext().get(PO_LINE_KEY), PoLine.class);
    assertNotNull(poLine.getPhysical());
    assertEquals(Physical.CreateInventory.INSTANCE_HOLDING_ITEM, poLine.getPhysical().getCreateInventory());
  }

  @Test
  public void shouldSetPhysicalPoLineCreateInventoryFieldBasedOnActionProfilesIfOrderFormatOtherAndHaveNoInventoryActionProfiles() throws InterruptedException {
    DataImportEventPayload eventPayload = importPoLineWithCreateInventoryFieldBasedOnActionsAndOrderFormat(List.of(), OTHER);

    PoLine poLine = Json.decodeValue(eventPayload.getContext().get(PO_LINE_KEY), PoLine.class);
    assertNotNull(poLine.getPhysical());
    assertEquals(Physical.CreateInventory.NONE, poLine.getPhysical().getCreateInventory());
  }

  @Test
  public void shouldSetPhysicalAndElectronicPoLineCreateInventoryFieldBasedOnActionProfilesIfOrderFormatMixAndInstanceActionExists() throws InterruptedException {
    List<ActionProfile> nextInventoryActionProfiles = List.of(new ActionProfile().withFolioRecord(INSTANCE));

    DataImportEventPayload eventPayload = importPoLineWithCreateInventoryFieldBasedOnActionsAndOrderFormat(nextInventoryActionProfiles, P_E_MIX);

    PoLine poLine = Json.decodeValue(eventPayload.getContext().get(PO_LINE_KEY), PoLine.class);
    assertNotNull(poLine.getPhysical());
    assertNotNull(poLine.getEresource());
    assertEquals(Physical.CreateInventory.INSTANCE, poLine.getPhysical().getCreateInventory());
    assertEquals(Eresource.CreateInventory.INSTANCE, poLine.getEresource().getCreateInventory());
  }

  @Test
  public void shouldSetPhysicalAndElectronicPoLineCreateInventoryFieldBasedOnActionProfilesIfOrderFormatMixAndInstanceHoldingActionsExist() throws InterruptedException {
    List<ActionProfile> nextInventoryActionProfiles = List.of(
      new ActionProfile().withFolioRecord(INSTANCE),
      new ActionProfile().withFolioRecord(HOLDINGS));

    DataImportEventPayload eventPayload = importPoLineWithCreateInventoryFieldBasedOnActionsAndOrderFormat(nextInventoryActionProfiles, P_E_MIX);

    PoLine poLine = Json.decodeValue(eventPayload.getContext().get(PO_LINE_KEY), PoLine.class);
    assertNotNull(poLine.getPhysical());
    assertNotNull(poLine.getEresource());
    assertEquals(Physical.CreateInventory.INSTANCE_HOLDING, poLine.getPhysical().getCreateInventory());
    assertEquals(Eresource.CreateInventory.INSTANCE_HOLDING, poLine.getEresource().getCreateInventory());
  }

  @Test
  public void shouldSetPhysicalAndElectronicPoLineCreateInventoryFieldBasedOnActionProfilesIfOrderFormatMixAndInstanceHoldingItemActionsExist() throws InterruptedException {
    List<ActionProfile> nextInventoryActionProfiles = List.of(
      new ActionProfile().withFolioRecord(INSTANCE),
      new ActionProfile().withFolioRecord(HOLDINGS),
      new ActionProfile().withFolioRecord(ITEM));

    DataImportEventPayload eventPayload = importPoLineWithCreateInventoryFieldBasedOnActionsAndOrderFormat(nextInventoryActionProfiles, P_E_MIX);

    PoLine poLine = Json.decodeValue(eventPayload.getContext().get(PO_LINE_KEY), PoLine.class);
    assertNotNull(poLine.getPhysical());
    assertNotNull(poLine.getEresource());
    assertEquals(Physical.CreateInventory.INSTANCE_HOLDING_ITEM, poLine.getPhysical().getCreateInventory());
    assertEquals(Eresource.CreateInventory.INSTANCE_HOLDING_ITEM, poLine.getEresource().getCreateInventory());
  }

  @Test
  public void shouldSetPhysicalAndElectronicPoLineCreateInventoryFieldBasedOnActionProfilesIfOrderFormatMixAndHaveNoInventoryActionProfiles() throws InterruptedException {
    DataImportEventPayload eventPayload = importPoLineWithCreateInventoryFieldBasedOnActionsAndOrderFormat(List.of(), P_E_MIX);

    PoLine poLine = Json.decodeValue(eventPayload.getContext().get(PO_LINE_KEY), PoLine.class);
    assertNotNull(poLine.getPhysical());
    assertNotNull(poLine.getEresource());
    assertEquals(Physical.CreateInventory.NONE, poLine.getPhysical().getCreateInventory());
    assertEquals(Eresource.CreateInventory.NONE, poLine.getEresource().getCreateInventory());
  }

  private DataImportEventPayload importPoLineWithCreateInventoryFieldBasedOnActionsAndOrderFormat(List<ActionProfile> inventoryActionProfiles,
                                                                                                  PoLine.OrderFormat orderFormat) throws InterruptedException {
    // given
    openOrderMappingProfile.getMappingDetails().getMappingFields().stream()
      .filter(mappingRule -> mappingRule.getPath().equals("order.poLine.orderFormat"))
      .forEach(mappingRule -> mappingRule.setValue(String.format("\"%s\"", orderFormat.value())));

    ProfileSnapshotWrapper profileSnapshotWrapper =
      buildProfileSnapshotWrapperWithInventoryActions(jobProfile, actionProfile, openOrderMappingProfile, inventoryActionProfiles);
    addMockEntry(JOB_PROFILE_SNAPSHOTS_MOCK, profileSnapshotWrapper);

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withJobExecutionId(jobExecutionJson.getString(ID_FIELD))
      .withEventType(DI_INCOMING_MARC_BIB_FOR_ORDER_PARSED.value())
      .withTenant(TENANT_ID)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN)
      .withContext(new HashMap<>() {{
        put(MARC_BIBLIOGRAPHIC.value(), Json.encode(record));
        put(JOB_PROFILE_SNAPSHOT_ID_KEY, profileSnapshotWrapper.getId());
      }});

    SendKeyValues<String, String> request = prepareKafkaRequest(dataImportEventPayload);

    // result event type depends on inventory action profiles existence
    DataImportEventTypes eventToObserve = inventoryActionProfiles.isEmpty()
      ? DI_ORDER_CREATED_READY_FOR_POST_PROCESSING : DI_PENDING_ORDER_CREATED;

    // when
    kafkaCluster.send(request);

    // then
    DataImportEventPayload eventPayload = observeEvent(eventToObserve.value());
    assertEquals(eventToObserve.value(), eventPayload.getEventType());
    return eventPayload;
  }

  @Test
  public void shouldCreateOrderWithSpecifiedAcquisitionUnitWhenUserHasUnitsAssignmentsPermission() throws InterruptedException {
    // given
    String expectedAcqUnitId = "f6d2cc9d-82ca-437c-a4e6-e5c30323df00";
    String acqUnitName = "Not protected";
    MappingRule acqUnitIdsRule = new MappingRule()
      .withName("acqUnitIds")
      .withPath("order.po.acqUnitIds[]")
      .withValue(String.format("\"%s\"", acqUnitName))
      .withEnabled("true");

    mappingProfile.getMappingDetails().getMappingFields().add(acqUnitIdsRule);
    ProfileSnapshotWrapper profileSnapshotWrapper = buildProfileSnapshotWrapper(jobProfile, actionProfile, mappingProfile);
    AcquisitionsUnit acquisitionsUnit = new AcquisitionsUnit().withId(expectedAcqUnitId).withName(acqUnitName);
    addMockEntry(JOB_PROFILE_SNAPSHOTS_MOCK, profileSnapshotWrapper);
    addMockEntry(ACQUISITIONS_UNITS, acquisitionsUnit);

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withJobExecutionId(jobExecutionJson.getString(ID_FIELD))
      .withEventType(DI_INCOMING_MARC_BIB_FOR_ORDER_PARSED.value())
      .withTenant(TENANT_ID)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN)
      .withContext(new HashMap<>() {{
        put(MARC_BIBLIOGRAPHIC.value(), Json.encode(record));
        put(OKAPI_PERMISSIONS_HEADER, JsonArray.of(AcqDesiredPermissions.ASSIGN.getPermission()).encode());
        put(OKAPI_USERID_HEADER, USER_ID_ASSIGNED_TO_ACQ_UNITS);
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
      .withEventType(DI_INCOMING_MARC_BIB_FOR_ORDER_PARSED.value())
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
    assertDoesNotThrow(() -> Json.decodeValue(eventPayload.getContext().get(PO_LINE_KEY), PoLine.class));
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
      .withEventType(DI_INCOMING_MARC_BIB_FOR_ORDER_PARSED.value())
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
      .withEventType(DI_INCOMING_MARC_BIB_FOR_ORDER_PARSED.value())
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
      .withEventType(DI_INCOMING_MARC_BIB_FOR_ORDER_PARSED.value())
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
        .withCurrentNode(profileSnapshotWrapper.getChildSnapshotWrappers().get(0))
        .withJobExecutionId(jobExecutionJson.getString(ID_FIELD))
        .withEventType(DI_INCOMING_MARC_BIB_FOR_ORDER_PARSED.value())
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
        addMockEntry(ORGANIZATIONS_MOCK, organization);
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

  @Test
  public void shouldCreatePendingOrderAndMapVendorMaterialSupplierAndAccessProviderFieldsWhenIncomingOrganizationCodeIsValid() throws InterruptedException {
    // given
    MappingProfile mappingProfile = new MappingProfile()
      .withId(UUID.randomUUID().toString())
      .withIncomingRecordType(MARC_BIBLIOGRAPHIC)
      .withExistingRecordType(ORDER)
      .withMappingDetails(new MappingDetail()
        .withMappingFields(new ArrayList<>(List.of(
          new MappingRule().withPath("order.po.workflowStatus").withValue("\"Pending\"").withEnabled("true"),
          new MappingRule().withPath("order.po.orderType").withValue("\"One-Time\"").withEnabled("true"),
          new MappingRule().withName("vendor").withPath("order.po.vendor").withValue("\"OrgCode\"").withEnabled("true"),
          new MappingRule().withName("materialSupplier").withPath("order.poLine.physical.materialSupplier").withValue("\"ORGCode\"").withEnabled("true"),
          new MappingRule().withName("accessProvider").withPath("order.poLine.eresource.accessProvider").withValue("\"OrGCoDe\"").withEnabled("true"),
          new MappingRule().withPath("order.po.approved").withValue("\"true\"").withEnabled("true"),
          new MappingRule().withPath("order.poLine.titleOrPackage").withValue("245$a").withEnabled("true"),
          new MappingRule().withPath("order.poLine.cost.currency").withValue("\"USD\"").withEnabled("true"),
          new MappingRule().withPath("order.poLine.orderFormat").withValue("\"Physical Resource\"").withEnabled("true"),
          new MappingRule().withPath("order.poLine.checkinItems").withValue("\"true\"").withEnabled("true"),
          new MappingRule().withPath("order.poLine.acquisitionMethod").withValue("\"Purchase\"").withEnabled("true")
            .withAcceptedValues(new HashMap<>(Map.of(
              "df26d81b-9d63-4ff8-bf41-49bf75cfa70e", "Purchase"
            )))))));

    ProfileSnapshotWrapper profileSnapshotWrapper = buildProfileSnapshotWrapper(jobProfile, actionProfile, mappingProfile);
    addMockEntry(JOB_PROFILE_SNAPSHOTS_MOCK, profileSnapshotWrapper);

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withJobExecutionId(jobExecutionJson.getString(ID_FIELD))
      .withCurrentNode(profileSnapshotWrapper.getChildSnapshotWrappers().get(0))
      .withEventType(DI_INCOMING_MARC_BIB_FOR_ORDER_PARSED.value())
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
    CompositePurchaseOrder createdOrder = Json.decodeValue(eventPayload.getContext().get(ORDER.value()), CompositePurchaseOrder.class);
    assertNotNull(createdOrder.getVendor());
    assertEquals(createdOrder.getVendor(), organization.getId());

    PoLine poLine = Json.decodeValue(eventPayload.getContext().get(PO_LINE_KEY), PoLine.class);
    assertNotNull(poLine.getPhysical());
    assertEquals(poLine.getPhysical().getMaterialSupplier(), organization.getId());

    assertNotNull(poLine.getEresource());
    assertEquals(poLine.getEresource().getAccessProvider(), organization.getId());
  }

  @Test
  public void shouldCreatePendingOrderAndNotMapVendorMaterialSupplierAndAccessProviderFieldsWhenIncomingOrganizationCodeIsNotValid() throws InterruptedException {
    // given
    MappingProfile mappingProfile = new MappingProfile()
      .withId(UUID.randomUUID().toString())
      .withIncomingRecordType(MARC_BIBLIOGRAPHIC)
      .withExistingRecordType(ORDER)
      .withMappingDetails(new MappingDetail()
        .withMappingFields(new ArrayList<>(List.of(
          new MappingRule().withPath("order.po.workflowStatus").withValue("\"Pending\"").withEnabled("true"),
          new MappingRule().withPath("order.po.orderType").withValue("\"One-Time\"").withEnabled("true"),
          new MappingRule().withName("vendor").withPath("order.po.vendor").withValue("\"InvalidCode\"").withEnabled("true"),
          new MappingRule().withName("materialSupplier").withPath("order.poLine.physical.materialSupplier").withValue("\"InvalidCode\"").withEnabled("true"),
          new MappingRule().withName("accessProvider").withPath("order.poLine.eresource.accessProvider").withValue("\"InvalidCode\"").withEnabled("true"),
          new MappingRule().withPath("order.po.approved").withValue("\"true\"").withEnabled("true"),
          new MappingRule().withPath("order.poLine.titleOrPackage").withValue("245$a").withEnabled("true"),
          new MappingRule().withPath("order.poLine.cost.currency").withValue("\"USD\"").withEnabled("true"),
          new MappingRule().withPath("order.poLine.orderFormat").withValue("\"Physical Resource\"").withEnabled("true"),
          new MappingRule().withPath("order.poLine.checkinItems").withValue("\"true\"").withEnabled("true"),
          new MappingRule().withPath("order.poLine.acquisitionMethod").withValue("\"Purchase\"").withEnabled("true")
            .withAcceptedValues(new HashMap<>(Map.of(
              "df26d81b-9d63-4ff8-bf41-49bf75cfa70e", "Purchase"
            )))))));

    ProfileSnapshotWrapper profileSnapshotWrapper = buildProfileSnapshotWrapper(jobProfile, actionProfile, mappingProfile);
    addMockEntry(JOB_PROFILE_SNAPSHOTS_MOCK, profileSnapshotWrapper);

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withJobExecutionId(jobExecutionJson.getString(ID_FIELD))
      .withCurrentNode(profileSnapshotWrapper.getChildSnapshotWrappers().get(0))
      .withEventType(DI_INCOMING_MARC_BIB_FOR_ORDER_PARSED.value())
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
    CompositePurchaseOrder createdOrder = Json.decodeValue(eventPayload.getContext().get(ORDER.value()), CompositePurchaseOrder.class);
    assertNull(createdOrder.getVendor());

    PoLine poLine = Json.decodeValue(eventPayload.getContext().get(PO_LINE_KEY), PoLine.class);
    assertNotNull(poLine.getPhysical());
    assertNull(poLine.getPhysical().getMaterialSupplier());

    assertNull(poLine.getEresource());
  }

  @Test
  public void shouldCreatePoLineWithFundIdAndFundCode() throws InterruptedException {
    // given
    String expectedFundId = "7fbd5d84-62d1-44c6-9c45-6cb173998bbd";
    String expectedFundCode = "AFRICAHIST";
    double expectedDistributionValue = 100;

    MappingRule fundDistributionsRule = new MappingRule()
      .withName("fundDistribution")
      .withPath("order.poLine.fundDistribution[]")
      .withEnabled("true")
      .withRepeatableFieldAction(EXTEND_EXISTING)
      .withSubfields(List.of(new RepeatableSubfieldMapping()
        .withOrder(0)
        .withPath("order.poLine.fundDistribution[]")
        .withFields(List.of(
          new MappingRule().withPath("order.poLine.fundDistribution[].fundId").withName("fundId")
            .withValue("\"African (History) (AFRICAHIST)\""),
          new MappingRule().withPath("order.poLine.fundDistribution[].value").withValue("\"100\""),
          new MappingRule().withPath("order.poLine.fundDistribution[].distributionType").withValue("\"percentage\"")))
      ));

    mappingProfile.getMappingDetails().getMappingFields().add(fundDistributionsRule);
    ProfileSnapshotWrapper profileSnapshotWrapper = buildProfileSnapshotWrapper(jobProfile, actionProfile, mappingProfile);
    Fund fund = new Fund().withId(expectedFundId).withCode(expectedFundCode).withName("African (History)");
    addMockEntry(JOB_PROFILE_SNAPSHOTS_MOCK, profileSnapshotWrapper);
    addMockEntry(FUNDS, fund);

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withJobExecutionId(jobExecutionJson.getString(ID_FIELD))
      .withEventType(DI_INCOMING_MARC_BIB_FOR_ORDER_PARSED.value())
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
    PoLine createdPoLine = verifyPoLine(eventPayload);
    assertNotNull(createdPoLine.getFundDistribution());
    assertEquals(1, createdPoLine.getFundDistribution().size());
    assertEquals(expectedFundId, createdPoLine.getFundDistribution().get(0).getFundId());
    assertEquals(expectedFundCode, createdPoLine.getFundDistribution().get(0).getCode());
    assertEquals(expectedDistributionValue, createdPoLine.getFundDistribution().get(0).getValue());
    assertEquals(PERCENTAGE, createdPoLine.getFundDistribution().get(0).getDistributionType());
  }

  @Test
  public void shouldReturnFailedByDuplicateEventExceptionFutureWhenRecordIdIsDuplicated(TestContext context) {
    Async async = context.async();
    ProfileSnapshotWrapper profileSnapshotWrapper = buildProfileSnapshotWrapper(jobProfile, actionProfile, mappingProfile);
    addMockEntry(JOB_PROFILE_SNAPSHOTS_MOCK, profileSnapshotWrapper);

    String recordId = UUID.randomUUID().toString();

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withCurrentNode(profileSnapshotWrapper.getChildSnapshotWrappers().get(0))
      .withEventType(DI_INCOMING_MARC_BIB_FOR_ORDER_PARSED.value())
      .withJobExecutionId(jobExecutionJson.getString(ID_FIELD))
      .withTenant(TENANT_ID)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN)
      .withContext(new HashMap<>() {{
        put(MARC_BIBLIOGRAPHIC.value(), Json.encode(record));
        put(JOB_PROFILE_SNAPSHOT_ID_KEY, profileSnapshotWrapper.getId());
        put(RECORD_ID_HEADER, recordId);
      }});

    CreateOrderEventHandler createOrderHandler = getBeanFromSpringContext(vertx, CreateOrderEventHandler.class);
    vertx.runOnContext(event -> Future.fromCompletionStage(createOrderHandler.handle(dataImportEventPayload))
      .onComplete(e -> Future.fromCompletionStage(createOrderHandler.handle(dataImportEventPayload))
        .onComplete(context.asyncAssertFailure(th -> {
          context.assertTrue(th instanceof DuplicateEventException);
          async.complete();
        }))));
  }

  @Test
  public void shouldNotReturnFailedByDuplicateEventExceptionFutureWhenRecordIdIsDifferent(TestContext context) {
    Async async = context.async();
    ProfileSnapshotWrapper profileSnapshotWrapper = buildProfileSnapshotWrapper(jobProfile, actionProfile, mappingProfile);
    addMockEntry(JOB_PROFILE_SNAPSHOTS_MOCK, profileSnapshotWrapper);

    String recordId = UUID.randomUUID().toString();
    String newRecordId = UUID.randomUUID().toString();

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withCurrentNode(profileSnapshotWrapper.getChildSnapshotWrappers().get(0))
      .withEventType(DI_INCOMING_MARC_BIB_FOR_ORDER_PARSED.value())
      .withJobExecutionId(jobExecutionJson.getString(ID_FIELD))
      .withTenant(TENANT_ID)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN)
      .withContext(new HashMap<>() {{
        put(MARC_BIBLIOGRAPHIC.value(), Json.encode(record));
        put(JOB_PROFILE_SNAPSHOT_ID_KEY, profileSnapshotWrapper.getId());
        put(RECORD_ID_HEADER, recordId);
      }});

    CreateOrderEventHandler createOrderHandler = getBeanFromSpringContext(vertx, CreateOrderEventHandler.class);
    vertx.runOnContext(event -> Future.fromCompletionStage(createOrderHandler.handle(dataImportEventPayload))
      .onComplete(e -> {
        dataImportEventPayload.withContext(new HashMap<>() {{
          put(MARC_BIBLIOGRAPHIC.value(), Json.encode(record));
          put(JOB_PROFILE_SNAPSHOT_ID_KEY, profileSnapshotWrapper.getId());
          put(RECORD_ID_HEADER, newRecordId);
        }});
        Future.fromCompletionStage(createOrderHandler.handle(dataImportEventPayload));
      })
      .onComplete(context.asyncAssertSuccess(th -> {
        context.assertEquals(th.getContext().get(RECORD_ID_HEADER), newRecordId);
        async.complete();
      })));
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

  private ProfileSnapshotWrapper buildProfileSnapshotWrapperWithInventoryActions(JobProfile jobProfile, ActionProfile actionProfile,
                                                                                 MappingProfile mappingProfile, List<ActionProfile> inventoryActionProfiles) {
    ProfileSnapshotWrapper profileSnapshotWrapper = new ProfileSnapshotWrapper()
      .withId(UUID.randomUUID().toString())
      .withProfileId(jobProfile.getId())
      .withContentType(JOB_PROFILE)
      .withContent(JsonObject.mapFrom(jobProfile).getMap())
      .withChildSnapshotWrappers(new ArrayList<>(List.of(
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
              .withContent(JsonObject.mapFrom(mappingProfile).getMap()))))));

    int wrapperOrder = 0;
    for (ActionProfile inventoryActionProfile : inventoryActionProfiles) {
      ProfileSnapshotWrapper actionProfileWrapper = new ProfileSnapshotWrapper()
        .withId(UUID.randomUUID().toString())
        .withProfileId(this.actionProfile.getId())
        .withOrder(++wrapperOrder)
        .withContentType(ACTION_PROFILE)
        .withContent(JsonObject.mapFrom(inventoryActionProfile).getMap());

      profileSnapshotWrapper.getChildSnapshotWrappers().add(actionProfileWrapper);
    }

    return profileSnapshotWrapper;
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
    return Json.decodeValue(eventPayload.getContext().get(PO_LINE_KEY), PoLine.class).getPurchaseOrderId();
  }

  private CompositePurchaseOrder verifyOrder(DataImportEventPayload eventPayload) {
    assertNotNull(eventPayload.getContext().get(ORDER.value()));
    CompositePurchaseOrder createdOrder = Json.decodeValue(eventPayload.getContext().get(ORDER.value()), CompositePurchaseOrder.class);
    assertNotNull(createdOrder.getId());
    assertEquals(CompositePurchaseOrder.WorkflowStatus.PENDING, createdOrder.getWorkflowStatus());
    assertNotNull(createdOrder.getVendor());
    assertEquals(createdOrder.getVendor(), organization.getId());
    assertEquals(CompositePurchaseOrder.OrderType.ONE_TIME, createdOrder.getOrderType());
    return createdOrder;
  }

  private PoLine verifyPoLine(DataImportEventPayload eventPayload) {
    assertNotNull(eventPayload.getContext().get(PO_LINE_KEY));
    PoLine poLine = Json.decodeValue(eventPayload.getContext().get(PO_LINE_KEY), PoLine.class);
    assertNotNull(poLine.getId());
    assertNotNull(poLine.getTitleOrPackage());
    assertNotNull(poLine.getPurchaseOrderId());
    assertEquals(PoLine.Source.MARC, poLine.getSource());
    assertEquals(PoLine.OrderFormat.PHYSICAL_RESOURCE, poLine.getOrderFormat());
    assertTrue(poLine.getCheckinItems());
    assertNotNull(poLine.getAcquisitionMethod());
    return poLine;
  }

  private void registerCreateOrderEventHandler() {
    // Overrides events handlers registration to avoid messages produced by these tests
    // from being processed by OrderPostProcessingHandler which in turn causes side effects on these tests' execution
    EventManager.clearEventHandlers();
    EventManager.registerEventHandler(getBeanFromSpringContext(vertx, CreateOrderEventHandler.class));
  }
}
