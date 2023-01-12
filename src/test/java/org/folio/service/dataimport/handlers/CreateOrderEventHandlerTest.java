package org.folio.service.dataimport.handlers;

import io.vertx.core.json.Json;
import io.vertx.core.json.JsonObject;
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
import org.folio.rest.RestConstants;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Event;
import org.folio.rest.jaxrs.model.MappingDetail;
import org.folio.rest.jaxrs.model.MappingRule;
import org.folio.rest.jaxrs.model.ProfileSnapshotWrapper;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.jupiter.api.Assertions;
import org.junit.runner.RunWith;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import static java.nio.charset.StandardCharsets.UTF_8;
import static org.folio.ActionProfile.Action.CREATE;
import static org.folio.DataImportEventTypes.DI_COMPLETED;
import static org.folio.DataImportEventTypes.DI_ERROR;
import static org.folio.DataImportEventTypes.DI_INVENTORY_INSTANCE_MATCHED;
import static org.folio.DataImportEventTypes.DI_MARC_BIB_FOR_ORDER_CREATED;
import static org.folio.DataImportEventTypes.DI_ORDER_CREATED;
import static org.folio.DataImportEventTypes.DI_SRS_MARC_BIB_RECORD_CREATED;
import static org.folio.TestConfig.closeMockServer;
import static org.folio.kafka.KafkaTopicNameHelper.getDefaultNameSpace;
import static org.folio.rest.impl.MockServer.addMockEntry;
import static org.folio.rest.jaxrs.model.EntityType.INSTANCE;
import static org.folio.rest.jaxrs.model.EntityType.MARC_BIBLIOGRAPHIC;
import static org.folio.rest.jaxrs.model.EntityType.ORDER;
import static org.folio.rest.jaxrs.model.ProfileSnapshotWrapper.ContentType.ACTION_PROFILE;
import static org.folio.rest.jaxrs.model.ProfileSnapshotWrapper.ContentType.JOB_PROFILE;
import static org.folio.rest.jaxrs.model.ProfileSnapshotWrapper.ContentType.MAPPING_PROFILE;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

@RunWith(VertxUnitRunner.class)
public class CreateOrderEventHandlerTest extends DiAbstractRestTest {

  private static final String PARSED_CONTENT = "{\"leader\":\"01314nam  22003851a 4500\",\"fields\":[{\"001\":\"ybp7406411\"},{\"245\":{\"ind1\":\"1\",\"ind2\":\"0\",\"subfields\":[{\"a\":\"titleValue\"}]}},{\"336\":{\"ind1\":\"1\",\"ind2\":\"0\",\"subfields\":[{\"b\":\"b6698d38-149f-11ec-82a8-0242ac130003\"}]}},{\"780\":{\"ind1\":\"0\",\"ind2\":\"0\",\"subfields\":[{\"t\":\"Houston oil directory\"}]}},{\"785\":{\"ind1\":\"0\",\"ind2\":\"0\",\"subfields\":[{\"t\":\"SAIS review of international affairs\"},{\"x\":\"1945-4724\"}]}},{\"500\":{\"ind1\":\" \",\"ind2\":\" \",\"subfields\":[{\"a\":\"Adaptation of Xi xiang ji by Wang Shifu.\"}]}},{\"520\":{\"ind1\":\" \",\"ind2\":\" \",\"subfields\":[{\"a\":\"Ben shu miao shu le cui ying ying he zhang sheng wei zheng qu hun yin zi you li jin qu zhe jian xin zhi hou, zhong cheng juan shu de ai qing gu shi. jie lu le bao ban hun yin he feng jian li jiao de zui e.\"}]}}]}";
  private static final String JOB_PROFILE_SNAPSHOT_ID_KEY = "JOB_PROFILE_SNAPSHOT_ID";
  private static final String ORDER_LINES_KEY = "ORDER_LINES";
  private static final String GROUP_ID = "test-consumers-group";
  private static final String RECORD_ID_HEADER = "recordId";
  private static final String INSTANCE_ID_KEY = "id";
  private static final String JOB_PROFILE_SNAPSHOTS_MOCK = "jobProfileSnapshots";

  private static final String OKAPI_URL = "http://localhost:" + TestConfig.mockPort;
  private static final String KAFKA_ENV_VALUE = "test-env";
  private static final String TENANT_ID = "test";


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
        new MappingRule().withPath("order.po.poStatus").withValue("\"Pending\"").withEnabled("true"),
        new MappingRule().withPath("order.po.orderType").withValue("\"One-Time\"").withEnabled("true"),
        new MappingRule().withPath("order.po.vendor").withValue("\"e0fb5df2-cdf1-11e8-a8d5-f2801f1b9fd1\"").withEnabled("true"),

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
      .withMappingFields(List.of(
        new MappingRule().withPath("order.po.poStatus").withValue("\"Open\"").withEnabled("true"),
        new MappingRule().withPath("order.po.orderType").withValue("\"One-Time\"").withEnabled("true"),
        new MappingRule().withPath("order.po.vendor").withValue("\"e0fb5df2-cdf1-11e8-a8d5-f2801f1b9fd1\"").withEnabled("true"),

        new MappingRule().withPath("order.po.poLinesLimit").withValue("7").withEnabled("true").withName("poLinesLimit"),

        new MappingRule().withPath("order.poLine.titleOrPackage").withValue("245$a").withEnabled("true"),
        new MappingRule().withPath("order.poLine.cost.currency").withValue("\"USD\"").withEnabled("true"),
        new MappingRule().withPath("order.poLine.orderFormat").withValue("\"Physical Resource\"").withEnabled("true"),
        new MappingRule().withPath("order.poLine.checkinItems").withValue("\"true\"").withEnabled("true"),
        new MappingRule().withPath("order.poLine.acquisitionMethod").withValue("\"Purchase\"").withEnabled("true")
          .withAcceptedValues(new HashMap<>(Map.of(
            "df26d81b-9d63-4ff8-bf41-49bf75cfa70e", "Purchase"
          ))))));

  private Record record;

  @BeforeClass
  public static void setupClass() throws ExecutionException, InterruptedException, TimeoutException {
    TestConfig.startMockServer();
  }

  @AfterClass
  public static void after() {
    closeMockServer();
  }

  @Before
  public void setup() {
    record = new Record().withId(UUID.randomUUID().toString())
      .withParsedRecord(new ParsedRecord().withContent(PARSED_CONTENT));
  }

  @Test
  public void shouldCreatePendingOrderAndPublishDiCompletedEvent() throws InterruptedException {
    // given
    ProfileSnapshotWrapper profileSnapshotWrapper = buildProfileSnapshotWrapper(jobProfile, actionProfile, mappingProfile);
    addMockEntry(JOB_PROFILE_SNAPSHOTS_MOCK, profileSnapshotWrapper);

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
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

    assertNotNull(eventPayload.getContext().get(ORDER.value()));
    CompositePurchaseOrder createdOrder = Json.decodeValue(eventPayload.getContext().get(ORDER.value()), CompositePurchaseOrder.class);
    assertNotNull(createdOrder.getId());
    assertEquals(CompositePurchaseOrder.WorkflowStatus.PENDING, createdOrder.getWorkflowStatus());
    assertNotNull(createdOrder.getVendor());
    assertEquals(CompositePurchaseOrder.OrderType.ONE_TIME, createdOrder.getOrderType());
    verifyPoLine(eventPayload);
  }

  @Test
  public void shouldCreatePendingOrderAndPublishDiCompletedEventWhenOpenStatusSpecified() throws InterruptedException {
    // given
    ProfileSnapshotWrapper profileSnapshotWrapper = buildProfileSnapshotWrapper(jobProfile, actionProfile, openOrderMappingProfile);
    addMockEntry(JOB_PROFILE_SNAPSHOTS_MOCK, profileSnapshotWrapper);

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withEventType(DI_MARC_BIB_FOR_ORDER_CREATED.value())
      .withTenant(TENANT_ID)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN)
      .withContext(new HashMap<>() {{
        put(MARC_BIBLIOGRAPHIC.value(), Json.encode(record));
        put(JOB_PROFILE_SNAPSHOT_ID_KEY, profileSnapshotWrapper.getId());
      }});

    String topic = KafkaTopicNameHelper.formatTopicName(KAFKA_ENV_VALUE, getDefaultNameSpace(), TENANT_ID, dataImportEventPayload.getEventType());
    Event event = new Event().withEventPayload(Json.encode(dataImportEventPayload));
    KeyValue<String, String> kafkaRecord = new KeyValue<>("test-key", Json.encode(event));
    kafkaRecord.addHeader(RECORD_ID_HEADER, record.getId(), UTF_8);
    kafkaRecord.addHeader(RestConstants.OKAPI_URL, dataImportEventPayload.getOkapiUrl(), UTF_8);
    SendKeyValues<String, String> request = SendKeyValues.to(topic, Collections.singletonList(kafkaRecord))
      .useDefaults();

    // when
    kafkaCluster.send(request);

    // then
    String topicToObserve = formatToKafkaTopicName(DI_COMPLETED.value());

    List<KeyValue<String, String>> observedRecords = kafkaCluster.observe(ObserveKeyValues.on(topicToObserve, 1)
      .with(ConsumerConfig.GROUP_ID_CONFIG, GROUP_ID)
      .observeFor(30, TimeUnit.SECONDS)
      .build());

    Event obtainedEvent = Json.decodeValue(observedRecords.get(0).getValue(), Event.class);
    DataImportEventPayload eventPayload = Json.decodeValue(obtainedEvent.getEventPayload(), DataImportEventPayload.class);
    assertEquals(DI_ORDER_CREATED.value(), eventPayload.getEventsChain().get(eventPayload.getEventsChain().size() - 1));

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
    JsonObject instanceJson = new JsonObject().put(INSTANCE_ID_KEY, UUID.randomUUID().toString());

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withEventType(DI_INVENTORY_INSTANCE_MATCHED.value())
      .withTenant(TENANT_ID)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN)
      .withContext(new HashMap<>() {{
        put(MARC_BIBLIOGRAPHIC.value(), Json.encode(record));
        put(INSTANCE.value(), instanceJson.encode());
        put(JOB_PROFILE_SNAPSHOT_ID_KEY, profileSnapshotWrapper.getId());
      }});

    String topic = KafkaTopicNameHelper.formatTopicName(KAFKA_ENV_VALUE, getDefaultNameSpace(), TENANT_ID, dataImportEventPayload.getEventType());
    Event event = new Event().withEventPayload(Json.encode(dataImportEventPayload));
    KeyValue<String, String> kafkaRecord = new KeyValue<>("test-key", Json.encode(event));
    kafkaRecord.addHeader(RECORD_ID_HEADER, record.getId(), UTF_8);
    kafkaRecord.addHeader(RestConstants.OKAPI_URL, dataImportEventPayload.getOkapiUrl(), UTF_8);
    SendKeyValues<String, String> request = SendKeyValues.to(topic, Collections.singletonList(kafkaRecord))
      .useDefaults();

    // when
    kafkaCluster.send(request);

    // then
    String topicToObserve = formatToKafkaTopicName(DI_COMPLETED.value());

    List<KeyValue<String, String>> observedRecords = kafkaCluster.observe(ObserveKeyValues.on(topicToObserve, 1)
      .with(ConsumerConfig.GROUP_ID_CONFIG, GROUP_ID)
      .observeFor(30, TimeUnit.SECONDS)
      .build());

    assertEquals(record.getId(), new String(observedRecords.get(0).getHeaders().lastHeader(RECORD_ID_HEADER).value(), UTF_8));
    Event obtainedEvent = Json.decodeValue(observedRecords.get(0).getValue(), Event.class);
    DataImportEventPayload eventPayload = Json.decodeValue(obtainedEvent.getEventPayload(), DataImportEventPayload.class);
    assertEquals(DI_ORDER_CREATED.value(), eventPayload.getEventsChain().get(eventPayload.getEventsChain().size() - 1));

    assertNotNull(eventPayload.getContext().get(ORDER.value()));
    CompositePurchaseOrder createdOrder = Json.decodeValue(eventPayload.getContext().get(ORDER.value()), CompositePurchaseOrder.class);
    assertNotNull(createdOrder.getId());
    assertEquals(CompositePurchaseOrder.WorkflowStatus.PENDING, createdOrder.getWorkflowStatus());
    assertNotNull(createdOrder.getVendor());
    assertEquals(CompositePurchaseOrder.OrderType.ONE_TIME, createdOrder.getOrderType());

    CompositePoLine createdPoLine = verifyPoLine(eventPayload);
    assertEquals(instanceJson.getString(INSTANCE_ID_KEY), createdPoLine.getInstanceId());
  }

  @Test
  public void shouldPublishDiErrorEventWhenHasNoSourceRecord() throws InterruptedException {
    // given
    ProfileSnapshotWrapper profileSnapshotWrapper = buildProfileSnapshotWrapper(jobProfile, actionProfile, mappingProfile);
    addMockEntry(JOB_PROFILE_SNAPSHOTS_MOCK, profileSnapshotWrapper);

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withEventType(DI_MARC_BIB_FOR_ORDER_CREATED.value())
      .withTenant(TENANT_ID)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN)
      .withContext(new HashMap<>() {{
        put(JOB_PROFILE_SNAPSHOT_ID_KEY, profileSnapshotWrapper.getId());
      }});

    Assert.assertFalse(dataImportEventPayload.getContext().containsKey(MARC_BIBLIOGRAPHIC.value()));
    String topic = KafkaTopicNameHelper.formatTopicName(KAFKA_ENV_VALUE, getDefaultNameSpace(), TENANT_ID, dataImportEventPayload.getEventType());
    Event event = new Event().withEventPayload(Json.encode(dataImportEventPayload));
    KeyValue<String, String> kafkaRecord = new KeyValue<>("test-key", Json.encode(event));
    kafkaRecord.addHeader(RestConstants.OKAPI_URL, dataImportEventPayload.getOkapiUrl(), UTF_8);
    SendKeyValues<String, String> request = SendKeyValues.to(topic, Collections.singletonList(kafkaRecord))
      .useDefaults();

    // when
    kafkaCluster.send(request);

    // then
    String topicToObserve = KafkaTopicNameHelper.formatTopicName(KAFKA_ENV_VALUE, getDefaultNameSpace(), TENANT_ID, DI_ERROR.value());

    List<KeyValue<String, String>> observedRecords = kafkaCluster.observe(ObserveKeyValues.on(topicToObserve, 1)
      .with(ConsumerConfig.GROUP_ID_CONFIG, GROUP_ID)
      .observeFor(30, TimeUnit.SECONDS)
      .build());

    Event obtainedEvent = Json.decodeValue(observedRecords.get(0).getValue(), Event.class);
    DataImportEventPayload obtainedPayload = Json.decodeValue(obtainedEvent.getEventPayload(), DataImportEventPayload.class);
    Assert.assertEquals(DI_ERROR.value(), obtainedPayload.getEventType());
    assertEquals(DI_ORDER_CREATED.value(), obtainedPayload.getEventsChain().get(obtainedPayload.getEventsChain().size() - 1));
  }

  @Test
  public void shouldCreatePoLineAndCalculateActivationDueFieldWhenActivationDueSpecified() throws InterruptedException {
    // given
    int expectedActivationDue = 3;
    String activationDueValue = LocalDate.now().plusDays(expectedActivationDue).toString();
    MappingRule activationDueRule = new MappingRule()
      .withPath("order.poLine.eresource.activationDue")
      .withValue(String.format("\"%s\"", activationDueValue))
      .withEnabled("true");

    mappingProfile.getMappingDetails().getMappingFields().add(activationDueRule);
    ProfileSnapshotWrapper profileSnapshotWrapper = buildProfileSnapshotWrapper(jobProfile, actionProfile, mappingProfile);
    addMockEntry(JOB_PROFILE_SNAPSHOTS_MOCK, profileSnapshotWrapper);

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
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

    assertNotNull(eventPayload.getContext().get(ORDER.value()));
    CompositePurchaseOrder createdOrder = Json.decodeValue(eventPayload.getContext().get(ORDER.value()), CompositePurchaseOrder.class);
    assertNotNull(createdOrder.getId());
    assertEquals(CompositePurchaseOrder.WorkflowStatus.PENDING, createdOrder.getWorkflowStatus());
    assertNotNull(createdOrder.getVendor());
    assertEquals(CompositePurchaseOrder.OrderType.ONE_TIME, createdOrder.getOrderType());

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

    assertNotNull(eventPayload.getContext().get(ORDER.value()));
    CompositePurchaseOrder createdOrder = Json.decodeValue(eventPayload.getContext().get(ORDER.value()), CompositePurchaseOrder.class);
    assertNotNull(createdOrder.getId());
    assertEquals(CompositePurchaseOrder.WorkflowStatus.PENDING, createdOrder.getWorkflowStatus());
    assertNotNull(createdOrder.getVendor());
    assertEquals(CompositePurchaseOrder.OrderType.ONE_TIME, createdOrder.getOrderType());

    CompositePoLine createdPoLine = verifyPoLine(eventPayload);
    assertEquals(expectedActivationDue, createdPoLine.getEresource().getActivationDue());
  }

  @Test
  public void shouldOverridePoLinesLimitWhenPoLinesLimitSpecifiedAtMappingProfile() throws InterruptedException {
    // given
    ProfileSnapshotWrapper profileSnapshotWrapper = buildProfileSnapshotWrapper(jobProfile, actionProfile, mappingProfile);
    addMockEntry(JOB_PROFILE_SNAPSHOTS_MOCK, profileSnapshotWrapper);

    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
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

    assertNotNull(eventPayload.getContext().get(ORDER.value()));
    CompositePurchaseOrder createdOrder = Json.decodeValue(eventPayload.getContext().get(ORDER.value()), CompositePurchaseOrder.class);
    assertNotNull(createdOrder.getId());
    assertEquals(CompositePurchaseOrder.WorkflowStatus.PENDING, createdOrder.getWorkflowStatus());
    assertNotNull(createdOrder.getVendor());
    assertEquals(CompositePurchaseOrder.OrderType.ONE_TIME, createdOrder.getOrderType());

    CompositePoLine createdPoLine = verifyPoLine(eventPayload);
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
      .withFolioRecord(ActionProfile.FolioRecord.INSTANCE);

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
    kafkaRecord.addHeader(RECORD_ID_HEADER, record.getId(), UTF_8);
    kafkaRecord.addHeader(RestConstants.OKAPI_URL, payload.getOkapiUrl(), UTF_8);
    String topic = formatToKafkaTopicName(payload.getEventType());
    return SendKeyValues.to(topic, Collections.singletonList(kafkaRecord)).useDefaults();
  }

  private DataImportEventPayload observeEvent(String eventType) throws InterruptedException {
    String topicToObserve = formatToKafkaTopicName(eventType);
    List<KeyValue<String, String>> observedRecords = kafkaCluster.observe(ObserveKeyValues.on(topicToObserve, 1)
      .with(ConsumerConfig.GROUP_ID_CONFIG, GROUP_ID)
      .observeFor(30, TimeUnit.SECONDS)
      .build());

    assertEquals(record.getId(), new String(observedRecords.get(0).getHeaders().lastHeader(RECORD_ID_HEADER).value(), UTF_8));
    Event obtainedEvent = Json.decodeValue(observedRecords.get(0).getValue(), Event.class);
    return Json.decodeValue(obtainedEvent.getEventPayload(), DataImportEventPayload.class);
  }

  private CompositePoLine verifyPoLine(DataImportEventPayload eventPayload) {
    assertNotNull(eventPayload.getContext().get(ORDER_LINES_KEY));
    CompositePoLine poLine = Json.decodeValue(eventPayload.getContext().get(ORDER_LINES_KEY), CompositePoLine.class);
    assertNotNull(poLine.getId());
    assertNotNull(poLine.getTitleOrPackage());
    assertEquals(CompositePoLine.Source.MARC, poLine.getSource());
    assertEquals(CompositePoLine.OrderFormat.PHYSICAL_RESOURCE, poLine.getOrderFormat());
    assertTrue(poLine.getCheckinItems());
    assertNotNull(poLine.getAcquisitionMethod());
    return poLine;
  }
}
