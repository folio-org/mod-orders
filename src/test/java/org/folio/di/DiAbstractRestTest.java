package org.folio.di;

import static java.nio.charset.StandardCharsets.UTF_8;
import static org.folio.dataimport.util.RestUtil.OKAPI_TENANT_HEADER;
import static org.folio.dataimport.util.RestUtil.OKAPI_URL_HEADER;
import static org.folio.kafka.KafkaTopicNameHelper.getDefaultNameSpace;
import static org.folio.rest.util.OkapiConnectionParams.OKAPI_TOKEN_HEADER;

import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.List;
import java.util.Optional;
import java.util.Properties;
import java.util.UUID;
import lombok.SneakyThrows;
import io.vertx.ext.unit.junit.VertxUnitRunner;
import org.apache.kafka.clients.consumer.ConsumerConfig;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.clients.consumer.ConsumerRecords;
import org.apache.kafka.clients.consumer.KafkaConsumer;
import org.apache.kafka.clients.producer.KafkaProducer;
import org.apache.kafka.clients.producer.ProducerConfig;
import org.apache.kafka.clients.producer.ProducerRecord;
import org.apache.kafka.clients.producer.RecordMetadata;
import org.apache.kafka.common.header.internals.RecordHeader;
import org.apache.kafka.common.serialization.StringDeserializer;
import org.apache.kafka.common.serialization.StringSerializer;
import org.folio.DataImportEventPayload;
import org.folio.TestConfig;
import org.folio.kafka.KafkaTopicNameHelper;
import org.folio.postgres.testing.PostgresTesterContainer;
import org.folio.processing.events.EventManager;
import org.folio.rest.RestVerticle;
import org.folio.rest.client.TenantClient;
import org.folio.rest.jaxrs.model.Event;
import org.folio.rest.jaxrs.model.TenantAttributes;
import org.folio.rest.jaxrs.model.TenantJob;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.NetworkUtils;
import org.folio.rest.util.OkapiConnectionParams;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.runner.RunWith;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.testcontainers.kafka.KafkaContainer;
import com.github.tomakehurst.wiremock.admin.NotFoundException;
import com.github.tomakehurst.wiremock.common.ConsoleNotifier;
import com.github.tomakehurst.wiremock.common.FileSource;
import com.github.tomakehurst.wiremock.core.WireMockConfiguration;
import com.github.tomakehurst.wiremock.extension.Parameters;
import com.github.tomakehurst.wiremock.extension.ResponseTransformer;
import com.github.tomakehurst.wiremock.http.Request;
import com.github.tomakehurst.wiremock.http.Response;
import com.github.tomakehurst.wiremock.junit.WireMockRule;
import io.restassured.builder.RequestSpecBuilder;
import io.restassured.http.ContentType;
import io.restassured.specification.RequestSpecification;
import io.vertx.core.DeploymentOptions;
import io.vertx.core.Vertx;
import io.vertx.core.impl.VertxImpl;
import io.vertx.core.json.Json;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.unit.Async;
import io.vertx.ext.unit.TestContext;
import io.vertx.kafka.client.consumer.KafkaConsumerRecord;
import io.vertx.kafka.client.consumer.impl.KafkaConsumerRecordImpl;

@RunWith(VertxUnitRunner.class)
public abstract class DiAbstractRestTest {

  protected static Vertx vertx;
  protected static final String okapiUserIdHeader = UUID.randomUUID().toString();
  private static final String KAFKA_HOST = "KAFKA_HOST";
  private static final String KAFKA_PORT = "KAFKA_PORT";
  private static final String KAFKA_ENV = "ENV";
  protected static final String KAFKA_ENV_VALUE = "test-env";
  public static final String OKAPI_URL_ENV = "OKAPI_URL";
  private static final int PORT = NetworkUtils.nextFreePort();
  protected static final String OKAPI_URL = "http://localhost:" + PORT;
  private static final String HTTP_PORT = "http.port";
  protected static int port;
  protected static final String TENANT_ID = "diku";
  protected static final String TOKEN = "token";
  protected static RequestSpecification spec;
  private static final String JOB_EXECUTION_ID_HEADER = "jobExecutionId";
  private static final String RECORDS_PROCESSED_TABLE = "processed_records";

  public static KafkaContainer kafkaContainer = TestConfig.getKafkaContainer();
  protected static KafkaProducer<String, String> kafkaProducer;

  @Rule
  public WireMockRule mockServer = new WireMockRule(
    WireMockConfiguration.wireMockConfig()
      .dynamicPort()
      .notifier(new ConsoleNotifier(true))
      .extensions(new RequestToResponseTransformer())
  );

  @BeforeClass
  public static void setUpClass(final TestContext context) {
    vertx = Vertx.vertx();
    kafkaContainer.start();
    System.setProperty(KAFKA_HOST, kafkaContainer.getHost());
    System.setProperty(KAFKA_PORT, kafkaContainer.getFirstMappedPort() + "");
    System.setProperty(KAFKA_ENV, KAFKA_ENV_VALUE);
    System.setProperty(OKAPI_URL_ENV, OKAPI_URL);
    kafkaProducer = createKafkaProducer();
    runDatabase();
    deployVerticle(context);
  }

  private static KafkaProducer<String, String> createKafkaProducer() {
    Properties producerProperties = new Properties();
    producerProperties.setProperty(ProducerConfig.BOOTSTRAP_SERVERS_CONFIG, kafkaContainer.getBootstrapServers());
    producerProperties.setProperty(ProducerConfig.KEY_SERIALIZER_CLASS_CONFIG, StringSerializer.class.getName());
    producerProperties.setProperty(ProducerConfig.VALUE_SERIALIZER_CLASS_CONFIG, StringSerializer.class.getName());
    return new KafkaProducer<>(producerProperties);
  }

  protected KafkaConsumer<String, String> createKafkaConsumer() {
    Properties consumerProperties = new Properties();
    consumerProperties.put(ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG, kafkaContainer.getBootstrapServers());
    consumerProperties.put(ConsumerConfig.KEY_DESERIALIZER_CLASS_CONFIG, StringDeserializer.class.getName());
    consumerProperties.put(ConsumerConfig.VALUE_DESERIALIZER_CLASS_CONFIG, StringDeserializer.class.getName());
    consumerProperties.put(ConsumerConfig.GROUP_ID_CONFIG, "test-group");
    consumerProperties.put(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, "earliest");
    return new KafkaConsumer<>(consumerProperties);
  }

  private static void runDatabase() {
    PostgresClient.stopPostgresTester();
    PostgresClient.setPostgresTester(new PostgresTesterContainer());
  }

  @AfterClass
  public static void tearDownClass(final TestContext context) {
    Async async = context.async();
    EventManager.clearEventHandlers();
    vertx.close().onComplete(context.asyncAssertSuccess(res -> {
      kafkaContainer.stop();
      kafkaProducer.close();
      async.complete();
    }));
  }

  private static void deployVerticle(TestContext context) {
    Async async = context.async();
    port = NetworkUtils.nextFreePort();
    String okapiUrl = "http://localhost:" + port;
    final DeploymentOptions options = new DeploymentOptions()
      .setConfig(new JsonObject()
        .put(HTTP_PORT, port));

    TenantClient tenantClient = new TenantClient(okapiUrl, TENANT_ID, TOKEN);
    vertx.deployVerticle(RestVerticle.class.getName(), options).onComplete(res -> {
      postTenant(context, async, tenantClient);
    });
  }

  protected static void postTenant(TestContext context, Async async, TenantClient tenantClient) {
    TenantAttributes tenantAttributes = new TenantAttributes();
    tenantClient.postTenant(tenantAttributes).onComplete(res2 -> {
      if (res2.result().statusCode() == 201) {
        tenantClient.getTenantByOperationId(res2.result().bodyAsJson(TenantJob.class).getId(), 60000, context.asyncAssertSuccess(res3 -> {
          context.assertTrue(res3.bodyAsJson(TenantJob.class).getComplete());
          async.complete();
        }));
      } else {
        context.assertEquals("Failed to make post tenant. Received status code 400", res2.result().bodyAsString());
        async.complete();
      }
    });
  }

  @Before
  public void setUp(TestContext context) {
    clearTables(context);
    spec = new RequestSpecBuilder()
      .setContentType(ContentType.JSON)
      .addHeader(OKAPI_URL_HEADER, "http://localhost:" + mockServer.port())
      .addHeader(OKAPI_TENANT_HEADER, TENANT_ID)
      .addHeader(RestVerticle.OKAPI_USERID_HEADER, okapiUserIdHeader)
      .addHeader("Accept", "text/plain, application/json")
      .setBaseUri("http://localhost:" + port)
      .build();
  }

  protected String formatToKafkaTopicName(String eventType) {
    return KafkaTopicNameHelper.formatTopicName(KAFKA_ENV_VALUE, getDefaultNameSpace(), TENANT_ID, eventType);
  }

  protected ProducerRecord<String, String> prepareWithSpecifiedEventPayload(String eventType, String eventPayload) {
    String topic = formatToKafkaTopicName(eventType);
    Event event = new Event().withId(UUID.randomUUID().toString()).withEventPayload(eventPayload);
    ProducerRecord<String, String> kafkaRecord = new ProducerRecord<>(topic, "key", Json.encode(event));
    addHeader(kafkaRecord, OkapiConnectionParams.OKAPI_TENANT_HEADER, TENANT_ID);
    addHeader(kafkaRecord, OKAPI_URL_HEADER, mockServer.baseUrl());
    addHeader(kafkaRecord, JOB_EXECUTION_ID_HEADER, UUID.randomUUID().toString());
    return kafkaRecord;
  }

  protected static void addHeader(ProducerRecord<String, String> producerRecord, String key, String value) {
    producerRecord.headers().add(key, value.getBytes(UTF_8));
  }

  @SneakyThrows
  protected RecordMetadata send(ProducerRecord<String, String> producerRecord) {
    return kafkaProducer.send(producerRecord).get();
  }

  protected KafkaConsumerRecord<String, String> buildKafkaConsumerRecord(DataImportEventPayload record) {
    String topic = KafkaTopicNameHelper.formatTopicName("folio", getDefaultNameSpace(), TENANT_ID, record.getEventType());
    Event event = new Event().withEventPayload(Json.encode(record));
    ConsumerRecord<String, String> consumerRecord = buildConsumerRecord(topic, event);
    return new KafkaConsumerRecordImpl<>(consumerRecord);
  }

  protected ConsumerRecord<String, String> buildConsumerRecord(String topic, Event event) {
    ConsumerRecord<java.lang.String, java.lang.String> consumerRecord = new ConsumerRecord<>("folio", 0, 0, topic, Json.encode(event));
    consumerRecord.headers().add(new RecordHeader(OkapiConnectionParams.OKAPI_TENANT_HEADER, TENANT_ID.getBytes(StandardCharsets.UTF_8)));
    consumerRecord.headers().add(new RecordHeader(OKAPI_URL_HEADER, ("http://localhost:" + mockServer.port()).getBytes(StandardCharsets.UTF_8)));
    consumerRecord.headers().add(new RecordHeader(OKAPI_TOKEN_HEADER, (TOKEN).getBytes(StandardCharsets.UTF_8)));
    return consumerRecord;
  }

  protected  <T> T getBeanFromSpringContext(Vertx vtx, Class<T> clazz) {

    String parentVerticleUUID = vertx.deploymentIDs().stream()
      .filter(v -> !((VertxImpl) vertx).getDeployment(v).isChild())
      .findFirst()
      .orElseThrow(() -> new NotFoundException("Couldn't find the parent verticle."));

    Optional<Object> context = Optional.of(((VertxImpl) vtx).getDeployment(parentVerticleUUID).getContexts().stream()
        .findFirst().map(v -> v.get("springContext")))
      .orElseThrow(() -> new NotFoundException("Couldn't find the spring context."));

    if (context.isPresent()) {
      return ((AnnotationConfigApplicationContext) context.get()).getBean(clazz);
    }
    throw new NotFoundException(String.format("Couldn't find bean %s", clazz.getName()));
  }

  public String observeTopic(String topic) {
    ConsumerRecords<String, String> records;
    try (var kafkaConsumer = createKafkaConsumer()) {
      kafkaConsumer.subscribe(List.of(topic));
      records = kafkaConsumer.poll(Duration.ofSeconds(30));
    }
    return records.iterator().next().value();
  }

  private void clearTables(TestContext context) {
    PostgresClient pgClient = PostgresClient.getInstance(vertx, TENANT_ID);
    pgClient.delete(RECORDS_PROCESSED_TABLE, new Criterion(), context.asyncAssertSuccess());
  }

  public static class RequestToResponseTransformer extends ResponseTransformer {

    public static final String NAME = "request-to-response-transformer";

    @Override
    public Response transform(Request request, Response response, FileSource files, Parameters parameters) {
      return Response.Builder.like(response).but().body(request.getBody()).build();
    }

    @Override
    public String getName() {
      return NAME;
    }

    @Override
    public boolean applyGlobally() {
      return false;
    }
  }
}
