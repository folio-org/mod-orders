package org.folio.di;

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
import net.mguenther.kafka.junit.EmbeddedKafkaCluster;
import net.mguenther.kafka.junit.KeyValue;
import net.mguenther.kafka.junit.ObserveKeyValues;
import net.mguenther.kafka.junit.ReadKeyValues;
import net.mguenther.kafka.junit.SendKeyValues;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.common.header.internals.RecordHeader;
import org.folio.DataImportEventPayload;
import org.folio.kafka.KafkaTopicNameHelper;
import org.folio.postgres.testing.PostgresTesterContainer;
import org.folio.rest.RestVerticle;
import org.folio.rest.client.TenantClient;
import org.folio.rest.jaxrs.model.Event;
import org.folio.rest.jaxrs.model.TenantAttributes;
import org.folio.rest.jaxrs.model.TenantJob;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.Envs;
import org.folio.rest.tools.utils.NetworkUtils;
import org.folio.rest.util.OkapiConnectionParams;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

import static java.nio.charset.StandardCharsets.UTF_8;
import static java.util.Collections.singletonList;
import static net.mguenther.kafka.junit.EmbeddedKafkaCluster.provisionWith;
import static net.mguenther.kafka.junit.EmbeddedKafkaClusterConfig.defaultClusterConfig;
import static org.folio.dataimport.util.RestUtil.OKAPI_TENANT_HEADER;
import static org.folio.dataimport.util.RestUtil.OKAPI_URL_HEADER;
import static org.folio.kafka.KafkaTopicNameHelper.getDefaultNameSpace;
import static org.folio.rest.util.OkapiConnectionParams.OKAPI_TOKEN_HEADER;

public abstract class DiAbstractRestTest {

  protected static Vertx vertx;
  protected static final String okapiUserIdHeader = UUID.randomUUID().toString();
  private static final String KAFKA_HOST = "KAFKA_HOST";
  private static final String KAFKA_PORT = "KAFKA_PORT";
  private static final String KAFKA_ENV = "ENV";
  private static final String KAFKA_ENV_VALUE = "test-env";
  public static final String OKAPI_URL_ENV = "OKAPI_URL";
  private static final int PORT = NetworkUtils.nextFreePort();
  protected static final String OKAPI_URL = "http://localhost:" + PORT;
  private static final String HTTP_PORT = "http.port";
  private static int port;
  protected static final String TENANT_ID = "diku";
  protected static final String TOKEN = "token";
  private static final String JOB_EXECUTION_ID_HEADER = "jobExecutionId";
  private static final String RECORDS_ORDERS_TABLE = "records_orders";
  protected static RequestSpecification spec;

  public static EmbeddedKafkaCluster kafkaCluster;

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
    kafkaCluster = provisionWith(defaultClusterConfig());
    kafkaCluster.start();
    String[] hostAndPort = kafkaCluster.getBrokerList().split(":");

    System.setProperty(KAFKA_HOST, hostAndPort[0]);
    System.setProperty(KAFKA_PORT, hostAndPort[1]);
    System.setProperty(KAFKA_ENV, KAFKA_ENV_VALUE);
    System.setProperty(OKAPI_URL_ENV, OKAPI_URL);

//    Envs.setEnv(
//      "127.0.0.1",
//      5432,
//      "folio_admin",
//      "folio_admin",
//      "okapi_modules"
//    );
    runDatabase();
    deployVerticle(context);
  }

  private static void runDatabase() {
    PostgresClient.stopPostgresTester();
    PostgresClient.closeAllClients();
    PostgresClient.setPostgresTester(new PostgresTesterContainer());
  }

  @AfterClass
  public static void tearDownClass(final TestContext context) {
    Async async = context.async();
    PostgresClient.closeAllClients();
    vertx.close(context.asyncAssertSuccess(res -> {
      PostgresClient.stopPostgresTester();
      kafkaCluster.stop();
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
    vertx.deployVerticle(RestVerticle.class.getName(), options, res -> {
      TenantAttributes tenantAttributes = new TenantAttributes();
      tenantClient.postTenant(tenantAttributes).onComplete(res2 -> {
        if (res2.result().statusCode() == 201) {
          tenantClient.getTenantByOperationId(res2.result().bodyAsJson(TenantJob.class).getId(), 60000, context.asyncAssertSuccess(res3 -> {
            context.assertTrue(res3.bodyAsJson(TenantJob.class).getComplete());
            async.complete();
          }));
        } else if (res2.result().statusCode() != 204) {
          context.assertEquals("Failed to make post tenant. Received status code 400", res2.result().bodyAsString());
        }
        async.complete();
      });
    });
  }

  @Before
  public void setUp(TestContext context) throws IOException {
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

  protected SendKeyValues<String, String> prepareWithSpecifiedEventPayload(String eventType, String eventPayload) {

    Event event = new Event().withId(UUID.randomUUID().toString()).withEventPayload(eventPayload);
    KeyValue<String, String> kafkaRecord = new KeyValue<>("key", Json.encode(event));
    kafkaRecord.addHeader(OkapiConnectionParams.OKAPI_TENANT_HEADER, TENANT_ID, UTF_8);
    kafkaRecord.addHeader(OKAPI_URL_HEADER, mockServer.baseUrl(), UTF_8);
    kafkaRecord.addHeader(JOB_EXECUTION_ID_HEADER, UUID.randomUUID().toString(), UTF_8);

    String topic = formatToKafkaTopicName(eventType);
    return SendKeyValues.to(topic, singletonList(kafkaRecord)).useDefaults();
  }

  protected KafkaConsumerRecord<String, String> buildKafkaConsumerRecord(DataImportEventPayload record) {
    String topic = KafkaTopicNameHelper.formatTopicName("folio", getDefaultNameSpace(), TENANT_ID, record.getEventType());
    Event event = new Event().withEventPayload(Json.encode(record));
    ConsumerRecord<String, String> consumerRecord = buildConsumerRecord(topic, event);
    return new KafkaConsumerRecordImpl<>(consumerRecord);
  }

  protected ConsumerRecord<String, String> buildConsumerRecord(String topic, Event event) {
    ConsumerRecord<java.lang.String, java.lang.String> consumerRecord = new ConsumerRecord("folio", 0, 0, topic, Json.encode(event));
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

  public List<String> observeValuesAndFilterByPartOfMessage(String message, String eventType, Integer countToObserve) throws InterruptedException {
    String topicToObserve = formatToKafkaTopicName(eventType);
    List<String> result = new ArrayList<>();
    List<String> observedValues = kafkaCluster.readValues(ReadKeyValues.from(topicToObserve).build());
    if (CollectionUtils.isEmpty(observedValues)) {
      observedValues = kafkaCluster.observeValues(ObserveKeyValues.on(topicToObserve, countToObserve)
        .observeFor(30, TimeUnit.SECONDS)
        .build());
    }
    for (String observedValue : observedValues) {
      if (observedValue.contains(message)) {
        result.add(observedValue);
      }
    }
    return result;
  }

  private void clearTables(TestContext context) {
    PostgresClient pgClient = PostgresClient.getInstance(vertx, TENANT_ID);
    pgClient.delete(RECORDS_ORDERS_TABLE, new Criterion(), context.asyncAssertSuccess());
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
