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
import net.mguenther.kafka.junit.*;
import org.apache.commons.collections4.CollectionUtils;
import org.folio.kafka.KafkaTopicNameHelper;
import org.folio.rest.RestVerticle;
import org.folio.rest.jaxrs.model.Event;
import org.folio.rest.tools.utils.NetworkUtils;
import org.folio.rest.util.OkapiConnectionParams;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import static java.nio.charset.StandardCharsets.UTF_8;
import static java.util.Collections.singletonList;
import static net.mguenther.kafka.junit.EmbeddedKafkaCluster.provisionWith;
import static net.mguenther.kafka.junit.EmbeddedKafkaClusterConfig.defaultClusterConfig;
import static org.folio.dataimport.util.RestUtil.OKAPI_TENANT_HEADER;
import static org.folio.dataimport.util.RestUtil.OKAPI_URL_HEADER;
import static org.folio.kafka.KafkaTopicNameHelper.getDefaultNameSpace;

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
  private static final String JOB_EXECUTION_ID_HEADER = "jobExecutionId";
  protected static RequestSpecification spec;

  public static EmbeddedKafkaCluster kafkaCluster;

  @Rule
  public WireMockRule snapshotMockServer = new WireMockRule(
    WireMockConfiguration.wireMockConfig()
      .dynamicPort()
      .notifier(new ConsoleNotifier(true))
      .extensions(new RequestToResponseTransformer())
  );

  @BeforeClass
  public static void setUpClass(final TestContext context) throws Exception {
    vertx = Vertx.vertx();
    kafkaCluster = provisionWith(defaultClusterConfig());
    kafkaCluster.start();
    String[] hostAndPort = kafkaCluster.getBrokerList().split(":");

    System.setProperty(KAFKA_HOST, hostAndPort[0]);
    System.setProperty(KAFKA_PORT, hostAndPort[1]);
    System.setProperty(KAFKA_ENV, KAFKA_ENV_VALUE);
    System.setProperty(OKAPI_URL_ENV, OKAPI_URL);
    deployVerticle();
  }

  @AfterClass
  public static void tearDownClass(final TestContext context) {
    Async async = context.async();
    vertx.close(context.asyncAssertSuccess(res -> {
      kafkaCluster.stop();
      async.complete();
    }));
  }
  private static void deployVerticle() throws ExecutionException, InterruptedException, TimeoutException {
    port = NetworkUtils.nextFreePort();
    String okapiUrl = "http://localhost:" + port;
    final DeploymentOptions options = new DeploymentOptions()
      .setConfig(new JsonObject()
        .put(HTTP_PORT, port));
    CompletableFuture<String> deploymentComplete = new CompletableFuture<>();

    vertx.deployVerticle(RestVerticle.class.getName(), options, res -> {
      if(res.succeeded()) {
        deploymentComplete.complete(res.result());
      }
      else {
        deploymentComplete.completeExceptionally(res.cause());
      }
    });
    deploymentComplete.get(60, TimeUnit.SECONDS);
  }

  @Before
  public void setUp(TestContext context) throws IOException {
    spec = new RequestSpecBuilder()
      .setContentType(ContentType.JSON)
      .addHeader(OKAPI_URL_HEADER, "http://localhost:" + snapshotMockServer.port())
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
    kafkaRecord.addHeader(OKAPI_URL_HEADER, snapshotMockServer.baseUrl(), UTF_8);
    kafkaRecord.addHeader(JOB_EXECUTION_ID_HEADER, UUID.randomUUID().toString(), UTF_8);

    String topic = formatToKafkaTopicName(eventType);
    return SendKeyValues.to(topic, singletonList(kafkaRecord)).useDefaults();
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
