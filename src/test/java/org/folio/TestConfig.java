package org.folio;

import static org.folio.rest.impl.EventBusContextConfiguration.eventMessages;

import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.folio.rest.RestVerticle;
import org.folio.rest.impl.MockServer;
import org.folio.rest.tools.utils.NetworkUtils;
import org.folio.spring.SpringContextUtil;
import org.testcontainers.kafka.KafkaContainer;
import org.testcontainers.utility.DockerImageName;
import com.github.tomakehurst.wiremock.admin.NotFoundException;

import io.restassured.RestAssured;
import io.restassured.http.Header;
import io.vertx.core.Context;
import io.vertx.core.DeploymentOptions;
import io.vertx.core.Promise;
import io.vertx.core.Verticle;
import io.vertx.core.Vertx;
import io.vertx.core.impl.VertxImpl;
import io.vertx.core.json.JsonObject;

public final class TestConfig {

  public static final int mockPort = NetworkUtils.nextFreePort();
  public static final Header X_OKAPI_URL = new Header(TestConstants.OKAPI_URL, "http://localhost:" + mockPort);

  private static final String KAFKA_HOST = "KAFKA_HOST";
  private static final String KAFKA_PORT = "KAFKA_PORT";
  private static final String KAFKA_ENV = "ENV";
  private static final String KAFKA_ENV_VALUE = "test-env";

  private static MockServer mockServer;
  public static final DockerImageName KAFKA_IMAGE_NAME = DockerImageName.parse("apache/kafka-native:3.8.0");
  public static final KafkaContainer kafkaContainer = getKafkaContainer();
  public static final Vertx vertx = Vertx.vertx();

  private TestConfig() {}

  public static void deployVerticle() throws InterruptedException, ExecutionException, TimeoutException {
    int okapiPort = NetworkUtils.nextFreePort();
    RestAssured.baseURI = "http://localhost:" + okapiPort;
    RestAssured.port = okapiPort;
    RestAssured.enableLoggingOfRequestAndResponseIfValidationFails();

    final JsonObject conf = new JsonObject();
    conf.put("http.port", okapiPort);

    final DeploymentOptions opt = new DeploymentOptions().setConfig(conf);
    Promise<String> deploymentComplete = Promise.promise();
    System.setProperty(KAFKA_HOST, kafkaContainer.getHost());
    System.setProperty(KAFKA_PORT, kafkaContainer.getFirstMappedPort() + "");
    System.setProperty(KAFKA_ENV, KAFKA_ENV_VALUE);

    vertx.deployVerticle(RestVerticle.class.getName(), opt, res -> {
      if(res.succeeded()) {
        deploymentComplete.complete(res.result());
      }
      else {
        deploymentComplete.fail(res.cause());
      }
    });
    deploymentComplete.future().toCompletionStage().toCompletableFuture().get(60, TimeUnit.SECONDS);
  }

  public static void initSpringContext(Class<?> defaultConfiguration) {
    SpringContextUtil.init(vertx, getFirstContextFromVertx(vertx), defaultConfiguration);
  }

  public static void autowireDependencies(Object target) {
    SpringContextUtil.autowireDependenciesFromFirstContext(target, getVertx());
  }

  public static Vertx getVertx() {
    return vertx;
  }

  public static void clearVertxContext() {
    Context context = getFirstContextFromVertx(vertx);
    context.remove("springContext");
  }

  public static void startMockServer() throws InterruptedException, ExecutionException, TimeoutException {
    mockServer = new MockServer(mockPort);
    mockServer.start();
  }

  public static void startKafkaMockServer() {
    kafkaContainer.start();
  }

  public static void clearServiceInteractions() {
    MockServer.serverRqRs.clear();
    eventMessages.clear();
    MockServer.serverRqQueries.clear();
  }

  public static void closeMockServer() {
    mockServer.close();
  }

  public static void closeKafkaMockServer() {
    kafkaContainer.stop();
  }

  public static void closeVertx() {
    vertx.close();
  }

  public static boolean isVerticleNotDeployed() {
    return vertx.deploymentIDs().isEmpty();
  }

  // Suppress "use try-with-resource" suggestion because in both places it is closed correctly in a deferred fashion
  @SuppressWarnings("resource")
  public static KafkaContainer getKafkaContainer() {
    return new KafkaContainer(KAFKA_IMAGE_NAME)
      .withStartupAttempts(20);
  }

  public static Context getFirstContextFromVertx(Vertx vertx) {
    return vertx.deploymentIDs().stream().flatMap((id) -> ((VertxImpl) vertx)
      .getDeployment(id).getVerticles().stream())
      .map(TestConfig::getContext)
      .filter(Objects::nonNull)
      .findFirst()
      .orElseThrow(() -> new IllegalStateException("Spring context was not created"));
  }

  private static Context getContext(Verticle verticle) {
    String parentVerticleUUID = vertx.deploymentIDs().stream()
      .filter(v -> !((VertxImpl) vertx).getDeployment(v).isChild())
      .findFirst()
      .orElseThrow(() -> new NotFoundException("Couldn't find the parent verticle."));

    Optional<Context> context = Optional.of(((VertxImpl) vertx)
        .getDeployment(parentVerticleUUID).getContexts().stream()
        .findFirst())
      .orElseThrow(() -> new NotFoundException("Couldn't find the spring context."));

    return context.orElseThrow(() -> new NotFoundException("Couldn't find the spring context."));
  }
}
