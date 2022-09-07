package org.folio;

import io.restassured.RestAssured;
import io.restassured.http.Header;
import io.vertx.core.AbstractVerticle;
import io.vertx.core.Context;
import io.vertx.core.DeploymentOptions;
import io.vertx.core.Verticle;
import io.vertx.core.Vertx;
import io.vertx.core.impl.VertxImpl;
import io.vertx.core.json.JsonObject;
import net.mguenther.kafka.junit.EmbeddedKafkaCluster;
import org.folio.rest.RestVerticle;
import org.folio.rest.impl.MockServer;
import org.folio.rest.tools.utils.NetworkUtils;
import org.folio.spring.SpringContextUtil;

import java.lang.reflect.Field;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import static net.mguenther.kafka.junit.EmbeddedKafkaCluster.provisionWith;
import static net.mguenther.kafka.junit.EmbeddedKafkaClusterConfig.defaultClusterConfig;
import static org.folio.rest.impl.EventBusContextConfiguration.eventMessages;

public final class TestConfig {

  public static final int mockPort = NetworkUtils.nextFreePort();
  public static final Header X_OKAPI_URL = new Header(TestConstants.OKAPI_URL, "http://localhost:" + mockPort);

  private static final String KAFKA_HOST = "KAFKA_HOST";
  private static final String KAFKA_PORT = "KAFKA_PORT";
  private static final String KAFKA_ENV = "ENV";
  private static final String KAFKA_ENV_VALUE = "test-env";

  private static MockServer mockServer;
  public static EmbeddedKafkaCluster kafkaCluster;
  private static final Vertx vertx = Vertx.vertx();

  private TestConfig() {}

  public static void deployVerticle() throws InterruptedException, ExecutionException, TimeoutException {
    int okapiPort = NetworkUtils.nextFreePort();
    RestAssured.baseURI = "http://localhost:" + okapiPort;
    RestAssured.port = okapiPort;
    RestAssured.enableLoggingOfRequestAndResponseIfValidationFails();

    final JsonObject conf = new JsonObject();
    conf.put("http.port", okapiPort);

    final DeploymentOptions opt = new DeploymentOptions().setConfig(conf);
    CompletableFuture<String> deploymentComplete = new CompletableFuture<>();

    String[] hostAndPort = kafkaCluster.getBrokerList().split(":");
    System.setProperty(KAFKA_HOST, hostAndPort[0]);
    System.setProperty(KAFKA_PORT, hostAndPort[1]);
    System.setProperty(KAFKA_ENV, KAFKA_ENV_VALUE);

    vertx.deployVerticle(RestVerticle.class.getName(), opt, res -> {
      if(res.succeeded()) {
        deploymentComplete.complete(res.result());
      }
      else {
        deploymentComplete.completeExceptionally(res.cause());
      }
    });
    deploymentComplete.get(60, TimeUnit.SECONDS);
  }

  public static void initSpringContext(Class<?> defaultConfiguration) {
    SpringContextUtil.init(vertx, getFirstContextFromVertx(vertx), defaultConfiguration);
  }

  public static void autowireDependencies(Object target) {
    SpringContextUtil.autowireDependenciesFromFirstContext(target, getVertx());
  }

  public static void clearVertxContext() {
    Context context = getFirstContextFromVertx(vertx);
    context.remove("springContext");
  }

  public static void startMockServer() throws InterruptedException, ExecutionException, TimeoutException {
    mockServer = new MockServer(mockPort);
    mockServer.start();
  }

  public static void startKafkaMockServer() throws InterruptedException, ExecutionException, TimeoutException {
    kafkaCluster = provisionWith(defaultClusterConfig());
    kafkaCluster.start();
  }

  public static Vertx getVertx() {
    return vertx;
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
    kafkaCluster.stop();
  }

  public static void closeVertx() {
    vertx.close();
  }

  public static boolean isVerticleNotDeployed() {
    return vertx.deploymentIDs().isEmpty();
  }

  public static Context getFirstContextFromVertx(Vertx vertx) {
    return vertx.deploymentIDs().stream().flatMap((id) -> ((VertxImpl)vertx)
      .getDeployment(id).getVerticles().stream())
      .map(TestConfig::getContextWithReflection)
      .filter(Objects::nonNull)
      .findFirst()
      .orElseThrow(() -> new IllegalStateException("Spring context was not created"));
  }

  private static Context getContextWithReflection(Verticle verticle) {
    try {
      Field field = AbstractVerticle.class.getDeclaredField("context");
      field.setAccessible(true);
      return ((Context)field.get(verticle));
    } catch (NoSuchFieldException | IllegalAccessException var2) {
      return null;
    }
  }
}
