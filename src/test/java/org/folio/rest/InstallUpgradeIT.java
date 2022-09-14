package org.folio.rest;

import io.restassured.RestAssured;
import io.restassured.builder.RequestSpecBuilder;
import io.restassured.http.ContentType;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.KafkaContainer;
import org.testcontainers.containers.Network;
import org.testcontainers.containers.output.Slf4jLogConsumer;
import org.testcontainers.images.builder.ImageFromDockerfile;
import org.testcontainers.utility.DockerImageName;

import java.nio.file.Path;

import static io.restassured.RestAssured.given;
import static io.restassured.RestAssured.when;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;

/**
 * Check the shaded fat uber jar and Dockerfile:
 *
 * <p>Test /admin/health.
 *
 * <p>Test that logging works.
 *
 * <p>Test installation and migration with smoke test.
 */
public class InstallUpgradeIT {

  private static final Logger LOG = LoggerFactory.getLogger(InstallUpgradeIT.class);
  private static final boolean IS_LOG_ENABLED = false;
  private static final Network NETWORK = Network.newNetwork();

  @ClassRule
  public static final KafkaContainer KAFKA =
    new KafkaContainer(DockerImageName.parse("confluentinc/cp-kafka:7.0.3"))
      .withNetwork(NETWORK)
      .withNetworkAliases("mykafka");

  @ClassRule
  public static final GenericContainer<?> MOD_MIS =
    new GenericContainer<>(
      new ImageFromDockerfile("mod-orders").withFileFromPath(".", Path.of(".")))
      .withNetwork(NETWORK)
      .withExposedPorts(8081)
      .withEnv("KAFKA_HOST", "mykafka")
      .withEnv("KAFKA_PORT", "9092");

  @BeforeClass
  public static void beforeClass() {
    RestAssured.reset();
    RestAssured.enableLoggingOfRequestAndResponseIfValidationFails();
    RestAssured.baseURI = "http://" + MOD_MIS.getHost() + ":" + MOD_MIS.getFirstMappedPort();
    if (IS_LOG_ENABLED) {
      KAFKA.followOutput(new Slf4jLogConsumer(LOG).withSeparateOutputStreams());
      MOD_MIS.followOutput(new Slf4jLogConsumer(LOG).withSeparateOutputStreams());
    }
  }

  @Before
  public void beforeEach() {
    RestAssured.requestSpecification = null;
  }

  @Test
  public void health() {
    // request without X-Okapi-Tenant
    when().
      get("/admin/health").
      then().
      statusCode(200).
      body(is("\"OK\""));
  }

  @Test
  public void installAndUpgrade() {
    setTenant("latest");

    postTenant(new JsonObject()
      .put("module_to", "999999.0.0")
      .put("parameters", new JsonArray()
        .add(new JsonObject().put("key", "loadReference").put("value", "true"))
        .add(new JsonObject().put("key", "loadSample").put("value", "true"))));

    // migrate from 0.0.0, migration should be idempotent
    postTenant(new JsonObject().put("module_to", "999999.0.0").put("module_from", "0.0.0"));
  }

  /**
   * Test logging. It broke several times caused by dependency order in pom.xml or by configuration:
   * <a href="https://issues.folio.org/browse/EDGPATRON-90">https://issues.folio.org/browse/EDGPATRON-90</a>
   * <a href="https://issues.folio.org/browse/CIRCSTORE-263">https://issues.folio.org/browse/CIRCSTORE-263</a>
   */
  @Test
  public void canLog() {
    setTenant("logtenant");

    given().
      header("X-Okapi-Request-Id", "987654321").
      header("X-Okapi-User-Id", "itsme").
      when().
      get("/location-units/libraries").
      then().
      statusCode(400);  // tenant hasn't been created

    assertThat(MOD_MIS.getLogs(), containsString("[987654321] [logtenant] [itsme] [mod_orders]"));
  }

  private void setTenant(String tenant) {
    RestAssured.requestSpecification = new RequestSpecBuilder()
      .addHeader("X-Okapi-Url", "http://localhost:8081")
      .addHeader("X-Okapi-Tenant", tenant)
      .setContentType(ContentType.JSON)
      .build();
  }

  private void postTenant(JsonObject body) {
    String location =
      given().
        body(body.encodePrettily()).
        when().
        post("/_/tenant").
        then().
        statusCode(201).
        extract().
        header("Location");

    when().
      get(location + "?wait=60000").
      then().
      statusCode(200).
      body("complete", is(true));
  }

}
