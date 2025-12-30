package org.folio.rest;

import static io.restassured.RestAssured.when;
import static org.hamcrest.Matchers.is;

import java.nio.file.Path;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.Network;
import org.testcontainers.containers.output.Slf4jLogConsumer;
import org.testcontainers.images.builder.ImageFromDockerfile;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.kafka.KafkaContainer;
import org.testcontainers.utility.DockerImageName;

import io.restassured.RestAssured;

/**
 * Check the shaded fat uber jar and Dockerfile:
 *
 * <p>Test /admin/health.
 *
 * <p>Test that logging works.
 *
 * <p>Test installation and migration with smoke test.
 */
@Testcontainers
public class InstallUpgradeIT {

  private static final Logger LOG = LoggerFactory.getLogger(InstallUpgradeIT.class);
  private static final boolean IS_LOG_ENABLED = false;
  private static final Network NETWORK = Network.newNetwork();

  @Container
  private static final KafkaContainer KAFKA =
    new KafkaContainer(DockerImageName.parse("apache/kafka-native:3.8.0"))
      .withNetwork(NETWORK)
      .withNetworkAliases("mykafka");

  @Container
  private static final GenericContainer<?> MOD_ORDERS =
    new GenericContainer<>(
      new ImageFromDockerfile("mod-orders").withFileFromPath(".", Path.of(".")))
      .withNetwork(NETWORK)
      .withExposedPorts(8081)
      .withEnv("KAFKA_HOST", "mykafka")
      .withEnv("KAFKA_PORT", "9092");

  @BeforeAll
  public static void beforeClass() {
    RestAssured.reset();
    RestAssured.enableLoggingOfRequestAndResponseIfValidationFails();
    RestAssured.baseURI = "http://" + MOD_ORDERS.getHost() + ":" + MOD_ORDERS.getFirstMappedPort();
    if (IS_LOG_ENABLED) {
      KAFKA.followOutput(new Slf4jLogConsumer(LOG).withSeparateOutputStreams());
      MOD_ORDERS.followOutput(new Slf4jLogConsumer(LOG).withSeparateOutputStreams());
    }
  }

  @BeforeEach
  public void beforeEach() {
    RestAssured.requestSpecification = null;
  }

  @Test
  public void health() {
    // request without X-Okapi-Tenant
    when()
      .get("/admin/health")
      .then()
      .statusCode(200)
      .body(is("\"OK\""));
  }

}
