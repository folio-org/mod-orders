package org.folio;

import static org.folio.TestConfig.closeKafkaMockServer;
import static org.folio.TestConfig.closeMockServer;
import static org.folio.TestConfig.closeVertx;
import static org.folio.TestConfig.deployVerticle;
import static org.folio.TestConfig.startKafkaMockServer;
import static org.folio.TestConfig.startMockServer;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;

public class ApiTestSuite {

  @BeforeAll
  public static void before() throws InterruptedException, ExecutionException, TimeoutException {
    startMockServer();
    startKafkaMockServer();
    deployVerticle();
  }

  @AfterAll
  public static void after() {
    closeMockServer();
    closeKafkaMockServer();
    closeVertx();
  }

}
