package org.folio.rest.impl;

import static org.awaitility.Awaitility.await;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import io.restassured.RestAssured;
import io.vertx.core.DeploymentOptions;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;

import org.folio.orders.events.handlers.OrderStatusTest;
import org.folio.orders.events.handlers.ReceiptStatusConsistencyTest;
import org.folio.rest.RestVerticle;
import org.folio.rest.tools.utils.NetworkUtils;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

@RunWith(Suite.class)
@Suite.SuiteClasses({
  AcquisitionsUnitsTests.class,
  PurchaseOrdersApiTest.class,
  PurchaseOrderLinesApiTest.class,
  CheckinReceivingApiTest.class,
  PieceApiTest.class,
  ReceivingHistoryApiTest.class,
  PoNumberApiTest.class,
  OrderStatusTest.class,
  ReceiptStatusConsistencyTest.class
})
public class ApiTestSuite {

  private static final int okapiPort = NetworkUtils.nextFreePort();
  static final int mockPort = NetworkUtils.nextFreePort();
  private static Vertx vertx;
  private static MockServer mockServer;
  private static boolean initialised;

  @BeforeClass
  public static void before() throws InterruptedException, ExecutionException, TimeoutException {
    if (vertx == null) {
      vertx = Vertx.vertx();
    }

    mockServer = new MockServer(mockPort);
    mockServer.start();

    RestAssured.baseURI = "http://localhost:" + okapiPort;
    RestAssured.port = okapiPort;
    RestAssured.enableLoggingOfRequestAndResponseIfValidationFails();

    final JsonObject conf = new JsonObject();
    conf.put("http.port", okapiPort);

    final DeploymentOptions opt = new DeploymentOptions().setConfig(conf);
    CompletableFuture<String> deploymentComplete = new CompletableFuture<>();
    vertx.deployVerticle(RestVerticle.class.getName(), opt, res -> {
      if(res.succeeded()) {
        // Make sure that post-deployment configs completed before test starts
        vertx.setTimer(500, timerId -> deploymentComplete.complete(res.result()));
      }
      else {
        deploymentComplete.completeExceptionally(res.cause());
      }
    });

    await().atMost(60, TimeUnit.SECONDS).until(deploymentComplete::isDone);
    assertThat(deploymentComplete.isCompletedExceptionally(), is(false));

    initialised = true;
  }

  @AfterClass
  public static void after() {
    vertx.close();
    mockServer.close();
    initialised = false;
  }

  public static boolean isNotInitialised() {
    return !initialised;
  }

}
