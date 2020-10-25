package org.folio;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.folio.helper.FinanceHelperTest;
import org.folio.helper.PiecesHelperTest;
import org.folio.helper.PurchaseOrderHelperTest;
import org.folio.helper.PurchaseOrderLineHelperTest;
import org.folio.orders.events.handlers.CheckInOrderStatusChangeChangeHandlerTest;
import org.folio.orders.events.handlers.ReceiptStatusConsistencyTest;
import org.folio.orders.events.handlers.ReceiveOrderStatusChangeHandlerTest;
import org.folio.orders.utils.HelperUtilsTest;
import org.folio.rest.RestVerticle;
import org.folio.rest.core.RestClientTest;
import org.folio.rest.impl.AcquisitionsMembershipsTest;
import org.folio.rest.impl.AcquisitionsUnitsTest;
import org.folio.rest.impl.CheckinReceivingApiTest;
import org.folio.rest.impl.MockServer;
import org.folio.rest.impl.OrderTemplateTest;
import org.folio.rest.impl.PieceApiTest;
import org.folio.rest.impl.PoNumberApiTest;
import org.folio.rest.impl.PurchaseOrderLinesApiTest;
import org.folio.rest.impl.PurchaseOrdersApiTest;
import org.folio.rest.impl.ReceivingHistoryApiTest;
import org.folio.rest.impl.TitlesApiTest;
import org.folio.rest.impl.crud.ConfigurationCrudTest;
import org.folio.rest.impl.protection.LinesProtectionTest;
import org.folio.rest.impl.protection.OrdersProtectionTest;
import org.folio.rest.impl.protection.PiecesProtectionTest;
import org.folio.rest.impl.protection.ReceivingCheckinProtectionTest;
import org.folio.rest.tools.utils.NetworkUtils;
import org.folio.service.PrefixServiceTest;
import org.folio.service.ReasonForClosureServiceTest;
import org.folio.service.SuffixServiceTest;
import org.folio.service.TransactionServiceTest;
import org.folio.service.exchange.ManualExchangeRateProviderTest;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Nested;
import org.junit.platform.runner.JUnitPlatform;
import org.junit.runner.RunWith;

import io.restassured.RestAssured;
import io.vertx.core.DeploymentOptions;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;


@RunWith(JUnitPlatform.class)
public class ApiTestSuite {

  private static final int okapiPort = NetworkUtils.nextFreePort();
  public static final int mockPort = NetworkUtils.nextFreePort();
  private static MockServer mockServer;
  private static Vertx vertx;
  private static boolean initialised;

  @BeforeAll
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
      if (res.succeeded()) {
        deploymentComplete.complete(res.result());
      } else {
        deploymentComplete.completeExceptionally(res.cause());
      }
    });
    deploymentComplete.get(60, TimeUnit.SECONDS);
    initialised = true;
  }

  @AfterAll
  public static void after() {
    mockServer.close();
    vertx.close();
    initialised = false;
  }

  public static boolean isNotInitialised() {
    return !initialised;
  }

  @Nested
  class AcquisitionsUnitsTestNested extends AcquisitionsUnitsTest {
  }

  @Nested
  class AcquisitionsMembershipsTestNested extends AcquisitionsMembershipsTest {
  }

  @Nested
  class PurchaseOrdersApiTestNested extends PurchaseOrdersApiTest {
  }

  @Nested
  class PurchaseOrderLinesApiTestNested extends PurchaseOrderLinesApiTest {
  }

  @Nested
  class CheckinReceivingApiTestNested extends CheckinReceivingApiTest {
  }

  @Nested
  class PieceApiTestNested extends PieceApiTest {
  }

  @Nested
  class ReceivingHistoryApiTestNested extends ReceivingHistoryApiTest {
  }

  @Nested
  class PoNumberApiTestNested extends PoNumberApiTest {
  }

  @Nested
  class ReceiveOrderStatusChangeHandlerTestNested extends ReceiveOrderStatusChangeHandlerTest {
  }

  @Nested
  class ReceiptStatusConsistencyTestNested extends ReceiptStatusConsistencyTest {
  }

  @Nested
  class OrdersProtectionTestNested extends OrdersProtectionTest {
  }

  @Nested
  class LinesProtectionTestNested extends LinesProtectionTest {
  }

  @Nested
  class PiecesProtectionTestNested extends PiecesProtectionTest {
  }

  @Nested
  class ReceivingCheckinProtectionTestNested extends ReceivingCheckinProtectionTest {
  }

  @Nested
  class OrderTemplateTestNested extends OrderTemplateTest {
  }

  @Nested
  class TitlesApiTestNested extends TitlesApiTest {
  }

  @Nested
  class ConfigurationCrudTestNested extends ConfigurationCrudTest {
  }


  @Nested
  class CheckInOrderStatusChangeChangeHandlerTestNested extends CheckInOrderStatusChangeChangeHandlerTest {
  }


  @Nested
  class SuffixServiceTestNested extends SuffixServiceTest {
  }

  @Nested
  class PrefixServiceTestNested extends PrefixServiceTest {
  }

  @Nested
  class ReasonForClosureServiceTestNested extends ReasonForClosureServiceTest {
  }

  @Nested
  class PiecesHelperTestNested extends PiecesHelperTest {
  }

  @Nested
  class PurchaseOrderHelperTestNested extends PurchaseOrderHelperTest {
  }

  @Nested
  class FinanceHelperTestNested extends FinanceHelperTest {
  }

  @Nested
  class PurchaseOrderLineHelperTestNested extends PurchaseOrderLineHelperTest {
  }

  @Nested
  class HelperUtilsTestNested extends HelperUtilsTest {
  }

  @Nested
  class TransactionServiceTestNested extends TransactionServiceTest {
  }

  @Nested
  class ManualExchangeRateProviderTestNested extends ManualExchangeRateProviderTest {
  }

  @Nested
  class RestClientTestNested extends RestClientTest {
  }

}
