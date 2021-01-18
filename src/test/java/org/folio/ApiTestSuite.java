package org.folio;

import static org.folio.TestConfig.closeMockServer;
import static org.folio.TestConfig.closeVertx;
import static org.folio.TestConfig.deployVerticle;
import static org.folio.TestConfig.startMockServer;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.folio.helper.InventoryHelperTest;
import org.folio.helper.PiecesHelperTest;
import org.folio.helper.PurchaseOrderHelperTest;
import org.folio.helper.PurchaseOrderLineHelperTest;
import org.folio.orders.events.handlers.CheckInOrderStatusChangeChangeHandlerTest;
import org.folio.orders.events.handlers.ReceiptStatusConsistencyTest;
import org.folio.orders.events.handlers.ReceiveOrderStatusChangeHandlerTest;
import org.folio.orders.utils.HelperUtilsTest;
import org.folio.rest.core.RestClientTest;
import org.folio.rest.impl.AcquisitionsMembershipsTest;
import org.folio.rest.impl.AcquisitionsUnitsTest;
import org.folio.rest.impl.CheckinReceivingApiTest;
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
import org.folio.service.PrefixServiceTest;
import org.folio.service.ReasonForClosureServiceTest;
import org.folio.service.SuffixServiceTest;
import org.folio.service.TransactionServiceTest;
import org.folio.service.exchange.ManualExchangeRateProviderTest;
import org.folio.service.finance.EncumbranceServiceTest;
import org.folio.service.finance.FundServiceTest;
import org.folio.service.finance.OpenToPendingEncumbranceStrategyTest;
import org.folio.service.orders.OrderRolloverServiceTest;
import org.folio.service.orders.PurchaseOrderLineServiceTest;
import org.folio.service.orders.PurchaseOrderServiceTest;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Nested;
import org.junit.platform.runner.JUnitPlatform;
import org.junit.runner.RunWith;


@RunWith(JUnitPlatform.class)
public class ApiTestSuite {

  @BeforeAll
  public static void before() throws InterruptedException, ExecutionException, TimeoutException {
    startMockServer();
    deployVerticle();
  }

  @AfterAll
  public static void after() {
    closeMockServer();
    closeVertx();
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
  class EncumbranceServiceTestNested extends EncumbranceServiceTest {
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

  @Nested
  class InventoryHelperTestNested extends InventoryHelperTest {
  }

  @Nested
  class PurchaseOrderLineServiceTestNested extends PurchaseOrderLineServiceTest {
  }

  @Nested
  class PurchaseOrderServiceTestNested extends PurchaseOrderServiceTest {
  }

  @Nested
  class FundServiceTestNested extends FundServiceTest {
  }

  @Nested
  class OrderRolloverServiceTestNested extends OrderRolloverServiceTest {
  }

  @Nested
  class OpenToPendingEncumbranceStrategyTestNested extends OpenToPendingEncumbranceStrategyTest {
  }
}
