package org.folio;

import static org.folio.TestConfig.closeMockServer;
import static org.folio.TestConfig.closeVertx;
import static org.folio.TestConfig.deployVerticle;
import static org.folio.TestConfig.startMockServer;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.folio.helper.PurchaseOrderHelperTest;
import org.folio.helper.PurchaseOrderLineHelperTest;
import org.folio.orders.events.handlers.CheckInOrderStatusChangeChangeHandlerTest;
import org.folio.orders.events.handlers.ReceiptStatusConsistencyTest;
import org.folio.orders.events.handlers.ReceiveOrderStatusChangeHandlerTest;
import org.folio.orders.utils.HelperUtilsTest;
import org.folio.orders.utils.validators.CompositePoLineValidationUtilTest;
import org.folio.orders.utils.validators.LocationsAndPiecesConsistencyValidatorTest;
import org.folio.orders.utils.validators.OngoingOrderValidatorTest;
import org.folio.rest.core.ResponseUtilTest;
import org.folio.rest.core.RestClientTest;
import org.folio.rest.core.exceptions.ExceptionUtilTest;
import org.folio.rest.impl.CheckinReceivingApiTest;
import org.folio.rest.impl.HoldingsSummaryAPITest;
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
import org.folio.service.exchange.ManualExchangeRateProviderTest;
import org.folio.service.finance.FundServiceTest;
import org.folio.service.expenceclass.ExpenseClassValidationServiceTest;
import org.folio.service.finance.budget.BudgetRestrictionServiceTest;
import org.folio.service.inventory.InventoryManagerTest;
import org.folio.service.finance.transaction.EncumbranceRelationsHoldersBuilderTest;
import org.folio.service.finance.transaction.EncumbranceServiceTest;
import org.folio.service.finance.transaction.OpenToPendingEncumbranceStrategyTest;
import org.folio.service.finance.transaction.TransactionServiceTest;
import org.folio.service.orders.AcquisitionsUnitsServiceTest;
import org.folio.service.inventory.HoldingsSummaryServiceTest;
import org.folio.service.orders.CombinedOrderDataPopulateServiceTest;
import org.folio.service.orders.CompositeOrderRetrieveHolderBuilderTest;
import org.folio.service.orders.FundsDistributionServiceTest;
import org.folio.service.orders.OrderInvoiceRelationServiceTest;
import org.folio.service.orders.OrderReEncumberServiceTest;
import org.folio.service.orders.OrderRolloverServiceTest;
import org.folio.service.orders.PurchaseOrderLineServiceTest;
import org.folio.service.orders.PurchaseOrderServiceTest;
import org.folio.service.orders.ReEncumbranceHoldersBuilderTest;
import org.folio.service.orders.TransactionsTotalFieldsPopulateServiceTest;
import org.folio.service.pieces.flows.PieceFlowUpdatePoLineStrategiesTest;
import org.folio.service.pieces.PieceServiceTest;
import org.folio.service.pieces.PieceStorageServiceTest;
import org.folio.service.pieces.PieceUpdateInventoryServiceTest;
import org.folio.service.pieces.flows.PieceFlowUpdatePoLineStrategyResolverTest;
import org.folio.service.pieces.validators.PieceValidatorUtilTest;
import org.folio.orders.utils.PoLineCommonUtilTest;
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
  class AcquisitionsUnitsServiceTestNested extends AcquisitionsUnitsServiceTest {
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
  class PieceServiceTestNested extends PieceServiceTest {
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
  class InventoryManagerTestNested extends InventoryManagerTest {
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

  @Nested
  class ReEncumbranceHoldersBuilderTestNested extends ReEncumbranceHoldersBuilderTest {
  }

  @Nested
  class OrderReEncumberServiceTestNested extends OrderReEncumberServiceTest {
  }

  @Nested
  class TransactionsTotalFieldsPopulateServiceTestNested extends TransactionsTotalFieldsPopulateServiceTest {
  }

  @Nested
  class CombinedOrderDataPopulateServiceTestNested extends CombinedOrderDataPopulateServiceTest {
  }

  @Nested
  class CompositeOrderRetrieveHolderBuilderTestNested extends CompositeOrderRetrieveHolderBuilderTest {
  }

  @Nested
  class EncumbranceRelationsHoldersBuilderTestNested extends EncumbranceRelationsHoldersBuilderTest {
  }

  @Nested
  class FundsDistributionServiceTestNested extends FundsDistributionServiceTest {
  }

  @Nested
  class BudgetRestrictionServiceTestNested extends BudgetRestrictionServiceTest {
  }

  @Nested
  class OngoingOrderValidatorTestNested extends OngoingOrderValidatorTest {
  }

  @Nested
  class HoldingsSummaryAPITestNested extends HoldingsSummaryAPITest {
  }

  @Nested
  class HoldingsSummaryServiceTestNested extends HoldingsSummaryServiceTest {
  }

  @Nested
  class ExpenseClassValidationServiceTestNested extends ExpenseClassValidationServiceTest {
  }

  @Nested
  class PoLineCommonUtilTestNested extends PoLineCommonUtilTest {

  }

  @Nested
  class CompositePoLineValidationUtilNested extends CompositePoLineValidationUtilTest {

  }

  @Nested
  class LocationsAndPiecesConsistencyValidatorTestNested extends LocationsAndPiecesConsistencyValidatorTest {

  }

  @Nested
  class PieceStorageServiceTestNested extends PieceStorageServiceTest {

  }

  @Nested
  class PieceUpdateInventoryServiceTestNested extends PieceUpdateInventoryServiceTest {

  }

  @Nested
  class OrderInvoiceRelationServiceTestNested extends OrderInvoiceRelationServiceTest {

  }

  @Nested
  class PieceFlowUpdatePoLineStrategiesTestNested extends PieceFlowUpdatePoLineStrategiesTest {

  }

  @Nested
  class ExceptionUtilTestNested extends ExceptionUtilTest {

  }

  @Nested
  class ResponseUtilTestNested extends ResponseUtilTest {

  }

  @Nested
  class PieceFlowUpdatePoLineStrategyResolverTestNested extends PieceFlowUpdatePoLineStrategyResolverTest {

  }

  @Nested
  class PieceValidatorUtilTestNested extends PieceValidatorUtilTest {

  }
}
