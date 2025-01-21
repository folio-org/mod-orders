package org.folio;

import static org.folio.TestConfig.closeKafkaMockServer;
import static org.folio.TestConfig.closeMockServer;
import static org.folio.TestConfig.closeVertx;
import static org.folio.TestConfig.deployVerticle;
import static org.folio.TestConfig.startKafkaMockServer;
import static org.folio.TestConfig.startMockServer;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.folio.helper.CheckinHelperTest;
import org.folio.helper.PurchaseOrderHelperTest;
import org.folio.helper.PurchaseOrderLineHelperTest;
import org.folio.orders.events.handlers.CheckInOrderStatusChangeChangeHandlerTest;
import org.folio.orders.events.handlers.ReceiptStatusConsistencyTest;
import org.folio.orders.events.handlers.ReceiveOrderStatusChangeHandlerTest;
import org.folio.orders.utils.FundDistributionUtilsTest;
import org.folio.orders.utils.HelperUtilsTest;
import org.folio.orders.utils.PoLineCommonUtilTest;
import org.folio.orders.utils.QueryUtilsTest;
import org.folio.orders.utils.StreamUtilsTest;
import org.folio.orders.utils.validators.LocationsAndPiecesConsistencyValidatorTest;
import org.folio.rest.core.ResponseUtilTest;
import org.folio.rest.core.RestClientTest;
import org.folio.rest.core.exceptions.ExceptionUtilTest;
import org.folio.rest.impl.AcquisitionMethodAPITest;
import org.folio.rest.impl.BaseApiTest;
import org.folio.rest.impl.CheckinReceivingApiTest;
import org.folio.rest.impl.ExportHistoryImplTest;
import org.folio.rest.impl.HoldingsSummaryAPITest;
import org.folio.rest.impl.OrderTemplateTest;
import org.folio.rest.impl.PieceApiTest;
import org.folio.rest.impl.PiecesClaimingApiTest;
import org.folio.rest.impl.PoNumberApiTest;
import org.folio.rest.impl.PurchaseOrderLinesApiTest;
import org.folio.rest.impl.PurchaseOrdersApiTest;
import org.folio.rest.impl.ReceivingHistoryApiTest;
import org.folio.rest.impl.RoutingListsApiTest;
import org.folio.rest.impl.TitlesApiTest;
import org.folio.rest.impl.crud.ConfigurationCrudTest;
import org.folio.rest.impl.protection.LinesProtectionTest;
import org.folio.rest.impl.protection.OrdersProtectionTest;
import org.folio.rest.impl.protection.PiecesProtectionTest;
import org.folio.rest.impl.protection.ReceivingCheckinProtectionTest;
import org.folio.service.CirculationRequestsRetrieverTest;
import org.folio.service.PrefixServiceTest;
import org.folio.service.ReasonForClosureServiceTest;
import org.folio.service.SuffixServiceTest;
import org.folio.service.TagServiceTest;
import org.folio.service.UserServiceTest;
import org.folio.service.consortium.ConsortiumConfigurationServiceTest;
import org.folio.service.consortium.SharingInstanceServiceTest;
import org.folio.service.exchange.ExchangeRateProviderResolverTest;
import org.folio.service.exchange.ManualCurrencyConversionTest;
import org.folio.service.exchange.ManualExchangeRateProviderTest;
import org.folio.service.expenceclass.ExpenseClassValidationServiceTest;
import org.folio.service.finance.FinanceHoldersBuilderTest;
import org.folio.service.finance.FiscalYearServiceTest;
import org.folio.service.finance.FundServiceTest;
import org.folio.service.finance.budget.BudgetRestrictionServiceTest;
import org.folio.service.finance.rollover.LedgerRolloverErrorServiceTest;
import org.folio.service.finance.rollover.LedgerRolloverProgressServiceTest;
import org.folio.service.finance.rollover.LedgerRolloverServiceTest;
import org.folio.service.finance.transaction.ClosedToOpenEncumbranceStrategyTest;
import org.folio.service.finance.transaction.EncumbranceRelationsHoldersBuilderTest;
import org.folio.service.finance.transaction.EncumbranceServiceTest;
import org.folio.service.finance.transaction.OpenToClosedEncumbranceStrategyTest;
import org.folio.service.finance.transaction.OpenToPendingEncumbranceStrategyTest;
import org.folio.service.finance.transaction.PendingToOpenEncumbranceStrategyTest;
import org.folio.service.finance.transaction.PendingToPendingEncumbranceStrategyTest;
import org.folio.service.finance.transaction.TransactionServiceTest;
import org.folio.service.inventory.*;
import org.folio.service.invoice.InvoiceLineServiceTest;
import org.folio.service.orders.AcquisitionsUnitsServiceTest;
import org.folio.service.orders.CombinedOrderDataPopulateServiceTest;
import org.folio.service.orders.CompositeOrderRetrieveHolderBuilderTest;
import org.folio.service.orders.CompositePoLineValidationServiceTest;
import org.folio.service.orders.FundsDistributionServiceTest;
import org.folio.service.orders.OrderInvoiceRelationServiceTest;
import org.folio.service.orders.OrderReEncumberServiceTest;
import org.folio.service.orders.OrderRolloverServiceTest;
import org.folio.service.orders.PurchaseOrderLineServiceTest;
import org.folio.service.orders.PurchaseOrderStorageServiceTest;
import org.folio.service.orders.ReEncumbranceHoldersBuilderTest;
import org.folio.service.orders.CompositeOrderTotalFieldsPopulateServiceTest;
import org.folio.service.orders.flows.update.open.OpenCompositeOrderFlowValidatorTest;
import org.folio.service.orders.flows.update.open.OpenCompositeOrderHolderBuilderTest;
import org.folio.service.orders.flows.update.open.OpenCompositeOrderInventoryServiceTest;
import org.folio.service.orders.flows.update.open.OpenCompositeOrderManagerTest;
import org.folio.service.orders.flows.update.open.OpenCompositeOrderPieceServiceTest;
import org.folio.service.orders.flows.update.reopen.ReOpenCompositeOrderManagerTest;
import org.folio.service.orders.lines.update.OrderLineUpdateInstanceHandlerTest;
import org.folio.service.orders.lines.update.instance.WithHoldingOrderLineUpdateInstanceStrategyTest;
import org.folio.service.orders.lines.update.instance.WithoutHoldingOrderLineUpdateInstanceStrategyTest;
import org.folio.service.pieces.PieceServiceTest;
import org.folio.service.pieces.PieceStorageServiceTest;
import org.folio.service.pieces.PieceUpdateInventoryServiceTest;
import org.folio.service.pieces.PieceUtilTest;
import org.folio.service.pieces.PiecesClaimingServiceTest;
import org.folio.service.pieces.WrapperPieceStorageServiceTest;
import org.folio.service.pieces.flows.BasePieceFlowHolderBuilderTest;
import org.folio.service.pieces.flows.DefaultPieceFlowsValidatorTest;
import org.folio.service.pieces.flows.create.PieceCreateFlowInventoryManagerTest;
import org.folio.service.pieces.flows.create.PieceCreateFlowPoLineServiceTest;
import org.folio.service.pieces.flows.delete.PieceDeleteFlowManagerTest;
import org.folio.service.pieces.flows.delete.PieceDeleteFlowPoLineServiceTest;
import org.folio.service.pieces.flows.update.PieceUpdateFlowInventoryManagerTest;
import org.folio.service.pieces.flows.update.PieceUpdateFlowManagerTest;
import org.folio.service.pieces.flows.update.PieceUpdateFlowPoLineServiceTest;
import org.folio.service.pieces.validators.PieceValidatorUtilTest;
import org.folio.service.routinglists.RoutingListServiceTest;
import org.folio.service.settings.SettingsRetrieverTest;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Nested;

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

  @Nested
  class AcquisitionsUnitsServiceTestNested extends AcquisitionsUnitsServiceTest {
  }

  @Nested
  class AcquisitionMethodAPITestNested extends AcquisitionMethodAPITest {
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
  class RoutingListsApiTestNested extends RoutingListsApiTest {
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
  class SharingInstanceServiceTestNested extends SharingInstanceServiceTest {
  }

  @Nested
  class ConsortiumConfigurationServiceTestNested extends ConsortiumConfigurationServiceTest {
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
  class FundDistributionUtilsTestNested extends FundDistributionUtilsTest {
  }

  @Nested
  class StreamUtilsTestNested extends StreamUtilsTest {
  }

  @Nested
  class HelperUtilsTestNested extends HelperUtilsTest {
  }

  @Nested
  class QueryUtilsTestNested extends QueryUtilsTest {
  }

  @Nested
  class TransactionServiceTestNested extends TransactionServiceTest {
  }

  @Nested
  class ManualExchangeRateProviderTestNested extends ManualExchangeRateProviderTest {
  }

  @Nested
  class ManualCurrencyConversionTestNested extends ManualCurrencyConversionTest {
  }

  @Nested
  class ExchangeRateProviderResolverTestNested extends ExchangeRateProviderResolverTest {
  }

  @Nested
  class RestClientTestNested extends RestClientTest {
  }

  @Nested
  class InventoryManagerTestNested extends InventoryManagerTest {
  }

  @Nested
  class InventoryUtilsTestNested extends InventoryUtilsTest {
  }

  @Nested
  class InventoryItemRequestServiceTestNested extends InventoryItemRequestServiceTest {
  }

  @Nested
  class InventoryItemSyncServiceTestNested extends InventoryItemStatusSyncServiceTest {
  }

  @Nested
  class InventoryInstanceManagerTestNested extends InventoryInstanceManagerTest {
  }

  @Nested
  class PurchaseOrderLineServiceTestNested extends PurchaseOrderLineServiceTest {
  }

  @Nested
  class PurchaseOrderStorageServiceTestNested extends PurchaseOrderStorageServiceTest {
  }

  @Nested
  class FundServiceTestNested extends FundServiceTest {
  }

  @Nested
  class OrderRolloverServiceTestNested extends OrderRolloverServiceTest {
  }

  @Nested
  class PendingToOpenEncumbranceStrategyTestNested extends PendingToOpenEncumbranceStrategyTest {
  }

  @Nested
  class PendingToPendingEncumbranceStrategyTestNested extends PendingToPendingEncumbranceStrategyTest {
  }

  @Nested
  class OpenToClosedEncumbranceStrategyTestNested extends OpenToClosedEncumbranceStrategyTest {
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
  class CompositeOrderTotalFieldsPopulateServiceTestNested extends CompositeOrderTotalFieldsPopulateServiceTest {
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
  class CompositePoLineValidationServiceTestNested extends CompositePoLineValidationServiceTest {
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
  class PieceFlowUpdatePoLineStrategiesTestNested {
  }

  @Nested
  class ExceptionUtilTestNested extends ExceptionUtilTest {
  }

  @Nested
  class ResponseUtilTestNested extends ResponseUtilTest {
  }

  @Nested
  class PieceValidatorUtilTestNested extends PieceValidatorUtilTest {
  }

  @Nested
  class PieceDeleteFlowManagerTestNested extends PieceDeleteFlowManagerTest {
  }

  @Nested
  class PieceCreateFlowInventoryManagerTestNested extends PieceCreateFlowInventoryManagerTest {
  }

  @Nested
  class DefaultPieceFlowsValidatorTestNested extends DefaultPieceFlowsValidatorTest {
  }

  @Nested
  class CheckinHelperTestNested extends CheckinHelperTest {
  }

  @Nested
  class PieceUpdateFlowManagerTestNested extends PieceUpdateFlowManagerTest {
  }

  @Nested
  class PieceUpdateFlowInventoryManagerTestNested extends PieceUpdateFlowInventoryManagerTest {
  }

  @Nested
  class BasePieceFlowHolderBuilderTestNested extends BasePieceFlowHolderBuilderTest {
  }

  @Nested
  class PieceCreateFlowPoLineServiceTestNested extends PieceCreateFlowPoLineServiceTest {
  }

  @Nested
  class PieceDeleteFlowPoLineServiceTestNested extends PieceDeleteFlowPoLineServiceTest {
  }

  @Nested
  class PieceUpdateFlowPoLineServiceTestNested extends PieceUpdateFlowPoLineServiceTest {
  }

  @Nested
  class OpenCompositeOrderInventoryServiceTestNested extends OpenCompositeOrderInventoryServiceTest {
  }

  @Nested
  class OpenCompositeOrderFlowValidatorTestNested extends OpenCompositeOrderFlowValidatorTest {
  }

  @Nested
  class OpenCompositeOrderManagerTestNested extends OpenCompositeOrderManagerTest {
  }

  @Nested
  class OpenCompositeOrderPieceServiceTestNested extends OpenCompositeOrderPieceServiceTest {
  }

  @Nested
  class OpenCompositeOrderHolderBuilderTestNested extends OpenCompositeOrderHolderBuilderTest {
  }

  @Nested
  class InvoiceLineServiceTestNested extends InvoiceLineServiceTest {
  }

  @Nested
  class ReOpenCompositeOrderManagerTestNested extends ReOpenCompositeOrderManagerTest {
  }

  @Nested
  class OrderLineUpdateInstanceHandlerTestNested extends OrderLineUpdateInstanceHandlerTest {
  }

  @Nested
  class WithHoldingOrderLineUpdateInstanceStrategy extends WithHoldingOrderLineUpdateInstanceStrategyTest {
  }

  @Nested
  class WithoutHoldingOrderLineUpdateInstanceStrategy extends WithoutHoldingOrderLineUpdateInstanceStrategyTest {
  }

  @Nested
  class ExportHistoryImplTestNested extends ExportHistoryImplTest {
  }

  @Nested
  class LedgerRolloverServiceTestNested extends LedgerRolloverServiceTest {
  }

  @Nested
  class LedgerRolloverErrorServiceTestNested extends LedgerRolloverErrorServiceTest {
  }

  @Nested
  class LedgerRolloverProgressServiceTestNested extends LedgerRolloverProgressServiceTest {
  }

  @Nested
  class ClosedToOpenEncumbranceStrategyTestNested extends ClosedToOpenEncumbranceStrategyTest {
  }

  @Nested
  class FiscalYearServiceTestNested extends FiscalYearServiceTest {
  }

  @Nested
  class TagServiceTestNested extends TagServiceTest {
  }

  @Nested
  class RoutingListServiceTestNested extends RoutingListServiceTest {
  }

  @Nested
  class UserServiceTestNested extends UserServiceTest {
  }

  @Nested
  class CirculationRequestsRetrieverTestNested extends CirculationRequestsRetrieverTest {
  }

  @Nested
  class BaseApiTestNested extends BaseApiTest {
  }

  @Nested
  class FinanceHoldersBuilderTestNested extends FinanceHoldersBuilderTest {
  }

  @Nested
  class SettingsRetrieverTestNested extends SettingsRetrieverTest {
  }

  @Nested
  class PiecesClaimingApiTestNested extends PiecesClaimingApiTest {
  }

  @Nested
  class PiecesClaimingServiceNested extends PiecesClaimingServiceTest {
  }

  @Nested
  class PieceUtilTestNested extends PieceUtilTest {
  }

  @Nested
  class WrapperPieceStorageServiceTestNested extends WrapperPieceStorageServiceTest {
  }
}
