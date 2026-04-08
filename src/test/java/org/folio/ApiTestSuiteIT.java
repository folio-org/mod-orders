package org.folio;

import static org.folio.TestConfig.closeKafkaMockServer;
import static org.folio.TestConfig.closeMockServer;
import static org.folio.TestConfig.closeVertx;
import static org.folio.TestConfig.deployVerticle;
import static org.folio.TestConfig.startKafkaMockServer;
import static org.folio.TestConfig.startMockServer;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.folio.helper.CheckinHelperIT;
import org.folio.helper.PurchaseOrderHelperTest;
import org.folio.helper.PurchaseOrderLineHelperTest;
import org.folio.orders.events.handlers.CheckInOrderStatusChangeChangeHandlerIT;
import org.folio.orders.events.handlers.ReceiptStatusConsistencyIT;
import org.folio.orders.events.handlers.ReceiveOrderStatusChangeHandlerIT;
import org.folio.orders.utils.FundDistributionUtilsTest;
import org.folio.orders.utils.HelperUtilsTest;
import org.folio.orders.utils.PoLineCommonUtilTest;
import org.folio.orders.utils.QueryUtilsTest;
import org.folio.orders.utils.StreamUtilsTest;
import org.folio.orders.utils.validators.LocationsAndPiecesConsistencyValidatorTest;
import org.folio.rest.core.ResponseUtilTest;
import org.folio.rest.core.RestClientTest;
import org.folio.rest.core.exceptions.ExceptionUtilTest;
import org.folio.rest.impl.AcquisitionMethodAPIIT;
import org.folio.rest.impl.BaseApiTest;
import org.folio.rest.impl.CheckinReceivingApiIT;
import org.folio.rest.impl.ExportHistoryImplIT;
import org.folio.rest.impl.HoldingsSummaryAPIIT;
import org.folio.rest.impl.OrderTemplateIT;
import org.folio.rest.impl.PieceApiIT;
import org.folio.rest.impl.PiecesClaimingApiIT;
import org.folio.rest.impl.PoNumberApiIT;
import org.folio.rest.impl.PurchaseOrderLinesApiIT;
import org.folio.rest.impl.PurchaseOrdersApiIT;
import org.folio.rest.impl.ReceivingHistoryApiIT;
import org.folio.rest.impl.RoutingListsApiIT;
import org.folio.rest.impl.TitlesApiIT;
import org.folio.rest.impl.WrapperPiecesAPIIT;
import org.folio.rest.impl.crud.ConfigurationCrudIT;
import org.folio.rest.impl.protection.LinesProtectionIT;
import org.folio.rest.impl.protection.OrdersProtectionIT;
import org.folio.rest.impl.protection.PiecesProtectionIT;
import org.folio.rest.impl.protection.ReceivingCheckinProtectionIT;
import org.folio.service.CirculationRequestsRetrieverIT;
import org.folio.service.PrefixServiceTest;
import org.folio.service.ReasonForClosureServiceTest;
import org.folio.service.SuffixServiceTest;
import org.folio.service.TagServiceTest;
import org.folio.service.UserServiceTest;
import org.folio.service.batch.BatchTrackingServiceTest;
import org.folio.service.consortium.ConsortiumConfigurationServiceTest;
import org.folio.service.consortium.ConsortiumUserTenantServiceTest;
import org.folio.service.consortium.SharingInstanceServiceTest;
import org.folio.service.exchange.CacheableExchangeRateServiceTest;
import org.folio.service.exchange.ManualCurrencyConversionTest;
import org.folio.service.exchange.CustomExchangeRateProviderTest;
import org.folio.service.expenceclass.ExpenseClassValidationServiceTest;
import org.folio.service.finance.EncumbranceUtilsTest;
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
import org.folio.service.finance.transaction.OpenToPendingEncumbranceStrategyIT;
import org.folio.service.finance.transaction.PendingToOpenEncumbranceStrategyTest;
import org.folio.service.finance.transaction.PendingToPendingEncumbranceStrategyTest;
import org.folio.service.finance.transaction.TransactionServiceTest;
import org.folio.service.inventory.*;
import org.folio.service.invoice.InvoiceLineServiceTest;
import org.folio.service.orders.AcquisitionsUnitsServiceIT;
import org.folio.service.orders.CombinedOrderDataPopulateServiceTest;
import org.folio.service.orders.CompositeOrderRetrieveHolderBuilderTest;
import org.folio.service.orders.CompositeOrderTotalFieldsPopulateServiceTest;
import org.folio.service.orders.FundsDistributionServiceTest;
import org.folio.service.orders.HoldingDetailServiceTest;
import org.folio.service.orders.OrderFiscalYearServiceTest;
import org.folio.service.orders.OrderInvoiceRelationServiceTest;
import org.folio.service.orders.OrderReEncumberServiceTest;
import org.folio.service.orders.OrderRolloverServiceTest;
import org.folio.service.orders.PoLineValidationServiceTest;
import org.folio.service.orders.PurchaseOrderLineServiceTest;
import org.folio.service.orders.PurchaseOrderStorageServiceTest;
import org.folio.service.orders.ReEncumbranceHoldersBuilderTest;
import org.folio.service.orders.flows.update.open.OpenCompositeOrderFlowValidatorTest;
import org.folio.service.orders.flows.update.open.OpenCompositeOrderHolderBuilderTest;
import org.folio.service.orders.flows.update.open.OpenCompositeOrderInventoryServiceIT;
import org.folio.service.orders.flows.update.open.OpenCompositeOrderManagerIT;
import org.folio.service.orders.flows.update.open.OpenCompositeOrderPieceServiceIT;
import org.folio.service.orders.flows.update.reopen.ReOpenCompositeOrderManagerIT;
import org.folio.service.orders.flows.update.unopen.UnOpenCompositeOrderManagerIT;
import org.folio.service.orders.lines.update.OrderLineUpdateInstanceHandlerIT;
import org.folio.service.orders.lines.update.instance.WithHoldingOrderLineUpdateInstanceStrategyTest;
import org.folio.service.orders.lines.update.instance.WithoutHoldingOrderLineUpdateInstanceStrategyTest;
import org.folio.service.pieces.PieceServiceIT;
import org.folio.service.pieces.PieceStorageServiceIT;
import org.folio.service.pieces.PieceUpdateInventoryServiceIT;
import org.folio.service.pieces.PieceUtilTest;
import org.folio.service.pieces.PiecesClaimingServiceTest;
import org.folio.service.pieces.WrapperPieceStorageServiceTest;
import org.folio.service.pieces.flows.BasePieceFlowHolderBuilderIT;
import org.folio.service.pieces.flows.DefaultPieceFlowsValidatorTest;
import org.folio.service.pieces.flows.create.PieceCreateFlowInventoryManagerIT;
import org.folio.service.pieces.flows.create.PieceCreateFlowPoLineServiceIT;
import org.folio.service.pieces.flows.delete.PieceDeleteFlowManagerIT;
import org.folio.service.pieces.flows.delete.PieceDeleteFlowPoLineServiceIT;
import org.folio.service.pieces.flows.update.PieceUpdateFlowInventoryManagerIT;
import org.folio.service.pieces.flows.update.PieceUpdateFlowManagerIT;
import org.folio.service.pieces.flows.update.PieceUpdateFlowPoLineServiceIT;
import org.folio.service.pieces.validators.PieceValidatorUtilTest;
import org.folio.service.routinglists.RoutingListServiceTest;
import org.folio.service.settings.SettingsRetrieverTest;
import org.folio.service.titles.TitlesServiceTest;
import org.folio.utils.iterators.FutureIteratorTest;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Tag;

@Tag("integration")
public class ApiTestSuiteIT {

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
  class AcquisitionsUnitsServiceTestNested extends AcquisitionsUnitsServiceIT {
  }

  @Nested
  class AcquisitionMethodAPITestNested extends AcquisitionMethodAPIIT {
  }

  @Nested
  class PurchaseOrdersApiTestNested extends PurchaseOrdersApiIT {
  }

  @Nested
  class PurchaseOrderLinesApiTestNested extends PurchaseOrderLinesApiIT {
  }

  @Nested
  class CheckinReceivingApiTestNested extends CheckinReceivingApiIT {
  }

  @Nested
  class PieceApiTestNested extends PieceApiIT {
  }

  @Nested
  class ReceivingHistoryApiTestNested extends ReceivingHistoryApiIT {
  }

  @Nested
  class PoNumberApiTestNested extends PoNumberApiIT {
  }

  @Nested
  class ReceiveOrderStatusChangeHandlerTestNested extends ReceiveOrderStatusChangeHandlerIT {
  }

  @Nested
  class ReceiptStatusConsistencyTestNested extends ReceiptStatusConsistencyIT {
  }

  @Nested
  class OrdersProtectionTestNested extends OrdersProtectionIT {
  }

  @Nested
  class LinesProtectionTestNested extends LinesProtectionIT {
  }

  @Nested
  class PiecesProtectionTestNested extends PiecesProtectionIT {
  }

  @Nested
  class ReceivingCheckinProtectionTestNested extends ReceivingCheckinProtectionIT {
  }

  @Nested
  class OrderTemplateTestNested extends OrderTemplateIT {
  }

  @Nested
  class TitlesApiTestNested extends TitlesApiIT {
  }

  @Nested
  class RoutingListsApiTestNested extends RoutingListsApiIT {
  }

  @Nested
  class ConfigurationCrudTestNested extends ConfigurationCrudIT {
  }


  @Nested
  class CheckInOrderStatusChangeChangeHandlerTestNested extends CheckInOrderStatusChangeChangeHandlerIT {
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
  class PieceServiceTestNested extends PieceServiceIT {
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
  class CustomExchangeRateProviderTestNested extends CustomExchangeRateProviderTest {
  }

  @Nested
  class ManualCurrencyConversionTestNested extends ManualCurrencyConversionTest {
  }

  @Nested
  class RestClientTestNested extends RestClientTest {
  }

  @Nested
  class InventoryManagerTestNested extends InventoryManagerIT {
  }

  @Nested
  class InventoryUtilsTestNested extends InventoryUtilsTest {
  }

  @Nested
  class InventoryItemRequestServiceTestNested extends InventoryItemRequestServiceIT {
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
  class OpenToPendingEncumbranceStrategyTestNested extends OpenToPendingEncumbranceStrategyIT {
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
  class HoldingsSummaryAPITestNested extends HoldingsSummaryAPIIT {
  }

  @Nested
  class HoldingsSummaryServiceTestNested extends HoldingsSummaryServiceTest {
  }

  @Nested
  class HoldingDetailServiceTestNested extends HoldingDetailServiceTest {
  }

  @Nested
  class ExpenseClassValidationServiceTestNested extends ExpenseClassValidationServiceTest {
  }

  @Nested
  class PoLineCommonUtilTestNested extends PoLineCommonUtilTest {
  }

  @Nested
  class PoLineValidationServiceTestNested extends PoLineValidationServiceTest {
  }

  @Nested
  class FutureIteratorTestNested extends FutureIteratorTest {
  }

  @Nested
  class LocationsAndPiecesConsistencyValidatorTestNested extends LocationsAndPiecesConsistencyValidatorTest {
  }

  @Nested
  class PieceStorageServiceTestNested extends PieceStorageServiceIT {
  }

  @Nested
  class PieceUpdateInventoryServiceTestNested extends PieceUpdateInventoryServiceIT {
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
  class PieceDeleteFlowManagerTestNested extends PieceDeleteFlowManagerIT {
  }

  @Nested
  class PieceCreateFlowInventoryManagerTestNested extends PieceCreateFlowInventoryManagerIT {
  }

  @Nested
  class DefaultPieceFlowsValidatorTestNested extends DefaultPieceFlowsValidatorTest {
  }

  @Nested
  class CheckinHelperTestNested extends CheckinHelperIT {
  }

  @Nested
  class PieceUpdateFlowManagerTestNested extends PieceUpdateFlowManagerIT {
  }

  @Nested
  class PieceUpdateFlowInventoryManagerTestNested extends PieceUpdateFlowInventoryManagerIT {
  }

  @Nested
  class BasePieceFlowHolderBuilderTestNested extends BasePieceFlowHolderBuilderIT {
  }

  @Nested
  class PieceCreateFlowPoLineServiceTestNested extends PieceCreateFlowPoLineServiceIT {
  }

  @Nested
  class PieceDeleteFlowPoLineServiceTestNested extends PieceDeleteFlowPoLineServiceIT {
  }

  @Nested
  class PieceUpdateFlowPoLineServiceTestNested extends PieceUpdateFlowPoLineServiceIT {
  }

  @Nested
  class OpenCompositeOrderInventoryServiceTestNested extends OpenCompositeOrderInventoryServiceIT {
  }

  @Nested
  class OpenCompositeOrderFlowValidatorTestNested extends OpenCompositeOrderFlowValidatorTest {
  }

  @Nested
  class OpenCompositeOrderManagerTestNested extends OpenCompositeOrderManagerIT {
  }

  @Nested
  class OpenCompositeOrderPieceServiceTestNested extends OpenCompositeOrderPieceServiceIT {
  }

  @Nested
  class OpenCompositeOrderHolderBuilderTestNested extends OpenCompositeOrderHolderBuilderTest {
  }

  @Nested
  class InvoiceLineServiceTestNested extends InvoiceLineServiceTest {
  }

  @Nested
  class ReOpenCompositeOrderManagerTestNested extends ReOpenCompositeOrderManagerIT {
  }

  @Nested
  class UnOpenCompositeOrderManagerTestNested extends UnOpenCompositeOrderManagerIT {
  }

  @Nested
  class OrderLineUpdateInstanceHandlerTestNested extends OrderLineUpdateInstanceHandlerIT {
  }

  @Nested
  class WithHoldingOrderLineUpdateInstanceStrategy extends WithHoldingOrderLineUpdateInstanceStrategyTest {
  }

  @Nested
  class WithoutHoldingOrderLineUpdateInstanceStrategy extends WithoutHoldingOrderLineUpdateInstanceStrategyTest {
  }

  @Nested
  class ExportHistoryImplTestNested extends ExportHistoryImplIT {
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
  class OrderFiscalYearServiceTestNested extends OrderFiscalYearServiceTest {
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
  class CirculationRequestsRetrieverTestNested extends CirculationRequestsRetrieverIT {
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
  class PiecesClaimingApiTestNested extends PiecesClaimingApiIT {
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

  @Nested
  class WrapperPiecesAPITestNested extends WrapperPiecesAPIIT {
  }

  @Nested
  class TitlesServiceTestNested extends TitlesServiceTest {
  }

  @Nested
  class CacheableExchangeRateServiceTestNested extends CacheableExchangeRateServiceTest {
  }

  @Nested
  class EncumbranceUtilsNestest extends EncumbranceUtilsTest {
  }

  @Nested
  class BatchTrackingServiceTestNested extends BatchTrackingServiceTest {
  }

  @Nested
  class ConsortiumUserTenantServiceTestNested extends ConsortiumUserTenantServiceTest {
  }
}
