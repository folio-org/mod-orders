package org.folio.config;

import org.folio.service.finance.transaction.EncumbrancesProcessingHolderBuilder;
import org.folio.service.finance.transaction.PendingToPendingEncumbranceStrategy;
import org.folio.service.finance.transaction.ReceivingEncumbranceStrategy;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.ProtectionService;
import org.folio.service.AcquisitionsUnitsService;
import org.folio.rest.core.RestClient;
import org.folio.service.FundsDistributionService;
import org.folio.service.PrefixService;
import org.folio.service.ReasonForClosureService;
import org.folio.service.SuffixService;
import org.folio.service.TagService;
import org.folio.service.configuration.ConfigurationEntriesService;
import org.folio.service.exchange.ExchangeRateProviderResolver;
import org.folio.service.exchange.FinanceExchangeRateService;
import org.folio.service.finance.expenceclass.BudgetExpenseClassService;
import org.folio.service.finance.budget.BudgetRestrictionService;
import org.folio.service.finance.budget.BudgetService;
import org.folio.service.finance.expenceclass.ExpenseClassService;
import org.folio.service.finance.expenceclass.ExpenseClassValidationService;
import org.folio.service.finance.FiscalYearService;
import org.folio.service.finance.FundService;
import org.folio.service.finance.LedgerService;
import org.folio.service.finance.rollover.RolloverErrorService;
import org.folio.service.finance.rollover.RolloverRetrieveService;
import org.folio.service.finance.transaction.ClosedToOpenEncumbranceStrategy;
import org.folio.service.finance.transaction.EncumbranceRelationsHoldersBuilder;
import org.folio.service.finance.transaction.EncumbranceService;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategy;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategyFactory;
import org.folio.service.finance.transaction.OpenToClosedEncumbranceStrategy;
import org.folio.service.finance.transaction.OpenToPendingEncumbranceStrategy;
import org.folio.service.finance.transaction.PendingToOpenEncumbranceStrategy;
import org.folio.service.finance.transaction.TransactionService;
import org.folio.service.finance.transaction.TransactionSummariesService;
import org.folio.service.invoice.InvoiceService;
import org.folio.service.orders.HoldingsSummaryService;
import org.folio.service.orders.CombinedOrderDataPopulateService;
import org.folio.service.orders.CompositeOrderDynamicDataPopulateService;
import org.folio.service.orders.CompositeOrderRetrieveHolderBuilder;
import org.folio.service.orders.OrderInvoiceRelationService;
import org.folio.service.orders.OrderLinesSummaryPopulateService;
import org.folio.service.orders.OrderReEncumberService;
import org.folio.service.orders.OrderRolloverService;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderService;
import org.folio.service.orders.ReEncumbranceHoldersBuilder;
import org.folio.service.orders.TransactionsTotalFieldsPopulateService;
import org.folio.service.orders.flows.unopen.UnOpenCompositeOrderManager;
import org.folio.service.pieces.PieceChangeReceiptStatusPublisher;
import org.folio.service.pieces.PieceCreationFlowManager;
import org.folio.service.pieces.PieceDeletionFlowManager;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.pieces.PieceService;
import org.folio.service.pieces.PieceUpdateInventoryService;
import org.folio.service.titles.TitlesService;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

import java.util.Set;

@Configuration
@ComponentScan({ "org.folio" })
public class ApplicationConfig {

  @Bean
  RestClient restClient() {
    return new RestClient();
  }

  @Bean
  ExchangeRateProviderResolver exchangeRateProviderResolver() {
    return new ExchangeRateProviderResolver();
  }

  @Bean
  FinanceExchangeRateService rateOfExchangeService(RestClient restClient) {
    return new FinanceExchangeRateService(restClient);
  }

  @Bean
  PurchaseOrderService purchaseOrderService(RestClient restClient, PurchaseOrderLineService purchaseOrderLineService) {
    return new PurchaseOrderService(restClient, purchaseOrderLineService);
  }

  @Bean
  PurchaseOrderLineService purchaseOrderLineService(RestClient restClient) {
    return new PurchaseOrderLineService(restClient);
  }

  @Bean
  FundService fundService(RestClient restClient) {
    return new FundService(restClient);
  }

  @Bean
  OrderRolloverService rolloverOrderService(FundService fundService, PurchaseOrderService purchaseOrderService,
                                            PurchaseOrderLineService purchaseOrderLineService, TransactionService transactionService,
                                            ConfigurationEntriesService configurationEntriesService, ExchangeRateProviderResolver exchangeRateProviderResolver) {
    return new OrderRolloverService(fundService, purchaseOrderService, purchaseOrderLineService, transactionService,
                                    configurationEntriesService, exchangeRateProviderResolver);
  }

  @Bean
  BudgetExpenseClassService budgetExpenseClassService(RestClient restClient) {
    return new BudgetExpenseClassService(restClient);
  }

  @Bean
  BudgetService budgetService(RestClient restClient) {
    return new BudgetService(restClient);
  }

  @Bean
  LedgerService ledgerService(RestClient restClient) {
    return new LedgerService(restClient);
  }

  @Bean
  ConfigurationEntriesService configurationEntriesService(RestClient restClient) {
    return new ConfigurationEntriesService(restClient);
  }

  @Bean
  BudgetRestrictionService budgetRestrictionService() {
    return new BudgetRestrictionService();
  }

  @Bean
  EncumbranceService encumbranceService(TransactionService transactionService, TransactionSummariesService transactionSummariesService, InvoiceService invoiceService) {
    return new EncumbranceService(transactionService, transactionSummariesService, invoiceService);
  }

  @Bean
  ExpenseClassService expenseClassService(RestClient restClient) {
    return new ExpenseClassService(restClient);
  }

  @Bean
  ExpenseClassValidationService expenseClassValidationService(BudgetExpenseClassService budgetExpenseClassService, ExpenseClassService expenseClassService) {
    return new ExpenseClassValidationService(budgetExpenseClassService, expenseClassService);
  }

  @Bean
  FiscalYearService fiscalYearService(RestClient restClient, FundService fundService) {
    return new FiscalYearService(restClient, fundService);
  }

  @Bean
  TransactionService transactionService(RestClient restClient) {
    return new TransactionService(restClient);
  }

  @Bean
  TransactionSummariesService transactionSummariesService(RestClient restClient) {
    return new TransactionSummariesService(restClient);
  }

  @Bean
  EncumbranceWorkflowStrategy openToPendingEncumbranceStrategy(EncumbranceService encumbranceService,
      TransactionSummariesService transactionSummariesService,
      EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder) {
    return new OpenToPendingEncumbranceStrategy(encumbranceService, transactionSummariesService,
        encumbranceRelationsHoldersBuilder);
  }

  @Bean
  FundsDistributionService fundsDistributionService() {
    return new FundsDistributionService();
  }

  @Bean
  InvoiceService invoiceService(RestClient restClient) {
    return new InvoiceService(restClient);
  }

  @Bean EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder(EncumbranceService encumbranceService,
                                                                              FundService fundService,
                                                                              FiscalYearService fiscalYearService,
                                                                              ExchangeRateProviderResolver exchangeRateProviderResolver,
                                                                              BudgetService budgetService,
                                                                              LedgerService ledgerService) {
    return new EncumbranceRelationsHoldersBuilder(encumbranceService, fundService, fiscalYearService, exchangeRateProviderResolver, budgetService,
                                                  ledgerService);
  }

  @Bean
  EncumbranceWorkflowStrategy pendingToOpenEncumbranceStrategy(EncumbranceService encumbranceService,
          FundsDistributionService fundsDistributionService, BudgetRestrictionService budgetRestrictionService,
          EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder,
          EncumbrancesProcessingHolderBuilder encumbrancesProcessingHolderBuilder) {
    return new PendingToOpenEncumbranceStrategy(encumbranceService, fundsDistributionService,
                                                budgetRestrictionService, encumbranceRelationsHoldersBuilder,
                                                encumbrancesProcessingHolderBuilder);
  }

  @Bean
  EncumbranceWorkflowStrategy openToClosedEncumbranceStrategy(EncumbranceService encumbranceService) {
    return new OpenToClosedEncumbranceStrategy(encumbranceService);
  }

  @Bean
  EncumbranceWorkflowStrategy closedToOpenEncumbranceStrategy(EncumbranceService encumbranceService,
      FundsDistributionService fundsDistributionService,
      BudgetRestrictionService budgetRestrictionService,
      EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder) {
    return new ClosedToOpenEncumbranceStrategy(encumbranceService, fundsDistributionService,
      budgetRestrictionService, encumbranceRelationsHoldersBuilder);
  }

  @Bean
  EncumbranceWorkflowStrategy pendingToPendingEncumbranceStrategy(EncumbranceService encumbranceService,
      EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder) {
    return new PendingToPendingEncumbranceStrategy(encumbranceService,  encumbranceRelationsHoldersBuilder);
  }

  @Bean
  ReEncumbranceHoldersBuilder reEncumbranceHoldersBuilder(BudgetService budgetService,
                                                          LedgerService ledgerService,
                                                          FundService fundService,
                                                          ExchangeRateProviderResolver exchangeRateProviderResolver,
                                                          FiscalYearService fiscalYearService,
                                                          RolloverRetrieveService rolloverRetrieveService,
                                                          TransactionService transactionService,
                                                          FundsDistributionService fundsDistributionService) {
    return new ReEncumbranceHoldersBuilder(budgetService,
                                           ledgerService,
                                           fundService,
                                           exchangeRateProviderResolver,
                                           fiscalYearService,
                                           rolloverRetrieveService,
                                           transactionService, fundsDistributionService);
  }

  @Bean
  OrderReEncumberService orderReEncumberService(PurchaseOrderService purchaseOrderService,
                                                ReEncumbranceHoldersBuilder reEncumbranceHoldersBuilder,
                                                RolloverErrorService rolloverErrorService,
                                                RolloverRetrieveService rolloverRetrieveService,
                                                PurchaseOrderLineService purchaseOrderLineService,
                                                TransactionService transactionService,
                                                TransactionSummariesService transactionSummaryService,
                                                BudgetRestrictionService budgetRestrictionService) {
    return new OrderReEncumberService(purchaseOrderService, reEncumbranceHoldersBuilder, rolloverErrorService,
                                      rolloverRetrieveService, purchaseOrderLineService, transactionService,
                                      transactionSummaryService, budgetRestrictionService);
  }

  @Bean
  RolloverErrorService rolloverErrorService(RestClient restClient) {
    return new RolloverErrorService(restClient);
  }

  @Bean
  RolloverRetrieveService rolloverRetrieveService(RestClient restClient) {
    return new RolloverRetrieveService(restClient);
  }

  @Bean
  SuffixService suffixService(RestClient restClient, PurchaseOrderService purchaseOrderService) {
    return new SuffixService(restClient, purchaseOrderService);
  }

  @Bean
  PrefixService prefixService(RestClient restClient, PurchaseOrderService purchaseOrderService) {
    return new PrefixService(restClient, purchaseOrderService);
  }

  @Bean
  ReasonForClosureService reasonForClosureService(RestClient restClient) {
    return new ReasonForClosureService(restClient);
  }

  @Bean
  EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory(Set<EncumbranceWorkflowStrategy> strategies) {
    return new EncumbranceWorkflowStrategyFactory(strategies);
  }

  @Bean
  OrderInvoiceRelationService orderInvoiceRelationService(RestClient orderInvoiceRelationRestClient) {
    return new OrderInvoiceRelationService(orderInvoiceRelationRestClient);
  }

  @Bean
  TagService tagService(RestClient restClient) {
    return new TagService(restClient);
  }

  @Bean
  HoldingsSummaryService holdingsSummaryService(PurchaseOrderService purchaseOrderService,
      PurchaseOrderLineService purchaseOrderLineService) {
    return new HoldingsSummaryService(purchaseOrderService, purchaseOrderLineService);
  }

  @Bean
  CompositeOrderDynamicDataPopulateService totalExpendedPopulateService(TransactionService transactionService) {
    return new TransactionsTotalFieldsPopulateService(transactionService);
  }

  @Bean
  CompositeOrderDynamicDataPopulateService orderLinesSummaryPopulateService(ConfigurationEntriesService configurationEntriesService,
                                                                            ExchangeRateProviderResolver exchangeRateProviderResolver) {
    return new OrderLinesSummaryPopulateService(configurationEntriesService, exchangeRateProviderResolver);
  }

  @Bean
  CompositeOrderRetrieveHolderBuilder compositeOrderRetrieveHolderBuilder(FiscalYearService fiscalYearService) {
    return new CompositeOrderRetrieveHolderBuilder(fiscalYearService);
  }

  @Bean
  CompositeOrderDynamicDataPopulateService combinedPopulateService(CompositeOrderRetrieveHolderBuilder compositeOrderRetrieveHolderBuilder,
                                                                   Set<CompositeOrderDynamicDataPopulateService> populateServices) {
    return new CombinedOrderDataPopulateService(compositeOrderRetrieveHolderBuilder, populateServices);
  }

  @Bean
  TitlesService titlesService(RestClient restClient, PurchaseOrderLineService purchaseOrderLineService) {
    return new TitlesService(restClient, purchaseOrderLineService);
  }

  @Bean
  AcquisitionsUnitsService acquisitionsUnitsService(RestClient restClient) {
    return new AcquisitionsUnitsService(restClient);
  }

  @Bean
  ProtectionService protectionHelper(AcquisitionsUnitsService acquisitionsUnitsService) {
    return new ProtectionService(acquisitionsUnitsService);
  }

  @Bean
  InventoryManager inventoryManager(RestClient restClient, ConfigurationEntriesService configurationEntriesService,
                                    PieceStorageService pieceStorageService) {
    return new InventoryManager(restClient, configurationEntriesService, pieceStorageService);
  }

  @Bean
  PieceChangeReceiptStatusPublisher receiptStatusPublisher() {
    return new PieceChangeReceiptStatusPublisher();
  }

  @Bean PieceStorageService pieceStorageService(RestClient restClient) {
    return new PieceStorageService(restClient);
  }

  @Bean PieceService piecesService(PieceStorageService pieceStorageService, ProtectionService protectionService,
                              PurchaseOrderLineService purchaseOrderLineService,
                              InventoryManager inventoryManager, PieceChangeReceiptStatusPublisher receiptStatusPublisher,
                              ReceivingEncumbranceStrategy receivingEncumbranceStrategy, PurchaseOrderService purchaseOrderService,
                              PieceUpdateInventoryService pieceUpdateInventoryService) {
    return new PieceService(pieceStorageService, protectionService, purchaseOrderLineService, inventoryManager,
                              receiptStatusPublisher, receivingEncumbranceStrategy,
                              purchaseOrderService, pieceUpdateInventoryService);
  }

  @Bean PieceCreationFlowManager pieceCreationService(PieceStorageService pieceStorageService, PurchaseOrderLineService purchaseOrderLineService,
                                                  PurchaseOrderService purchaseOrderService, ProtectionService protectionService,
                                                  ReceivingEncumbranceStrategy receivingEncumbranceStrategy,
                                                  PieceUpdateInventoryService pieceUpdateInventoryService) {
    return new PieceCreationFlowManager(pieceStorageService, purchaseOrderLineService, purchaseOrderService, protectionService,
      receivingEncumbranceStrategy, pieceUpdateInventoryService);
  }

  @Bean
  UnOpenCompositeOrderManager unOpenCompositeOrderManager(PurchaseOrderLineService purchaseOrderLineService,
                                      EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory,
                                      InventoryManager inventoryManager, PieceStorageService pieceStorageService,
                                      PieceDeletionFlowManager pieceDeletionFlowManager, PurchaseOrderService purchaseOrderService,
                                      ProtectionService protectionService) {
    return new UnOpenCompositeOrderManager(purchaseOrderLineService, encumbranceWorkflowStrategyFactory, inventoryManager,
                                          pieceStorageService, pieceDeletionFlowManager, purchaseOrderService, protectionService);
  }

  @Bean
  EncumbrancesProcessingHolderBuilder encumbrancesProcessingHolderBuilder() {
    return new EncumbrancesProcessingHolderBuilder();
  }

  @Bean
  EncumbranceWorkflowStrategy receivingEncumbranceStrategy(EncumbranceService encumbranceService,
                          FundsDistributionService fundsDistributionService,
                          BudgetRestrictionService budgetRestrictionService,
                          EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder,
                          TransactionSummariesService transactionSummariesService,
                          EncumbrancesProcessingHolderBuilder encumbrancesProcessingHolderBuilder) {
    return new ReceivingEncumbranceStrategy(encumbranceService, fundsDistributionService,
      budgetRestrictionService, encumbranceRelationsHoldersBuilder, transactionSummariesService,
      encumbrancesProcessingHolderBuilder);
  }

  @Bean PieceUpdateInventoryService pieceUpdateInventoryService(TitlesService titlesService, InventoryManager inventoryManager) {
    return new PieceUpdateInventoryService(titlesService, inventoryManager);
  }

  @Bean PieceDeletionFlowManager pieceDeletionFlowManager(PieceStorageService pieceStorageService, ProtectionService protectionService,
    PurchaseOrderService purchaseOrderService, PurchaseOrderLineService purchaseOrderLineService, InventoryManager inventoryManager,
    ReceivingEncumbranceStrategy receivingEncumbranceStrategy) {
    return new PieceDeletionFlowManager(pieceStorageService, protectionService, purchaseOrderService,
                                        purchaseOrderLineService, inventoryManager, receivingEncumbranceStrategy);
  }
}
