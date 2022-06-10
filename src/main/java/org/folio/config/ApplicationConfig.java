package org.folio.config;

import java.util.EnumMap;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.folio.helper.PoNumberHelper;
import org.folio.helper.PurchaseOrderHelper;
import org.folio.helper.PurchaseOrderLineHelper;
import org.folio.rest.core.RestClient;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CreateInventoryType;
import org.folio.rest.jaxrs.model.PatchOrderLineRequest;
import org.folio.service.AcquisitionMethodsService;
import org.folio.service.AcquisitionsUnitsService;
import org.folio.service.FundsDistributionService;
import org.folio.service.PrefixService;
import org.folio.service.ProtectionService;
import org.folio.service.ReasonForClosureService;
import org.folio.service.SuffixService;
import org.folio.service.TagService;
import org.folio.service.configuration.ConfigurationEntriesService;
import org.folio.service.exchange.ExchangeRateProviderResolver;
import org.folio.service.exchange.FinanceExchangeRateService;
import org.folio.service.finance.FiscalYearService;
import org.folio.service.finance.FundService;
import org.folio.service.finance.LedgerService;
import org.folio.service.finance.budget.BudgetRestrictionService;
import org.folio.service.finance.budget.BudgetService;
import org.folio.service.finance.expenceclass.BudgetExpenseClassService;
import org.folio.service.finance.expenceclass.ExpenseClassService;
import org.folio.service.finance.expenceclass.ExpenseClassValidationService;
import org.folio.service.finance.rollover.RolloverErrorService;
import org.folio.service.finance.rollover.RolloverRetrieveService;
import org.folio.service.finance.transaction.ClosedToOpenEncumbranceStrategy;
import org.folio.service.finance.transaction.EncumbranceRelationsHoldersBuilder;
import org.folio.service.finance.transaction.EncumbranceService;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategy;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategyFactory;
import org.folio.service.finance.transaction.EncumbrancesProcessingHolderBuilder;
import org.folio.service.finance.transaction.OpenToClosedEncumbranceStrategy;
import org.folio.service.finance.transaction.OpenToPendingEncumbranceStrategy;
import org.folio.service.finance.transaction.PendingToOpenEncumbranceStrategy;
import org.folio.service.finance.transaction.PendingToPendingEncumbranceStrategy;
import org.folio.service.finance.transaction.ReceivingEncumbranceStrategy;
import org.folio.service.finance.transaction.TransactionService;
import org.folio.service.finance.transaction.TransactionSummariesService;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.invoice.InvoiceLineService;
import org.folio.service.invoice.InvoiceService;
import org.folio.service.orders.CombinedOrderDataPopulateService;
import org.folio.service.orders.CompositeOrderDynamicDataPopulateService;
import org.folio.service.orders.CompositeOrderRetrieveHolderBuilder;
import org.folio.service.orders.CompositePoLineValidationService;
import org.folio.service.orders.HoldingsSummaryService;
import org.folio.service.orders.OrderInvoiceRelationService;
import org.folio.service.orders.OrderLinesSummaryPopulateService;
import org.folio.service.orders.OrderReEncumberService;
import org.folio.service.orders.OrderRolloverService;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.folio.service.orders.ReEncumbranceHoldersBuilder;
import org.folio.service.orders.TransactionsTotalFieldsPopulateService;
import org.folio.service.orders.flows.update.open.OpenCompositeOrderFlowValidator;
import org.folio.service.orders.flows.update.open.OpenCompositeOrderHolderBuilder;
import org.folio.service.orders.flows.update.open.OpenCompositeOrderInventoryService;
import org.folio.service.orders.flows.update.open.OpenCompositeOrderManager;
import org.folio.service.orders.flows.update.open.OpenCompositeOrderPieceService;
import org.folio.service.orders.flows.update.reopen.ReOpenCompositeOrderManager;
import org.folio.service.orders.flows.update.unopen.UnOpenCompositeOrderManager;
import org.folio.service.orders.lines.update.OrderLinePatchOperationHandlerResolver;
import org.folio.service.orders.lines.update.OrderLinePatchOperationService;
import org.folio.service.orders.lines.update.OrderLineUpdateInstanceHandler;
import org.folio.service.orders.lines.update.OrderLineUpdateInstanceStrategy;
import org.folio.service.orders.lines.update.OrderLineUpdateInstanceStrategyResolver;
import org.folio.service.orders.lines.update.PatchOperationHandler;
import org.folio.service.orders.lines.update.instance.WithHoldingOrderLineUpdateInstanceStrategy;
import org.folio.service.orders.lines.update.instance.WithoutHoldingOrderLineUpdateInstanceStrategy;
import org.folio.service.pieces.PieceChangeReceiptStatusPublisher;
import org.folio.service.pieces.PieceService;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.pieces.PieceUpdateInventoryService;
import org.folio.service.pieces.flows.BasePieceFlowHolderBuilder;
import org.folio.service.pieces.flows.DefaultPieceFlowsValidator;
import org.folio.service.pieces.flows.create.PieceCreateFlowInventoryManager;
import org.folio.service.pieces.flows.create.PieceCreateFlowManager;
import org.folio.service.pieces.flows.create.PieceCreateFlowPoLineService;
import org.folio.service.pieces.flows.delete.PieceDeleteFlowManager;
import org.folio.service.pieces.flows.delete.PieceDeleteFlowPoLineService;
import org.folio.service.pieces.flows.strategies.ProcessInventoryElectronicStrategy;
import org.folio.service.pieces.flows.strategies.ProcessInventoryMixedStrategy;
import org.folio.service.pieces.flows.strategies.ProcessInventoryPhysicalStrategy;
import org.folio.service.pieces.flows.strategies.ProcessInventoryStrategy;
import org.folio.service.pieces.flows.strategies.ProcessInventoryStrategyResolver;
import org.folio.service.pieces.flows.update.PieceUpdateFlowInventoryManager;
import org.folio.service.pieces.flows.update.PieceUpdateFlowManager;
import org.folio.service.pieces.flows.update.PieceUpdateFlowPoLineService;
import org.folio.service.titles.TitlesService;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

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

  @Bean PurchaseOrderStorageService purchaseOrderService(RestClient restClient, PurchaseOrderLineService purchaseOrderLineService) {
    return new PurchaseOrderStorageService(restClient, purchaseOrderLineService);
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
  OrderRolloverService rolloverOrderService(FundService fundService, PurchaseOrderStorageService purchaseOrderStorageService,
                                            PurchaseOrderLineService purchaseOrderLineService, TransactionService transactionService,
                                            ConfigurationEntriesService configurationEntriesService, ExchangeRateProviderResolver exchangeRateProviderResolver) {
    return new OrderRolloverService(fundService, purchaseOrderStorageService, purchaseOrderLineService, transactionService,
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
  EncumbranceService encumbranceService(TransactionService transactionService,
      TransactionSummariesService transactionSummariesService,
      InvoiceLineService invoiceLineService,
      OrderInvoiceRelationService orderInvoiceRelationService,
      FiscalYearService fiscalYearService) {
    return new EncumbranceService(transactionService, transactionSummariesService, invoiceLineService, orderInvoiceRelationService, fiscalYearService);
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
  InvoiceLineService invoiceLineService(RestClient restClient) {
    return new InvoiceLineService(restClient);
  }

  @Bean
  InvoiceService invoiceService(RestClient restClient, OrderInvoiceRelationService orderInvoiceRelationService) {
    return new InvoiceService(restClient, orderInvoiceRelationService);
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
  EncumbranceWorkflowStrategy openToClosedEncumbranceStrategy(EncumbranceService encumbranceService, EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder) {
    return new OpenToClosedEncumbranceStrategy(encumbranceService, encumbranceRelationsHoldersBuilder);
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
  OrderReEncumberService orderReEncumberService(PurchaseOrderStorageService purchaseOrderStorageService,
                                                ReEncumbranceHoldersBuilder reEncumbranceHoldersBuilder,
                                                RolloverErrorService rolloverErrorService,
                                                RolloverRetrieveService rolloverRetrieveService,
                                                PurchaseOrderLineService purchaseOrderLineService,
                                                TransactionService transactionService,
                                                TransactionSummariesService transactionSummaryService,
                                                BudgetRestrictionService budgetRestrictionService) {
    return new OrderReEncumberService(purchaseOrderStorageService, reEncumbranceHoldersBuilder, rolloverErrorService,
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
  SuffixService suffixService(RestClient restClient, PurchaseOrderStorageService purchaseOrderStorageService) {
    return new SuffixService(restClient, purchaseOrderStorageService);
  }

  @Bean
  PrefixService prefixService(RestClient restClient, PurchaseOrderStorageService purchaseOrderStorageService) {
    return new PrefixService(restClient, purchaseOrderStorageService);
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
  OrderInvoiceRelationService orderInvoiceRelationService(RestClient orderInvoiceRelationRestClient, InvoiceLineService invoiceLineService) {
    return new OrderInvoiceRelationService(orderInvoiceRelationRestClient, invoiceLineService);
  }

  @Bean
  TagService tagService(RestClient restClient) {
    return new TagService(restClient);
  }

  @Bean
  HoldingsSummaryService holdingsSummaryService(PurchaseOrderStorageService purchaseOrderStorageService,
      PurchaseOrderLineService purchaseOrderLineService, PieceStorageService pieceStorageService) {
    return new HoldingsSummaryService(purchaseOrderStorageService, purchaseOrderLineService, pieceStorageService);
  }

  @Bean
  CompositeOrderDynamicDataPopulateService totalExpendedPopulateService(TransactionService transactionService) {
    return new TransactionsTotalFieldsPopulateService(transactionService);
  }

  @Bean("orderLinesSummaryPopulateService")
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
  AcquisitionMethodsService acquisitionMethodsService(RestClient restClient, PurchaseOrderLineService purchaseOrderLineService) {
    return new AcquisitionMethodsService(restClient, purchaseOrderLineService);
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
                              PurchaseOrderStorageService purchaseOrderStorageService, PieceUpdateInventoryService pieceUpdateInventoryService) {
    return new PieceService(pieceStorageService, protectionService, purchaseOrderLineService, inventoryManager,
                              receiptStatusPublisher, pieceUpdateInventoryService);
  }

  @Bean DefaultPieceFlowsValidator pieceCreateFlowValidator() {
    return new DefaultPieceFlowsValidator();
  }

  @Bean PieceCreateFlowPoLineService pieceCreateFlowPoLineService(PurchaseOrderStorageService purchaseOrderStorageService,
    PurchaseOrderLineService purchaseOrderLineService, ReceivingEncumbranceStrategy receivingEncumbranceStrategy) {
    return new PieceCreateFlowPoLineService(purchaseOrderStorageService, purchaseOrderLineService, receivingEncumbranceStrategy);
  }

  @Bean PieceCreateFlowManager pieceCreationService(PieceStorageService pieceStorageService, ProtectionService protectionService,
                PieceCreateFlowInventoryManager pieceCreateFlowInventoryManager, DefaultPieceFlowsValidator defaultPieceFlowsValidator,
                PieceCreateFlowPoLineService pieceCreateFlowPoLineService, BasePieceFlowHolderBuilder basePieceFlowHolderBuilder) {
    return new PieceCreateFlowManager(pieceStorageService, protectionService, pieceCreateFlowInventoryManager,
      defaultPieceFlowsValidator, pieceCreateFlowPoLineService, basePieceFlowHolderBuilder);
  }

  @Bean
  UnOpenCompositeOrderManager unOpenCompositeOrderManager(PurchaseOrderLineService purchaseOrderLineService,
                                      EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory,
                                      InventoryManager inventoryManager, PieceStorageService pieceStorageService,
                                      PurchaseOrderStorageService purchaseOrderStorageService,
                                      ProtectionService protectionService) {
    return new UnOpenCompositeOrderManager(purchaseOrderLineService, encumbranceWorkflowStrategyFactory, inventoryManager,
                                          pieceStorageService, purchaseOrderStorageService, protectionService);
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

  @Bean PieceUpdateInventoryService pieceUpdateInventoryService(InventoryManager inventoryManager,
                                PieceStorageService pieceStorageService) {
    return new PieceUpdateInventoryService(inventoryManager, pieceStorageService);
  }

  @Bean PieceDeleteFlowPoLineService pieceDeleteFlowPoLineService(PurchaseOrderStorageService purchaseOrderStorageService,
                    PurchaseOrderLineService purchaseOrderLineService, ReceivingEncumbranceStrategy receivingEncumbranceStrategy) {
    return new PieceDeleteFlowPoLineService(purchaseOrderStorageService, purchaseOrderLineService, receivingEncumbranceStrategy);
  }

  @Bean PieceDeleteFlowManager pieceDeletionFlowManager(PieceStorageService pieceStorageService, ProtectionService protectionService,
    InventoryManager inventoryManager, PieceUpdateInventoryService pieceUpdateInventoryService,
    PieceDeleteFlowPoLineService pieceDeleteFlowPoLineService, BasePieceFlowHolderBuilder basePieceFlowHolderBuilder) {
    return new PieceDeleteFlowManager(pieceStorageService, protectionService, inventoryManager, pieceUpdateInventoryService,
                      pieceDeleteFlowPoLineService, basePieceFlowHolderBuilder);
  }


  @Bean BasePieceFlowHolderBuilder basePieceFlowHolderBuilder(PurchaseOrderStorageService purchaseOrderStorageService, PurchaseOrderLineService purchaseOrderLineService) {
      return new BasePieceFlowHolderBuilder(purchaseOrderStorageService, purchaseOrderLineService);
  }

  @Bean PieceUpdateFlowPoLineService pieceUpdateFlowPoLineService(PurchaseOrderStorageService purchaseOrderStorageService, PurchaseOrderLineService purchaseOrderLineService,
          ReceivingEncumbranceStrategy receivingEncumbranceStrategy, PieceDeleteFlowPoLineService pieceDeleteFlowPoLineService,
          PieceCreateFlowPoLineService pieceCreateFlowPoLineService) {
    return new PieceUpdateFlowPoLineService(purchaseOrderStorageService, purchaseOrderLineService, receivingEncumbranceStrategy,
          pieceCreateFlowPoLineService, pieceDeleteFlowPoLineService);
  }

  @Bean PieceUpdateFlowManager pieceUpdateFlowManager(PieceStorageService pieceStorageService, PieceService pieceService, ProtectionService protectionService,
            PieceUpdateFlowPoLineService pieceUpdateFlowPoLineService, PieceUpdateFlowInventoryManager pieceUpdateFlowInventoryManager,
            BasePieceFlowHolderBuilder basePieceFlowHolderBuilder, DefaultPieceFlowsValidator defaultPieceFlowsValidator) {
    return new PieceUpdateFlowManager(pieceStorageService, pieceService, protectionService, pieceUpdateFlowPoLineService,
                            pieceUpdateFlowInventoryManager, basePieceFlowHolderBuilder, defaultPieceFlowsValidator);
  }

  @Bean PieceUpdateFlowInventoryManager pieceUpdateFlowInventoryManager(TitlesService titlesService,
    PieceUpdateInventoryService pieceUpdateInventoryService, InventoryManager inventoryManager) {
    return new PieceUpdateFlowInventoryManager(titlesService, pieceUpdateInventoryService, inventoryManager);
  }

  @Bean PieceCreateFlowInventoryManager pieceCreateFlowInventoryManager(TitlesService titlesService,
                            PieceUpdateInventoryService pieceUpdateInventoryService, InventoryManager inventoryManager) {
    return new PieceCreateFlowInventoryManager(titlesService, pieceUpdateInventoryService, inventoryManager);
  }

  @Bean OpenCompositeOrderPieceService openCompositeOrderPieceCreateService(PurchaseOrderStorageService purchaseOrderStorageService,
              PieceStorageService pieceStorageService, ProtectionService protectionService,
              PieceChangeReceiptStatusPublisher receiptStatusPublisher, InventoryManager inventoryManager, TitlesService titlesService,
              OpenCompositeOrderHolderBuilder openCompositeOrderHolderBuilder) {
    return new OpenCompositeOrderPieceService(purchaseOrderStorageService, pieceStorageService, protectionService,
                              receiptStatusPublisher, inventoryManager, titlesService, openCompositeOrderHolderBuilder);
  }

  @Bean OpenCompositeOrderInventoryService openCompositeOrderInventoryService(InventoryManager inventoryManager,
                                                                              OpenCompositeOrderPieceService openCompositeOrderPieceService,
                                                                              ProcessInventoryStrategyResolver processInventoryStrategyResolver) {
    return new OpenCompositeOrderInventoryService(inventoryManager, openCompositeOrderPieceService, processInventoryStrategyResolver) ;
  }

  @Bean OpenCompositeOrderFlowValidator openCompositeOrderFlowValidator(ExpenseClassValidationService expenseClassValidationService,
    PieceStorageService pieceStorageService, EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory,
    CompositePoLineValidationService compositePoLineValidationService) {
    return new OpenCompositeOrderFlowValidator(expenseClassValidationService, pieceStorageService,
      encumbranceWorkflowStrategyFactory, compositePoLineValidationService);
  }

  @Bean PoNumberHelper poNumberHelper(RestClient restClient, PurchaseOrderStorageService purchaseOrderStorageService) {
    return new PoNumberHelper(restClient, purchaseOrderStorageService);
  }

  @Bean
  PurchaseOrderHelper purchaseOrderHelper(PurchaseOrderLineHelper purchaseOrderLineHelper,
    CompositeOrderDynamicDataPopulateService orderLinesSummaryPopulateService, EncumbranceService encumbranceService,
    CompositeOrderDynamicDataPopulateService combinedPopulateService,
    EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory, OrderInvoiceRelationService orderInvoiceRelationService,
    TagService tagService, PurchaseOrderLineService purchaseOrderLineService, TitlesService titlesService,
    AcquisitionsUnitsService acquisitionsUnitsService, ProtectionService protectionService, InventoryManager inventoryManager,
    UnOpenCompositeOrderManager unOpenCompositeOrderManager,
    OpenCompositeOrderManager openCompositeOrderManager, PurchaseOrderStorageService purchaseOrderStorageService,
    ConfigurationEntriesService configurationEntriesService, PoNumberHelper poNumberHelper,
    OpenCompositeOrderFlowValidator openCompositeOrderFlowValidator,
    CompositePoLineValidationService compositePoLineValidationService, ReOpenCompositeOrderManager reOpenCompositeOrderManager) {
    return new PurchaseOrderHelper(purchaseOrderLineHelper, orderLinesSummaryPopulateService, encumbranceService,
      combinedPopulateService, encumbranceWorkflowStrategyFactory, orderInvoiceRelationService, tagService,
      purchaseOrderLineService, titlesService, acquisitionsUnitsService, protectionService, inventoryManager,
      unOpenCompositeOrderManager, openCompositeOrderManager, purchaseOrderStorageService, configurationEntriesService,
      poNumberHelper, openCompositeOrderFlowValidator, compositePoLineValidationService, reOpenCompositeOrderManager);
  }

  @Bean PurchaseOrderLineHelper purchaseOrderLineHelper(InventoryManager inventoryManager, EncumbranceService encumbranceService,
        ExpenseClassValidationService expenseClassValidationService,
        EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory, OrderInvoiceRelationService orderInvoiceRelationService,
        TitlesService titlesService, AcquisitionsUnitsService acquisitionsUnitsService, ProtectionService protectionService,
        PurchaseOrderLineService purchaseOrderLineService, PurchaseOrderStorageService purchaseOrderStorageService,
        ConfigurationEntriesService configurationEntriesService, RestClient restClient,
        CompositePoLineValidationService compositePoLineValidationService) {
    return new PurchaseOrderLineHelper(inventoryManager, encumbranceService, expenseClassValidationService,
      encumbranceWorkflowStrategyFactory, orderInvoiceRelationService, titlesService, acquisitionsUnitsService, protectionService,
      purchaseOrderLineService, purchaseOrderStorageService, configurationEntriesService, restClient, compositePoLineValidationService);
  }

  @Bean CompositePoLineValidationService compositePoLineValidationService(ExpenseClassValidationService expenseClassValidationService) {
    return new CompositePoLineValidationService(expenseClassValidationService);
  }

  @Bean ReOpenCompositeOrderManager reOpenCompositeOrderManager(EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory,
                    PieceStorageService pieceStorageService, InvoiceLineService invoiceLineService, InvoiceService invoiceService) {
    return new ReOpenCompositeOrderManager(encumbranceWorkflowStrategyFactory, pieceStorageService, invoiceLineService,
        invoiceService);
  }

  @Bean OpenCompositeOrderManager openCompositeOrderManager(PurchaseOrderLineService purchaseOrderLineService,
    EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory,
    TitlesService titlesService, OpenCompositeOrderInventoryService openCompositeOrderInventoryService,
    OpenCompositeOrderFlowValidator openCompositeOrderFlowValidator) {
    return new OpenCompositeOrderManager(purchaseOrderLineService, encumbranceWorkflowStrategyFactory,
      titlesService, openCompositeOrderInventoryService, openCompositeOrderFlowValidator);
  }

  @Bean OpenCompositeOrderHolderBuilder openCompositeOrderHolderBuilder(PieceStorageService pieceStorageService) {
    return new OpenCompositeOrderHolderBuilder(pieceStorageService);
  }

  @Bean ProcessInventoryStrategyResolver resolver() {
    Map<String, ProcessInventoryStrategy> strategy = new HashMap<>();

    ProcessInventoryElectronicStrategy processInventoryElectronicStrategy = new ProcessInventoryElectronicStrategy();
    ProcessInventoryPhysicalStrategy processInventoryPhysicalStrategy = new ProcessInventoryPhysicalStrategy();
    ProcessInventoryMixedStrategy processInventoryMixedStrategy = new ProcessInventoryMixedStrategy();

    strategy.put(CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE.value(), processInventoryElectronicStrategy);
    strategy.put(CompositePoLine.OrderFormat.PHYSICAL_RESOURCE.value(), processInventoryPhysicalStrategy);
    strategy.put(CompositePoLine.OrderFormat.OTHER.value(), processInventoryPhysicalStrategy);
    strategy.put(CompositePoLine.OrderFormat.P_E_MIX.value(), processInventoryMixedStrategy);

    return new ProcessInventoryStrategyResolver(strategy);
  }

  @Bean OrderLinePatchOperationService orderLinePatchOperationService(
      RestClient restClient,
      OrderLinePatchOperationHandlerResolver orderLinePatchOperationHandlerResolver,
      PurchaseOrderLineService purchaseOrderLineService) {
    return new OrderLinePatchOperationService(restClient, orderLinePatchOperationHandlerResolver, purchaseOrderLineService);
  }

  @Bean PatchOperationHandler orderLineUpdateInstanceHandler(
      OrderLineUpdateInstanceStrategyResolver updateInstanceStrategyResolver) {
    return new OrderLineUpdateInstanceHandler(updateInstanceStrategyResolver);
  }

  @Bean OrderLinePatchOperationHandlerResolver orderLinePatchOperationHandlerResolver(
      PatchOperationHandler orderLineUpdateInstanceHandler) {
    Map<PatchOrderLineRequest.Operation, PatchOperationHandler> handlers = new EnumMap<>(PatchOrderLineRequest.Operation.class);
    handlers.put(PatchOrderLineRequest.Operation.REPLACE_INSTANCE_REF, orderLineUpdateInstanceHandler);
    return new OrderLinePatchOperationHandlerResolver(handlers);
  }

  @Bean OrderLineUpdateInstanceStrategy withHoldingOrderLineUpdateInstanceStrategy(InventoryManager inventoryManager) {
    return new WithHoldingOrderLineUpdateInstanceStrategy(inventoryManager);
  }

  @Bean OrderLineUpdateInstanceStrategy withoutHoldingOrderLineUpdateInstanceStrategy() {
    return new WithoutHoldingOrderLineUpdateInstanceStrategy();
  }

  @Bean OrderLineUpdateInstanceStrategyResolver updateInstanceStrategyResolver(OrderLineUpdateInstanceStrategy withHoldingOrderLineUpdateInstanceStrategy,
      OrderLineUpdateInstanceStrategy withoutHoldingOrderLineUpdateInstanceStrategy) {
    Map<CreateInventoryType, OrderLineUpdateInstanceStrategy> strategies = new EnumMap<>(CreateInventoryType.class);

    strategies.put(CreateInventoryType.INSTANCE_HOLDING_ITEM, withHoldingOrderLineUpdateInstanceStrategy);
    strategies.put(CreateInventoryType.INSTANCE_HOLDING, withHoldingOrderLineUpdateInstanceStrategy);
    strategies.put(CreateInventoryType.INSTANCE, withoutHoldingOrderLineUpdateInstanceStrategy);
    strategies.put(CreateInventoryType.NONE, withoutHoldingOrderLineUpdateInstanceStrategy);

    return new OrderLineUpdateInstanceStrategyResolver(strategies);
  }
}
