package org.folio.config;

import java.util.EnumMap;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.dao.FailedLedgerRolloverPoLineDao;
import org.folio.helper.PoNumberHelper;
import org.folio.helper.PurchaseOrderHelper;
import org.folio.helper.PurchaseOrderLineHelper;
import org.folio.kafka.KafkaConfig;
import org.folio.rest.core.RestClient;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CreateInventoryType;
import org.folio.service.AcquisitionMethodsService;
import org.folio.service.AcquisitionsUnitsService;
import org.folio.service.CirculationRequestsRetriever;
import org.folio.service.ExportHistoryService;
import org.folio.service.FundsDistributionService;
import org.folio.service.OrderTemplatesService;
import org.folio.service.PrefixService;
import org.folio.service.ProtectionService;
import org.folio.service.ReasonForClosureService;
import org.folio.service.SuffixService;
import org.folio.service.TagService;
import org.folio.service.UserService;
import org.folio.service.caches.ConfigurationEntriesCache;
import org.folio.service.caches.InventoryCache;
import org.folio.service.configuration.ConfigurationEntriesService;
import org.folio.service.consortium.ConsortiumConfigurationService;
import org.folio.service.consortium.ConsortiumUserTenantsRetriever;
import org.folio.service.consortium.SharingInstanceService;
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
import org.folio.service.finance.rollover.LedgerRolloverErrorService;
import org.folio.service.finance.rollover.LedgerRolloverProgressService;
import org.folio.service.finance.rollover.LedgerRolloverService;
import org.folio.service.finance.transaction.ClosedToOpenEncumbranceStrategy;
import org.folio.service.finance.transaction.EncumbranceRelationsHoldersBuilder;
import org.folio.service.finance.transaction.EncumbranceService;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategy;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategyFactory;
import org.folio.service.finance.transaction.EncumbrancesProcessingHolderBuilder;
import org.folio.service.finance.transaction.OpenToClosedEncumbranceStrategy;
import org.folio.service.finance.transaction.OpenToPendingEncumbranceStrategy;
import org.folio.service.finance.transaction.PendingPaymentService;
import org.folio.service.finance.transaction.PendingToOpenEncumbranceStrategy;
import org.folio.service.finance.transaction.PendingToPendingEncumbranceStrategy;
import org.folio.service.finance.transaction.ReceivingEncumbranceStrategy;
import org.folio.service.finance.transaction.TransactionService;
import org.folio.service.inventory.InventoryHoldingManager;
import org.folio.service.inventory.InventoryInstanceManager;
import org.folio.service.inventory.InventoryItemManager;
import org.folio.service.inventory.InventoryItemRequestService;
import org.folio.service.inventory.InventoryItemStatusSyncService;
import org.folio.service.inventory.InventoryService;
import org.folio.service.invoice.InvoiceLineService;
import org.folio.service.invoice.InvoiceService;
import org.folio.service.invoice.POLInvoiceLineRelationService;
import org.folio.service.orders.CombinedOrderDataPopulateService;
import org.folio.service.orders.CompositeOrderDynamicDataPopulateService;
import org.folio.service.orders.CompositeOrderRetrieveHolderBuilder;
import org.folio.service.orders.CompositePoLineValidationService;
import org.folio.service.orders.HoldingsSummaryService;
import org.folio.service.orders.OrderInvoiceRelationService;
import org.folio.service.orders.OrderLinesSummaryPopulateService;
import org.folio.service.orders.OrderReEncumberService;
import org.folio.service.orders.OrderRolloverService;
import org.folio.service.orders.OrderValidationService;
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
import org.folio.service.orders.lines.update.OrderLinePatchOperationService;
import org.folio.service.orders.lines.update.OrderLineUpdateInstanceStrategy;
import org.folio.service.orders.lines.update.OrderLineUpdateInstanceStrategyResolver;
import org.folio.service.orders.lines.update.instance.WithHoldingOrderLineUpdateInstanceStrategy;
import org.folio.service.orders.lines.update.instance.WithoutHoldingOrderLineUpdateInstanceStrategy;
import org.folio.service.organization.OrganizationService;
import org.folio.service.pieces.PieceChangeReceiptStatusPublisher;
import org.folio.service.pieces.PieceDeleteInventoryService;
import org.folio.service.pieces.ItemRecreateInventoryService;
import org.folio.service.pieces.PieceService;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.pieces.PieceUpdateInventoryService;
import org.folio.service.pieces.flows.BasePieceFlowHolderBuilder;
import org.folio.service.pieces.flows.DefaultPieceFlowsValidator;
import org.folio.service.pieces.flows.create.PieceCreateFlowInventoryManager;
import org.folio.service.pieces.flows.create.PieceCreateFlowManager;
import org.folio.service.pieces.flows.create.PieceCreateFlowPoLineService;
import org.folio.service.pieces.flows.delete.PieceDeleteFlowInventoryManager;
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
import org.folio.service.routinglists.RoutingListService;
import org.folio.service.settings.SettingsRetriever;
import org.folio.service.titles.TitleInstanceService;
import org.folio.service.titles.TitleValidationService;
import org.folio.service.titles.TitlesService;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

@Configuration
@ComponentScan({ "org.folio" })
public class ApplicationConfig {

  private static final Logger LOGGER = LogManager.getLogger();

  @Value("${KAFKA_HOST:kafka}")
  private String kafkaHost;
  @Value("${KAFKA_PORT:9092}")
  private String kafkaPort;
  @Value("${OKAPI_URL:http://okapi:9130}")
  private String okapiUrl;
  @Value("${REPLICATION_FACTOR:1}")
  private int replicationFactor;
  @Value("${MAX_REQUEST_SIZE:4000000}")
  private int maxRequestSize;
  @Value("${ENV:folio}")
  private String envId;

  @Bean(name = "newKafkaConfig")
  public KafkaConfig kafkaConfigBean() {
    KafkaConfig kafkaConfig = KafkaConfig.builder()
      .envId(envId)
      .kafkaHost(kafkaHost)
      .kafkaPort(kafkaPort)
      .okapiUrl(okapiUrl)
      .replicationFactor(replicationFactor)
      .maxRequestSize(maxRequestSize)
      .build();

    LOGGER.info("kafkaConfig: {}", kafkaConfig);
    return kafkaConfig;
  }

  @Bean
  RestClient restClient() {
    return new RestClient();
  }

  @Bean
  ExchangeRateProviderResolver exchangeRateProviderResolver() {
    return new ExchangeRateProviderResolver();
  }

  @Bean
  FinanceExchangeRateService financeExchangeRateService(RestClient restClient) {
    return new FinanceExchangeRateService(restClient);
  }

  @Bean PurchaseOrderStorageService purchaseOrderService(RestClient restClient, PurchaseOrderLineService purchaseOrderLineService) {
    return new PurchaseOrderStorageService(restClient, purchaseOrderLineService);
  }

  @Bean
  PurchaseOrderLineService purchaseOrderLineService(RestClient restClient,
                                                    InventoryCache inventoryCache,
                                                    InventoryHoldingManager inventoryHoldingManager) {
    return new PurchaseOrderLineService(restClient, inventoryCache, inventoryHoldingManager);
  }

  @Bean
  FundService fundService(RestClient restClient) {
    return new FundService(restClient);
  }

  @Bean
  InventoryService inventoryService(RestClient restClient) {
    return new InventoryService(restClient);
  }

  @Bean
  InventoryCache inventoryCache(InventoryService inventoryService) {
    return new InventoryCache(inventoryService);
  }

  @Bean
  ConfigurationEntriesCache configurationEntriesCache (ConfigurationEntriesService configurationEntriesService) {
    return new ConfigurationEntriesCache(configurationEntriesService);
  }

  @Bean
  OrderRolloverService rolloverOrderService(FundService fundService, PurchaseOrderLineService purchaseOrderLineService, TransactionService transactionService,
                                            ConfigurationEntriesCache configurationEntriesCache, ExchangeRateProviderResolver exchangeRateProviderResolver,
                                            LedgerRolloverProgressService ledgerRolloverProgressService, LedgerRolloverErrorService ledgerRolloverErrorService,
                                            FailedLedgerRolloverPoLineDao failedLedgerRolloverPoLineDao) {
    return new OrderRolloverService(fundService, purchaseOrderLineService, transactionService,
                                    configurationEntriesCache, exchangeRateProviderResolver,
                                    ledgerRolloverProgressService, ledgerRolloverErrorService, failedLedgerRolloverPoLineDao);
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
      InvoiceLineService invoiceLineService,
      OrderInvoiceRelationService orderInvoiceRelationService,
      FiscalYearService fiscalYearService) {
    return new EncumbranceService(transactionService, invoiceLineService, orderInvoiceRelationService, fiscalYearService);
  }

  @Bean
  PendingPaymentService pendingPaymentService(TransactionService transactionService) {
    return new PendingPaymentService(transactionService);
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
  OrganizationService organizationService(RestClient restClient) {
    return new OrganizationService(restClient);
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
  EncumbranceWorkflowStrategy openToPendingEncumbranceStrategy(EncumbranceService encumbranceService,
      EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder) {
    return new OpenToPendingEncumbranceStrategy(encumbranceService, encumbranceRelationsHoldersBuilder);
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
          EncumbrancesProcessingHolderBuilder encumbrancesProcessingHolderBuilder, POLInvoiceLineRelationService polInvoiceLineRelationService) {
    return new PendingToOpenEncumbranceStrategy(encumbranceService, fundsDistributionService,
                                                budgetRestrictionService, encumbranceRelationsHoldersBuilder,
                                                encumbrancesProcessingHolderBuilder, polInvoiceLineRelationService);
  }

  @Bean
  EncumbranceWorkflowStrategy openToClosedEncumbranceStrategy(EncumbranceService encumbranceService,
      EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder) {
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
      EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder, PendingPaymentService pendingPaymentService) {
    return new PendingToPendingEncumbranceStrategy(encumbranceService,  encumbranceRelationsHoldersBuilder, pendingPaymentService);
  }

  @Bean
  ReEncumbranceHoldersBuilder reEncumbranceHoldersBuilder(BudgetService budgetService,
                                                          LedgerService ledgerService,
                                                          FundService fundService,
                                                          ExchangeRateProviderResolver exchangeRateProviderResolver,
                                                          FiscalYearService fiscalYearService,
                                                          LedgerRolloverService ledgerRolloverService,
                                                          TransactionService transactionService,
                                                          FundsDistributionService fundsDistributionService) {
    return new ReEncumbranceHoldersBuilder(budgetService,
                                           ledgerService,
                                           fundService,
                                           exchangeRateProviderResolver,
                                           fiscalYearService,
                                           ledgerRolloverService,
                                           transactionService, fundsDistributionService);
  }

  @Bean
  OrderReEncumberService orderReEncumberService(PurchaseOrderStorageService purchaseOrderStorageService,
                                                ReEncumbranceHoldersBuilder reEncumbranceHoldersBuilder,
                                                LedgerRolloverErrorService ledgerRolloverErrorService,
                                                LedgerRolloverProgressService ledgerRolloverProgressService,
                                                PurchaseOrderLineService purchaseOrderLineService,
                                                TransactionService transactionService,
                                                BudgetRestrictionService budgetRestrictionService) {
    return new OrderReEncumberService(purchaseOrderStorageService, reEncumbranceHoldersBuilder, ledgerRolloverErrorService,
      ledgerRolloverProgressService, purchaseOrderLineService, transactionService, budgetRestrictionService);
  }

  @Bean
  LedgerRolloverErrorService ledgerRolloverErrorService(RestClient restClient) {
    return new LedgerRolloverErrorService(restClient);
  }

  @Bean
  LedgerRolloverService ledgerRolloverService(RestClient restClient) {
    return new LedgerRolloverService(restClient);
  }

  @Bean
  LedgerRolloverProgressService ledgerRolloverProgressService(RestClient restClient) {
    return new LedgerRolloverProgressService(restClient);
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
  CompositeOrderDynamicDataPopulateService orderLinesSummaryPopulateService(ConfigurationEntriesCache configurationEntriesCache,
                                                                            ExchangeRateProviderResolver exchangeRateProviderResolver) {
    return new OrderLinesSummaryPopulateService(configurationEntriesCache, exchangeRateProviderResolver);
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
  RoutingListService routingListService(RestClient restClient, PurchaseOrderLineService purchaseOrderLineService, UserService userService) {
    return new RoutingListService(restClient, purchaseOrderLineService, userService);
  }

  @Bean
  UserService userService(RestClient restClient) {
    return new UserService(restClient);
  }

  @Bean
  TitlesService titlesService(RestClient restClient, ProtectionService protectionService, TitleInstanceService titleInstanceService) {
    return new TitlesService(restClient, protectionService, titleInstanceService);
  }

  @Bean
  ExportHistoryService exportHistoryService(RestClient restClient) {
    return new ExportHistoryService(restClient);
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
  InventoryItemManager inventoryItemManager(RestClient restClient,
                                            ConfigurationEntriesCache configurationEntriesCache,
                                            InventoryCache inventoryCache,
                                            ConsortiumConfigurationService consortiumConfigurationService) {
    return new InventoryItemManager(restClient, configurationEntriesCache, inventoryCache, consortiumConfigurationService);
  }

  @Bean
  InventoryHoldingManager inventoryHoldingManager(RestClient restClient,
                                                  ConfigurationEntriesCache configurationEntriesCache,
                                                  InventoryCache inventoryCache) {
    return new InventoryHoldingManager(restClient, configurationEntriesCache, inventoryCache);
  }

  @Bean
  InventoryInstanceManager inventoryInstanceManager(RestClient restClient,
                                                    ConfigurationEntriesCache configurationEntriesCache,
                                                    InventoryCache inventoryCache,
                                                    ConsortiumConfigurationService consortiumConfigurationService,
                                                    SharingInstanceService sharingInstanceService) {
    return new InventoryInstanceManager(restClient, configurationEntriesCache,
      inventoryCache, sharingInstanceService, consortiumConfigurationService);
  }

  @Bean
  InventoryItemRequestService inventoryItemRequestService(RestClient restClient,
                                                          CirculationRequestsRetriever circulationRequestsRetriever) {
    return new InventoryItemRequestService(restClient, circulationRequestsRetriever);
  }

  @Bean
  InventoryItemStatusSyncService itemStatusSyncService(InventoryItemManager inventoryItemManager) {
    return new InventoryItemStatusSyncService(inventoryItemManager);
  }

  @Bean
  PieceChangeReceiptStatusPublisher receiptStatusPublisher() {
    return new PieceChangeReceiptStatusPublisher();
  }

  @Bean
  PieceStorageService pieceStorageService(ConsortiumConfigurationService consortiumConfigurationService,
                                          ConsortiumUserTenantsRetriever consortiumUserTenantsRetriever,
                                          SettingsRetriever settingsRetriever,
                                          RestClient restClient) {
    return new PieceStorageService(consortiumConfigurationService, consortiumUserTenantsRetriever, settingsRetriever, restClient);
  }

  @Bean
  PieceService piecesService(PieceChangeReceiptStatusPublisher receiptStatusPublisher) {
    return new PieceService(receiptStatusPublisher);
  }

  @Bean
  DefaultPieceFlowsValidator pieceCreateFlowValidator() {
    return new DefaultPieceFlowsValidator();
  }

  @Bean
  PieceCreateFlowPoLineService pieceCreateFlowPoLineService(PurchaseOrderStorageService purchaseOrderStorageService,
                                                            PurchaseOrderLineService purchaseOrderLineService,
                                                            ReceivingEncumbranceStrategy receivingEncumbranceStrategy) {
    return new PieceCreateFlowPoLineService(purchaseOrderStorageService, purchaseOrderLineService, receivingEncumbranceStrategy);
  }

  @Bean
  PieceCreateFlowManager pieceCreationService(PieceStorageService pieceStorageService, ProtectionService protectionService,
                                              PieceCreateFlowInventoryManager pieceCreateFlowInventoryManager,
                                              DefaultPieceFlowsValidator defaultPieceFlowsValidator,
                                              PieceCreateFlowPoLineService pieceCreateFlowPoLineService,
                                              BasePieceFlowHolderBuilder basePieceFlowHolderBuilder) {
    return new PieceCreateFlowManager(pieceStorageService, protectionService, pieceCreateFlowInventoryManager,
      defaultPieceFlowsValidator, pieceCreateFlowPoLineService, basePieceFlowHolderBuilder);
  }

  @Bean
  UnOpenCompositeOrderManager unOpenCompositeOrderManager(PurchaseOrderLineService purchaseOrderLineService,
                                                          EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory,
                                                          InventoryItemManager inventoryItemManager,
                                                          InventoryHoldingManager inventoryHoldingManager,
                                                          PieceStorageService pieceStorageService,
                                                          PurchaseOrderStorageService purchaseOrderStorageService,
                                                          ProtectionService protectionService,
                                                          CirculationRequestsRetriever circulationRequestsRetriever) {
    return new UnOpenCompositeOrderManager(purchaseOrderLineService, encumbranceWorkflowStrategyFactory, inventoryItemManager,
      inventoryHoldingManager, pieceStorageService, purchaseOrderStorageService, protectionService, circulationRequestsRetriever);
  }

  @Bean
  EncumbrancesProcessingHolderBuilder encumbrancesProcessingHolderBuilder() {
    return new EncumbrancesProcessingHolderBuilder();
  }

  @Bean
  ReceivingEncumbranceStrategy receivingEncumbranceStrategy(EncumbranceService encumbranceService,
                          FundsDistributionService fundsDistributionService,
                          BudgetRestrictionService budgetRestrictionService,
                          EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder,
                          EncumbrancesProcessingHolderBuilder encumbrancesProcessingHolderBuilder) {
    return new ReceivingEncumbranceStrategy(encumbranceService, fundsDistributionService,
      budgetRestrictionService, encumbranceRelationsHoldersBuilder, encumbrancesProcessingHolderBuilder);
  }

  @Bean
  PieceUpdateInventoryService pieceUpdateInventoryService(InventoryItemManager inventoryItemManager,
                                                          InventoryHoldingManager inventoryHoldingManager,
                                                          PieceStorageService pieceStorageService) {
    return new PieceUpdateInventoryService(inventoryItemManager, inventoryHoldingManager, pieceStorageService);
  }

  @Bean
  ItemRecreateInventoryService pieceRecreateInventoryService(InventoryItemManager inventoryItemManager) {
    return new ItemRecreateInventoryService(inventoryItemManager);
  }

  @Bean
  PieceDeleteInventoryService pieceDeleteInventoryService(InventoryItemManager inventoryItemManager) {
    return new PieceDeleteInventoryService(inventoryItemManager);
  }

  @Bean
  PieceDeleteFlowPoLineService pieceDeleteFlowPoLineService(PurchaseOrderStorageService purchaseOrderStorageService,
                    PurchaseOrderLineService purchaseOrderLineService, ReceivingEncumbranceStrategy receivingEncumbranceStrategy) {
    return new PieceDeleteFlowPoLineService(purchaseOrderStorageService, purchaseOrderLineService, receivingEncumbranceStrategy);
  }

  @Bean
  PieceDeleteFlowManager pieceDeletionFlowManager(PieceDeleteFlowInventoryManager pieceDeleteFlowInventoryManager,
                                                  PieceStorageService pieceStorageService,
                                                  ProtectionService protectionService,
                                                  PieceDeleteFlowPoLineService pieceDeleteFlowPoLineService,
                                                  BasePieceFlowHolderBuilder basePieceFlowHolderBuilder,
                                                  CirculationRequestsRetriever circulationRequestsRetriever) {
    return new PieceDeleteFlowManager(pieceDeleteFlowInventoryManager, pieceStorageService, protectionService,
      pieceDeleteFlowPoLineService, basePieceFlowHolderBuilder, circulationRequestsRetriever);
  }

  @Bean
  PieceDeleteFlowInventoryManager pieceDeleteFlowInventoryManager(PieceDeleteInventoryService pieceDeleteInventoryService,
                                                                  PieceUpdateInventoryService pieceUpdateInventoryService) {
    return new PieceDeleteFlowInventoryManager(pieceDeleteInventoryService, pieceUpdateInventoryService);
  }


  @Bean
  BasePieceFlowHolderBuilder basePieceFlowHolderBuilder(PurchaseOrderStorageService purchaseOrderStorageService,
                                                              PurchaseOrderLineService purchaseOrderLineService, TitlesService titlesService) {
      return new BasePieceFlowHolderBuilder(purchaseOrderStorageService, purchaseOrderLineService, titlesService);
  }

  @Bean
  PieceUpdateFlowPoLineService pieceUpdateFlowPoLineService(PurchaseOrderStorageService purchaseOrderStorageService, PurchaseOrderLineService purchaseOrderLineService,
          ReceivingEncumbranceStrategy receivingEncumbranceStrategy, PieceDeleteFlowPoLineService pieceDeleteFlowPoLineService,
          PieceCreateFlowPoLineService pieceCreateFlowPoLineService) {
    return new PieceUpdateFlowPoLineService(purchaseOrderStorageService, purchaseOrderLineService, receivingEncumbranceStrategy,
          pieceCreateFlowPoLineService, pieceDeleteFlowPoLineService);
  }

  @Bean
  PieceUpdateFlowManager pieceUpdateFlowManager(PieceStorageService pieceStorageService, PieceService pieceService, ProtectionService protectionService,
            PieceUpdateFlowPoLineService pieceUpdateFlowPoLineService, PieceUpdateFlowInventoryManager pieceUpdateFlowInventoryManager,
            BasePieceFlowHolderBuilder basePieceFlowHolderBuilder, DefaultPieceFlowsValidator defaultPieceFlowsValidator,
            PurchaseOrderLineService purchaseOrderLineService) {
    return new PieceUpdateFlowManager(pieceStorageService, pieceService, protectionService, pieceUpdateFlowPoLineService,
                            pieceUpdateFlowInventoryManager, basePieceFlowHolderBuilder, defaultPieceFlowsValidator,
                            purchaseOrderLineService);
  }

  @Bean
  PieceUpdateFlowInventoryManager pieceUpdateFlowInventoryManager(TitlesService titlesService,
                                                                  PieceUpdateInventoryService pieceUpdateInventoryService,
                                                                  ItemRecreateInventoryService itemRecreateInventoryService,
                                                                  InventoryItemManager inventoryItemManager,
                                                                  InventoryHoldingManager inventoryHoldingManager) {
    return new PieceUpdateFlowInventoryManager(titlesService, pieceUpdateInventoryService, itemRecreateInventoryService, inventoryItemManager, inventoryHoldingManager);
  }

  @Bean
  PieceCreateFlowInventoryManager pieceCreateFlowInventoryManager(TitlesService titlesService,
                                                                  PieceUpdateInventoryService pieceUpdateInventoryService,
                                                                  InventoryHoldingManager inventoryHoldingManager) {
    return new PieceCreateFlowInventoryManager(titlesService, pieceUpdateInventoryService, inventoryHoldingManager);
  }

  @Bean
  OpenCompositeOrderPieceService openCompositeOrderPieceCreateService(PurchaseOrderStorageService purchaseOrderStorageService,
                                                                      PieceStorageService pieceStorageService,
                                                                      ProtectionService protectionService,
                                                                      PieceChangeReceiptStatusPublisher receiptStatusPublisher,
                                                                      InventoryItemManager inventoryItemManager,
                                                                      InventoryHoldingManager inventoryHoldingManager,
                                                                      TitlesService titlesService,
                                                                      OpenCompositeOrderHolderBuilder openCompositeOrderHolderBuilder) {
    return new OpenCompositeOrderPieceService(purchaseOrderStorageService, pieceStorageService, protectionService,
      receiptStatusPublisher, inventoryItemManager, inventoryHoldingManager, titlesService, openCompositeOrderHolderBuilder);
  }

  @Bean OpenCompositeOrderInventoryService openCompositeOrderInventoryService(InventoryItemManager inventoryItemManager,
                                                                              InventoryHoldingManager inventoryHoldingManager,
                                                                              InventoryInstanceManager inventoryInstanceManager,
                                                                              OpenCompositeOrderPieceService openCompositeOrderPieceService,
                                                                              ProcessInventoryStrategyResolver processInventoryStrategyResolver,
                                                                              RestClient restClient) {
    return new OpenCompositeOrderInventoryService(inventoryItemManager, inventoryHoldingManager, inventoryInstanceManager,
      openCompositeOrderPieceService, processInventoryStrategyResolver, restClient) ;
  }

  @Bean
  OpenCompositeOrderFlowValidator openCompositeOrderFlowValidator(FundService fundService,
                                                                  ExpenseClassValidationService expenseClassValidationService,
                                                                  PieceStorageService pieceStorageService,
                                                                  EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory,
                                                                  CompositePoLineValidationService compositePoLineValidationService,
                                                                  InventoryHoldingManager inventoryHoldingManager) {
    return new OpenCompositeOrderFlowValidator(fundService, expenseClassValidationService, pieceStorageService,
      encumbranceWorkflowStrategyFactory, compositePoLineValidationService, inventoryHoldingManager);
  }

  @Bean PoNumberHelper poNumberHelper(RestClient restClient, PurchaseOrderStorageService purchaseOrderStorageService) {
    return new PoNumberHelper(restClient, purchaseOrderStorageService);
  }

  @Bean
  PurchaseOrderHelper purchaseOrderHelper(PurchaseOrderLineHelper purchaseOrderLineHelper,
    @Qualifier("orderLinesSummaryPopulateService") CompositeOrderDynamicDataPopulateService orderLinesSummaryPopulateService, EncumbranceService encumbranceService,
    @Qualifier("combinedPopulateService") CompositeOrderDynamicDataPopulateService combinedPopulateService,
    EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory, OrderInvoiceRelationService orderInvoiceRelationService,
    TagService tagService, PurchaseOrderLineService purchaseOrderLineService, TitlesService titlesService,
    ProtectionService protectionService, InventoryItemStatusSyncService itemStatusSyncService,
    OpenCompositeOrderManager openCompositeOrderManager, PurchaseOrderStorageService purchaseOrderStorageService,
    ConfigurationEntriesCache configurationEntriesCache, PoNumberHelper poNumberHelper,
    OpenCompositeOrderFlowValidator openCompositeOrderFlowValidator,
    ReOpenCompositeOrderManager reOpenCompositeOrderManager, OrderValidationService orderValidationService) {
    return new PurchaseOrderHelper(purchaseOrderLineHelper, orderLinesSummaryPopulateService, encumbranceService,
      combinedPopulateService, encumbranceWorkflowStrategyFactory, orderInvoiceRelationService, tagService,
      purchaseOrderLineService, titlesService, protectionService, itemStatusSyncService,
      openCompositeOrderManager, purchaseOrderStorageService, configurationEntriesCache,
      poNumberHelper, openCompositeOrderFlowValidator, reOpenCompositeOrderManager,
      orderValidationService);
  }

  @Bean
  public OrderValidationService orderValidationService(CompositePoLineValidationService compositePoLineValidationService,
    ConfigurationEntriesCache configurationEntriesCache, OrganizationService organizationService,
    ProtectionService protectionService, PrefixService prefixService, PurchaseOrderLineHelper purchaseOrderLineHelper,
    PurchaseOrderLineService purchaseOrderLineService, SuffixService suffixService, PoNumberHelper poNumberHelper,
    UnOpenCompositeOrderManager unOpenCompositeOrderManager) {
    return new OrderValidationService(compositePoLineValidationService, configurationEntriesCache, organizationService,
      protectionService, prefixService, purchaseOrderLineHelper, purchaseOrderLineService, suffixService, poNumberHelper,
      unOpenCompositeOrderManager);
  }

  @Bean
  PurchaseOrderLineHelper purchaseOrderLineHelper(InventoryItemStatusSyncService itemStatusSyncService,
                                                  InventoryInstanceManager inventoryInstanceManager,
                                                  EncumbranceService encumbranceService,
                                                  ExpenseClassValidationService expenseClassValidationService,
                                                  EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory,
                                                  OrderInvoiceRelationService orderInvoiceRelationService,
                                                  TitlesService titlesService,
                                                  ProtectionService protectionService,
                                                  PurchaseOrderLineService purchaseOrderLineService,
                                                  PurchaseOrderStorageService purchaseOrderStorageService,
                                                  RestClient restClient, CompositePoLineValidationService compositePoLineValidationService,
                                                  POLInvoiceLineRelationService polInvoiceLineRelationService,
                                                  OrganizationService organizationService) {
    return new PurchaseOrderLineHelper(itemStatusSyncService, inventoryInstanceManager, encumbranceService, expenseClassValidationService,
      encumbranceWorkflowStrategyFactory, orderInvoiceRelationService, titlesService, protectionService,
      purchaseOrderLineService, purchaseOrderStorageService, restClient, compositePoLineValidationService, polInvoiceLineRelationService,
      organizationService);
  }

  @Bean CompositePoLineValidationService compositePoLineValidationService(ExpenseClassValidationService expenseClassValidationService) {
    return new CompositePoLineValidationService(expenseClassValidationService);
  }

  @Bean TitleValidationService titleValidationService() {
    return new TitleValidationService();
  }

  @Bean ReOpenCompositeOrderManager reOpenCompositeOrderManager(EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory,
                    PieceStorageService pieceStorageService, InvoiceLineService invoiceLineService, InvoiceService invoiceService) {
    return new ReOpenCompositeOrderManager(encumbranceWorkflowStrategyFactory, pieceStorageService, invoiceLineService,
        invoiceService);
  }

  @Bean OpenCompositeOrderManager openCompositeOrderManager(PurchaseOrderLineService purchaseOrderLineService,
    EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory,
    TitlesService titlesService, OpenCompositeOrderInventoryService openCompositeOrderInventoryService,
    OpenCompositeOrderFlowValidator openCompositeOrderFlowValidator, UnOpenCompositeOrderManager unOpenCompositeOrderManager) {
    return new OpenCompositeOrderManager(purchaseOrderLineService, encumbranceWorkflowStrategyFactory,
      titlesService, openCompositeOrderInventoryService, openCompositeOrderFlowValidator, unOpenCompositeOrderManager);
  }

  @Bean OpenCompositeOrderHolderBuilder openCompositeOrderHolderBuilder(PieceStorageService pieceStorageService) {
    return new OpenCompositeOrderHolderBuilder(pieceStorageService);
  }

  @Bean ProcessInventoryStrategyResolver resolver(ConsortiumConfigurationService consortiumConfigurationService) {
    Map<String, ProcessInventoryStrategy> strategy = new HashMap<>();

    ProcessInventoryElectronicStrategy processInventoryElectronicStrategy = new ProcessInventoryElectronicStrategy(consortiumConfigurationService);
    ProcessInventoryPhysicalStrategy processInventoryPhysicalStrategy = new ProcessInventoryPhysicalStrategy(consortiumConfigurationService);
    ProcessInventoryMixedStrategy processInventoryMixedStrategy = new ProcessInventoryMixedStrategy(consortiumConfigurationService);

    strategy.put(CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE.value(), processInventoryElectronicStrategy);
    strategy.put(CompositePoLine.OrderFormat.PHYSICAL_RESOURCE.value(), processInventoryPhysicalStrategy);
    strategy.put(CompositePoLine.OrderFormat.OTHER.value(), processInventoryPhysicalStrategy);
    strategy.put(CompositePoLine.OrderFormat.P_E_MIX.value(), processInventoryMixedStrategy);

    return new ProcessInventoryStrategyResolver(strategy);
  }

  @Bean OrderLinePatchOperationService orderLinePatchOperationService(
      RestClient restClient,
      OrderLineUpdateInstanceStrategyResolver orderLineUpdateInstanceStrategyResolver,
      PurchaseOrderLineService purchaseOrderLineService,
      InventoryCache inventoryCache,
      InventoryInstanceManager inventoryInstanceManager) {
    return new OrderLinePatchOperationService(restClient, orderLineUpdateInstanceStrategyResolver,
      purchaseOrderLineService, inventoryCache, inventoryInstanceManager);
  }

  @Bean OrderLineUpdateInstanceStrategy withHoldingOrderLineUpdateInstanceStrategy(InventoryInstanceManager inventoryInstanceManager,
                                                                                   InventoryItemManager inventoryItemManager,
                                                                                   InventoryHoldingManager inventoryHoldingManager,
                                                                                   PieceStorageService pieceStorageService) {
    return new WithHoldingOrderLineUpdateInstanceStrategy(inventoryInstanceManager, inventoryItemManager, inventoryHoldingManager, pieceStorageService);
  }

  @Bean
  OrderLineUpdateInstanceStrategy withoutHoldingOrderLineUpdateInstanceStrategy(InventoryInstanceManager inventoryInstanceManager,
                                                                                InventoryItemManager inventoryItemManager,
                                                                                InventoryHoldingManager inventoryHoldingManager) {
    return new WithoutHoldingOrderLineUpdateInstanceStrategy(inventoryInstanceManager, inventoryItemManager, inventoryHoldingManager);
  }

  @Bean
  OrderTemplatesService orderTemplatesService() {
    return new OrderTemplatesService();
  }



  @Bean OrderLineUpdateInstanceStrategyResolver updateInstanceStrategyResolver(
    @Qualifier("withHoldingOrderLineUpdateInstanceStrategy") OrderLineUpdateInstanceStrategy withHoldingOrderLineUpdateInstanceStrategy,
    @Qualifier("withoutHoldingOrderLineUpdateInstanceStrategy") OrderLineUpdateInstanceStrategy withoutHoldingOrderLineUpdateInstanceStrategy) {
    Map<CreateInventoryType, OrderLineUpdateInstanceStrategy> strategies = new EnumMap<>(CreateInventoryType.class);

    strategies.put(CreateInventoryType.INSTANCE_HOLDING_ITEM, withHoldingOrderLineUpdateInstanceStrategy);
    strategies.put(CreateInventoryType.INSTANCE_HOLDING, withHoldingOrderLineUpdateInstanceStrategy);
    strategies.put(CreateInventoryType.INSTANCE, withoutHoldingOrderLineUpdateInstanceStrategy);
    strategies.put(CreateInventoryType.NONE, withoutHoldingOrderLineUpdateInstanceStrategy);

    return new OrderLineUpdateInstanceStrategyResolver(strategies);
  }

  @Bean POLInvoiceLineRelationService polInvoiceLineRelationService(InvoiceLineService invoiceLineService,
      PendingPaymentService pendingPaymentService) {
    return new POLInvoiceLineRelationService(invoiceLineService, pendingPaymentService);
  }

  @Bean
  ConsortiumConfigurationService consortiumConfigurationService(RestClient restClient) {
    return new ConsortiumConfigurationService(restClient);
  }

  @Bean
  ConsortiumUserTenantsRetriever consortiumUserTenantsRetriever(RestClient restClient) {
    return new ConsortiumUserTenantsRetriever(restClient);
  }

  @Bean
  SharingInstanceService sharingInstanceService(RestClient restClient) {
    return new SharingInstanceService(restClient);
  }

  @Bean
  TitleInstanceService titleInstanceService(InventoryInstanceManager inventoryInstanceManager) {
    return new TitleInstanceService(inventoryInstanceManager);
  }

  @Bean
  CirculationRequestsRetriever circulationRequestsRetriever(PieceStorageService pieceStorageService, RestClient restClient) {
    return new CirculationRequestsRetriever(pieceStorageService, restClient);
  }

  @Bean
  SettingsRetriever settingsRetriever(RestClient restClient) {
    return new SettingsRetriever(restClient);
  }

}
