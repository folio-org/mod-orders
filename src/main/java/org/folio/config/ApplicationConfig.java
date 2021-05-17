package org.folio.config;

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
import org.folio.service.finance.transaction.EncumbranceRelationsHoldersBuilder;
import org.folio.service.finance.transaction.EncumbranceService;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategy;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategyFactory;
import org.folio.service.finance.expenceclass.ExpenseClassService;
import org.folio.service.finance.expenceclass.ExpenseClassValidationService;
import org.folio.service.finance.FiscalYearService;
import org.folio.service.finance.FundService;
import org.folio.service.finance.LedgerService;
import org.folio.service.finance.transaction.OpenToCloseEncumbranceStrategy;
import org.folio.service.finance.transaction.OpenToPendingEncumbranceStrategy;
import org.folio.service.finance.transaction.PendingToOpenEncumbranceStrategy;
import org.folio.service.finance.rollover.RolloverErrorService;
import org.folio.service.finance.rollover.RolloverRetrieveService;
import org.folio.service.finance.transaction.TransactionService;
import org.folio.service.finance.transaction.TransactionSummariesService;
import org.folio.service.orders.HoldingsSummaryService;
import org.folio.service.orders.CombinedOrderDataPopulateService;
import org.folio.service.orders.CompositeOrderDynamicDataPopulateService;
import org.folio.service.orders.CompositeOrderRetrieveHolderBuilder;
import org.folio.service.orders.CompositePurchaseOrderService;
import org.folio.service.orders.OrderInvoiceRelationService;
import org.folio.service.orders.OrderLinesSummaryPopulateService;
import org.folio.service.orders.OrderReEncumberService;
import org.folio.service.orders.OrderRolloverService;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderService;
import org.folio.service.orders.ReEncumbranceHoldersBuilder;
import org.folio.service.orders.TransactionsTotalFieldsPopulateService;
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
  PurchaseOrderService purchaseOrderService(RestClient restClient) {
    return new PurchaseOrderService(restClient);
  }

  @Bean
  PurchaseOrderLineService purchaseOrderLineService(RestClient restClient) {
    return new PurchaseOrderLineService(restClient);
  }

  @Bean
  CompositePurchaseOrderService compositePurchaseOrderService(PurchaseOrderService purchaseOrderService, PurchaseOrderLineService purchaseOrderLineService) {
    return new CompositePurchaseOrderService(purchaseOrderService, purchaseOrderLineService);
  }

  @Bean
  FundService fundService(RestClient restClient) {
    return new FundService(restClient);
  }

  @Bean
  OrderRolloverService rolloverOrderService(FundService fundService, PurchaseOrderService purchaseOrderService,
                                            PurchaseOrderLineService purchaseOrderLineService, TransactionService transactionService) {
    return new OrderRolloverService(fundService, purchaseOrderService, purchaseOrderLineService, transactionService);
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
  EncumbranceService encumbranceService(TransactionService transactionService, TransactionSummariesService transactionSummariesService) {
    return new EncumbranceService(transactionService, transactionSummariesService);
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
  EncumbranceWorkflowStrategy openToPendingEncumbranceStrategy(EncumbranceService encumbranceService, TransactionSummariesService transactionSummariesService) {
    return new OpenToPendingEncumbranceStrategy(encumbranceService, transactionSummariesService);
  }

  @Bean
  FundsDistributionService fundsDistributionService() {
    return new FundsDistributionService();
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
                                                               FundsDistributionService fundsDistributionService,
                                                               BudgetRestrictionService budgetRestrictionService,
                                                               EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder) {
    return new PendingToOpenEncumbranceStrategy(encumbranceService, fundsDistributionService,
                                                budgetRestrictionService, encumbranceRelationsHoldersBuilder);
  }

  @Bean
  EncumbranceWorkflowStrategy openToCloseEncumbranceStrategy(EncumbranceService encumbranceService) {
    return new OpenToCloseEncumbranceStrategy(encumbranceService);
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
  OrderReEncumberService orderReEncumberService(CompositePurchaseOrderService compositePurchaseOrderService,
                                                ReEncumbranceHoldersBuilder reEncumbranceHoldersBuilder,
                                                RolloverErrorService rolloverErrorService,
                                                RolloverRetrieveService rolloverRetrieveService,
                                                PurchaseOrderLineService purchaseOrderLineService,
                                                TransactionService transactionService,
                                                TransactionSummariesService transactionSummaryService,
                                                BudgetRestrictionService budgetRestrictionService) {
    return new OrderReEncumberService(compositePurchaseOrderService,
            reEncumbranceHoldersBuilder,
            rolloverErrorService,
            rolloverRetrieveService,
            purchaseOrderLineService,
            transactionService,
            transactionSummaryService,
            budgetRestrictionService);
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
}
