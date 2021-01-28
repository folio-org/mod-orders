package org.folio.config;

import org.folio.rest.core.RestClient;
import org.folio.service.PrefixService;
import org.folio.service.ReasonForClosureService;
import org.folio.service.SuffixService;
import org.folio.service.configuration.ConfigurationEntriesService;
import org.folio.service.exchange.ExchangeRateProviderResolver;
import org.folio.service.exchange.FinanceExchangeRateService;
import org.folio.service.finance.BudgetExpenseClassService;
import org.folio.service.finance.BudgetRestrictionService;
import org.folio.service.finance.BudgetService;
import org.folio.service.finance.EncumbranceService;
import org.folio.service.finance.EncumbranceWorkflowStrategy;
import org.folio.service.finance.EncumbranceWorkflowStrategyFactory;
import org.folio.service.finance.ExpenseClassService;
import org.folio.service.finance.ExpenseClassValidationService;
import org.folio.service.finance.FiscalYearService;
import org.folio.service.finance.FundService;
import org.folio.service.finance.LedgerService;
import org.folio.service.finance.OpenToCloseEncumbranceStrategy;
import org.folio.service.finance.OpenToPendingEncumbranceStrategy;
import org.folio.service.finance.PendingToOpenEncumbranceStrategy;
import org.folio.service.finance.RolloverErrorService;
import org.folio.service.finance.RolloverRetrieveService;
import org.folio.service.finance.TransactionService;
import org.folio.service.finance.TransactionSummariesService;
import org.folio.service.orders.CompositePurchaseOrderService;
import org.folio.service.orders.OrderInvoiceRelationService;
import org.folio.service.orders.OrderReEncumberService;
import org.folio.service.orders.OrderRolloverService;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderService;
import org.folio.service.orders.ReEncumbranceHoldersBuilder;
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
  BudgetRestrictionService budgetRestrictionService(BudgetService budgetService, LedgerService ledgerService) {
    return new BudgetRestrictionService(budgetService, ledgerService);
  }

  @Bean
  EncumbranceService encumbranceService(TransactionService transactionService, TransactionSummariesService transactionSummariesService,
                                        ExchangeRateProviderResolver exchangeRateProviderResolver,
                                        FundService fundService,
                                        BudgetRestrictionService budgetRestrictionService,
                                        FiscalYearService fiscalYearService,
                                        ConfigurationEntriesService configurationEntriesService) {
    return new EncumbranceService(transactionService, transactionSummariesService, exchangeRateProviderResolver, fundService, budgetRestrictionService, fiscalYearService, configurationEntriesService);
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
  FiscalYearService fiscalYearService(RestClient restClient) {
    return new FiscalYearService(restClient);
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
  EncumbranceWorkflowStrategy pendingToOpenEncumbranceStrategy(EncumbranceService encumbranceService, TransactionSummariesService transactionSummariesService) {
    return new PendingToOpenEncumbranceStrategy(encumbranceService, transactionSummariesService);
  }

  @Bean
  EncumbranceWorkflowStrategy openToCloseEncumbranceStrategy(EncumbranceService encumbranceService, TransactionSummariesService transactionSummariesService) {
    return new OpenToCloseEncumbranceStrategy(encumbranceService, transactionSummariesService);
  }

  @Bean
  ReEncumbranceHoldersBuilder reEncumbranceHoldersBuilder(BudgetService budgetService,
                                                          LedgerService ledgerService,
                                                          FundService fundService,
                                                          ExchangeRateProviderResolver exchangeRateProviderResolver,
                                                          FiscalYearService fiscalYearService,
                                                          RolloverRetrieveService rolloverRetrieveService,
                                                          TransactionService transactionService) {
    return new ReEncumbranceHoldersBuilder(budgetService,
            ledgerService,
            fundService,
            exchangeRateProviderResolver,
            fiscalYearService,
            rolloverRetrieveService,
            transactionService);
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
  OrderInvoiceRelationService orderInvoiceRelationService (RestClient orderInvoiceRelationRestClient) {
    return new OrderInvoiceRelationService(orderInvoiceRelationRestClient);
  }
}
