package org.folio.service.orders;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

import org.folio.models.CompositeOrderRetrieveHolder;
import org.folio.rest.acq.model.finance.FiscalYear;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.service.finance.transaction.TransactionService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class TotalExpendedPopulateServiceTest {

  @InjectMocks
  private TotalExpendedPopulateService populateService;
  @Mock
  private TransactionService transactionService;

  @Mock
  private RequestContext requestContext;

  @BeforeEach
  public void initMocks() {
    MockitoAnnotations.openMocks(this);
  }

  @Test
  void shouldPopulateTotalEncumberedFieldWithSumOfEncumbrancesAmount() {
    FundDistribution fundDistribution1 = new FundDistribution().withEncumbrance(UUID.randomUUID().toString());
    FundDistribution fundDistribution2 = new FundDistribution().withEncumbrance(UUID.randomUUID().toString());
    CompositePoLine poLine = new CompositePoLine().withFundDistribution(List.of(fundDistribution1, fundDistribution2));
    CompositePurchaseOrder order = new CompositePurchaseOrder().withId(UUID.randomUUID()
      .toString()).withCompositePoLines(Collections.singletonList(poLine));
    CompositeOrderRetrieveHolder holder = new CompositeOrderRetrieveHolder(order)
            .withFiscalYear(new FiscalYear().withId(UUID.randomUUID().toString()));

    Transaction payment = new Transaction()
            .withTransactionType(Transaction.TransactionType.PAYMENT)
            .withPaymentEncumbranceId(fundDistribution1.getEncumbrance())
            .withAmount(14.11d)
            .withCurrency("USD");
    Transaction credit = new Transaction()
            .withTransactionType(Transaction.TransactionType.CREDIT)
            .withPaymentEncumbranceId(fundDistribution2.getEncumbrance())
            .withAmount(13.43d)
            .withCurrency("USD");

    List<Transaction> payments = List.of(payment, credit);
    when(transactionService.getCurrentPaymentsByEncumbranceIds(anyList(), anyString(), any()))
      .thenReturn(CompletableFuture.completedFuture(payments));

    CompositeOrderRetrieveHolder resultHolder = populateService.populate(holder, requestContext)
      .join();

    assertEquals(0.68, resultHolder.getOrder()
      .getTotalExpended());
  }

  @Test
  void shouldPopulateTotalEncumberedFieldWithZeroWhenFiscalYearIsEmpty() {

    CompositePurchaseOrder order = new CompositePurchaseOrder().withId(UUID.randomUUID().toString());
    CompositeOrderRetrieveHolder holder = new CompositeOrderRetrieveHolder(order);

    CompositeOrderRetrieveHolder resultHolder = populateService.populate(holder, requestContext).join();

    assertEquals(0d, resultHolder.getOrder()
            .getTotalExpended());
  }

  @Test
  void shouldPopulateTotalEncumberedFieldWithZeroWhenTransactionsNotFound() {
    FundDistribution fundDistribution1 = new FundDistribution().withEncumbrance(UUID.randomUUID().toString());
    FundDistribution fundDistribution2 = new FundDistribution().withEncumbrance(UUID.randomUUID().toString());
    CompositePoLine poLine = new CompositePoLine().withFundDistribution(List.of(fundDistribution1, fundDistribution2));
    CompositePurchaseOrder order = new CompositePurchaseOrder().withId(UUID.randomUUID()
            .toString()).withCompositePoLines(Collections.singletonList(poLine));
    CompositeOrderRetrieveHolder holder = new CompositeOrderRetrieveHolder(order)
            .withFiscalYear(new FiscalYear().withId(UUID.randomUUID().toString()));

    List<Transaction> payments = Collections.emptyList();
    when(transactionService.getCurrentPaymentsByEncumbranceIds(anyList(), anyString(), any()))
            .thenReturn(CompletableFuture.completedFuture(payments));

    CompositeOrderRetrieveHolder resultHolder = populateService.populate(holder, requestContext).join();

    assertEquals(0d, resultHolder.getOrder().getTotalExpended());
  }
}
