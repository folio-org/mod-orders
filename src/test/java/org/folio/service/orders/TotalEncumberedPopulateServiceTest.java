package org.folio.service.orders;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

import org.folio.models.CompositeOrderRetrieveHolder;
import org.folio.rest.acq.model.finance.FiscalYear;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.acq.model.finance.TransactionCollection;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.service.finance.transaction.TransactionService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class TotalEncumberedPopulateServiceTest {

  @InjectMocks
  private TotalEncumberedPopulateService populateService;
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
    CompositePurchaseOrder order = new CompositePurchaseOrder().withId(UUID.randomUUID()
      .toString());
    CompositeOrderRetrieveHolder holder = new CompositeOrderRetrieveHolder(order)
            .withFiscalYear(new FiscalYear().withId(UUID.randomUUID().toString()));

    Transaction encumbrance1 = new Transaction().withAmount(13.11d).withCurrency("USD");
    Transaction encumbrance2 = new Transaction().withAmount(13.43d).withCurrency("USD");

    List<Transaction> encumbrances = List.of(encumbrance1, encumbrance2);
    when(transactionService.getTransactions(anyString(), anyInt(), anyInt(), any()))
      .thenReturn(CompletableFuture.completedFuture(new TransactionCollection().withTransactions(encumbrances)));

    CompositeOrderRetrieveHolder resultHolder = populateService.populate(holder, requestContext)
      .join();

    assertEquals(26.54, resultHolder.getOrder()
      .getTotalEncumbered());
  }

  @Test
  void shouldPopulateTotalEncumberedFieldWithZeroWhenFiscalYearIsEmpty() {
    CompositePurchaseOrder order = new CompositePurchaseOrder().withId(UUID.randomUUID()
            .toString());
    CompositeOrderRetrieveHolder holder = new CompositeOrderRetrieveHolder(order);

    CompositeOrderRetrieveHolder resultHolder = populateService.populate(holder, requestContext)
            .join();

    assertEquals(0d, resultHolder.getOrder()
            .getTotalEncumbered());
  }

  @Test
  void shouldPopulateTotalEncumberedFieldWithZeroWhenTransactionsNotFound() {
    CompositePurchaseOrder order = new CompositePurchaseOrder().withId(UUID.randomUUID()
            .toString());
    CompositeOrderRetrieveHolder holder = new CompositeOrderRetrieveHolder(order)
            .withFiscalYear(new FiscalYear().withId(UUID.randomUUID().toString()));

    List<Transaction> encumbrances = Collections.emptyList();
    when(transactionService.getTransactions(anyString(), anyInt(), anyInt(), any()))
            .thenReturn(CompletableFuture.completedFuture(new TransactionCollection().withTransactions(encumbrances)));

    CompositeOrderRetrieveHolder resultHolder = populateService.populate(holder, requestContext).join();

    assertEquals(0d, resultHolder.getOrder().getTotalEncumbered());
  }
}
