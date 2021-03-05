package org.folio.service.orders;

import org.folio.models.CompositeOrderRetrieveHolder;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.ErrorCodes;
import org.folio.rest.acq.model.finance.FiscalYear;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.service.finance.FiscalYearService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

public class CompositeOrderRetrieveHolderBuilderTest {

  @InjectMocks
  private CompositeOrderRetrieveHolderBuilder holderBuilder;
  @Mock
  private FiscalYearService fiscalYearService;
  @Mock
  private RequestContext requestContext;

  @BeforeEach
  public void initMocks() {
    MockitoAnnotations.openMocks(this);
  }

  @Test
  void shouldNotFailWhenRetrieveFiscalYearReturns404Status() {
    FundDistribution fundDistribution = new FundDistribution().withFundId(UUID.randomUUID()
      .toString());
    CompositePoLine poLine = new CompositePoLine().withFundDistribution(List.of(fundDistribution));
    CompositePurchaseOrder order = new CompositePurchaseOrder().withId(UUID.randomUUID()
      .toString())
      .withCompositePoLines(Collections.singletonList(poLine));
    CompositeOrderRetrieveHolder holder = new CompositeOrderRetrieveHolder(order);
    CompletableFuture<FiscalYear> failedFuture = new CompletableFuture<>();
    failedFuture.completeExceptionally(new HttpException(404, ErrorCodes.CURRENT_FISCAL_YEAR_NOT_FOUND));
    when(fiscalYearService.getCurrentFiscalYearByFundId(anyString(), any())).thenReturn(failedFuture);

    CompositeOrderRetrieveHolder resultHolder = holderBuilder.withCurrentFiscalYear(holder, requestContext)
      .join();

    assertNull(resultHolder.getFiscalYear());
  }

  @Test
  void shouldFailWhenWhenRetrieveFiscalYearReturnsDifferentFrom404Status() {
    FundDistribution fundDistribution = new FundDistribution().withFundId(UUID.randomUUID()
      .toString());
    CompositePoLine poLine = new CompositePoLine().withFundDistribution(List.of(fundDistribution));
    CompositePurchaseOrder order = new CompositePurchaseOrder().withId(UUID.randomUUID()
      .toString())
      .withCompositePoLines(Collections.singletonList(poLine));
    CompositeOrderRetrieveHolder holder = new CompositeOrderRetrieveHolder(order);
    CompletableFuture<FiscalYear> failedFuture = new CompletableFuture<>();
    HttpException thrownException = new HttpException(500, ErrorCodes.GENERIC_ERROR_CODE);
    failedFuture.completeExceptionally(thrownException);
    when(fiscalYearService.getCurrentFiscalYearByFundId(anyString(), any())).thenReturn(failedFuture);
    CompletionException exception = assertThrows(CompletionException.class,
        () -> holderBuilder.withCurrentFiscalYear(holder, requestContext)
          .join());

    assertEquals(thrownException, exception.getCause());
  }

  @Test
  void shouldNotPopulateFiscalYearWhenThereAreNoFundDistributions() {

    CompositePurchaseOrder order = new CompositePurchaseOrder().withId(UUID.randomUUID()
      .toString());
    CompositeOrderRetrieveHolder holder = new CompositeOrderRetrieveHolder(order);
    CompositeOrderRetrieveHolder resultHolder = holderBuilder.withCurrentFiscalYear(holder, requestContext)
      .join();

    assertNull(resultHolder.getFiscalYear());
  }

}
