package org.folio.service.orders;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.List;
import java.util.UUID;

import org.folio.models.CompositeOrderRetrieveHolder;
import org.folio.rest.acq.model.finance.FiscalYear;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.service.finance.FiscalYearService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import io.vertx.core.Promise;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;

@ExtendWith(VertxExtension.class)
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
    PoLine poLine = new PoLine().withFundDistribution(List.of(fundDistribution));
    CompositePurchaseOrder order = new CompositePurchaseOrder().withId(UUID.randomUUID()
      .toString())
      .withPoLines(Collections.singletonList(poLine));
    CompositeOrderRetrieveHolder holder = new CompositeOrderRetrieveHolder(order);
    Promise<FiscalYear> failedFuture = Promise.promise();
    failedFuture.fail(new HttpException(404, ErrorCodes.CURRENT_FISCAL_YEAR_NOT_FOUND));
    when(fiscalYearService.getCurrentFiscalYearByFundId(anyString(), any())).thenReturn(failedFuture.future());

    CompositeOrderRetrieveHolder resultHolder = holderBuilder.withCurrentFiscalYear(holder, requestContext)
      .result();

    assertNull(resultHolder.getFiscalYear());
  }

  @Test
  void shouldFailWhenWhenRetrieveFiscalYearReturnsDifferentFrom404Status(VertxTestContext vertxTestContext) {
    FundDistribution fundDistribution = new FundDistribution().withFundId(UUID.randomUUID()
      .toString());
    PoLine poLine = new PoLine().withFundDistribution(List.of(fundDistribution));
    CompositePurchaseOrder order = new CompositePurchaseOrder().withId(UUID.randomUUID()
      .toString())
      .withPoLines(Collections.singletonList(poLine));
    CompositeOrderRetrieveHolder holder = new CompositeOrderRetrieveHolder(order);
    Promise<FiscalYear> failedFuture = Promise.promise();
    HttpException thrownException = new HttpException(500, ErrorCodes.GENERIC_ERROR_CODE);
    failedFuture.fail(thrownException);
    when(fiscalYearService.getCurrentFiscalYearByFundId(anyString(), any())).thenReturn(failedFuture.future());

    var future = holderBuilder.withCurrentFiscalYear(holder, requestContext);
    vertxTestContext.assertFailure(future)
      .onComplete(exception -> {
        assertEquals(thrownException, exception.cause().getCause());
        vertxTestContext.completeNow();
      });
  }

  @Test
  void shouldNotPopulateFiscalYearWhenThereAreNoFundDistributions() {

    CompositePurchaseOrder order = new CompositePurchaseOrder().withId(UUID.randomUUID()
      .toString());
    CompositeOrderRetrieveHolder holder = new CompositeOrderRetrieveHolder(order);
    CompositeOrderRetrieveHolder resultHolder = holderBuilder.withCurrentFiscalYear(holder, requestContext)
      .result();

    assertNull(resultHolder.getFiscalYear());
  }

}
