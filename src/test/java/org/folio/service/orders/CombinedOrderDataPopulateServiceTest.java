package org.folio.service.orders;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

import org.folio.models.CompositeOrderRetrieveHolder;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.ErrorCodes;
import org.folio.rest.acq.model.finance.FiscalYear;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InOrder;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class CombinedOrderDataPopulateServiceTest {

  @InjectMocks
  private CombinedOrderDataPopulateService populateService;
  @Mock
  private CompositeOrderRetrieveHolderBuilder holderBuilder;
  @Mock
  private Set<CompositeOrderDynamicDataPopulateService> populateServices;

  @Mock
  private RequestContext requestContext;

  @BeforeEach
  public void initMocks() {
    MockitoAnnotations.openMocks(this);
  }

  @Test
  void shouldShouldCallPopulateServiceAfterHolderDataIsPopulated() {

    CompositePurchaseOrder order = new CompositePurchaseOrder().withId(UUID.randomUUID().toString());
    CompositeOrderRetrieveHolder holder = new CompositeOrderRetrieveHolder(order);

    when(holderBuilder.withCurrentFiscalYear(any(), any()))
      .thenReturn(CompletableFuture.completedFuture(holder));
    when(holderBuilder.withCurrentEncumbranceIds(any(), any()))
            .thenReturn(CompletableFuture.completedFuture(holder));

    populateService.populate(holder, requestContext).join();

    InOrder inOrder = inOrder(holderBuilder, populateServices);
    inOrder.verify(holderBuilder).withCurrentFiscalYear(any(), any());
    inOrder.verify(holderBuilder).withCurrentEncumbranceIds(any(), any());
    inOrder.verify(populateServices).stream();

  }





}
