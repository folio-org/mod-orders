package org.folio.service.orders;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.when;

import java.util.Set;
import java.util.UUID;

import io.vertx.core.Future;
import io.vertx.junit5.VertxExtension;
import org.folio.models.CompositeOrderRetrieveHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InOrder;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;


@ExtendWith(VertxExtension.class)
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
      .thenReturn(Future.succeededFuture(holder));


    populateService.populate(holder, requestContext).result();

    InOrder inOrder = inOrder(holderBuilder, populateServices);
    inOrder.verify(holderBuilder).withCurrentFiscalYear(any(), any());
    inOrder.verify(populateServices).stream();

  }

}
