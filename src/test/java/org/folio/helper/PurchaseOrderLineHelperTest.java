package org.folio.helper;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

import java.util.stream.Stream;
import org.folio.rest.acq.model.SequenceNumbers;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.service.orders.PurchaseOrderLineService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.stubbing.Answer;

import java.util.List;
import java.util.UUID;

import static io.vertx.core.Future.succeededFuture;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;

public class PurchaseOrderLineHelperTest {
  private AutoCloseable mockitoMocks;
  @InjectMocks
  private PurchaseOrderLineHelper purchaseOrderLineHelper;
  @Mock
  private PurchaseOrderLineService purchaseOrderLineService;
  @Mock
  private RequestContext requestContext;
  @Mock
  private RestClient restClient;

  @BeforeEach
  void beforeEach() {
    mockitoMocks = MockitoAnnotations.openMocks(this);
  }

  @AfterEach
  void resetMocks() throws Exception {
    mockitoMocks.close();
  }

  @Test
  @DisplayName("Test createPoLineWithOrder")
  void testCreatePoLineWithOrder() {
    // Given
    CompositePurchaseOrder compPO = new CompositePurchaseOrder()
      .withId(UUID.randomUUID().toString())
      .withPoNumber("1");
    Cost cost = new Cost()
      .withCurrency("USD");
    PoLine poLine = new PoLine()
      .withCost(cost);

    SequenceNumbers seqNumbers = new SequenceNumbers()
      .withSequenceNumbers(List.of("1"));
    doReturn(succeededFuture(JsonObject.mapFrom(seqNumbers)))
      .when(restClient).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
    doReturn(succeededFuture())
      .when(purchaseOrderLineService).updateSearchLocations(any(PoLine.class), eq(requestContext));
    doAnswer((Answer<Future<PoLine>>) invocation -> {
      PoLine pol = invocation.getArgument(1);
      return succeededFuture(pol);
    }).when(restClient).post(any(RequestEntry.class), any(PoLine.class), eq(PoLine.class), eq(requestContext));

    // When
    Future<PoLine> future = purchaseOrderLineHelper.createPoLineWithOrder(poLine, compPO, requestContext);

    // Then
    assertTrue(future.succeeded());
  }

  private static Stream<Arguments> testHasAlteredExchangeRateArgs() {
    return Stream.of(
      Arguments.of(false, 0.7d, 0.7d),
      Arguments.of(false, null, null),
      Arguments.of(true, 0.7d, 0.8d),
      Arguments.of(true, null, 0.8d),
      Arguments.of(true, 0.7d, null)
    );
  }

  @ParameterizedTest
  @MethodSource("testHasAlteredExchangeRateArgs")
  void testHasAlteredExchangeRate(boolean expected, Double oldExchangeRate, Double newExchangeRate) {
    var currencyCode = "AUD";
    var oldCost = new Cost().withCurrency(currencyCode).withExchangeRate(newExchangeRate);
    var newCost = new Cost().withCurrency(currencyCode).withExchangeRate(oldExchangeRate);
    assertEquals(expected, PurchaseOrderLineHelper.hasAlteredExchangeRate(oldCost, newCost));
  }
}
