package org.folio.helper;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import org.folio.rest.acq.model.SequenceNumbers;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.Alert;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.ReportingCode;
import org.folio.service.orders.PurchaseOrderLineService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.stubbing.Answer;

import java.util.List;
import java.util.UUID;

import static io.vertx.core.Future.failedFuture;
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
    Alert alert = new Alert()
      .withAlert("alert");
    ReportingCode reportingCode = new ReportingCode()
      .withCode("code");
    CompositePurchaseOrder compPO = new CompositePurchaseOrder()
      .withId(UUID.randomUUID().toString())
      .withPoNumber("1");
    Cost cost = new Cost()
      .withCurrency("USD");
    CompositePoLine poLine = new CompositePoLine()
      .withAlerts(List.of(alert))
      .withReportingCodes(List.of(reportingCode))
        .withCost(cost);

    doAnswer((Answer<Future<Alert>>) invocation -> {
      Alert al = invocation.getArgument(1);
      if (al.getId() == null) {
        al.setId(UUID.randomUUID().toString());
      }
      return succeededFuture(al);
    }).when(restClient).post(any(RequestEntry.class), any(Alert.class), eq(Alert.class), eq(requestContext));
    doAnswer((Answer<Future<ReportingCode>>) invocation -> {
      ReportingCode rc = invocation.getArgument(1);
      if (rc.getId() == null) {
        rc.setId(UUID.randomUUID().toString());
      }
      return succeededFuture(rc);
    }).when(restClient).post(any(RequestEntry.class), any(ReportingCode.class), eq(ReportingCode.class), eq(requestContext));
    SequenceNumbers seqNumbers = new SequenceNumbers()
      .withSequenceNumbers(List.of("1"));
    doReturn(succeededFuture(JsonObject.mapFrom(seqNumbers)))
      .when(restClient).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
    doReturn(succeededFuture(List.of()))
      .when(purchaseOrderLineService).retrieveSearchLocationIds(any(PoLine.class), eq(requestContext));
    doAnswer((Answer<Future<PoLine>>) invocation -> {
      PoLine pol = invocation.getArgument(1);
      return succeededFuture(pol);
    }).when(restClient).post(any(RequestEntry.class), any(PoLine.class), eq(PoLine.class), eq(requestContext));

    // When
    Future<CompositePoLine> future = purchaseOrderLineHelper.createPoLineWithOrder(poLine, compPO, requestContext);

    // Then
    assertTrue(future.succeeded());
  }

  @Test
  @DisplayName("Test error when creating a reporting code")
  void testErrorWhenCreatingAReportingCode() {
    // Given
    Alert alert = new Alert()
      .withAlert("alert");
    ReportingCode reportingCode = new ReportingCode()
      .withCode("code");
    CompositePurchaseOrder compPO = new CompositePurchaseOrder()
      .withId(UUID.randomUUID().toString())
      .withPoNumber("1");
    Cost cost = new Cost()
      .withCurrency("USD");
    CompositePoLine poLine = new CompositePoLine()
      .withAlerts(List.of(alert))
      .withReportingCodes(List.of(reportingCode))
      .withCost(cost);

    doAnswer((Answer<Future<Alert>>) invocation -> {
      Alert al = invocation.getArgument(1);
      if (al.getId() == null) {
        al.setId(UUID.randomUUID().toString());
      }
      return succeededFuture(al);
    }).when(restClient).post(any(RequestEntry.class), any(Alert.class), eq(Alert.class), eq(requestContext));
    doReturn(failedFuture(new Exception("test error")))
      .when(restClient).post(any(RequestEntry.class), any(ReportingCode.class), eq(ReportingCode.class), eq(requestContext));
    SequenceNumbers seqNumbers = new SequenceNumbers()
      .withSequenceNumbers(List.of("1"));
    doReturn(succeededFuture(JsonObject.mapFrom(seqNumbers)))
      .when(restClient).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
    doReturn(succeededFuture(List.of()))
      .when(purchaseOrderLineService).retrieveSearchLocationIds(any(PoLine.class), eq(requestContext));
    doAnswer((Answer<Future<PoLine>>) invocation -> {
      PoLine pol = invocation.getArgument(1);
      return succeededFuture(pol);
    }).when(restClient).post(any(RequestEntry.class), any(PoLine.class), eq(PoLine.class), eq(requestContext));

    // When
    Future<CompositePoLine> future = purchaseOrderLineHelper.createPoLineWithOrder(poLine, compPO, requestContext);

    // Then
    assertTrue(future.failed());
    Throwable cause = future.cause();
    HttpException ex = (HttpException)cause;
    assertEquals("Failed to create reporting codes", ex.getMessage());
    assertEquals("test error", ex.getCause().getMessage());
  }

}
