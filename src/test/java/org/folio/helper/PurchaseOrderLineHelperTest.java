package org.folio.helper;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

import java.lang.reflect.InvocationTargetException;
import java.util.Optional;
import java.util.stream.Stream;

import org.folio.models.consortium.ConsortiumConfiguration;
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
import org.folio.service.consortium.ConsortiumConfigurationService;
import org.folio.service.consortium.ConsortiumUserTenantsRetriever;
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

import static io.vertx.core.Future.failedFuture;
import static io.vertx.core.Future.succeededFuture;
import static org.folio.TestUtils.callPrivateMethod;
import static org.folio.TestUtils.getLocationsForTenants;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
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
  private ConsortiumConfigurationService consortiumConfigurationService;
  @Mock
  private ConsortiumUserTenantsRetriever consortiumUserTenantsRetriever;
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
    doReturn(succeededFuture())
      .when(purchaseOrderLineService).updateSearchLocations(any(CompositePoLine.class), eq(requestContext));
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
    doReturn(succeededFuture())
      .when(purchaseOrderLineService).updateSearchLocations(any(CompositePoLine.class), eq(requestContext));
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

  @Test
  void testValidateUserUnaffiliatedLocationUpdates() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
    var locationsStored = getLocationsForTenants(List.of("tenant1", "tenant2", "tenant3"));
    var locationsUpdated = getLocationsForTenants(List.of("tenant1", "tenant3"));
    var storagePoLine = new PoLine().withLocations(locationsStored);
    var updatedPoLine = new CompositePoLine().withLocations(locationsUpdated);

    doReturn(succeededFuture(Optional.of(new ConsortiumConfiguration("tenant1", "consortiumId"))))
      .when(consortiumConfigurationService).getConsortiumConfiguration(any(RequestContext.class));
    doReturn(succeededFuture(List.of("tenant1", "tenant2")))
      .when(consortiumUserTenantsRetriever).getUserTenants(eq("consortiumId"), eq("centralTenantId"), any(RequestContext.class));

    var future = callValidateUserUnaffiliatedLocationUpdates(updatedPoLine, storagePoLine, requestContext, purchaseOrderLineHelper);

    assertTrue(future.succeeded());
  }

  @Test
  void testValidateUserUnaffiliatedLocationUpdatesInvalid() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
    var locationsStored = getLocationsForTenants(List.of("tenant1", "tenant2", "tenant3"));
    var locationsUpdated = getLocationsForTenants(List.of("tenant1", "tenant3"));
    locationsUpdated.get(1).withQuantityPhysical(10);
    var storagePoLine = new PoLine().withLocations(locationsStored);
    var updatedPoLine = new CompositePoLine().withLocations(locationsUpdated);

    doReturn(succeededFuture(Optional.of(new ConsortiumConfiguration("tenant1", "consortiumId"))))
      .when(consortiumConfigurationService).getConsortiumConfiguration(any(RequestContext.class));
    doReturn(succeededFuture(List.of("tenant1")))
      .when(consortiumUserTenantsRetriever).getUserTenants(eq("consortiumId"), anyString(), any(RequestContext.class));

    var future = callValidateUserUnaffiliatedLocationUpdates(updatedPoLine, storagePoLine, requestContext, purchaseOrderLineHelper);

    assertTrue(future.failed());
    assertInstanceOf(HttpException.class, future.cause());
  }

  private Future<Void> callValidateUserUnaffiliatedLocationUpdates(CompositePoLine updatedPoLine, PoLine storagePoLine,
                                                                   RequestContext requestContext, PurchaseOrderLineHelper purchaseOrderLineHelper)
    throws NoSuchMethodException, IllegalAccessException, InvocationTargetException {

    return callPrivateMethod(purchaseOrderLineHelper, "validateUserUnaffiliatedLocationUpdates", Future.class,
      new Class[]{ CompositePoLine.class, PoLine.class, RequestContext.class }, new Object[] { updatedPoLine, storagePoLine, requestContext });
  }

}
