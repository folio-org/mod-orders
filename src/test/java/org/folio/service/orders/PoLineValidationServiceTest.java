package org.folio.service.orders;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.rest.core.exceptions.ErrorCodes.*;
import static org.folio.rest.jaxrs.model.PoLine.OrderFormat.OTHER;
import static org.folio.rest.jaxrs.model.PoLine.OrderFormat.P_E_MIX;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import io.vertx.core.Future;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import org.folio.CopilotGenerated;
import org.folio.models.consortium.ConsortiumConfiguration;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Details;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.service.consortium.ConsortiumConfigurationService;
import org.folio.service.consortium.ConsortiumUserTenantsRetriever;
import org.folio.service.finance.expenceclass.ExpenseClassValidationService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

@ExtendWith(VertxExtension.class)
public class PoLineValidationServiceTest {

  @Mock private ExpenseClassValidationService expenseClassValidationService;
  @Mock private ConsortiumConfigurationService consortiumConfigurationService;
  @Mock private ConsortiumUserTenantsRetriever consortiumUserTenantsRetriever;
  @Mock private RequestContext requestContext;
  @InjectMocks private PoLineValidationService poLineValidationService;

  private AutoCloseable mockitoMocks;

  @BeforeEach
  public void initMocks() {
    mockitoMocks = MockitoAnnotations.openMocks(this);
  }

  @AfterEach
  void afterEach() throws Exception {
    mockitoMocks.close();
  }

  @Test
  @DisplayName("Should return error if location and holding reference are not present in the location")
  void shouldReturnErrorIfLocationAndHoldingReferenceAreNotPresentInTheLocation() {
    Location location1 = new Location().withQuantity(1).withQuantityPhysical(1);
    Location location2 = new Location().withQuantity(1).withQuantityPhysical(1);
    Physical physical = new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM);
    Cost cost = new Cost().withQuantityPhysical(2);
    PoLine poLine = new PoLine().withPhysical(physical)
      .withCost(cost).withLocations(List.of(location1, location2));
    List<Error> errors = poLineValidationService.validateLocations(poLine);

    assertEquals(2, errors.size());
    errors.forEach(error -> assertEquals(ErrorCodes.HOLDINGS_ID_AND_LOCATION_ID_IS_NULL_ERROR.getCode(), error.getCode()));
  }

  @Test
  @DisplayName("Should return error if location and holding reference are not present in the location")
  void shouldReturnErrorIfLocationAndHoldingReferenceArePresentAtTheSameTimeInTheLocation() {
    Location location1 = new Location().withHoldingId(UUID.randomUUID().toString()).withLocationId(UUID.randomUUID().toString())
      .withQuantity(1).withQuantityPhysical(1);
    Location location2 = new Location().withHoldingId(UUID.randomUUID().toString()).withLocationId(UUID.randomUUID().toString())
      .withQuantity(1).withQuantityPhysical(1);
    Physical physical = new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM);
    Cost cost = new Cost().withQuantityPhysical(2);
    PoLine poLine = new PoLine().withPhysical(physical)
      .withCost(cost).withLocations(List.of(location1, location2));
    List<Error> errors = poLineValidationService.validateLocations(poLine);

    assertEquals(2, errors.size());
    errors.forEach(error -> assertEquals(ErrorCodes.MAY_BE_LINK_TO_EITHER_HOLDING_OR_LOCATION_ERROR.getCode(), error.getCode()));
  }

  @Test
  void shouldReturnSuccessIfClaimingConfigValid() {
    PoLine poLine = new PoLine()
      .withClaimingActive(true)
      .withClaimingInterval(5);
    List<Error> errors = poLineValidationService.validateClaimingConfig(poLine);

    assertTrue(errors.isEmpty());
  }

  @Test
  void shouldReturnSuccessIfClaimingConfigNotSet() {
    PoLine poLine = new PoLine();
    List<Error> errors = poLineValidationService.validateClaimingConfig(poLine);

    assertTrue(errors.isEmpty());
  }

  @Test
  void shouldReturnErrorIfClaimingIntervalNotSetWhenClaimingActive() {
    PoLine poLine = new PoLine()
      .withClaimingActive(true);
    List<Error> errors = poLineValidationService.validateClaimingConfig(poLine);

    assertEquals(1, errors.size());
    assertEquals(ErrorCodes.CLAIMING_CONFIG_INVALID.getCode(), errors.get(0).getCode());
  }

  @Test
  void shouldReturnErrorIfClaimingIntervalIsZeroWhenClaimingActive() {
    PoLine poLine = new PoLine()
      .withClaimingActive(true)
      .withClaimingInterval(0);
    List<Error> errors = poLineValidationService.validateClaimingConfig(poLine);

    assertEquals(1, errors.size());
    assertEquals(ErrorCodes.CLAIMING_CONFIG_INVALID.getCode(), errors.get(0).getCode());
  }

  @Test
  void shouldReturnErrorIfClaimingIntervalIsNegativeWhenClaimingActive() {
    PoLine poLine = new PoLine()
      .withClaimingActive(true)
      .withClaimingInterval(-1);
    List<Error> errors = poLineValidationService.validateClaimingConfig(poLine);

    assertEquals(1, errors.size());
    assertEquals(ErrorCodes.CLAIMING_CONFIG_INVALID.getCode(), errors.get(0).getCode());
  }

  @Test
  void shouldReturnErrorIfIncorrectOrderFormatWhenBindaryActive() {
    var poLine = new PoLine()
      .withDetails(new Details().withIsBinderyActive(true))
      .withPhysical(new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM))
      .withOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE)
      .withCheckinItems(true);
    var errors = poLineValidationService.validateForBinadryActive(poLine);

    assertEquals(1, errors.size());
    assertEquals(ORDER_FORMAT_INCORRECT_FOR_BINDARY_ACTIVE.getCode(), errors.get(0).getCode());
  }

  @Test
  void shouldReturnErrorIfIncorrectCreateInventoryWhenBindaryActive() {
    var poLine = new PoLine()
      .withDetails(new Details().withIsBinderyActive(true))
      .withPhysical(new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING))
      .withOrderFormat(P_E_MIX)
      .withCheckinItems(true);
    var errors = poLineValidationService.validateForBinadryActive(poLine);

    assertEquals(1, errors.size());
    assertEquals(CREATE_INVENTORY_INCORRECT_FOR_BINDARY_ACTIVE.getCode(), errors.get(0).getCode());
  }

  @Test
  void shouldReturnErrorIfCheckInItemsFalseWhenBindaryActive() {
    var poLine = new PoLine()
      .withDetails(new Details().withIsBinderyActive(true))
      .withPhysical(new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM))
      .withOrderFormat(P_E_MIX)
      .withCheckinItems(false);
    var errors = poLineValidationService.validateForBinadryActive(poLine);

    assertEquals(1, errors.size());
    assertEquals(RECEIVING_WORKFLOW_INCORRECT_FOR_BINDARY_ACTIVE.getCode(), errors.get(0).getCode());
  }

  @Test
  void shouldPassWhenBindaryActiveAndCorrectFormat() {
    var compositePoLineWithPhysical = new PoLine()
      .withDetails(new Details().withIsBinderyActive(true))
      .withPhysical(new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM))
      .withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE)
      .withCheckinItems(true);
    var errors1 = poLineValidationService.validateForBinadryActive(compositePoLineWithPhysical);

    assertEquals(0, errors1.size());

    var compositePoLineWithPEMix = new PoLine()
      .withDetails(new Details().withIsBinderyActive(true))
      .withPhysical(new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM))
      .withOrderFormat(P_E_MIX)
      .withCheckinItems(true);
    var errors2 = poLineValidationService.validateForBinadryActive(compositePoLineWithPEMix);

    assertEquals(0, errors2.size());
  }

  @Test
  void shouldPassWhenBindaryNotActive() {
    var poLine = new PoLine()
      .withDetails(new Details())
      .withPhysical(new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM))
      .withOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE);
    var errors = poLineValidationService.validateForBinadryActive(poLine);

    assertEquals(0, errors.size());
  }

  @Test
  @DisplayName("Test validatePoLine with incorrect cost for PE Mix format")
  void testValidatePoLineWithIncorrectCostForPEMixFormat() {
    // Given
    // Set incorrect quantities for the PO Line
    Cost cost = new Cost()
      .withQuantityPhysical(1)
      .withQuantityElectronic(0)
      .withListUnitPrice(-10d)
      .withListUnitPriceElectronic(-5d);
    Location location = new Location()
      .withLocationId(UUID.randomUUID().toString())
      .withQuantityElectronic(1)
      .withQuantityPhysical(2);
    PoLine poLine = new PoLine()
      .withOrderFormat(P_E_MIX)
      .withCost(cost)
      .withLocations(List.of(location));

    doReturn(succeededFuture(null))
      .when(expenseClassValidationService).validateExpenseClasses(eq(List.of(poLine)), eq(false), eq(requestContext));

    // When
    Future<List<Error>> future = poLineValidationService.validatePoLine(poLine, requestContext);

    // Then
    if (future.failed()) {
      fail(future.cause());
    }
    List<Error> errors = future.result();
    assertThat(errors, hasSize(5));
    Set<String> errorCodes = errorsToCodes(errors);

    assertThat(errorCodes, containsInAnyOrder(
      ZERO_COST_ELECTRONIC_QTY.getCode(),
      PHYSICAL_COST_LOC_QTY_MISMATCH.getCode(),
      ELECTRONIC_COST_LOC_QTY_MISMATCH.getCode(),
      COST_UNIT_PRICE_ELECTRONIC_INVALID.getCode(),
      COST_UNIT_PRICE_INVALID.getCode()));
  }

  @Test
  @DisplayName("Test validatePoLine with incorrect cost for Other format")
  void testValidatePoLineWithIncorrectCostForOtherFormat() {
    // Given
    // Set incorrect quantities for the PO Line
    Cost cost = new Cost()
      .withQuantityPhysical(0)
      .withQuantityElectronic(1)
      .withListUnitPrice(-1d)
      .withListUnitPriceElectronic(10d);
    Location location = new Location()
      .withLocationId(UUID.randomUUID().toString())
      .withQuantityElectronic(0)
      .withQuantityPhysical(1);
    PoLine poLine = new PoLine()
      .withOrderFormat(OTHER)
      .withCost(cost)
      .withLocations(List.of(location));

    doReturn(succeededFuture(null))
      .when(expenseClassValidationService).validateExpenseClasses(eq(List.of(poLine)), eq(false), eq(requestContext));

    // When
    Future<List<Error>> future = poLineValidationService.validatePoLine(poLine, requestContext);

    // Then
    if (future.failed()) {
      fail(future.cause());
    }
    List<Error> errors = future.result();
    assertThat(errors, hasSize(5));
    Set<String> errorCodes = errorsToCodes(errors);

    assertThat(errorCodes, containsInAnyOrder(ZERO_COST_PHYSICAL_QTY.getCode(),
      NON_ZERO_COST_ELECTRONIC_QTY.getCode(),
      PHYSICAL_COST_LOC_QTY_MISMATCH.getCode(),
      COST_UNIT_PRICE_ELECTRONIC_INVALID.getCode(),
      COST_UNIT_PRICE_INVALID.getCode()));
  }

  @Test
  @DisplayName("Test validatePoLine with incorrect quantities")
  void testValidatePoLineWithIncorrectQuantities() {
    // Given
    // Set incorrect quantities for the PO Line
    Cost cost = new Cost()
      .withQuantityPhysical(0)
      .withQuantityElectronic(0)
      .withListUnitPrice(24.99)
      .withListUnitPriceElectronic(20.99);
    Location location1 = new Location()
      .withLocationId(UUID.randomUUID().toString())
      .withQuantityElectronic(1)
      .withQuantityPhysical(1);
    Location location2 = new Location()
      .withLocationId(location1.getLocationId())
      .withQuantityElectronic(0)
      .withQuantityPhysical(0);
    PoLine poLine = new PoLine()
      .withOrderFormat(P_E_MIX)
      .withCost(cost)
      .withLocations(List.of(location1, location2));

    doReturn(succeededFuture(null))
      .when(expenseClassValidationService).validateExpenseClasses(eq(List.of(poLine)), eq(false), eq(requestContext));

    // When
    Future<List<Error>> future = poLineValidationService.validatePoLine(poLine, requestContext);

    // Then
    if (future.failed()) {
      fail(future.cause());
    }
    List<Error> errors = future.result();
    assertThat(errors, hasSize(5));
    Set<String> errorCodes = errorsToCodes(errors);

    assertThat(errorCodes, containsInAnyOrder(ZERO_COST_ELECTRONIC_QTY.getCode(),
      ZERO_COST_PHYSICAL_QTY.getCode(),
      ELECTRONIC_COST_LOC_QTY_MISMATCH.getCode(),
      PHYSICAL_COST_LOC_QTY_MISMATCH.getCode(),
      ZERO_LOCATION_QTY.getCode()));
  }

  @Test
  @DisplayName("Test validatePoLine with zero quantities without locations")
  void testValidatePoLineWithZeroQuantitiesWithoutLocations() {
    // MODORDERS-584
    // Skip quantity validation with 0 electronic and physical quantities and without location
    // Given
    Cost cost = new Cost()
      .withQuantityPhysical(0)
      .withQuantityElectronic(0)
      .withListUnitPrice(24.99)
      .withListUnitPriceElectronic(20.99);
    PoLine poLine = new PoLine()
      .withOrderFormat(P_E_MIX)
      .withCost(cost);

    doReturn(succeededFuture(null))
      .when(expenseClassValidationService).validateExpenseClasses(eq(List.of(poLine)), eq(false), eq(requestContext));

    // When
    Future<List<Error>> future = poLineValidationService.validatePoLine(poLine, requestContext);

    // Then
    if (future.failed()) {
      fail(future.cause());
    }
    List<Error> errors = future.result();
    assertThat(errors, hasSize(0));
  }


  @Test
  @DisplayName("Test validatePoLine with checkinItems = false and receivingStatus = 'Receipt Not Required'")
  void testValidatePoLineWithCheckinItemsFalseAndReceiptNotRequired() {
    // Given
    Cost cost = new Cost()
      .withQuantityPhysical(1)
      .withListUnitPrice(1.0);
    Location location = new Location()
      .withLocationId(UUID.randomUUID().toString())
      .withQuantityPhysical(1);
    PoLine poLine = new PoLine()
      .withReceiptStatus(PoLine.ReceiptStatus.RECEIPT_NOT_REQUIRED)
      .withCheckinItems(false)
      .withCost(cost)
      .withLocations(List.of(location));

    when(expenseClassValidationService.validateExpenseClasses(eq(List.of(poLine)), eq(false), eq(requestContext)))
      .thenReturn(succeededFuture(null));

    // When
    Future<List<Error>> future = poLineValidationService.validatePoLine(poLine, requestContext);

    // Then
    if (future.failed()) {
      fail(future.cause());
    }
    List<Error> errors = future.result();
    assertThat(errors, hasSize(1));
    assertThat(errors.getFirst().getCode(), is(RECEIVING_WORKFLOW_INCORRECT_FOR_RECEIPT_NOT_REQUIRED.getCode()));
  }

  private Set<String> errorsToCodes(List<Error> errors) {
    return errors
      .stream()
      .map(Error::getCode)
      .collect(Collectors.toSet());
  }

  @Test
  void testValidateUserUnaffiliatedLocations(VertxTestContext testContext) {
    var locationsUpdated = List.of(
      new Location().withLocationId(UUID.randomUUID().toString()).withTenantId("tenant1"),
      new Location().withLocationId(UUID.randomUUID().toString()).withTenantId("tenant2"));
    var updatedPoLineId = UUID.randomUUID().toString();

    when(consortiumConfigurationService.getConsortiumConfiguration(eq(requestContext)))
      .thenReturn(Future.succeededFuture(Optional.empty()));
    when(consortiumUserTenantsRetriever.getUserTenants(eq("consortiumId"), anyString(), eq(requestContext)))
      .thenReturn(Future.succeededFuture(List.of("tenant1")));

    poLineValidationService.validateUserUnaffiliatedLocations(updatedPoLineId, locationsUpdated, requestContext)
      .onComplete(testContext.succeeding(result -> testContext.verify(testContext::completeNow)));
  }

  @Test
  void testValidateUserUnaffiliatedLocationsAllValidLocations(VertxTestContext testContext) {
    var locationsUpdated = List.of(
      new Location().withLocationId(UUID.randomUUID().toString()).withTenantId("tenant1"),
      new Location().withLocationId(UUID.randomUUID().toString()).withTenantId("tenant2"));
    var updatedPoLineId = UUID.randomUUID().toString();

    when(consortiumConfigurationService.getConsortiumConfiguration(eq(requestContext)))
      .thenReturn(Future.succeededFuture(Optional.of(new ConsortiumConfiguration("tenant1", "consortiumId"))));
    when(consortiumConfigurationService.isCentralOrderingEnabled(eq(requestContext)))
      .thenReturn(Future.succeededFuture(true));
    when(consortiumUserTenantsRetriever.getUserTenants(eq("consortiumId"), anyString(), eq(requestContext)))
      .thenReturn(Future.succeededFuture(List.of("tenant1", "tenant2")));

    poLineValidationService.validateUserUnaffiliatedLocations(updatedPoLineId, locationsUpdated, requestContext)
      .onComplete(testContext.succeeding(result -> testContext.verify(testContext::completeNow)));
  }

  @Test
  void testValidateUserUnaffiliatedLocationsAllValidDuplicateTenantLocations(VertxTestContext testContext) {
    var locationsUpdated = List.of(
      new Location().withLocationId(UUID.randomUUID().toString()).withTenantId("tenant1"),
      new Location().withLocationId(UUID.randomUUID().toString()).withTenantId("tenant2"),
      new Location().withLocationId(UUID.randomUUID().toString()).withTenantId("tenant2"));
    var updatedPoLineId = UUID.randomUUID().toString();

    when(consortiumConfigurationService.getConsortiumConfiguration(eq(requestContext)))
      .thenReturn(Future.succeededFuture(Optional.of(new ConsortiumConfiguration("tenant1", "consortiumId"))));
    when(consortiumConfigurationService.isCentralOrderingEnabled(eq(requestContext)))
      .thenReturn(Future.succeededFuture(true));
    when(consortiumUserTenantsRetriever.getUserTenants(eq("consortiumId"), anyString(), eq(requestContext)))
      .thenReturn(Future.succeededFuture(List.of("tenant1", "tenant2")));

    poLineValidationService.validateUserUnaffiliatedLocations(updatedPoLineId, locationsUpdated, requestContext)
      .onComplete(testContext.succeeding(result -> testContext.verify(testContext::completeNow)));
  }

  @Test
  void testValidateUserUnaffiliatedLocationsWhenCentralOrderingIsDisabled(VertxTestContext testContext) {
    var locationsUpdated = List.of(
      new Location().withLocationId(UUID.randomUUID().toString()).withTenantId("tenant1"),
      new Location().withLocationId(UUID.randomUUID().toString()).withTenantId("tenant2"));
    var updatedPoLineId = UUID.randomUUID().toString();

    when(consortiumConfigurationService.getConsortiumConfiguration(eq(requestContext)))
      .thenReturn(Future.succeededFuture(Optional.of(new ConsortiumConfiguration("tenant1", "consortiumId"))));
    when(consortiumConfigurationService.isCentralOrderingEnabled(eq(requestContext)))
      .thenReturn(Future.succeededFuture(false));
    when(consortiumUserTenantsRetriever.getUserTenants(eq("consortiumId"), anyString(), eq(requestContext)))
      .thenReturn(Future.succeededFuture(List.of("tenant1")));

    poLineValidationService.validateUserUnaffiliatedLocations(updatedPoLineId, locationsUpdated, requestContext)
      .onComplete(testContext.succeeding(result -> {
        testContext.verify(testContext::completeNow);
        verifyNoInteractions(consortiumUserTenantsRetriever);
      }));
  }

  @Test
  void testValidateUserUnaffiliatedLocationsOneValidAndOneInvalidLocations(VertxTestContext testContext) {
    var locationsUpdated = List.of(
      new Location().withLocationId(UUID.randomUUID().toString()).withTenantId("tenant1"),
      new Location().withLocationId(UUID.randomUUID().toString()).withTenantId("tenant2"));
    var updatedPoLineId = UUID.randomUUID().toString();

    when(consortiumConfigurationService.getConsortiumConfiguration(eq(requestContext)))
      .thenReturn(Future.succeededFuture(Optional.of(new ConsortiumConfiguration("tenant1", "consortiumId"))));
    when(consortiumConfigurationService.isCentralOrderingEnabled(eq(requestContext)))
      .thenReturn(Future.succeededFuture(true));
    when(consortiumUserTenantsRetriever.getUserTenants(eq("consortiumId"), anyString(), eq(requestContext)))
      .thenReturn(Future.succeededFuture(List.of("tenant1")));

    poLineValidationService.validateUserUnaffiliatedLocations(updatedPoLineId, locationsUpdated, requestContext)
      .onComplete(testContext.failing(cause -> testContext.verify(() -> {
        assertInstanceOf(HttpException.class, cause);
        assertTrue(cause.getMessage().contains(ErrorCodes.LOCATION_UPDATE_WITHOUT_AFFILIATION.getDescription()));
        testContext.completeNow();
      })));
  }

  @Test
  void testValidateUserUnaffiliatedLocationsTwoInvalidLocations(VertxTestContext testContext) {
    var locationsUpdated = List.of(
      new Location().withLocationId(UUID.randomUUID().toString()).withTenantId("tenant1"),
      new Location().withLocationId(UUID.randomUUID().toString()).withTenantId("tenant2"));
    var updatedPoLineId = UUID.randomUUID().toString();

    when(consortiumConfigurationService.getConsortiumConfiguration(eq(requestContext)))
      .thenReturn(Future.succeededFuture(Optional.of(new ConsortiumConfiguration("tenant1", "consortiumId"))));
    when(consortiumConfigurationService.isCentralOrderingEnabled(eq(requestContext)))
      .thenReturn(Future.succeededFuture(true));
    when(consortiumUserTenantsRetriever.getUserTenants(eq("consortiumId"), anyString(), eq(requestContext)))
      .thenReturn(Future.succeededFuture(List.of("tenant3")));

    poLineValidationService.validateUserUnaffiliatedLocations(updatedPoLineId, locationsUpdated, requestContext)
      .onComplete(testContext.failing(cause -> testContext.verify(() -> {
        assertInstanceOf(HttpException.class, cause);
        assertTrue(cause.getMessage().contains(ErrorCodes.LOCATION_UPDATE_WITHOUT_AFFILIATION.getDescription()));
        testContext.completeNow();
      })));
  }

  @Test
  @CopilotGenerated(partiallyGenerated = true)
  void shouldFailValidationWhenCompositePurchaseOrderHasNullPoLines(VertxTestContext testContext) {
    var compPO = new CompositePurchaseOrder();
    compPO.setPoLines(null);

    poLineValidationService.validatePurchaseOrderHasPoLines(compPO.getPoLines())
      .onComplete(testContext.failing(cause -> testContext.verify(() -> {
        assertInstanceOf(HttpException.class, cause);
        assertEquals(COMPOSITE_ORDER_MISSING_PO_LINES.getCode(), ((HttpException) cause).getError().getCode());
        testContext.completeNow();
      })));
  }

  @Test
  @CopilotGenerated(partiallyGenerated = true)
  void shouldFailValidationWhenCompositePurchaseOrderHasEmptyPoLines(VertxTestContext testContext) {
    var compPO = new CompositePurchaseOrder();
    compPO.setPoLines(Collections.emptyList());

    poLineValidationService.validatePurchaseOrderHasPoLines(compPO.getPoLines())
      .onComplete(testContext.failing(cause -> testContext.verify(() -> {
        assertInstanceOf(HttpException.class, cause);
        assertEquals(COMPOSITE_ORDER_MISSING_PO_LINES.getCode(), ((HttpException) cause).getError().getCode());
        testContext.completeNow();
      })));
  }

  @Test
  @CopilotGenerated(partiallyGenerated = true)
  void shouldPassValidationWhenCompositePurchaseOrderHasPoLines(VertxTestContext testContext) {
    var compPO = new CompositePurchaseOrder();
    var poLine = new PoLine();
    poLine.setId(UUID.randomUUID().toString());
    compPO.setPoLines(List.of(poLine));

    poLineValidationService.validatePurchaseOrderHasPoLines(compPO.getPoLines())
      .onComplete(testContext.succeeding(result -> testContext.verify(testContext::completeNow)));
  }
}
