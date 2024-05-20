package org.folio.service.orders;

import static org.folio.rest.core.exceptions.ErrorCodes.CREATE_INVENTORY_INCORRECT_FOR_BINDARY_ACTIVE;
import static org.folio.rest.core.exceptions.ErrorCodes.ORDER_FORMAT_INCORRECT_FOR_BINDARY_ACTIVE;
import static org.folio.rest.core.exceptions.ErrorCodes.RECEIVING_WORKFLOW_INCORRECT_FOR_BINDARY_ACTIVE;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;
import java.util.UUID;

import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Details;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Physical;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

public class CompositePoLineValidationServiceTest {
  @InjectMocks
  private CompositePoLineValidationService compositePoLineValidationService;

  @BeforeEach
  public void initMocks(){
    MockitoAnnotations.openMocks(this);
  }

  @Test
  @DisplayName("Should return error if location and holding reference are not present in the location")
  void shouldReturnErrorIfLocationAndHoldingReferenceAreNotPresentInTheLocation() {
    Location location1 = new Location().withQuantity(1).withQuantityPhysical(1);
    Location location2 = new Location().withQuantity(1).withQuantityPhysical(1);
    Physical physical = new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM);
    Cost cost = new Cost().withQuantityPhysical(2);
    CompositePoLine compositePoLine = new CompositePoLine().withPhysical(physical)
      .withCost(cost).withLocations(List.of(location1, location2));
    List<Error> errors = compositePoLineValidationService.validateLocations(compositePoLine);

    assertEquals(2, errors.size());
    errors.forEach(error -> {
      assertEquals(ErrorCodes.HOLDINGS_ID_AND_LOCATION_ID_IS_NULL_ERROR.getCode(), error.getCode());
    });
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
    CompositePoLine compositePoLine = new CompositePoLine().withPhysical(physical)
      .withCost(cost).withLocations(List.of(location1, location2));
    List<Error> errors = compositePoLineValidationService.validateLocations(compositePoLine);

    assertEquals(2, errors.size());
    errors.forEach(error -> {
      assertEquals(ErrorCodes.MAY_BE_LINK_TO_EITHER_HOLDING_OR_LOCATION_ERROR.getCode(), error.getCode());
    });
  }

  @Test
  void shouldReturnSuccessIfClaimingConfigValid() {
    CompositePoLine compositePoLine = new CompositePoLine()
      .withClaimingActive(true)
      .withClaimingInterval(5);
    List<Error> errors = compositePoLineValidationService.validateClaimingConfig(compositePoLine);

    assertTrue(errors.isEmpty());
  }

  @Test
  void shouldReturnSuccessIfClaimingConfigNotSet() {
    CompositePoLine compositePoLine = new CompositePoLine();
    List<Error> errors = compositePoLineValidationService.validateClaimingConfig(compositePoLine);

    assertTrue(errors.isEmpty());
  }

  @Test
  void shouldReturnErrorIfClaimingIntervalNotSetWhenClaimingActive() {
    CompositePoLine compositePoLine = new CompositePoLine()
      .withClaimingActive(true);
    List<Error> errors = compositePoLineValidationService.validateClaimingConfig(compositePoLine);

    assertEquals(1, errors.size());
    assertEquals(ErrorCodes.CLAIMING_CONFIG_INVALID.getCode(), errors.get(0).getCode());
  }

  @Test
  void shouldReturnErrorIfClaimingIntervalIsZeroWhenClaimingActive() {
    CompositePoLine compositePoLine = new CompositePoLine()
      .withClaimingActive(true)
      .withClaimingInterval(0);
    List<Error> errors = compositePoLineValidationService.validateClaimingConfig(compositePoLine);

    assertEquals(1, errors.size());
    assertEquals(ErrorCodes.CLAIMING_CONFIG_INVALID.getCode(), errors.get(0).getCode());
  }

  @Test
  void shouldReturnErrorIfClaimingIntervalIsNegativeWhenClaimingActive() {
    CompositePoLine compositePoLine = new CompositePoLine()
      .withClaimingActive(true)
      .withClaimingInterval(-1);
    List<Error> errors = compositePoLineValidationService.validateClaimingConfig(compositePoLine);

    assertEquals(1, errors.size());
    assertEquals(ErrorCodes.CLAIMING_CONFIG_INVALID.getCode(), errors.get(0).getCode());
  }

  @Test
  void shouldReturnErrorIfIncorrectOrderFormatWhenBindaryActive() {
    var compositePoLine = new CompositePoLine()
      .withDetails(new Details().withIsBinderyActive(true))
      .withPhysical(new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM))
      .withOrderFormat(CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE)
      .withCheckinItems(true);
    var errors = compositePoLineValidationService.validateForBinadryActive(compositePoLine);

    assertEquals(1, errors.size());
    assertEquals(ORDER_FORMAT_INCORRECT_FOR_BINDARY_ACTIVE.getCode(), errors.get(0).getCode());
  }


  @Test
  void shouldReturnErrorIfIncorrectCreateInventoryWhenBindaryActive() {
    var compositePoLine = new CompositePoLine()
      .withDetails(new Details().withIsBinderyActive(true))
      .withPhysical(new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING))
      .withOrderFormat(CompositePoLine.OrderFormat.P_E_MIX)
      .withCheckinItems(true);
    var errors = compositePoLineValidationService.validateForBinadryActive(compositePoLine);

    assertEquals(1, errors.size());
    assertEquals(CREATE_INVENTORY_INCORRECT_FOR_BINDARY_ACTIVE.getCode(), errors.get(0).getCode());
  }

  @Test
  void shouldReturnErrorIfCheckInItemsFalseWhenBindaryActive() {
    var compositePoLine = new CompositePoLine()
      .withDetails(new Details().withIsBinderyActive(true))
      .withPhysical(new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM))
      .withOrderFormat(CompositePoLine.OrderFormat.P_E_MIX)
      .withCheckinItems(false);
    var errors = compositePoLineValidationService.validateForBinadryActive(compositePoLine);

    assertEquals(1, errors.size());
    assertEquals(RECEIVING_WORKFLOW_INCORRECT_FOR_BINDARY_ACTIVE.getCode(), errors.get(0).getCode());
  }

  @Test
  void shouldPassWhenBindaryActiveAndCorrectFormat() {
    var compositePoLineWithPhysical = new CompositePoLine()
      .withDetails(new Details().withIsBinderyActive(true))
      .withPhysical(new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM))
      .withOrderFormat(CompositePoLine.OrderFormat.PHYSICAL_RESOURCE)
      .withCheckinItems(true);
    var errors1 = compositePoLineValidationService.validateForBinadryActive(compositePoLineWithPhysical);

    assertEquals(0, errors1.size());

    var compositePoLineWithPEMix = new CompositePoLine()
      .withDetails(new Details().withIsBinderyActive(true))
      .withPhysical(new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM))
      .withOrderFormat(CompositePoLine.OrderFormat.P_E_MIX)
      .withCheckinItems(true);
    var errors2 = compositePoLineValidationService.validateForBinadryActive(compositePoLineWithPEMix);

    assertEquals(0, errors2.size());
  }

  @Test
  void shouldPassWhenBindaryNotActive() {
    var compositePoLine = new CompositePoLine()
      .withDetails(new Details())
      .withPhysical(new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM))
      .withOrderFormat(CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE);
    var errors = compositePoLineValidationService.validateForBinadryActive(compositePoLine);

    assertEquals(0, errors.size());
  }
}
