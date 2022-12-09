package org.folio.service.orders;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.List;
import java.util.UUID;

import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Cost;
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
}
