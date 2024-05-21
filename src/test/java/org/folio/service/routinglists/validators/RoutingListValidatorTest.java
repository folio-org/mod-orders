package org.folio.service.routinglists.validators;

import static org.folio.TestUtils.getLocationPhysicalCopies;
import static org.folio.TestUtils.getMinimalContentPoLine;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.rest.core.exceptions.ErrorCodes.INVALID_ROUTING_LIST_FOR_PO_LINE_FORMAT;
import static org.folio.rest.core.exceptions.ErrorCodes.ROUTING_LIST_LIMIT_REACHED_FOR_PO_LINE;
import static org.folio.rest.impl.MockServer.ROUTING_LISTS_MOCK_DATA_PATH;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.RoutingList;
import org.folio.rest.jaxrs.model.RoutingListCollection;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class RoutingListValidatorTest {

  private static final String ROUTING_LIST_SAMPLE = ROUTING_LISTS_MOCK_DATA_PATH + "routing-list.json";

  private static final String PO_LINE_UUID = "0009662b-8b80-4001-b704-ca10971f222d";

  private PoLine samplePoLine;
  private RoutingList sampleRoutingList;

  @BeforeEach
  void before() {
    sampleRoutingList = getMockAsJson(ROUTING_LIST_SAMPLE).mapTo(RoutingList.class);
    samplePoLine = getMinimalContentPoLine()
      .withId(PO_LINE_UUID)
      .withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE)
      .withLocations(getLocationPhysicalCopies(1));
  }

  @Test
  void testValidateRoutingList() {
    RoutingListCollection collection = getRoutingListCollection(0);
    List<Error> errorList = RoutingListValidatorUtil.validateRoutingList(sampleRoutingList, collection, samplePoLine);
    assertEquals(errorList.size(), 0);
  }

  @Test
  void testValidateRoutingListWithPOLineLimitReached() {
    RoutingListCollection collection = getRoutingListCollection(1);
    var distinctRoutingList = getMockAsJson(ROUTING_LIST_SAMPLE).mapTo(RoutingList.class).withId(UUID.randomUUID().toString());
    List<Error> errors = RoutingListValidatorUtil.validateRoutingList(distinctRoutingList, collection, samplePoLine);
    assertEquals(errors.size(), 1);
    assertEquals(errors.get(0).getMessage(), ROUTING_LIST_LIMIT_REACHED_FOR_PO_LINE.getDescription());
  }

  @Test
  void testValidateRoutingListWithPOLineInvalidOrderFormat() {
    samplePoLine.setOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE);
    RoutingListCollection collection = getRoutingListCollection(1);
    List<Error> errors = RoutingListValidatorUtil.validateRoutingList(sampleRoutingList, collection, samplePoLine);
    assertEquals(errors.size(), 1);
    assertEquals(errors.get(0).getMessage(), INVALID_ROUTING_LIST_FOR_PO_LINE_FORMAT.getDescription());
  }

  @Test
  void testValidateRoutingListWithPOLinePEMixOrderFormat() {
    var locations = new ArrayList<>(samplePoLine.getLocations());
    locations.add(new Location().withLocationId(UUID.randomUUID().toString()).withQuantityElectronic(1).withQuantity(1));
    samplePoLine.withOrderFormat(PoLine.OrderFormat.P_E_MIX).withLocations(locations);

    RoutingListCollection collection = getRoutingListCollection(1);
    List<Error> errorList = RoutingListValidatorUtil.validateRoutingList(sampleRoutingList, collection, samplePoLine);
    assertEquals(errorList.size(), 0);
  }

  private RoutingListCollection getRoutingListCollection(int n) {
    List<RoutingList> lists = new ArrayList<>();
    for (int i = 0; i < n; i++) {
      lists.add(sampleRoutingList);
    }
    return new RoutingListCollection()
      .withRoutingLists(lists)
      .withTotalRecords(n);
  }

}
