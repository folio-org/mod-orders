package org.folio.service.pieces.flows;

import static org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE;
import static org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat.PHYSICAL_RESOURCE;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.pieces.flows.PieceFlowUpdatePoLineStrategies;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;

public class PieceFlowUpdatePoLineStrategiesTest {

  @Test
  @DisplayName("Add 1 electronic piece with holding to electronic pol with 1 location and same holding id as in piece")
  void electAddStrategyShouldIncreaseQuantityTo2ForCostAndLocationIfInitiallyWas1AndHoldingIdInPOLAndPieceTheSame() {
    String holdingId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    Piece piece = new Piece().withPoLineId(lineId).withHoldingId(holdingId).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withHoldingId(holdingId).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1);
    CompositePoLine poLine = new CompositePoLine().withOrderFormat(ELECTRONIC_RESOURCE).withId(lineId)
                                                  .withLocations(List.of(loc)).withCost(cost);
    //When
    PieceFlowUpdatePoLineStrategies.ADD.updateQuantity(1, piece, poLine);
    assertNull(poLine.getCost().getQuantityPhysical());
    assertEquals(2, poLine.getCost().getQuantityElectronic());
    assertEquals(1, poLine.getLocations().size());
    assertNull(poLine.getLocations().get(0).getQuantityPhysical());
    assertEquals(2, poLine.getLocations().get(0).getQuantityElectronic());
    assertEquals(2, poLine.getLocations().get(0).getQuantity());
  }

  @Test
  @DisplayName("Add 1 physical piece with holding to physical pol with 1 location and same holding id as in piece")
  void physAddStrategyShouldIncreaseQuantityTo2ForCostAndLocationIfInitiallyWas1AndHoldingIdInPOLAndPieceTheSame() {
    String holdingId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    Piece piece = new Piece().withPoLineId(lineId).withHoldingId(holdingId).withFormat(Piece.Format.PHYSICAL);
    Location loc = new Location().withHoldingId(holdingId).withQuantityPhysical(1).withQuantity(1);
    Cost cost = new Cost().withQuantityPhysical(1);
    CompositePoLine poLine = new CompositePoLine().withOrderFormat(PHYSICAL_RESOURCE).withId(lineId)
                                                  .withLocations(List.of(loc)).withCost(cost);
    //When
    PieceFlowUpdatePoLineStrategies.ADD.updateQuantity(1, piece, poLine);
    assertNull(poLine.getCost().getQuantityElectronic());
    assertEquals(2, poLine.getCost().getQuantityPhysical());
    assertEquals(1, poLine.getLocations().size());
    assertNull(poLine.getLocations().get(0).getQuantityElectronic());
    assertEquals(2, poLine.getLocations().get(0).getQuantityPhysical());
    assertEquals(2, poLine.getLocations().get(0).getQuantity());
  }

  @Test
  @DisplayName("Add 1 physical piece with location to physical pol with 1 location and same location id as in piece")
  void physAddStrategyShouldIncreaseQuantityTo2ForCostAndLocationIfInitiallyWas1AndLocationIdInPOLAndPieceTheSame() {
    String locationId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    Piece piece = new Piece().withPoLineId(lineId).withLocationId(locationId).withFormat(Piece.Format.PHYSICAL);
    Location loc = new Location().withLocationId(locationId).withQuantityPhysical(1).withQuantity(1);
    Cost cost = new Cost().withQuantityPhysical(1);
    CompositePoLine poLine = new CompositePoLine().withOrderFormat(PHYSICAL_RESOURCE).withId(lineId)
                                                  .withLocations(List.of(loc)).withCost(cost);
    //When
    PieceFlowUpdatePoLineStrategies.ADD.updateQuantity(1, piece, poLine);
    assertNull(poLine.getCost().getQuantityElectronic());
    assertEquals(2, poLine.getCost().getQuantityPhysical());
    assertEquals(1, poLine.getLocations().size());
    assertNull(poLine.getLocations().get(0).getQuantityElectronic());
    assertEquals(2, poLine.getLocations().get(0).getQuantityPhysical());
    assertEquals(2, poLine.getLocations().get(0).getQuantity());
  }

  @Test
  @DisplayName("Add 1 electronic piece with location to electronic pol with 1 location and same location id as in piece")
  void elecAddStrategyShouldIncreaseQuantityTo2ForCostAndLocationIfInitiallyWas1AndLocationIdInPOLAndPieceTheSame() {
    String locationId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    Piece piece = new Piece().withPoLineId(lineId).withLocationId(locationId).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withLocationId(locationId).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1);
    CompositePoLine poLine = new CompositePoLine().withOrderFormat(ELECTRONIC_RESOURCE).withId(lineId)
                                                  .withLocations(List.of(loc)).withCost(cost);
    //When
    PieceFlowUpdatePoLineStrategies.ADD.updateQuantity(1, piece, poLine);
    assertNull(poLine.getCost().getQuantityPhysical());
    assertEquals(2, poLine.getCost().getQuantityElectronic());
    assertEquals(1, poLine.getLocations().size());
    assertNull(poLine.getLocations().get(0).getQuantityPhysical());
    assertEquals(2, poLine.getLocations().get(0).getQuantityElectronic());
    assertEquals(2, poLine.getLocations().get(0).getQuantity());
  }

  @Test
  @DisplayName("Add 1 electronic piece with location to electronic pol with 1 location with holding id")
  void elecAddStrategyShouldIncreaseQuantityTo2ForCostAndLocationIfInitiallyWas1AndLocationIdInPieceAndPOLWithHoldingId() {
    String locationId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    Piece piece = new Piece().withPoLineId(lineId).withLocationId(locationId).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withHoldingId(holdingId).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1);
    List<Location> locations = new ArrayList<>();
    locations.add(loc);
    CompositePoLine poLine = new CompositePoLine().withOrderFormat(ELECTRONIC_RESOURCE).withId(lineId)
                                                  .withLocations(locations).withCost(cost);
    //When
    PieceFlowUpdatePoLineStrategies.ADD.updateQuantity(1, piece, poLine);
    assertNull(poLine.getCost().getQuantityPhysical());
    assertEquals(2, poLine.getCost().getQuantityElectronic());
    assertEquals(2, poLine.getLocations().size());
    assertNull(poLine.getLocations().get(0).getQuantityPhysical());
    assertNull(poLine.getLocations().get(1).getQuantityPhysical());
    assertEquals(1, poLine.getLocations().get(0).getQuantityElectronic());
    assertEquals(1, poLine.getLocations().get(0).getQuantity());
    assertEquals(holdingId, poLine.getLocations().get(0).getHoldingId());
    assertEquals(1, poLine.getLocations().get(1).getQuantityElectronic());
    assertEquals(1, poLine.getLocations().get(1).getQuantity());
    assertEquals(locationId, poLine.getLocations().get(1).getLocationId());
  }

  @Test
  @DisplayName("Add 1 physical piece with location to physical pol with 1 location with holding id")
  void physAddStrategyShouldIncreaseQuantityTo2ForCostAndLocationIfInitiallyWas1AndLocationIdInPieceAndPOLWithHoldingId() {
    String locationId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    Piece piece = new Piece().withPoLineId(lineId).withLocationId(locationId).withFormat(Piece.Format.PHYSICAL);
    Location loc = new Location().withHoldingId(holdingId).withQuantityPhysical(1).withQuantity(1);
    Cost cost = new Cost().withQuantityPhysical(1);
    List<Location> locations = new ArrayList<>();
    locations.add(loc);
    CompositePoLine poLine = new CompositePoLine().withOrderFormat(PHYSICAL_RESOURCE).withId(lineId)
      .withLocations(locations).withCost(cost);
    //When
    PieceFlowUpdatePoLineStrategies.ADD.updateQuantity(1, piece, poLine);
    assertNull(poLine.getCost().getQuantityElectronic());
    assertEquals(2, poLine.getCost().getQuantityPhysical());
    assertEquals(2, poLine.getLocations().size());
    assertNull(poLine.getLocations().get(0).getQuantityElectronic());
    assertNull(poLine.getLocations().get(1).getQuantityElectronic());
    assertEquals(1, poLine.getLocations().get(0).getQuantityPhysical());
    assertEquals(1, poLine.getLocations().get(0).getQuantity());
    assertEquals(holdingId, poLine.getLocations().get(0).getHoldingId());
    assertEquals(1, poLine.getLocations().get(1).getQuantityPhysical());
    assertEquals(1, poLine.getLocations().get(1).getQuantity());
    assertEquals(locationId, poLine.getLocations().get(1).getLocationId());
  }

  @Test
  @DisplayName("Delete 1 electronic piece with holding to electronic pol with 1 location and same holding id as in piece")
  void electDeleteStrategyShouldIncreaseQuantityTo2ForCostAndLocationIfInitiallyWas1AndHoldingIdInPOLAndPieceTheSame() {
    String holdingId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    Piece piece = new Piece().withPoLineId(lineId).withHoldingId(holdingId).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withHoldingId(holdingId).withQuantityElectronic(2).withQuantity(2);
    List<Location> locations = new ArrayList<>();
    locations.add(loc);
    Cost cost = new Cost().withQuantityElectronic(2);
    CompositePoLine poLine = new CompositePoLine().withOrderFormat(ELECTRONIC_RESOURCE).withId(lineId)
      .withLocations(locations).withCost(cost);
    //When
    PieceFlowUpdatePoLineStrategies.DELETE.updateQuantity(1, piece, poLine);
    assertNull(poLine.getCost().getQuantityPhysical());
    assertEquals(1, poLine.getCost().getQuantityElectronic());
    assertEquals(1, poLine.getLocations().size());
    assertNull(poLine.getLocations().get(0).getQuantityPhysical());
    assertEquals(1, poLine.getLocations().get(0).getQuantityElectronic());
    assertEquals(1, poLine.getLocations().get(0).getQuantity());
  }

  @Test
  @DisplayName("Delete 1 physical piece with holding to physical pol with 1 location and same holding id as in piece")
  void physDeleteStrategyShouldIncreaseQuantityTo2ForCostAndLocationIfInitiallyWas1AndHoldingIdInPOLAndPieceTheSame() {
    String holdingId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    Piece piece = new Piece().withPoLineId(lineId).withHoldingId(holdingId).withFormat(Piece.Format.PHYSICAL);
    Location loc = new Location().withHoldingId(holdingId).withQuantityPhysical(2).withQuantity(2);
    List<Location> locations = new ArrayList<>();
    locations.add(loc);
    Cost cost = new Cost().withQuantityPhysical(2);
    CompositePoLine poLine = new CompositePoLine().withOrderFormat(PHYSICAL_RESOURCE).withId(lineId)
      .withLocations(locations).withCost(cost);
    //When
    PieceFlowUpdatePoLineStrategies.DELETE.updateQuantity(1, piece, poLine);
    assertNull(poLine.getCost().getQuantityElectronic());
    assertEquals(1, poLine.getCost().getQuantityPhysical());
    assertEquals(1, poLine.getLocations().size());
    assertNull(poLine.getLocations().get(0).getQuantityElectronic());
    assertEquals(1, poLine.getLocations().get(0).getQuantityPhysical());
    assertEquals(1, poLine.getLocations().get(0).getQuantity());
  }

  @Test
  @DisplayName("Delete 1 physical piece with location to physical pol with 1 location and same location id as in piece")
  void physDeleteStrategyShouldIncreaseQuantityTo2ForCostAndLocationIfInitiallyWas1AndLocationIdInPOLAndPieceTheSame() {
    String locationId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    Piece piece = new Piece().withPoLineId(lineId).withLocationId(locationId).withFormat(Piece.Format.PHYSICAL);
    Location loc = new Location().withLocationId(locationId).withQuantityPhysical(2).withQuantity(2);
    List<Location> locations = new ArrayList<>();
    locations.add(loc);
    Cost cost = new Cost().withQuantityPhysical(2);
    CompositePoLine poLine = new CompositePoLine().withOrderFormat(PHYSICAL_RESOURCE).withId(lineId)
      .withLocations(locations).withCost(cost);
    //When
    PieceFlowUpdatePoLineStrategies.DELETE.updateQuantity(1, piece, poLine);
    assertNull(poLine.getCost().getQuantityElectronic());
    assertEquals(1, poLine.getCost().getQuantityPhysical());
    assertEquals(1, poLine.getLocations().size());
    assertNull(poLine.getLocations().get(0).getQuantityElectronic());
    assertEquals(1, poLine.getLocations().get(0).getQuantityPhysical());
    assertEquals(1, poLine.getLocations().get(0).getQuantity());
  }

  @Test
  @DisplayName("Delete 1 electronic piece with location to electronic pol with 1 location and same location id as in piece")
  void elecDeleteStrategyShouldIncreaseQuantityTo2ForCostAndLocationIfInitiallyWas1AndLocationIdInPOLAndPieceTheSame() {
    String locationId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    Piece piece = new Piece().withPoLineId(lineId).withLocationId(locationId).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withLocationId(locationId).withQuantityElectronic(2).withQuantity(2);
    List<Location> locations = new ArrayList<>();
    locations.add(loc);
    Cost cost = new Cost().withQuantityElectronic(2);
    CompositePoLine poLine = new CompositePoLine().withOrderFormat(ELECTRONIC_RESOURCE).withId(lineId)
      .withLocations(locations).withCost(cost);
    //When
    PieceFlowUpdatePoLineStrategies.DELETE.updateQuantity(1, piece, poLine);
    assertNull(poLine.getCost().getQuantityPhysical());
    assertEquals(1, poLine.getCost().getQuantityElectronic());
    assertEquals(1, poLine.getLocations().size());
    assertNull(poLine.getLocations().get(0).getQuantityPhysical());
    assertEquals(1, poLine.getLocations().get(0).getQuantityElectronic());
    assertEquals(1, poLine.getLocations().get(0).getQuantity());
  }

  @Test
  @DisplayName("Should delete POL location if 1 physical piece with location to physical pol with 1 location and same location id as in piece")
  void physDeleteLocationStrategyShouldIncreaseQuantityTo2ForCostAndLocationIfInitiallyWas1AndLocationIdInPOLAndPieceTheSame() {
    String locationId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    Piece piece = new Piece().withPoLineId(lineId).withLocationId(locationId).withFormat(Piece.Format.PHYSICAL);
    Location loc = new Location().withLocationId(locationId).withQuantityPhysical(1).withQuantity(1);
    List<Location> locations = new ArrayList<>();
    locations.add(loc);
    Cost cost = new Cost().withQuantityPhysical(1);
    CompositePoLine poLine = new CompositePoLine().withOrderFormat(PHYSICAL_RESOURCE).withId(lineId)
      .withLocations(locations).withCost(cost);
    //When
    PieceFlowUpdatePoLineStrategies.DELETE.updateQuantity(1, piece, poLine);
    assertNull(poLine.getCost().getQuantityElectronic());
    assertNull(poLine.getCost().getQuantityPhysical());
    assertEquals(Collections.emptyList(), poLine.getLocations());
  }

  @Test
  @DisplayName("Should delete POL location if 1 electronic piece with location to electronic pol with 1 location and same location id as in piece")
  void elecDeleteLocationStrategyShouldDecreaseIncreaseQuantityForCostAndLocationIfInitiallyWas1AndLocationIdInPOLAndPieceTheSame() {
    String locationId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    Piece piece = new Piece().withPoLineId(lineId).withLocationId(locationId).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withLocationId(locationId).withQuantityElectronic(1).withQuantity(1);
    List<Location> locations = new ArrayList<>();
    locations.add(loc);
    Cost cost = new Cost().withQuantityElectronic(1);
    CompositePoLine poLine = new CompositePoLine().withOrderFormat(ELECTRONIC_RESOURCE).withId(lineId)
      .withLocations(locations).withCost(cost);
    //When
    PieceFlowUpdatePoLineStrategies.DELETE.updateQuantity(1, piece, poLine);
    assertNull(poLine.getCost().getQuantityPhysical());
    assertNull(poLine.getCost().getQuantityElectronic());
    assertEquals(Collections.emptyList(), poLine.getLocations());
  }
}
