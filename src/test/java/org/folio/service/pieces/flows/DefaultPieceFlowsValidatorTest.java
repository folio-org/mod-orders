package org.folio.service.pieces.flows;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;
import java.util.UUID;

import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

public class DefaultPieceFlowsValidatorTest {
  private DefaultPieceFlowsValidator defaultPieceFlowsValidator = new DefaultPieceFlowsValidator();

  @Test
  void createPieceIsForbiddenIfPieceAndLIneFormatIsNotCompatible() {
    String orderId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    Piece piece = new Piece().withPoLineId(lineId).withLocationId(locationId).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withLocationId(locationId).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1);
    CompositePoLine originPoLine = new CompositePoLine().withIsPackage(true).withPurchaseOrderId(orderId)
                                    .withOrderFormat(CompositePoLine.OrderFormat.PHYSICAL_RESOURCE).withId(lineId)
                                    .withLocations(List.of(loc)).withCost(cost);

    HttpException exception = Assertions.assertThrows(HttpException.class, () -> {
      defaultPieceFlowsValidator.isPieceRequestValid(piece, originPoLine, true);
    });

    boolean isErrorPresent = exception.getErrors().getErrors().stream()
      .anyMatch(error -> error.getCode().equals(ErrorCodes.PIECE_FORMAT_IS_NOT_VALID_ERROR.getCode()));
    assertEquals(true, isErrorPresent);
  }

  @Test
  void createPieceIsForbiddenIfCreateInventoryInTheLineDonNotAllowThat() {
    String orderId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    Piece piece = new Piece().withPoLineId(lineId).withLocationId(locationId).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withLocationId(locationId).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1);
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING);
    CompositePoLine originPoLine = new CompositePoLine().withIsPackage(true).withPurchaseOrderId(orderId)
      .withOrderFormat(CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE).withId(lineId)
      .withEresource(eresource)
      .withLocations(List.of(loc)).withCost(cost);

    HttpException exception = Assertions.assertThrows(HttpException.class, () -> {
      defaultPieceFlowsValidator.isPieceRequestValid(piece, originPoLine, true);
    });

    boolean isErrorPresent = exception.getErrors().getErrors().stream()
      .anyMatch(error -> error.getCode().equals(ErrorCodes.CREATE_ITEM_FOR_PIECE_IS_NOT_ALLOWED_ERROR.getCode()));
    assertEquals(true, isErrorPresent);
  }

  @Test
  void createPieceAllowableIfCreateInventoryInTheLineAllowThatButCreateItemFlagIsFalse() {
    String orderId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    Piece piece = new Piece().withPoLineId(lineId).withLocationId(locationId).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withLocationId(locationId).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1);
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING_ITEM);
    CompositePoLine originPoLine = new CompositePoLine().withIsPackage(true).withPurchaseOrderId(orderId)
      .withOrderFormat(CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE).withId(lineId)
      .withEresource(eresource)
      .withLocations(List.of(loc)).withCost(cost);

    defaultPieceFlowsValidator.isPieceRequestValid(piece, originPoLine, true);
  }

  @Test
  void createPieceIsValid() {
    String orderId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    Piece piece = new Piece().withPoLineId(lineId).withLocationId(locationId).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withLocationId(locationId).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1);
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING_ITEM);
    CompositePoLine originPoLine = new CompositePoLine().withIsPackage(true).withPurchaseOrderId(orderId)
                .withOrderFormat(CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE).withId(lineId)
                .withEresource(eresource)
                .withLocations(List.of(loc)).withCost(cost);

    defaultPieceFlowsValidator.isPieceRequestValid(piece, originPoLine, true);
  }

  @ParameterizedTest
  @CsvSource(value = {"true:true",
                      "true:false",
                      "false:false"}, delimiter = ':')
  void createPieceWithValidDisplayFlagsCombinations(Boolean displayOnHoldings, Boolean displayToPublic) {
    String orderId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    Piece piece = new Piece().withPoLineId(lineId).withLocationId(locationId).withFormat(Piece.Format.ELECTRONIC)
      .withDisplayOnHolding(displayOnHoldings)
      .withDisplayToPublic(displayToPublic);
    Location loc = new Location().withLocationId(locationId).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1);
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING_ITEM);
    CompositePoLine originPoLine = new CompositePoLine().withIsPackage(true).withPurchaseOrderId(orderId)
      .withOrderFormat(CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE).withId(lineId)
      .withEresource(eresource)
      .withLocations(List.of(loc)).withCost(cost);

    defaultPieceFlowsValidator.isPieceRequestValid(piece, originPoLine, true);
  }

  @Test
  void createPieceWithInvalidDisplayFlagsCombination() {
    String orderId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    Piece piece = new Piece().withPoLineId(lineId).withLocationId(locationId).withFormat(Piece.Format.ELECTRONIC)
      .withDisplayOnHolding(false)
      .withDisplayToPublic(true);
    Location loc = new Location().withLocationId(locationId).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1);
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING_ITEM);
    CompositePoLine originPoLine = new CompositePoLine().withIsPackage(true).withPurchaseOrderId(orderId)
      .withOrderFormat(CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE).withId(lineId)
      .withEresource(eresource)
      .withLocations(List.of(loc)).withCost(cost);

    HttpException exception = Assertions.assertThrows(HttpException.class, () -> {
      defaultPieceFlowsValidator.isPieceRequestValid(piece, originPoLine, true);
    });
    boolean isErrorPresent = exception.getErrors().getErrors().stream()
      .anyMatch(error -> error.getCode().equals(ErrorCodes.PIECE_DISPLAY_ON_HOLDINGS_IS_NOT_CONSISTENT.getCode()));
    assertTrue(isErrorPresent);
  }
}
