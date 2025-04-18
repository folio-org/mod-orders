package org.folio.service.pieces.flows;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;
import java.util.UUID;

import org.folio.CopilotGenerated;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

@CopilotGenerated(partiallyGenerated = true)
public class DefaultPieceFlowsValidatorTest {
  private static final String ORDER_IRD = UUID.randomUUID().toString();
  private static final String LOCATION_ID = UUID.randomUUID().toString();
  private static final String PO_LINE_ID = UUID.randomUUID().toString();

  private DefaultPieceFlowsValidator defaultPieceFlowsValidator = new DefaultPieceFlowsValidator();

  @Test
  void createPieceIsForbiddenIfPieceAndLIneFormatIsNotCompatible() {
    Piece piece = new Piece().withPoLineId(PO_LINE_ID).withLocationId(LOCATION_ID).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withLocationId(LOCATION_ID).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1);
    CompositePurchaseOrder originOrder = new CompositePurchaseOrder().withId(ORDER_IRD).withWorkflowStatus(WorkflowStatus.OPEN);
    PoLine originPoLine = new PoLine().withIsPackage(true).withPurchaseOrderId(ORDER_IRD)
                                    .withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE).withId(PO_LINE_ID)
                                    .withLocations(List.of(loc)).withCost(cost);

    HttpException exception = Assertions.assertThrows(HttpException.class, () -> {
      defaultPieceFlowsValidator.isPieceRequestValid(piece, originOrder, originPoLine, true);
    });

    boolean isErrorPresent = exception.getErrors().getErrors().stream()
      .anyMatch(error -> error.getCode().equals(ErrorCodes.PIECE_FORMAT_IS_NOT_VALID_ERROR.getCode()));
    assertEquals(true, isErrorPresent);
  }

  @Test
  void createPieceIsForbiddenIfCreateInventoryInTheLineDonNotAllowThat() {
    Piece piece = new Piece().withPoLineId(PO_LINE_ID).withLocationId(LOCATION_ID).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withLocationId(LOCATION_ID).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1);
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING);
    CompositePurchaseOrder originOrder = new CompositePurchaseOrder().withId(ORDER_IRD).withWorkflowStatus(WorkflowStatus.OPEN);
    PoLine originPoLine = new PoLine().withIsPackage(true).withPurchaseOrderId(ORDER_IRD)
      .withOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE).withId(PO_LINE_ID)
      .withEresource(eresource)
      .withLocations(List.of(loc)).withCost(cost);

    HttpException exception = Assertions.assertThrows(HttpException.class, () -> {
      defaultPieceFlowsValidator.isPieceRequestValid(piece, originOrder, originPoLine, true);
    });

    boolean isErrorPresent = exception.getErrors().getErrors().stream()
      .anyMatch(error -> error.getCode().equals(ErrorCodes.CREATE_ITEM_FOR_PIECE_IS_NOT_ALLOWED_ERROR.getCode()));
    assertEquals(true, isErrorPresent);
  }

  @Test
  void createPieceAllowableIfCreateInventoryInTheLineAllowThatButCreateItemFlagIsFalse() {
    Piece piece = new Piece().withPoLineId(PO_LINE_ID).withLocationId(LOCATION_ID).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withLocationId(LOCATION_ID).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1);
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING_ITEM);
    CompositePurchaseOrder originOrder = new CompositePurchaseOrder().withId(ORDER_IRD).withWorkflowStatus(WorkflowStatus.OPEN);
    PoLine originPoLine = new PoLine().withIsPackage(true).withPurchaseOrderId(ORDER_IRD)
      .withOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE).withId(PO_LINE_ID)
      .withEresource(eresource)
      .withLocations(List.of(loc)).withCost(cost);

    defaultPieceFlowsValidator.isPieceRequestValid(piece, originOrder, originPoLine, true);
  }

  @Test
  void createPieceIsValid() {
    Piece piece = new Piece().withPoLineId(PO_LINE_ID).withLocationId(LOCATION_ID).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withLocationId(LOCATION_ID).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1);
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING_ITEM);
    CompositePurchaseOrder originOrder = new CompositePurchaseOrder().withId(ORDER_IRD).withWorkflowStatus(WorkflowStatus.OPEN);
    PoLine originPoLine = new PoLine().withIsPackage(true).withPurchaseOrderId(ORDER_IRD)
                .withOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE).withId(PO_LINE_ID)
                .withEresource(eresource)
                .withLocations(List.of(loc)).withCost(cost);

    defaultPieceFlowsValidator.isPieceRequestValid(piece, originOrder, originPoLine, true);
  }

  @ParameterizedTest
  @CsvSource(value = {"true:true",
                      "true:false",
                      "false:false"}, delimiter = ':')
  void createPieceWithValidDisplayFlagsCombinations(Boolean displayOnHoldings, Boolean displayToPublic) {
    Piece piece = new Piece().withPoLineId(PO_LINE_ID).withLocationId(LOCATION_ID).withFormat(Piece.Format.ELECTRONIC)
      .withDisplayOnHolding(displayOnHoldings)
      .withDisplayToPublic(displayToPublic);
    Location loc = new Location().withLocationId(LOCATION_ID).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1);
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING_ITEM);
    CompositePurchaseOrder originOrder = new CompositePurchaseOrder().withId(ORDER_IRD).withWorkflowStatus(WorkflowStatus.OPEN);
    PoLine originPoLine = new PoLine().withIsPackage(true).withPurchaseOrderId(ORDER_IRD)
      .withOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE).withId(PO_LINE_ID)
      .withEresource(eresource)
      .withLocations(List.of(loc)).withCost(cost);

    defaultPieceFlowsValidator.isPieceRequestValid(piece, originOrder, originPoLine, true);
  }

  @Test
  void createPieceWithInvalidDisplayFlagsCombination() {
    Piece piece = new Piece().withPoLineId(PO_LINE_ID).withLocationId(LOCATION_ID).withFormat(Piece.Format.ELECTRONIC)
      .withDisplayOnHolding(false)
      .withDisplayToPublic(true);
    Location loc = new Location().withLocationId(LOCATION_ID).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1);
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING_ITEM);
    CompositePurchaseOrder originOrder = new CompositePurchaseOrder().withId(ORDER_IRD).withWorkflowStatus(WorkflowStatus.OPEN);
    PoLine originPoLine = new PoLine().withIsPackage(true).withPurchaseOrderId(ORDER_IRD)
      .withOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE).withId(PO_LINE_ID)
      .withEresource(eresource)
      .withLocations(List.of(loc)).withCost(cost);

    HttpException exception = Assertions.assertThrows(HttpException.class, () -> {
      defaultPieceFlowsValidator.isPieceRequestValid(piece, originOrder, originPoLine, true);
    });
    boolean isErrorPresent = exception.getErrors().getErrors().stream()
      .anyMatch(error -> error.getCode().equals(ErrorCodes.PIECE_DISPLAY_ON_HOLDINGS_IS_NOT_CONSISTENT.getCode()));
    assertTrue(isErrorPresent);
  }

  @Test
  void createPieceWithPendingOrderThatHasSynchronizedStatus() {
    Piece piece = new Piece().withPoLineId(PO_LINE_ID).withLocationId(LOCATION_ID).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withLocationId(LOCATION_ID).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1);
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING_ITEM);
    // creating order with pending status
    CompositePurchaseOrder originOrder = new CompositePurchaseOrder().withId(ORDER_IRD).withWorkflowStatus(WorkflowStatus.PENDING);
    // creating poLine with synchronized status(by default)
    PoLine originPoLine = new PoLine().withIsPackage(true).withPurchaseOrderId(ORDER_IRD)
      .withOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE).withId(PO_LINE_ID)
      .withEresource(eresource)
      .withLocations(List.of(loc)).withCost(cost);

    HttpException exception = Assertions.assertThrows(HttpException.class, () -> {
      defaultPieceFlowsValidator.isPieceRequestValid(piece, originOrder, originPoLine, true);
    });
    boolean isErrorPresent = exception.getErrors().getErrors().stream()
      .anyMatch(error -> error.getCode().equals(ErrorCodes.PIECE_RELATED_ORDER_DATA_IS_NOT_VALID.getCode()));
    assertTrue(isErrorPresent);
  }

  @Test
  void testIsPieceBatchRequestValidWithSameTitleIdAndPoLineId() {
    var piece1 = new Piece().withPoLineId(PO_LINE_ID).withLocationId(LOCATION_ID).withFormat(Piece.Format.ELECTRONIC);
    var piece2 = new Piece().withPoLineId(PO_LINE_ID).withLocationId(LOCATION_ID).withFormat(Piece.Format.ELECTRONIC);
    var loc = new Location().withLocationId(LOCATION_ID).withQuantityElectronic(1).withQuantity(1);
    var cost = new Cost().withQuantityElectronic(1);
    var eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING_ITEM);
    var originOrder = new CompositePurchaseOrder().withId(ORDER_IRD).withWorkflowStatus(WorkflowStatus.OPEN);
    var originPoLine = new PoLine().withIsPackage(true).withPurchaseOrderId(ORDER_IRD)
      .withOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE).withId(PO_LINE_ID)
      .withEresource(eresource)
      .withLocations(List.of(loc)).withCost(cost);

    defaultPieceFlowsValidator.isPieceBatchRequestValid(List.of(piece1, piece2), originOrder, originPoLine, true);
  }

  @Test
  void testIsPieceBatchRequestCheckEachPieceValidation() {
    var piece1 = new Piece().withPoLineId(PO_LINE_ID).withTitleId(ORDER_IRD).withLocationId(LOCATION_ID).withFormat(Piece.Format.ELECTRONIC);
    var piece2 = new Piece().withPoLineId(PO_LINE_ID).withTitleId(ORDER_IRD).withLocationId(LOCATION_ID).withFormat(Piece.Format.PHYSICAL);
    var originOrder = new CompositePurchaseOrder().withId(ORDER_IRD).withWorkflowStatus(WorkflowStatus.OPEN);
    var originPoLine = new PoLine().withIsPackage(true).withPurchaseOrderId(ORDER_IRD)
      .withOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE).withId(PO_LINE_ID);

    var exception = Assertions.assertThrows(HttpException.class, () ->
      defaultPieceFlowsValidator.isPieceBatchRequestValid(List.of(piece1, piece2), originOrder, originPoLine, true));
    boolean isErrorPresent = exception.getErrors().getErrors().stream()
      .anyMatch(error -> error.getCode().equals(ErrorCodes.CREATE_ITEM_FOR_PIECE_IS_NOT_ALLOWED_ERROR.getCode()));
    assertTrue(isErrorPresent);
  }

  @Test
  void testIsPieceBatchRequestValidWithDifferentTitleIds() {
    var piece1 = new Piece().withPoLineId(PO_LINE_ID).withTitleId(UUID.randomUUID().toString()).withLocationId(LOCATION_ID).withFormat(Piece.Format.PHYSICAL);
    var piece2 = new Piece().withPoLineId(PO_LINE_ID).withTitleId(UUID.randomUUID().toString()).withLocationId(LOCATION_ID).withFormat(Piece.Format.PHYSICAL);
    var originOrder = new CompositePurchaseOrder().withId(ORDER_IRD).withWorkflowStatus(WorkflowStatus.OPEN);
    var originPoLine = new PoLine().withIsPackage(true).withPurchaseOrderId(ORDER_IRD)
      .withOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE).withId(PO_LINE_ID);

    var exception = Assertions.assertThrows(HttpException.class, () ->
      defaultPieceFlowsValidator.isPieceBatchRequestValid(List.of(piece1, piece2), originOrder, originPoLine, true));

    boolean isErrorPresent = exception.getErrors().getErrors().stream()
      .anyMatch(error -> error.getMessage().equals("All pieces in the batch should have the same titleId and poLineId"));
    assertTrue(isErrorPresent);
  }

  @Test
  void testIsPieceBatchRequestValidWithDifferentPoLineIds() {
    var piece1 = new Piece().withPoLineId(UUID.randomUUID().toString()).withTitleId(ORDER_IRD)
      .withLocationId(LOCATION_ID).withFormat(Piece.Format.ELECTRONIC);
    var piece2 = new Piece().withPoLineId(UUID.randomUUID().toString()).withTitleId(ORDER_IRD)
      .withLocationId(LOCATION_ID).withFormat(Piece.Format.ELECTRONIC);
    var originOrder = new CompositePurchaseOrder().withId(ORDER_IRD).withWorkflowStatus(WorkflowStatus.OPEN);
    var originPoLine = new PoLine().withIsPackage(true).withPurchaseOrderId(ORDER_IRD)
      .withOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE).withId(PO_LINE_ID);

    var exception = Assertions.assertThrows(HttpException.class, () ->
      defaultPieceFlowsValidator.isPieceBatchRequestValid(List.of(piece1, piece2), originOrder, originPoLine, true));

    boolean isErrorPresent = exception.getErrors().getErrors().stream()
      .anyMatch(error -> error.getMessage().equals("All pieces in the batch should have the same titleId and poLineId"));
    assertTrue(isErrorPresent);
  }

}
