package org.folio.service.pieces.flows.create;

import static org.folio.rest.jaxrs.model.PurchaseOrder.WorkflowStatus.OPEN;
import static org.folio.rest.jaxrs.model.PurchaseOrder.WorkflowStatus.PENDING;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.List;
import java.util.UUID;

import org.folio.models.pieces.PieceCreationHolder;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class PieceCreateFlowValidatorTest {
  private PieceCreateFlowValidator pieceCreateFlowValidator = new PieceCreateFlowValidator();

  @Test
  void createPieceIsForbiddenIfPieceAndLIneFormatIsNotCompatible() {
    String orderId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    Piece piece = new Piece().withPoLineId(lineId).withLocationId(locationId).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withLocationId(locationId).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(PENDING);
    PoLine originPoLine = new PoLine().withIsPackage(true).withPurchaseOrderId(orderId)
                                    .withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE).withId(lineId)
                                    .withLocations(List.of(loc)).withCost(cost);
    PieceCreationHolder holder = new PieceCreationHolder().withPieceToCreate(piece).withCreateItem(true);
    holder.withOrderInformation(purchaseOrder, originPoLine);

    HttpException exception = Assertions.assertThrows(HttpException.class, () -> {
      pieceCreateFlowValidator.isCreatePieceRequestValid(holder);
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
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(OPEN);
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING);
    PoLine originPoLine = new PoLine().withIsPackage(true).withPurchaseOrderId(orderId)
      .withOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE).withId(lineId)
      .withEresource(eresource)
      .withLocations(List.of(loc)).withCost(cost);
    PieceCreationHolder holder = new PieceCreationHolder().withPieceToCreate(piece).withCreateItem(true);
    holder.withOrderInformation(purchaseOrder, originPoLine);

    HttpException exception = Assertions.assertThrows(HttpException.class, () -> {
      pieceCreateFlowValidator.isCreatePieceRequestValid(holder);
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
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(OPEN);
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING_ITEM);
    PoLine originPoLine = new PoLine().withIsPackage(true).withPurchaseOrderId(orderId)
      .withOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE).withId(lineId)
      .withEresource(eresource)
      .withLocations(List.of(loc)).withCost(cost);
    PieceCreationHolder holder = new PieceCreationHolder().withPieceToCreate(piece).withCreateItem(true);
    holder.withOrderInformation(purchaseOrder, originPoLine);

    pieceCreateFlowValidator.isCreatePieceRequestValid(holder);
  }

  @Test
  void createPieceIsValid() {
    String orderId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    Piece piece = new Piece().withPoLineId(lineId).withLocationId(locationId).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withLocationId(locationId).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(OPEN);
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING_ITEM);
    PoLine originPoLine = new PoLine().withIsPackage(true).withPurchaseOrderId(orderId)
                .withOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE).withId(lineId)
                .withEresource(eresource)
                .withLocations(List.of(loc)).withCost(cost);
    PieceCreationHolder holder = new PieceCreationHolder().withPieceToCreate(piece).withCreateItem(true);
    holder.withOrderInformation(purchaseOrder, originPoLine);

    pieceCreateFlowValidator.isCreatePieceRequestValid(holder);
  }
}
