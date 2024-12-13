package org.folio.service.pieces.validators;

import static org.folio.rest.core.exceptions.ErrorCodes.HOLDINGS_ID_AND_LOCATION_ID_IS_NULL_ERROR;
import static org.folio.rest.core.exceptions.ErrorCodes.MAY_BE_LINK_TO_EITHER_HOLDING_OR_LOCATION_ERROR;
import static org.folio.rest.core.exceptions.ErrorCodes.PIECE_FORMAT_IS_NOT_VALID_ERROR;
import static org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE;
import static org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat.PHYSICAL_RESOURCE;
import static org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat.P_E_MIX;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.List;
import java.util.UUID;

import org.folio.CopilotGenerated;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.Piece;
import org.junit.jupiter.api.Test;

@CopilotGenerated(partiallyGenerated = true)
public class PieceValidatorUtilTest {

  @Test
  void testShouldReturnErrorsIfLocationAndHoldingIsNotProvided() {
    Piece piece = new Piece().withFormat(Piece.Format.ELECTRONIC);
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING);
    CompositePoLine originPoLine = new CompositePoLine().withIsPackage(false).withEresource(eresource);
    List<Error> errorList = PieceValidatorUtil.validatePieceLocation(piece, originPoLine);
    assertEquals(HOLDINGS_ID_AND_LOCATION_ID_IS_NULL_ERROR.toError(), errorList.get(0));
  }

  @Test
  void testShouldReturnErrorsIfElectrLocationAndHoldingIsNotProvidedAndCreateInventoryNontNone() {
    Piece piece = new Piece().withFormat(Piece.Format.ELECTRONIC);
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING);
    CompositePoLine originPoLine = new CompositePoLine().withIsPackage(false).withEresource(eresource);
    List<Error> errorList = PieceValidatorUtil.validatePieceLocation(piece, originPoLine);
    assertEquals(HOLDINGS_ID_AND_LOCATION_ID_IS_NULL_ERROR.toError(), errorList.get(0));
  }

    @Test
  void testShouldValidIfElectrLocationAndHoldingIsNotProvided() {
    Piece piece = new Piece().withFormat(Piece.Format.ELECTRONIC);
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.NONE);
    CompositePoLine originPoLine = new CompositePoLine().withIsPackage(false).withEresource(eresource);
    List<Error> errorList = PieceValidatorUtil.validatePieceLocation(piece, originPoLine);
    assertEquals(0, errorList.size());
  }

  @Test
  void testShouldReturnErrorsIfLocationPhysAndHoldingIsNotProvided() {
    Piece piece = new Piece().withFormat(Piece.Format.ELECTRONIC);
    Physical physical = new Physical().withCreateInventory(Physical.CreateInventory.NONE);
    CompositePoLine originPoLine = new CompositePoLine().withIsPackage(false).withPhysical(physical);
    List<Error> errorList = PieceValidatorUtil.validatePieceLocation(piece, originPoLine);
    assertEquals(0, errorList.size());
  }

  @Test
  void testShouldReturnErrorsIfLocationAndHoldingProvidedAtOneTime() {
    Piece piece = new Piece().withFormat(Piece.Format.PHYSICAL).withLocationId(UUID.randomUUID().toString())
                             .withHoldingId(UUID.randomUUID().toString());
    Physical physical = new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING);
    CompositePoLine originPoLine = new CompositePoLine().withIsPackage(false).withPhysical(physical);
    List<Error> errorList = PieceValidatorUtil.validatePieceLocation(piece, originPoLine);
    assertEquals(MAY_BE_LINK_TO_EITHER_HOLDING_OR_LOCATION_ERROR.toError(), errorList.get(0));
  }

  @Test
  void testShouldReturnErrorWhenPiecePhysicalAndLineIsElectronic() {
    Piece piece = new Piece().withLocationId(UUID.randomUUID().toString())
      .withFormat(Piece.Format.PHYSICAL);
    CompositePoLine poLine = new CompositePoLine().withOrderFormat(ELECTRONIC_RESOURCE);
    List<Error> errorList = PieceValidatorUtil.validatePieceFormat(piece, poLine);
    assertEquals(PIECE_FORMAT_IS_NOT_VALID_ERROR.getCode(), errorList.get(0).getCode());
  }

  @Test
  void testShouldReturnErrorWhenPieceElectronicAndLineIsPhysical() {
    Piece piece = new Piece().withLocationId(UUID.randomUUID().toString())
      .withFormat(Piece.Format.ELECTRONIC);
    CompositePoLine poLine = new CompositePoLine().withOrderFormat(PHYSICAL_RESOURCE);
    List<Error> errorList = PieceValidatorUtil.validatePieceFormat(piece, poLine);
    assertEquals(PIECE_FORMAT_IS_NOT_VALID_ERROR.getCode(), errorList.get(0).getCode());
  }

  @Test
  void testShouldReturnErrorWhenPieceOtherAndLineIsPhysical() {
    Piece piece = new Piece().withLocationId(UUID.randomUUID().toString())
      .withFormat(Piece.Format.OTHER);
    CompositePoLine poLine = new CompositePoLine().withOrderFormat(PHYSICAL_RESOURCE);
    List<Error> errorList = PieceValidatorUtil.validatePieceFormat(piece, poLine);
    assertEquals(PIECE_FORMAT_IS_NOT_VALID_ERROR.getCode(), errorList.get(0).getCode());
  }

  @Test
  void testPieceIsValidWhenLineAndPieceIsPhysical() {
    Piece piece = new Piece().withLocationId(UUID.randomUUID().toString())
      .withFormat(Piece.Format.PHYSICAL);
    CompositePoLine poLine = new CompositePoLine().withOrderFormat(PHYSICAL_RESOURCE);
    List<Error> errorList = PieceValidatorUtil.validatePieceFormat(piece, poLine);
    assertEquals(0, errorList.size());
  }

  @Test
  void testPieceIsValidWhenLineAndPieceIsElectronic() {
    Piece piece = new Piece().withLocationId(UUID.randomUUID().toString())
      .withFormat(Piece.Format.ELECTRONIC);
    CompositePoLine poLine = new CompositePoLine().withOrderFormat(ELECTRONIC_RESOURCE);
    List<Error> errorList = PieceValidatorUtil.validatePieceFormat(piece, poLine);
    assertEquals(0, errorList.size());
  }

  @Test
  void testPieceIsValidWhenLineIsMixedAndPieceIsElectronic() {
    Piece piece = new Piece().withLocationId(UUID.randomUUID().toString())
      .withFormat(Piece.Format.ELECTRONIC);
    CompositePoLine poLine = new CompositePoLine().withOrderFormat(P_E_MIX);
    List<Error> errorList = PieceValidatorUtil.validatePieceFormat(piece, poLine);
    assertEquals(0, errorList.size());
  }

  @Test
  void testPieceIsValidWhenLineIsMixedAndPieceIsPhysical() {
    Piece piece = new Piece().withLocationId(UUID.randomUUID().toString())
      .withFormat(Piece.Format.PHYSICAL);
    CompositePoLine poLine = new CompositePoLine().withOrderFormat(P_E_MIX);
    List<Error> errorList = PieceValidatorUtil.validatePieceFormat(piece, poLine);
    assertEquals(0, errorList.size());
  }

  @Test
  void testValidateRelatedOrderWhenOrderIsNull() {
    CompositePoLine poLine = new CompositePoLine();
    List<Error> errorList = PieceValidatorUtil.validatePieceRelatedOrder(null, poLine);
    assertEquals(0, errorList.size());
  }

  @Test
  void testValidateRelatedOrderWhenPoLineIsNull() {
    CompositePurchaseOrder order = new CompositePurchaseOrder();
    List<Error> errorList = PieceValidatorUtil.validatePieceRelatedOrder(order, null);
    assertEquals(0, errorList.size());
  }

  @Test
  void testValidateRelatedWhenOrderIsPendingAndCheckinItemsIsFalse() {
    CompositePurchaseOrder order = new CompositePurchaseOrder().withWorkflowStatus(WorkflowStatus.PENDING);
    CompositePoLine poLine = new CompositePoLine().withCheckinItems(false);
    List<Error> errorList = PieceValidatorUtil.validatePieceRelatedOrder(order, poLine);
    assertEquals(ErrorCodes.PIECE_RELATED_ORDER_DATA_IS_NOT_VALID.getCode(), errorList.get(0).getCode());
  }

  @Test
  void testValidateRelatedOrderWhenOrderIsPendingAndCheckinItemsIsTrue() {
    CompositePurchaseOrder order = new CompositePurchaseOrder().withWorkflowStatus(WorkflowStatus.PENDING);
    CompositePoLine poLine = new CompositePoLine().withCheckinItems(true);
    List<Error> errorList = PieceValidatorUtil.validatePieceRelatedOrder(order, poLine);
    assertEquals(0, errorList.size());
  }

  @Test
  void testValidateRelatedWhenOrderIsNotPending() {
    CompositePurchaseOrder order = new CompositePurchaseOrder().withWorkflowStatus(WorkflowStatus.OPEN);
    CompositePoLine poLine = new CompositePoLine().withCheckinItems(false);
    List<Error> errorList = PieceValidatorUtil.validatePieceRelatedOrder(order, poLine);
    assertEquals(0, errorList.size());
  }
}
