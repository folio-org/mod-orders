package org.folio.service.pieces.validators;

import static org.folio.rest.core.exceptions.ErrorCodes.HOLDINGS_ID_AND_LOCATION_ID_IS_NULL_ERROR;
import static org.folio.rest.core.exceptions.ErrorCodes.MAY_BE_LINK_TO_EITHER_HOLDING_OR_LOCATION_ERROR;
import static org.folio.rest.core.exceptions.ErrorCodes.PIECE_FORMAT_IS_NOT_VALID_ERROR;
import static org.folio.rest.jaxrs.model.PoLine.OrderFormat.ELECTRONIC_RESOURCE;
import static org.folio.rest.jaxrs.model.PoLine.OrderFormat.PHYSICAL_RESOURCE;
import static org.folio.rest.jaxrs.model.PoLine.OrderFormat.P_E_MIX;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.List;
import java.util.UUID;

import org.folio.CopilotGenerated;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.Piece;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertFalse;

@CopilotGenerated(partiallyGenerated = true)
public class PieceValidatorUtilTest {

  @Test
  void testShouldReturnErrorsIfLocationAndHoldingIsNotProvided() {
    Piece piece = new Piece().withFormat(Piece.Format.ELECTRONIC);
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING);
    PoLine originPoLine = new PoLine().withIsPackage(false).withEresource(eresource);
    List<Error> errorList = PieceValidatorUtil.validatePieceLocation(piece, originPoLine);
    assertEquals(HOLDINGS_ID_AND_LOCATION_ID_IS_NULL_ERROR.toError(), errorList.get(0));
  }

  @Test
  void testShouldReturnErrorsIfElectrLocationAndHoldingIsNotProvidedAndCreateInventoryNontNone() {
    Piece piece = new Piece().withFormat(Piece.Format.ELECTRONIC);
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING);
    PoLine originPoLine = new PoLine().withIsPackage(false).withEresource(eresource);
    List<Error> errorList = PieceValidatorUtil.validatePieceLocation(piece, originPoLine);
    assertEquals(HOLDINGS_ID_AND_LOCATION_ID_IS_NULL_ERROR.toError(), errorList.get(0));
  }

    @Test
  void testShouldValidIfElectrLocationAndHoldingIsNotProvided() {
    Piece piece = new Piece().withFormat(Piece.Format.ELECTRONIC);
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.NONE);
    PoLine originPoLine = new PoLine().withIsPackage(false).withEresource(eresource);
    List<Error> errorList = PieceValidatorUtil.validatePieceLocation(piece, originPoLine);
    assertEquals(0, errorList.size());
  }

  @Test
  void testShouldReturnErrorsIfLocationPhysAndHoldingIsNotProvided() {
    Piece piece = new Piece().withFormat(Piece.Format.ELECTRONIC);
    Physical physical = new Physical().withCreateInventory(Physical.CreateInventory.NONE);
    PoLine originPoLine = new PoLine().withIsPackage(false).withPhysical(physical);
    List<Error> errorList = PieceValidatorUtil.validatePieceLocation(piece, originPoLine);
    assertEquals(0, errorList.size());
  }

  @Test
  void testShouldReturnErrorsIfLocationAndHoldingProvidedAtOneTime() {
    Piece piece = new Piece().withFormat(Piece.Format.PHYSICAL).withLocationId(UUID.randomUUID().toString())
                             .withHoldingId(UUID.randomUUID().toString());
    Physical physical = new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING);
    PoLine originPoLine = new PoLine().withIsPackage(false).withPhysical(physical);
    List<Error> errorList = PieceValidatorUtil.validatePieceLocation(piece, originPoLine);
    assertEquals(MAY_BE_LINK_TO_EITHER_HOLDING_OR_LOCATION_ERROR.toError(), errorList.get(0));
  }

  @Test
  void testShouldReturnErrorWhenPiecePhysicalAndLineIsElectronic() {
    Piece piece = new Piece().withLocationId(UUID.randomUUID().toString())
      .withFormat(Piece.Format.PHYSICAL);
    PoLine poLine = new PoLine().withOrderFormat(ELECTRONIC_RESOURCE);
    List<Error> errorList = PieceValidatorUtil.validatePieceFormat(piece, poLine);
    assertEquals(PIECE_FORMAT_IS_NOT_VALID_ERROR.getCode(), errorList.get(0).getCode());
  }

  @Test
  void testShouldReturnErrorWhenPieceElectronicAndLineIsPhysical() {
    Piece piece = new Piece().withLocationId(UUID.randomUUID().toString())
      .withFormat(Piece.Format.ELECTRONIC);
    PoLine poLine = new PoLine().withOrderFormat(PHYSICAL_RESOURCE);
    List<Error> errorList = PieceValidatorUtil.validatePieceFormat(piece, poLine);
    assertEquals(PIECE_FORMAT_IS_NOT_VALID_ERROR.getCode(), errorList.get(0).getCode());
  }

  @Test
  void testShouldReturnErrorWhenPieceOtherAndLineIsPhysical() {
    Piece piece = new Piece().withLocationId(UUID.randomUUID().toString())
      .withFormat(Piece.Format.OTHER);
    PoLine poLine = new PoLine().withOrderFormat(PHYSICAL_RESOURCE);
    List<Error> errorList = PieceValidatorUtil.validatePieceFormat(piece, poLine);
    assertEquals(PIECE_FORMAT_IS_NOT_VALID_ERROR.getCode(), errorList.get(0).getCode());
  }

  @Test
  void testPieceIsValidWhenLineAndPieceIsPhysical() {
    Piece piece = new Piece().withLocationId(UUID.randomUUID().toString())
      .withFormat(Piece.Format.PHYSICAL);
    PoLine poLine = new PoLine().withOrderFormat(PHYSICAL_RESOURCE);
    List<Error> errorList = PieceValidatorUtil.validatePieceFormat(piece, poLine);
    assertEquals(0, errorList.size());
  }

  @Test
  void testPieceIsValidWhenLineAndPieceIsElectronic() {
    Piece piece = new Piece().withLocationId(UUID.randomUUID().toString())
      .withFormat(Piece.Format.ELECTRONIC);
    PoLine poLine = new PoLine().withOrderFormat(ELECTRONIC_RESOURCE);
    List<Error> errorList = PieceValidatorUtil.validatePieceFormat(piece, poLine);
    assertEquals(0, errorList.size());
  }

  @Test
  void testPieceIsValidWhenLineIsMixedAndPieceIsElectronic() {
    Piece piece = new Piece().withLocationId(UUID.randomUUID().toString())
      .withFormat(Piece.Format.ELECTRONIC);
    PoLine poLine = new PoLine().withOrderFormat(P_E_MIX);
    List<Error> errorList = PieceValidatorUtil.validatePieceFormat(piece, poLine);
    assertEquals(0, errorList.size());
  }

  @Test
  void testPieceIsValidWhenLineIsMixedAndPieceIsPhysical() {
    Piece piece = new Piece().withLocationId(UUID.randomUUID().toString())
      .withFormat(Piece.Format.PHYSICAL);
    PoLine poLine = new PoLine().withOrderFormat(P_E_MIX);
    List<Error> errorList = PieceValidatorUtil.validatePieceFormat(piece, poLine);
    assertEquals(0, errorList.size());
  }

  @Test
  void testValidateRelatedOrderWhenOrderIsNull() {
    PoLine poLine = new PoLine();
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
    PoLine poLine = new PoLine().withCheckinItems(false);
    List<Error> errorList = PieceValidatorUtil.validatePieceRelatedOrder(order, poLine);
    assertEquals(ErrorCodes.PIECE_RELATED_ORDER_DATA_IS_NOT_VALID.getCode(), errorList.get(0).getCode());
  }

  @Test
  void testValidateRelatedOrderWhenOrderIsPendingAndCheckinItemsIsTrue() {
    CompositePurchaseOrder order = new CompositePurchaseOrder().withWorkflowStatus(WorkflowStatus.PENDING);
    PoLine poLine = new PoLine().withCheckinItems(true);
    List<Error> errorList = PieceValidatorUtil.validatePieceRelatedOrder(order, poLine);
    assertEquals(0, errorList.size());
  }

  @Test
  void testValidateRelatedWhenOrderIsNotPending() {
    CompositePurchaseOrder order = new CompositePurchaseOrder().withWorkflowStatus(WorkflowStatus.OPEN);
    PoLine poLine = new PoLine().withCheckinItems(false);
    List<Error> errorList = PieceValidatorUtil.validatePieceRelatedOrder(order, poLine);
    assertEquals(0, errorList.size());
  }

    @Test
  void testIsLocationRequiredWhenElectronicAndCreateInventoryIsHoldingItemShouldReturnTrue() {
    // TestMate-3ee5c4b05b44b0478d995b251d624b1f
    // Given
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING_ITEM);
    PoLine poLine = new PoLine().withEresource(eresource);
    // When
    boolean isLocationRequired = PieceValidatorUtil.isLocationRequired(Piece.Format.ELECTRONIC, poLine);
    // Then
    assertTrue(isLocationRequired);
  }

    @Test
  void testIsLocationRequiredWhenElectronicAndCreateInventoryIsHoldingShouldReturnTrue() {
    // TestMate-5e776d521c90447c4670b8acd09d8c63
    // Given
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING);
    PoLine poLine = new PoLine().withEresource(eresource);
    // When
    boolean isLocationRequired = PieceValidatorUtil.isLocationRequired(Piece.Format.ELECTRONIC, poLine);
    // Then
    assertTrue(isLocationRequired);
  }

    @Test
  void testIsLocationRequiredWhenElectronicAndCreateInventoryIsInstanceShouldReturnFalse() {
    // TestMate-72de1f63343d930e731bec2a686e87bd
    // Given
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE);
    PoLine poLine = new PoLine().withEresource(eresource);
    // When
    boolean isLocationRequired = PieceValidatorUtil.isLocationRequired(Piece.Format.ELECTRONIC, poLine);
    // Then
    assertFalse(isLocationRequired);
  }

    @Test
  void testIsLocationRequiredWhenElectronicAndCreateInventoryIsNoneShouldReturnFalse() {
    // TestMate-1f4a25597574012801e7916f512f8ed4
    // Given
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.NONE);
    PoLine poLine = new PoLine().withEresource(eresource);
    // When
    boolean isLocationRequired = PieceValidatorUtil.isLocationRequired(Piece.Format.ELECTRONIC, poLine);
    // Then
    assertFalse(isLocationRequired);
  }

    @Test
  void testIsLocationRequiredWhenPhysicalAndCreateInventoryIsHoldingItemShouldReturnTrue() {
    // TestMate-8469d3eb0e4c41abc1b1393f67938efe
    // Given
    Physical physical = new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM);
    PoLine poLine = new PoLine().withPhysical(physical);
    // When
    boolean isLocationRequired = PieceValidatorUtil.isLocationRequired(Piece.Format.PHYSICAL, poLine);
    // Then
    assertTrue(isLocationRequired);
  }

    @Test
  void testIsLocationRequiredWhenPhysicalAndCreateInventoryIsHoldingShouldReturnTrue() {
    // TestMate-8e84f41eeb08e8b5edb239aefae293b3
    // Given
    Physical physical = new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING);
    PoLine poLine = new PoLine().withPhysical(physical);
    // When
    boolean isLocationRequired = PieceValidatorUtil.isLocationRequired(Piece.Format.PHYSICAL, poLine);
    // Then
    assertTrue(isLocationRequired);
  }

    @Test
  void testIsLocationRequiredWhenPhysicalAndCreateInventoryIsInstanceShouldReturnFalse() {
    // TestMate-d3d97ce03f510b5155e55b6d2cc73453
    // Given
    Physical physical = new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE);
    PoLine poLine = new PoLine().withPhysical(physical);
    // When
    boolean isLocationRequired = PieceValidatorUtil.isLocationRequired(Piece.Format.PHYSICAL, poLine);
    // Then
    assertFalse(isLocationRequired);
  }

    @Test
  void testIsLocationRequiredWhenPhysicalAndCreateInventoryIsNoneShouldReturnFalse() {
    // TestMate-06face1ff76938ed8fa883f7f5f645c3
    // Given
    Physical physical = new Physical().withCreateInventory(Physical.CreateInventory.NONE);
    PoLine poLine = new PoLine().withPhysical(physical);
    // When
    boolean isLocationRequired = PieceValidatorUtil.isLocationRequired(Piece.Format.PHYSICAL, poLine);
    // Then
    assertFalse(isLocationRequired);
  }
}
