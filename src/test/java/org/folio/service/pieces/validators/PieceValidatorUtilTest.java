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
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import static org.junit.jupiter.api.Assertions.assertFalse;
import org.junit.jupiter.params.provider.CsvSource;

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

    @ParameterizedTest
  @EnumSource(value = Physical.CreateInventory.class, names = {"INSTANCE_HOLDING", "INSTANCE_HOLDING_ITEM"})
  void testIsLocationRequiredWhenPhysicalPieceAndPhysicalCreateInventoryIsHoldingOrItem(Physical.CreateInventory inventoryType) {
    // TestMate-ed82770e6d633b49d5db3d33db07ceee
    //given
    Physical physical = new Physical().withCreateInventory(inventoryType);
    PoLine poLine = new PoLine().withPhysical(physical);
    Piece.Format pieceFormat = Piece.Format.PHYSICAL;
    //When
    boolean isLocationRequired = PieceValidatorUtil.isLocationRequired(pieceFormat, poLine);
    //Then
    assertTrue(isLocationRequired);
  }

    @ParameterizedTest
  @CsvSource({
    "ELECTRONIC, true, false",
    "PHYSICAL, false, true",
    "OTHER, false, true"
  })
  void testIsLocationRequiredWhenResourceDetailsAreMissing(Piece.Format pieceFormat, boolean withPhysical, boolean withEresource) {
    // TestMate-693d44ada71c2d071ee90a6521d9dc04
    // Given
    PoLine poLine = new PoLine();
    if (withPhysical) {
      poLine.setPhysical(new Physical());
    }
    if (withEresource) {
      poLine.setEresource(new Eresource());
    }
    // When
    boolean isLocationRequired = PieceValidatorUtil.isLocationRequired(pieceFormat, poLine);
    // Then
    assertFalse(isLocationRequired);
  }

    @ParameterizedTest
  @CsvSource({
    "INSTANCE_HOLDING, true",
    "INSTANCE_HOLDING_ITEM, true",
    "NONE, false",
    "INSTANCE, false"
  })
  void testIsLocationRequiredWhenPieceFormatIsOther(Physical.CreateInventory inventoryType, boolean expectedResult) {
    // TestMate-dfb94abd5db2d1a039b299cb78ea44d1
    // Given
    Physical physical = new Physical().withCreateInventory(inventoryType);
    PoLine poLine = new PoLine().withPhysical(physical);
    Piece.Format pieceFormat = Piece.Format.OTHER;
    // When
    boolean actualResult = PieceValidatorUtil.isLocationRequired(pieceFormat, poLine);
    // Then
    assertEquals(expectedResult, actualResult);
  }
}
