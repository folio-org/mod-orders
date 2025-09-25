package org.folio.service.pieces.flows;

import static org.folio.rest.core.exceptions.ErrorCodes.ALL_PIECES_MUST_HAVE_THE_SAME_POLINE_ID_AND_TITLE_ID;
import static org.folio.rest.core.exceptions.ErrorCodes.CREATE_ITEM_FOR_PIECE_IS_NOT_ALLOWED_ERROR;
import static org.folio.rest.core.exceptions.ErrorCodes.PIECE_DISPLAY_ON_HOLDINGS_IS_NOT_CONSISTENT;
import static org.folio.rest.core.exceptions.ErrorCodes.PIECE_SEQUENCE_NUMBER_IS_INVALID;

import java.util.Collection;
import java.util.List;
import java.util.Optional;

import java.util.stream.Stream;

import lombok.extern.log4j.Log4j2;
import one.util.streamex.StreamEx;
import org.apache.commons.collections4.CollectionUtils;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.RestConstants;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.Title;
import org.folio.service.pieces.validators.PieceValidatorUtil;

import io.vertx.core.json.JsonObject;

@Log4j2
public class DefaultPieceFlowsValidator {

  public void isPieceRequestValid(Piece pieceToCreate, CompositePurchaseOrder originalOrder, PoLine originPoLine, Title title, boolean isCreateItem) {
    isPieceRequestValid(pieceToCreate, originalOrder, originPoLine, title, 1, isCreateItem);
  }

  public void isPieceRequestValid(Piece pieceToCreate, CompositePurchaseOrder originalOrder, PoLine originPoLine, Title title, int piecesToCreate, boolean isCreateItem) {
    List<Error> combinedErrors = Stream.of(
        PieceValidatorUtil.validatePieceLocation(pieceToCreate, originPoLine),
        PieceValidatorUtil.validatePieceFormat(pieceToCreate, originPoLine),
        PieceValidatorUtil.validatePieceRelatedOrder(originalOrder, originPoLine),
        validateItemCreateFlag(pieceToCreate, originPoLine, isCreateItem),
        validateDisplayOnHoldingsConsistency(pieceToCreate),
        validatePieceSequenceNumber(pieceToCreate, title, piecesToCreate))
      .flatMap(Collection::stream)
      .toList();
    if (CollectionUtils.isNotEmpty(combinedErrors)) {
      Errors errors = new Errors().withErrors(combinedErrors).withTotalRecords(combinedErrors.size());
      if (log.isErrorEnabled()) log.error("Validation error: {}", JsonObject.mapFrom(errors).encodePrettily());
      throw new HttpException(RestConstants.BAD_REQUEST, errors);
    }
  }

  public void isPieceBatchRequestValid(List<Piece> piecesToCreate, CompositePurchaseOrder originalOrder, PoLine originPoLine, Title title, boolean isCreateItem) {
    var titlePoLineIds = StreamEx.of(piecesToCreate).groupingBy(piece -> piece.getTitleId() + ":" + piece.getPoLineId());
    if (titlePoLineIds.size() > 1) {
      var param = new Parameter().withKey("titlePoLineIds").withValue(titlePoLineIds.keySet().toString());
      var error = ALL_PIECES_MUST_HAVE_THE_SAME_POLINE_ID_AND_TITLE_ID.toError().withParameters(List.of(param));
      log.error("isPieceBatchRequestValid:: Validation Error {}", error.getMessage());
      throw new HttpException(RestConstants.VALIDATION_ERROR, ALL_PIECES_MUST_HAVE_THE_SAME_POLINE_ID_AND_TITLE_ID);
    }
    piecesToCreate.forEach(piece -> isPieceRequestValid(piece, originalOrder, originPoLine, title, piecesToCreate.size(), isCreateItem));
  }

  public static List<Error> validateItemCreateFlag(Piece pieceToCreate, PoLine originPoLine, boolean createItem) {
    return createItem && !isCreateItemForPiecePossible(pieceToCreate, originPoLine)
      ? List.of(new Error().withCode(CREATE_ITEM_FOR_PIECE_IS_NOT_ALLOWED_ERROR.getCode())
        .withMessage(CREATE_ITEM_FOR_PIECE_IS_NOT_ALLOWED_ERROR.getDescription().formatted(pieceToCreate.getFormat(), originPoLine.getId())))
      : List.of();
  }

  public static List<Error> validateDisplayOnHoldingsConsistency(Piece piece) {
    return Boolean.FALSE.equals(piece.getDisplayOnHolding()) && Boolean.TRUE.equals(piece.getDisplayToPublic())
      ? List.of(PIECE_DISPLAY_ON_HOLDINGS_IS_NOT_CONSISTENT.toError())
      : List.of();
  }

  public static List<Error> validatePieceSequenceNumber(Piece piece, Title title, int piecesToCreate) {
    return piece.getSequenceNumber() != null && (piece.getSequenceNumber() <= 0 || piece.getSequenceNumber() >= title.getNextSequenceNumber() + piecesToCreate)
      ? List.of(PIECE_SEQUENCE_NUMBER_IS_INVALID.toError())
      : List.of();
  }

  public static boolean isCreateHoldingForPiecePossible(Piece pieceToCreate, PoLine originPoLine) {
    return isPieceFormatElectronic(pieceToCreate) && PoLineCommonUtil.isHoldingUpdateRequiredForEresource(originPoLine)
        || isPieceFormatNonElectronic(pieceToCreate) && PoLineCommonUtil.isHoldingUpdateRequiredForPhysical(originPoLine);
  }

  public static boolean isCreateItemForPiecePossible(Piece pieceToCreate, PoLine originPoLine) {
    return isCreateItemForElectronicPiecePossible(pieceToCreate, originPoLine)
        || isCreateItemForNonElectronicPiecePossible(pieceToCreate, originPoLine);
  }

  public static boolean isCreateItemForElectronicPiecePossible(Piece pieceToCreate, PoLine originPoLine) {
    return isPieceFormatElectronic(pieceToCreate) && isItemsUpdateRequiredForEresource(originPoLine);
  }

  public static boolean isCreateItemForNonElectronicPiecePossible(Piece pieceToCreate, PoLine originPoLine) {
    return isPieceFormatNonElectronic(pieceToCreate) && isItemsUpdateRequiredForPhysical(originPoLine);
  }

  public static boolean isItemsUpdateRequiredForEresource(PoLine poLine) {
    return Optional.ofNullable(poLine.getEresource())
      .map(eresource -> eresource.getCreateInventory() == Eresource.CreateInventory.INSTANCE_HOLDING_ITEM)
      .orElse(false);
  }

  public static boolean isItemsUpdateRequiredForPhysical(PoLine poLine) {
    return Optional.ofNullable(poLine.getPhysical())
      .map(physical -> physical.getCreateInventory() == Physical.CreateInventory.INSTANCE_HOLDING_ITEM)
      .orElse(false);
  }

  private static boolean isPieceFormatElectronic(Piece piece) {
    return piece.getFormat() == Piece.Format.ELECTRONIC;
  }

  private static boolean isPieceFormatNonElectronic(Piece piece) {
    return piece.getFormat() == Piece.Format.PHYSICAL || piece.getFormat() == Piece.Format.OTHER;
  }

}
