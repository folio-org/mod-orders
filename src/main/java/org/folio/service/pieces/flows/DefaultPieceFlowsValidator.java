package org.folio.service.pieces.flows;

import static org.folio.rest.core.exceptions.ErrorCodes.CREATE_ITEM_FOR_PIECE_IS_NOT_ALLOWED_ERROR;
import static org.folio.rest.core.exceptions.ErrorCodes.PIECE_DISPLAY_ON_HOLDINGS_IS_NOT_CONSISTENT;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.RestConstants;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.pieces.validators.PieceValidatorUtil;

import io.vertx.core.json.JsonObject;

public class DefaultPieceFlowsValidator {
  private static final Logger logger = LogManager.getLogger(DefaultPieceFlowsValidator.class);

  public void isPieceRequestValid(Piece pieceToCreate, CompositePurchaseOrder originalOrder, CompositePoLine originPoLine, boolean isCreateItem) {
    List<Error> isItemCreateValidError = validateItemCreateFlag(pieceToCreate, originPoLine, isCreateItem);
    List<Error> combinedErrors = new ArrayList<>(isItemCreateValidError);
    List<Error> pieceLocationErrors = Optional.ofNullable(PieceValidatorUtil.validatePieceLocation(pieceToCreate, originPoLine)).orElse(new ArrayList<>());
    combinedErrors.addAll(pieceLocationErrors);
    List<Error> pieceFormatErrors = Optional.ofNullable(PieceValidatorUtil.validatePieceFormat(pieceToCreate, originPoLine)).orElse(new ArrayList<>());
    combinedErrors.addAll(pieceFormatErrors);
    List<Error> displayOnHoldingsErrors = validateDisplayOnHoldingsConsistency(pieceToCreate);
    combinedErrors.addAll(displayOnHoldingsErrors);
    List<Error> relatedOrderErrors = PieceValidatorUtil.validatePieceRelatedOrder(originalOrder, originPoLine);
    combinedErrors.addAll(relatedOrderErrors);
    if (CollectionUtils.isNotEmpty(combinedErrors)) {
      Errors errors = new Errors().withErrors(combinedErrors).withTotalRecords(combinedErrors.size());
      logger.error("Validation error : " + JsonObject.mapFrom(errors).encodePrettily());
      throw new HttpException(RestConstants.BAD_REQUEST, errors);
    }
  }

  public static List<Error> validateItemCreateFlag(Piece pieceToCreate, CompositePoLine originPoLine, boolean createItem) {
    if (createItem && !isCreateItemForPiecePossible(pieceToCreate, originPoLine)) {
      String msg = String.format(CREATE_ITEM_FOR_PIECE_IS_NOT_ALLOWED_ERROR.getDescription(), pieceToCreate.getFormat(), originPoLine.getId());
      return List.of(new Error().withCode(CREATE_ITEM_FOR_PIECE_IS_NOT_ALLOWED_ERROR.getCode()).withMessage(msg));
    }
    return Collections.emptyList();
  }

  public static List<Error> validateDisplayOnHoldingsConsistency(Piece piece) {
    if (Boolean.FALSE.equals(piece.getDisplayOnHolding()) && Boolean.TRUE.equals(piece.getDisplayToPublic())) {
      return List.of(PIECE_DISPLAY_ON_HOLDINGS_IS_NOT_CONSISTENT.toError());
    }
    return Collections.emptyList();
  }

  public static boolean isCreateHoldingForPiecePossible(Piece pieceToCreate, CompositePoLine originPoLine) {
    Piece.Format pieceFormat = pieceToCreate.getFormat();
    return (pieceFormat == Piece.Format.ELECTRONIC && PoLineCommonUtil.isHoldingUpdateRequiredForEresource(originPoLine)) ||
              ((pieceFormat == Piece.Format.PHYSICAL || pieceFormat == Piece.Format.OTHER)
                        && PoLineCommonUtil.isHoldingUpdateRequiredForPhysical(originPoLine));
  }

  public static boolean isCreateItemForPiecePossible(Piece pieceToCreate, CompositePoLine originPoLine) {
    return isCreateItemForElectronicPiecePossible(pieceToCreate, originPoLine) ||
                  isCreateItemForNonElectronicPiecePossible(pieceToCreate, originPoLine);
  }

  public static boolean isCreateItemForElectronicPiecePossible(Piece pieceToCreate, CompositePoLine originPoLine) {
    Piece.Format pieceFormat = pieceToCreate.getFormat();
    return (pieceFormat == Piece.Format.ELECTRONIC && isItemsUpdateRequiredForEresource(originPoLine));
  }


  public static boolean isCreateItemForNonElectronicPiecePossible(Piece pieceToCreate, CompositePoLine originPoLine) {
    Piece.Format pieceFormat = pieceToCreate.getFormat();
    return (pieceFormat == Piece.Format.PHYSICAL || pieceFormat == Piece.Format.OTHER)
                            && isItemsUpdateRequiredForPhysical(originPoLine);
  }

  public static boolean isItemsUpdateRequiredForEresource(CompositePoLine compPOL) {
    return Optional.ofNullable(compPOL.getEresource())
      .map(eresource -> eresource.getCreateInventory() == Eresource.CreateInventory.INSTANCE_HOLDING_ITEM)
      .orElse(false);
  }

  public static boolean isItemsUpdateRequiredForPhysical(CompositePoLine compPOL) {
    return Optional.ofNullable(compPOL.getPhysical())
      .map(physical -> physical.getCreateInventory() == Physical.CreateInventory.INSTANCE_HOLDING_ITEM)
      .orElse(false);
  }
}
