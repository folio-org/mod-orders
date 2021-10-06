package org.folio.service.pieces.flows.create;

import static org.folio.rest.core.exceptions.ErrorCodes.CREATE_ITEM_FOR_PIECE_IS_NOT_ALLOWED_ERROR;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.pieces.PieceCreationHolder;
import org.folio.rest.RestConstants;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.pieces.validators.PieceValidatorUtil;

import io.vertx.core.json.JsonObject;

public class PieceCreateFlowValidator {
  private static final Logger logger = LogManager.getLogger(PieceCreateFlowValidator.class);

  public void isCreatePieceRequestValid(PieceCreationHolder holder) {
    Piece pieceToCreate = holder.getPieceToCreate();
    CompositePoLine originPoLine = holder.getOriginPoLine();
    List<Error> combinedErrors = new ArrayList<>();
    List<Error> isItemCreateValidError = validateItemCreateFlag(pieceToCreate, originPoLine, holder.isCreateItem());
    combinedErrors.addAll(isItemCreateValidError);
    List<Error> pieceLocationErrors = Optional.ofNullable(PieceValidatorUtil.validatePieceLocation(pieceToCreate)).orElse(new ArrayList<>());
    combinedErrors.addAll(pieceLocationErrors);
    List<Error> pieceFormatErrors = Optional.ofNullable(PieceValidatorUtil.validatePieceFormat(pieceToCreate, originPoLine)).orElse(new ArrayList<>());
    combinedErrors.addAll(pieceFormatErrors);
    if (CollectionUtils.isNotEmpty(combinedErrors)) {
      Errors errors = new Errors().withErrors(combinedErrors).withTotalRecords(combinedErrors.size());
      logger.error("Validation error : " + JsonObject.mapFrom(errors).encodePrettily());
      throw new HttpException(RestConstants.VALIDATION_ERROR, errors);
    }
  }

  private List<Error> validateItemCreateFlag(Piece pieceToCreate, CompositePoLine originPoLine, boolean createItem) {
    if (createItem && !isCreateItemForPiecePossible(pieceToCreate, originPoLine)) {
      String msg = String.format(CREATE_ITEM_FOR_PIECE_IS_NOT_ALLOWED_ERROR.getDescription(), pieceToCreate.getFormat(), originPoLine.getId());
      return List.of(new Error().withCode(CREATE_ITEM_FOR_PIECE_IS_NOT_ALLOWED_ERROR.getCode()).withMessage(msg));
    }
    return Collections.emptyList();
  }

  public static boolean isCreateItemForPiecePossible(Piece pieceToCreate, CompositePoLine originPoLine) {
    Piece.Format pieceFormat = pieceToCreate.getFormat();
    return (pieceFormat == Piece.Format.ELECTRONIC && isItemsUpdateRequiredForEresource(originPoLine)) ||
                  ((pieceFormat == Piece.Format.PHYSICAL || pieceFormat == Piece.Format.OTHER)
                                && isItemsUpdateRequiredForPhysical(originPoLine));
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
