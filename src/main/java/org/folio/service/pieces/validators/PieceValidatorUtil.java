package org.folio.service.pieces.validators;

import static java.util.stream.Collectors.toList;

import java.util.ArrayList;
import java.util.List;

import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Piece;

public class PieceValidatorUtil {

  public static List<Error> validatePieceLocation(Piece piece) {
    List<ErrorCodes> errors = new ArrayList<>();
    if (piece.getHoldingId() == null && piece.getLocationId() == null) {
     errors.add(ErrorCodes.HOLDINGS_ID_AND_LOCATION_ID_IS_NULL_ERROR);
    } else if (piece.getHoldingId() != null && piece.getLocationId() != null) {
     errors.add(ErrorCodes.MAY_BE_LINK_TO_EITHER_HOLDING_OR_LOCATION_ERROR);
    }
    return errors.stream().map(ErrorCodes::toError).collect(toList());
  }
}
