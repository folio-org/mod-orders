package org.folio.service.pieces.validators;

import static org.folio.rest.core.exceptions.ErrorCodes.HOLDINGS_ID_AND_LOCATION_ID_IS_NULL_ERROR;
import static org.folio.rest.core.exceptions.ErrorCodes.MAY_BE_LINK_TO_EITHER_HOLDING_OR_LOCATION_ERROR;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.List;
import java.util.UUID;

import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Piece;
import org.junit.jupiter.api.Test;

public class PieceValidatorUtilTest {

  @Test
  void testShouldReturnErrorsIfLocationAndHoldingIsNotProvided() {
    Piece piece = new Piece();
    List<Error> errorList = PieceValidatorUtil.validatePieceLocation(piece);
    assertEquals(HOLDINGS_ID_AND_LOCATION_ID_IS_NULL_ERROR.toError(), errorList.get(0));
  }

  @Test
  void testShouldReturnErrorsIfLocationAndHoldingProvidedAtOneTime() {
    Piece piece = new Piece().withLocationId(UUID.randomUUID().toString())
                             .withHoldingId(UUID.randomUUID().toString());
    List<Error> errorList = PieceValidatorUtil.validatePieceLocation(piece);
    assertEquals(MAY_BE_LINK_TO_EITHER_HOLDING_OR_LOCATION_ERROR.toError(), errorList.get(0));
  }

}
