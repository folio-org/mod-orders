package org.folio.orders.utils.validators;

import static org.folio.rest.RestConstants.VALIDATION_ERROR;
import static org.folio.rest.core.exceptions.ErrorCodes.PIECES_TO_BE_DELETED;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.List;
import java.util.UUID;

import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PieceCollection;
import org.junit.jupiter.api.Test;

public class LocationsAndPiecesConsistencyValidatorTest {
  @Test
  public void testVerifyLocationsAndPiecesConsistencyWhenTwoLocationWithHoldingAndPiecesWithHolding(){
    String holdingId1 = UUID.randomUUID().toString();
    String holdingId2 = UUID.randomUUID().toString();
    Location location1 = new Location().withHoldingId(holdingId1).withQuantity(1).withQuantityPhysical(1);
    Location location2 = new Location().withHoldingId(holdingId2).withQuantity(1).withQuantityPhysical(1);
    String poLineId = UUID.randomUUID().toString();
    CompositePoLine poLine = new CompositePoLine().withId(poLineId).withLocations(List.of(location1, location2));
    List<CompositePoLine> poLines = List.of(poLine);
    Piece piece1 = new Piece().withPoLineId(poLineId).withHoldingId(holdingId1).withReceivingStatus(Piece.ReceivingStatus.EXPECTED);
    Piece piece2 = new Piece().withPoLineId(poLineId).withHoldingId(holdingId2).withReceivingStatus(Piece.ReceivingStatus.EXPECTED);
    PieceCollection pieces = new PieceCollection().withPieces(List.of(piece1, piece2)).withTotalRecords(2);
    //Expect
    LocationsAndPiecesConsistencyValidator.verifyLocationsAndPiecesConsistency(poLines, pieces);
  }

  @Test
  public void testVerifyLocationsAndPiecesConsistencyWhenTwoLocationWithHoldingAndPiecesWithHoldingIdAndLocationId(){
    String holdingId1 = UUID.randomUUID().toString();
    String holdingId2 = UUID.randomUUID().toString();
    Location location1 = new Location().withHoldingId(holdingId1).withQuantity(1).withQuantityPhysical(1);
    Location location2 = new Location().withHoldingId(holdingId2).withQuantity(1).withQuantityPhysical(1);
    String poLineId = UUID.randomUUID().toString();
    CompositePoLine poLine = new CompositePoLine().withId(poLineId).withLocations(List.of(location1, location2));
    List<CompositePoLine> poLines = List.of(poLine);
    Piece piece1 = new Piece().withPoLineId(poLineId).withLocationId( UUID.randomUUID().toString())
      .withReceivingStatus(Piece.ReceivingStatus.EXPECTED);
    Piece piece2 = new Piece().withPoLineId(poLineId).withHoldingId(holdingId2).withReceivingStatus(Piece.ReceivingStatus.EXPECTED);
    PieceCollection pieces = new PieceCollection().withPieces(List.of(piece1, piece2)).withTotalRecords(2);
    //Expect
    HttpException actHttpException = assertThrows(
      HttpException.class,
      () ->  LocationsAndPiecesConsistencyValidator.verifyLocationsAndPiecesConsistency(poLines, pieces),      "Expected exception"
    );
    Error actError = actHttpException.getError();
    assertEquals(PIECES_TO_BE_DELETED.getCode(), actError.getCode());
    assertEquals(VALIDATION_ERROR, actHttpException.getCode());
  }

  @Test
  public void testVerifyLocationsAndPiecesConsistencyWhenTwoLocationWithHoldingAndPiecesWithHoldingIdDifferenceWithPol(){
    String holdingId1 = UUID.randomUUID().toString();
    String holdingId2 = UUID.randomUUID().toString();
    Location location1 = new Location().withHoldingId(holdingId1).withQuantity(1).withQuantityPhysical(1);
    Location location2 = new Location().withHoldingId(holdingId2).withQuantity(1).withQuantityPhysical(1);
    String poLineId = UUID.randomUUID().toString();
    CompositePoLine poLine = new CompositePoLine().withId(poLineId).withLocations(List.of(location1, location2));
    List<CompositePoLine> poLines = List.of(poLine);
    Piece piece1 = new Piece().withPoLineId(poLineId).withHoldingId(UUID.randomUUID().toString())
      .withReceivingStatus(Piece.ReceivingStatus.EXPECTED);
    Piece piece2 = new Piece().withPoLineId(poLineId).withHoldingId(holdingId2).withReceivingStatus(Piece.ReceivingStatus.EXPECTED);
    PieceCollection pieces = new PieceCollection().withPieces(List.of(piece1, piece2)).withTotalRecords(2);
    //Expect
    HttpException actHttpException = assertThrows(
      HttpException.class,
      () ->  LocationsAndPiecesConsistencyValidator.verifyLocationsAndPiecesConsistency(poLines, pieces),      "Expected exception"
    );
    Error actError = actHttpException.getError();
    assertEquals(PIECES_TO_BE_DELETED.getCode(), actError.getCode());
    assertEquals(VALIDATION_ERROR, actHttpException.getCode());
  }
}
