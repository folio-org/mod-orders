package org.folio.orders.utils;

import static org.folio.orders.utils.ErrorCodes.PIECES_TO_BE_DELETED;
import static org.folio.rest.RestConstants.NOT_FOUND;
import static org.folio.rest.RestConstants.VALIDATION_ERROR;
import static org.folio.service.exchange.ExchangeRateProviderResolver.RATE_KEY;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.isEmptyOrNullString;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletionException;

import javax.money.convert.ConversionQuery;

import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.validators.LocationsAndPiecesConsistencyValidator;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PieceCollection;
import org.folio.rest.jaxrs.model.PoLine;
import org.junit.jupiter.api.Test;

public class HelperUtilsTest {

  @Test
  public void testShouldReturnEmptyString(){
    String act = HelperUtils.combineCqlExpressions("");
    assertThat(act, isEmptyOrNullString());
  }

  @Test
  public void testShouldReturnConversionQueryWithRateKeyForGetConversionQueryWithExchangeRate(){
    ConversionQuery conversionQuery = HelperUtils.getConversionQuery(2.0d, "USD", "EUR");
    assertThat(conversionQuery.get(RATE_KEY, Double.class), is(2.0d));
    assertThat(conversionQuery.getBaseCurrency().getCurrencyCode(), is("USD"));
    assertThat(conversionQuery.getCurrency().getCurrencyCode(), is("EUR"));
  }

  @Test
  public void testShouldBuildQueryWithoutExchangeRate(){
    String systemCurrency = "USD";
    Cost costOneTime = new Cost().withListUnitPrice(595d).withQuantityPhysical(1).withCurrency("EUR").withPoLineEstimatedPrice(595d);
    PoLine poLineOneTime = new PoLine().withId(UUID.randomUUID().toString()).withPurchaseOrderId(UUID.randomUUID().toString()).withCost(costOneTime);
    ConversionQuery actQuery = HelperUtils.buildConversionQuery(poLineOneTime, systemCurrency);
    assertEquals(actQuery.getCurrency().getCurrencyCode(), systemCurrency);
    assertNull(actQuery.get(RATE_KEY, Double.class));
  }

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
