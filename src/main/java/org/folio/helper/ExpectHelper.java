package org.folio.helper;

import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import one.util.streamex.StreamEx;
import org.apache.commons.lang3.StringUtils;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.ExpectCollection;
import org.folio.rest.jaxrs.model.ExpectPiece;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.ProcessingStatus;
import org.folio.rest.jaxrs.model.ReceivingResult;
import org.folio.rest.jaxrs.model.ReceivingResults;
import org.folio.rest.jaxrs.model.ToBeExpected;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static java.util.stream.Collectors.collectingAndThen;
import static java.util.stream.Collectors.mapping;
import static java.util.stream.Collectors.toList;

public class ExpectHelper extends CheckinReceivePiecesHelper<ExpectPiece> {

  /**
   * Map with PO line id as a key and value is map with piece id as a key and
   * {@link ExpectPiece} as a value
   */
  private final Map<String, Map<String, ExpectPiece>> expectPieces;

  public ExpectHelper(ExpectCollection expectCollection, Map<String, String> okapiHeaders, Context ctx) {
    super(okapiHeaders, ctx);
    // Convert request to map representation
    expectPieces = groupExpectPieceByPoLineId(expectCollection);

    // Logging quantity of the piece records to be expected
    if (logger.isDebugEnabled()) {
      int poLinesQty = expectPieces.size();
      int piecesQty = StreamEx.ofValues(expectPieces)
        .mapToInt(Map::size)
        .sum();
      logger.debug("{} piece record(s) are going to be expected for {} PO line(s)", piecesQty, poLinesQty);
    }
  }

  public Future<ReceivingResults> expectPieces(ExpectCollection expectCollection, RequestContext requestContext) {
    return removeForbiddenEntities(expectPieces, requestContext)
      .compose(vVoid -> processExpectPieces(expectCollection, requestContext));
  }

  private Future<ReceivingResults> processExpectPieces(ExpectCollection expectCollection, RequestContext requestContext) {
    // 1. Get piece records from storage
    return retrievePieceRecords(expectPieces, requestContext)
      // 2. Update piece status to Expected
      .map(this::updatePieceRecords)
      // 3. Update received piece records in the storage
      .compose(piecesGroupedByPoLine -> storeUpdatedPieceRecords(piecesGroupedByPoLine, requestContext))
      // 4. Return results to the client
      .map(piecesGroupedByPoLine -> prepareResponseBody(expectCollection, piecesGroupedByPoLine));
  }

  private Map<String, Map<String, ExpectPiece>> groupExpectPieceByPoLineId(ExpectCollection expectCollection) {
    return StreamEx
      .of(expectCollection.getToBeExpected())
      .distinct()
      .groupingBy(ToBeExpected::getPoLineId,
        mapping(ToBeExpected::getExpectPieces,
          collectingAndThen(toList(),
            lists -> StreamEx.of(lists)
              .flatMap(List::stream)
              .toMap(ExpectPiece::getId, expectPiece -> expectPiece))));
  }

  private ReceivingResults prepareResponseBody(ExpectCollection expectCollection,
                                               Map<String, List<Piece>> piecesGroupedByPoLine) {
    ReceivingResults results = new ReceivingResults();
    results.setTotalRecords(expectCollection.getTotalRecords());
    for (ToBeExpected toBeExpected : expectCollection.getToBeExpected()) {
      String poLineId = toBeExpected.getPoLineId();
      ReceivingResult result = new ReceivingResult();
      results.getReceivingResults().add(result);

      // Get all processed piece records for PO Line
      Map<String, Piece> processedPiecesForPoLine = StreamEx
        .of(piecesGroupedByPoLine.getOrDefault(poLineId, Collections.emptyList()))
        .toMap(Piece::getId, piece -> piece);

      Map<String, Integer> resultCounts = new HashMap<>();
      resultCounts.put(ProcessingStatus.Type.SUCCESS.toString(), 0);
      resultCounts.put(ProcessingStatus.Type.FAILURE.toString(), 0);
      for (ExpectPiece expectPiece : toBeExpected.getExpectPieces()) {
        String pieceId = expectPiece.getId();

        calculateProcessingErrors(poLineId, result, processedPiecesForPoLine, resultCounts, pieceId);
      }

      result.withPoLineId(poLineId)
        .withProcessedSuccessfully(resultCounts.get(ProcessingStatus.Type.SUCCESS.toString()))
        .withProcessedWithError(resultCounts.get(ProcessingStatus.Type.FAILURE.toString()));
    }

    return results;
  }

  private Map<String, List<Piece>> updatePieceRecords(Map<String, List<Piece>> piecesGroupedByPoLine) {
    StreamEx.ofValues(piecesGroupedByPoLine)
      .flatMap(List::stream)
      .forEach(this::updatePieceWithExpectInfo);

    return piecesGroupedByPoLine;
  }

  private void updatePieceWithExpectInfo(Piece piece) {
    ExpectPiece expectPiece = piecesByLineId.get(piece.getPoLineId())
      .get(piece.getId());

    piece.setComment(expectPiece.getComment());
    piece.setReceivedDate(null);
    piece.setReceivingStatus(Piece.ReceivingStatus.EXPECTED);
  }

  @Override
  protected boolean isRevertToOnOrder(Piece piece) {
    return false;
  }

  @Override
  protected Future<Boolean> receiveInventoryItemAndUpdatePiece(JsonObject item, Piece piece, RequestContext requestContext) {
    return Future.succeededFuture(false);
  }

  @Override
  protected Map<String, List<Piece>> updatePieceRecordsWithoutItems(Map<String, List<Piece>> piecesGroupedByPoLine) {
    return Collections.emptyMap();
  }

  @Override
  protected String getHoldingId(Piece piece) {
    return StringUtils.EMPTY;
  }

  @Override
  protected String getLocationId(Piece piece) {
    return StringUtils.EMPTY;
  }
}
