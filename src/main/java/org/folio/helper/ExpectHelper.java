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
import java.util.List;
import java.util.Map;

import static java.util.stream.Collectors.collectingAndThen;
import static java.util.stream.Collectors.mapping;
import static java.util.stream.Collectors.toList;

public class ExpectHelper extends CheckinReceivePiecesHelper<ExpectPiece> {

  public ExpectHelper(ExpectCollection expectCollection, Map<String, String> okapiHeaders, Context ctx) {
    super(okapiHeaders, ctx);
    // Convert request to map representation
    piecesByLineId = groupExpectPieceByPoLineId(expectCollection);

    // Logging quantity of the piece records to be expected
    if (logger.isDebugEnabled()) {
      int poLinesQty = piecesByLineId.size();
      int piecesQty = StreamEx.ofValues(piecesByLineId)
        .mapToInt(Map::size)
        .sum();
      logger.debug("{} piece record(s) are going to be expected for {} PO line(s)", piecesQty, poLinesQty);
    }
  }

  public Future<ReceivingResults> expectPieces(ExpectCollection expectCollection, RequestContext requestContext) {
    return removeForbiddenEntities(requestContext)
      .compose(vVoid -> processExpectPieces(expectCollection, requestContext));
  }

  private Future<ReceivingResults> processExpectPieces(ExpectCollection expectCollection, RequestContext requestContext) {
    // 1. Get piece records from storage
    return retrievePieceRecords(requestContext)
      // 2. Update piece status to Expected
      .map(this::updatePieceRecords)
      // 3. Update received piece records in the storage
      .compose(piecesGroupedByPoLine -> storeUpdatedPieceRecords(piecesGroupedByPoLine, requestContext))
      // 4. Update PO Line status
      .compose(piecesByPoLineIds -> updateOrderAndPoLinesStatus(piecesByPoLineIds, requestContext))
      // 5. Return results to the client
      .map(piecesGroupedByPoLine -> prepareResponseBody(expectCollection, piecesGroupedByPoLine));
  }

  private Future<Map<String, List<Piece>>> updateOrderAndPoLinesStatus(Map<String, List<Piece>> piecesGroupedByPoLine, RequestContext requestContext) {
    return updateOrderAndPoLinesStatus(
      piecesGroupedByPoLine,
      requestContext,
      poLines -> {}
    );
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
      Map<String, Piece> processedPiecesForPoLine = getProcessedPiecesForPoLine(poLineId, piecesGroupedByPoLine);

      Map<ProcessingStatus.Type, Integer> resultCounts = getEmptyResultCounts();
      for (ExpectPiece expectPiece : toBeExpected.getExpectPieces()) {
        String pieceId = expectPiece.getId();

        calculateProcessingErrors(poLineId, result, processedPiecesForPoLine, resultCounts, pieceId);
      }

      result.withPoLineId(poLineId)
        .withProcessedSuccessfully(resultCounts.get(ProcessingStatus.Type.SUCCESS))
        .withProcessedWithError(resultCounts.get(ProcessingStatus.Type.FAILURE));
    }

    return results;
  }

  private Map<String, List<Piece>> updatePieceRecords(Map<String, List<Piece>> piecesGroupedByPoLine) {
    extractAllPieces(piecesGroupedByPoLine)
      .forEach(this::updatePieceWithExpectInfo);

    return piecesGroupedByPoLine;
  }

  private void updatePieceWithExpectInfo(Piece piece) {
    ExpectPiece expectPiece = getByPiece(piece);

    piece.setComment(expectPiece.getComment());
    piece.setReceivedDate(null);
    piece.setReceivingStatus(Piece.ReceivingStatus.EXPECTED);
  }

  @Override
  protected boolean isRevertToOnOrder(Piece piece) {
    return false;
  }

  @Override
  protected Future<Boolean> receiveInventoryItemAndUpdatePiece(JsonObject item, Piece piece, RequestContext locationContext) {
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
