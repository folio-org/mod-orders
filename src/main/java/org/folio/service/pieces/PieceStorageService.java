package org.folio.service.pieces;

import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PieceCollection;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;

import static org.folio.orders.utils.ResourcePathResolver.PIECES_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.service.inventory.InventoryManager.SEARCH_PARAMS_WITHOUT_LANG;

public class PieceStorageService {
  private static final String PIECES_BY_POL_ID_AND_STATUS_QUERY = "poLineId==%s and receivingStatus==%s";
  private static final String GET_PIECES_BY_QUERY = resourcesPath(PIECES_STORAGE) + SEARCH_PARAMS_WITHOUT_LANG;
  private static final String PIECE_STORAGE_ENDPOINT = resourcesPath(PIECES_STORAGE);
  private static final String PIECE_STORAGE_BY_ID_ENDPOINT = PIECE_STORAGE_ENDPOINT + "/{id}";

  private final RestClient restClient;

  public PieceStorageService(RestClient restClient) {
    this.restClient = restClient;
  }

  /**
   * Search for pieces which might be already created for the PO line
   * @param compPOL PO line to retrieve Piece Records for
   * @return future with list of Pieces
   */
  public CompletableFuture<List<Piece>> getPiecesByPoLineId(CompositePoLine compPOL, RequestContext requestContext) {
    String query = String.format("poLineId==%s", compPOL.getId());
    RequestEntry requestEntry = new RequestEntry(resourcesPath(PIECES_STORAGE)).withQuery(query)
      .withLimit(Integer.MAX_VALUE)
      .withOffset(0);

    return restClient.get(requestEntry, requestContext, PieceCollection.class)
      .thenApply(PieceCollection::getPieces);
  }

  public CompletableFuture<Piece> getPieceById(String pieceId, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(PIECE_STORAGE_BY_ID_ENDPOINT).withId(pieceId);
    return restClient.get(requestEntry, requestContext, Piece.class);
  }

  public CompletableFuture<Void> updatePiece(Piece piece, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(PIECE_STORAGE_BY_ID_ENDPOINT).withId(piece.getId());
    return restClient.put(requestEntry, piece, requestContext);
  }

  public CompletionStage<Piece> insertPiece(Piece piece, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(PIECE_STORAGE_ENDPOINT);
    return restClient.post(requestEntry, piece, requestContext, Piece.class);
  }

  public CompletableFuture<Void> deletePiece(String pieceId, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(PIECE_STORAGE_BY_ID_ENDPOINT).withId(pieceId);
    return restClient.delete(requestEntry, requestContext);
  }

  public CompletableFuture<PieceCollection> getExpectedPiecesByLineId(String poLineId, RequestContext requestContext) {
    String query = String.format(PIECES_BY_POL_ID_AND_STATUS_QUERY, poLineId, Piece.ReceivingStatus.EXPECTED.value());
    return getPieces(Integer.MAX_VALUE, 0, query, requestContext);
  }

  public CompletableFuture<PieceCollection> getPieces(int limit, int offset, String query, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(PIECE_STORAGE_ENDPOINT).withQuery(query)
      .withOffset(offset)
      .withLimit(limit);
    return restClient.get(requestEntry, requestContext, PieceCollection.class);
  }
}
