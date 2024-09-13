package org.folio.service.pieces;

import static one.util.streamex.StreamEx.ofSubLists;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.HelperUtils.convertIdsToCqlQuery;
import static org.folio.orders.utils.ResourcePathResolver.PIECES_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ_15;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PieceCollection;

import io.vertx.core.Future;
import org.folio.service.consortium.ConsortiumUserTenantsRetriever;
import org.folio.service.consortium.ConsortiumConfigurationService;

public class PieceStorageService {
  private static final Logger logger = LogManager.getLogger(PieceStorageService.class);

  private static final String PIECES_BY_POL_ID_AND_STATUS_QUERY = "poLineId==%s and receivingStatus==%s";
  private static final String PIECES_BY_HOLDING_ID_QUERY = "holdingId==%s";
  private static final String PIECE_STORAGE_ENDPOINT = resourcesPath(PIECES_STORAGE);
  private static final String PIECE_STORAGE_BY_ID_ENDPOINT = PIECE_STORAGE_ENDPOINT + "/{id}";

  private final ConsortiumConfigurationService consortiumConfigurationService;
  private final ConsortiumUserTenantsRetriever consortiumUserTenantsRetriever;
  private final RestClient restClient;

  public PieceStorageService(ConsortiumConfigurationService consortiumConfigurationService,
                             ConsortiumUserTenantsRetriever consortiumUserTenantsRetriever,
                             RestClient restClient) {
    this.consortiumConfigurationService = consortiumConfigurationService;
    this.consortiumUserTenantsRetriever = consortiumUserTenantsRetriever;
    this.restClient = restClient;
  }

  public Future<List<Piece>> getPiecesByPoLineId(CompositePoLine compPOL, RequestContext requestContext) {
    return getPiecesByLineId(compPOL.getId(), requestContext);
  }

  /**
   * Search for pieces which might be already created for the PO line
   * @param lineId PO line id to retrieve Piece Records for
   * @return future with list of Pieces
   */
  public Future<List<Piece>> getPiecesByLineId(String lineId, RequestContext requestContext) {
    String query = String.format("poLineId==%s", lineId);
    RequestEntry requestEntry = new RequestEntry(resourcesPath(PIECES_STORAGE)).withQuery(query)
      .withLimit(Integer.MAX_VALUE)
      .withOffset(0);

    return restClient.get(requestEntry, PieceCollection.class, requestContext)
      .map(PieceCollection::getPieces);
  }

  public Future<Piece> getPieceById(String pieceId, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(PIECE_STORAGE_BY_ID_ENDPOINT).withId(pieceId);
    return restClient.get(requestEntry, Piece.class, requestContext);
  }

  public Future<Void> updatePiece(Piece piece, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(PIECE_STORAGE_BY_ID_ENDPOINT).withId(piece.getId());
    return restClient.put(requestEntry, piece, requestContext);
  }

  public Future<Piece> insertPiece(Piece piece, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(PIECE_STORAGE_ENDPOINT);
    return restClient.post(requestEntry, piece, Piece.class, requestContext);
  }

  public Future<Void> deletePiece(String pieceId, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(PIECE_STORAGE_BY_ID_ENDPOINT).withId(pieceId);
    return restClient.delete(requestEntry, requestContext);
  }

  public Future<Void> deletePiece(String pieceId, boolean skipNotFoundException, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(PIECE_STORAGE_BY_ID_ENDPOINT).withId(pieceId);
    return restClient.delete(requestEntry, skipNotFoundException, requestContext);
  }

  public Future<Void> deletePiecesByIds(List<String> pieceIds, RequestContext requestContext) {
    List<Future<Void>> deletedItems = new ArrayList<>(pieceIds.size());
    pieceIds.forEach(pieceId -> deletedItems.add(deletePiece(pieceId, requestContext)));
    return collectResultsOnSuccess(deletedItems)
      .onSuccess(v -> {
        if (logger.isDebugEnabled()) {
          String deletedIds = String.join(",", pieceIds);
          logger.debug("Pieces were removed : {}", deletedIds);
        }
      })
      .mapEmpty();
  }

  public Future<PieceCollection> getExpectedPiecesByLineId(String poLineId, RequestContext requestContext) {
    String query = String.format(PIECES_BY_POL_ID_AND_STATUS_QUERY, poLineId, Piece.ReceivingStatus.EXPECTED.value());
    return getAllPieces(query, requestContext);
  }

  public Future<List<Piece>> getPiecesByHoldingId(String holdingId, RequestContext requestContext) {
    if (holdingId != null) {
      String query = String.format(PIECES_BY_HOLDING_ID_QUERY, holdingId);
      return getAllPieces(query, requestContext).map(PieceCollection::getPieces);
    }
    return Future.succeededFuture(Collections.emptyList());
  }

  public Future<PieceCollection> getAllPieces(String query, RequestContext requestContext) {
    var requestEntry = new RequestEntry(PIECE_STORAGE_ENDPOINT).withQuery(query).withOffset(0).withLimit(Integer.MAX_VALUE);
    return restClient.get(requestEntry, PieceCollection.class, requestContext);
  }

  public Future<PieceCollection> getPieces(int limit, int offset, String query, RequestContext requestContext) {
    var requestEntry = new RequestEntry(PIECE_STORAGE_ENDPOINT).withQuery(query).withOffset(offset).withLimit(limit);
    return restClient.get(requestEntry, PieceCollection.class, requestContext)
      .compose(piecesCollection -> filterPiecesByUserTenantsIfNecessary(piecesCollection.getPieces(), requestContext)
        .map(piecesCollection::withPieces));
  }

  private Future<List<Piece>> filterPiecesByUserTenantsIfNecessary(List<Piece> pieces, RequestContext requestContext) {
    return consortiumConfigurationService.getConsortiumConfiguration(requestContext)
      .compose(consortiumConfiguration -> consortiumConfiguration
        .map(configuration -> consortiumUserTenantsRetriever.getUserTenants(configuration.consortiumId(), requestContext)
          .map(userTenants -> filterPiecesByUserTenants(pieces, userTenants)))
        .orElse(Future.succeededFuture(pieces)));
  }

  private List<Piece> filterPiecesByUserTenants(List<Piece> pieces, List<String> userTenants) {
    return pieces.stream()
      .filter(piece -> Optional.ofNullable(piece.getReceivingTenantId())
        .map(userTenants::contains)
        .orElse(true))
      .toList();
  }

  public Future<List<Piece>> getPiecesByIds(List<String> pieceIds, RequestContext requestContext) {
    logger.debug("getPiecesByIds:: start to retrieving pieces by ids: {}", pieceIds);
    var futures = ofSubLists(new ArrayList<>(pieceIds), MAX_IDS_FOR_GET_RQ_15)
      .map(HelperUtils::convertIdsToCqlQuery)
      .map(query -> getAllPieces(query, requestContext))
      .toList();
    return collectResultsOnSuccess(futures)
      .map(lists -> lists.stream()
        .map(PieceCollection::getPieces)
        .flatMap(Collection::stream)
        .toList())
      .onSuccess(v -> logger.info("getPiecesByIds:: pieces by ids successfully retrieve: {}", pieceIds));
  }

  public Future<List<Piece>> getPiecesByLineIdsByChunks(List<String> lineIds, RequestContext requestContext) {
    logger.info("getPiecesByLineIdsByChunks start");
    var futures = ofSubLists(new ArrayList<>(lineIds), MAX_IDS_FOR_GET_RQ_15)
      .map(ids -> getPieceChunkByLineIds(ids, requestContext))
      .toList();
    return collectResultsOnSuccess(futures)
      .map(lists -> lists.stream()
        .flatMap(Collection::stream)
        .collect(Collectors.toList()))
      .onSuccess(v -> logger.info("getPiecesByLineIdsByChunks end"));

  }

  private Future<List<Piece>> getPieceChunkByLineIds(Collection<String> poLineIds, RequestContext requestContext) {
    String query = convertIdsToCqlQuery(poLineIds, "poLineId");
    RequestEntry requestEntry = new RequestEntry(resourcesPath(PIECES_STORAGE)).withQuery(query)
      .withOffset(0)
      .withLimit(Integer.MAX_VALUE);
    return restClient.get(requestEntry, PieceCollection.class, requestContext)
      .map(PieceCollection::getPieces);
  }

}
