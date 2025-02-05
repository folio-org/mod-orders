package org.folio.service.pieces;

import static one.util.streamex.StreamEx.ofSubLists;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.QueryUtils.combineCqlExpressions;
import static org.folio.orders.utils.QueryUtils.convertIdsToCqlQuery;
import static org.folio.orders.utils.QueryUtils.getCqlExpressionForFieldNullValue;
import static org.folio.orders.utils.ResourcePathResolver.PIECES_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.PIECES_STORAGE_BATCH;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ_15;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import lombok.extern.log4j.Log4j2;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.folio.orders.utils.QueryUtils;
import org.folio.rest.acq.model.Setting;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PieceCollection;

import io.vertx.core.Future;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.service.consortium.ConsortiumUserTenantsRetriever;
import org.folio.service.consortium.ConsortiumConfigurationService;
import org.folio.service.settings.SettingsRetriever;
import org.folio.service.settings.util.SettingKey;

@Log4j2
public class PieceStorageService {

  private static final String PIECES_BY_POL_ID_AND_STATUS_QUERY = "poLineId==%s and receivingStatus==%s";
  private static final String PIECES_BY_HOLDING_ID_QUERY = "holdingId==%s";
  private static final String PIECE_STORAGE_ENDPOINT = resourcesPath(PIECES_STORAGE);
  private static final String PIECE_STORAGE_BY_ID_ENDPOINT = PIECE_STORAGE_ENDPOINT + "/{id}";
  private static final String PIECES_STORAGE_BATCH_ENDPOINT = resourcesPath(PIECES_STORAGE_BATCH);

  private final ConsortiumConfigurationService consortiumConfigurationService;
  private final ConsortiumUserTenantsRetriever consortiumUserTenantsRetriever;
  private final SettingsRetriever settingsRetriever;
  protected final RestClient restClient;

  public PieceStorageService(ConsortiumConfigurationService consortiumConfigurationService,
                             ConsortiumUserTenantsRetriever consortiumUserTenantsRetriever,
                             SettingsRetriever settingsRetriever, RestClient restClient) {
    this.consortiumConfigurationService = consortiumConfigurationService;
    this.consortiumUserTenantsRetriever = consortiumUserTenantsRetriever;
    this.settingsRetriever = settingsRetriever;
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

  public Future<Void> updatePiecesBatch(PieceCollection pieceCollection, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(PIECES_STORAGE_BATCH_ENDPOINT);
    return restClient.put(requestEntry, pieceCollection, requestContext);
  }

  public Future<Piece> insertPiece(Piece piece, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(PIECE_STORAGE_ENDPOINT);
    return restClient.post(requestEntry, piece, Piece.class, requestContext);
  }

  public Future<PieceCollection> insertPieceBatch(List<Piece> pieces, RequestContext requestContext) {
    var pieceCollection = new PieceCollection().withPieces(pieces).withTotalRecords(pieces.size());
    RequestEntry requestEntry = new RequestEntry(PIECES_STORAGE_BATCH_ENDPOINT);
    return restClient.postBatch(requestEntry, pieceCollection, PieceCollection.class, requestContext);
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
        if (log.isDebugEnabled()) {
          String deletedIds = String.join(",", pieceIds);
          log.debug("Pieces were removed : {}", deletedIds);
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

  public Future<PieceCollection> getPieces(int limit, int offset, String query, RequestContext requestContext) {
    return getUserTenantsIfNeeded(requestContext)
      .map(userTenants -> getQueryForUserTenants(userTenants, query))
      .compose(cql -> getAllPieces(limit, offset, cql, requestContext));
  }

  public Future<PieceCollection> getAllPieces(String query, RequestContext requestContext) {
    return getAllPieces(Integer.MAX_VALUE, 0, query, requestContext);
  }

  public Future<PieceCollection> getAllPieces(int limit, int offset, String query, RequestContext requestContext) {
    log.debug("getAllPieces:: limit: {}, offset: {}, query: {}", limit, offset, query);
    var requestEntry = new RequestEntry(PIECE_STORAGE_ENDPOINT).withQuery(query).withOffset(offset).withLimit(limit);
    return restClient.get(requestEntry, PieceCollection.class, requestContext);
  }

  protected Future<List<String>> getUserTenantsIfNeeded(RequestContext requestContext) {
    return consortiumConfigurationService.getConsortiumConfiguration(requestContext)
      .compose(consortiumConfiguration -> {
        if (consortiumConfiguration.isEmpty()) {
          return Future.succeededFuture();
        }
        var configuration = consortiumConfiguration.get();
        return settingsRetriever.getSettingByKey(SettingKey.CENTRAL_ORDERING_ENABLED, requestContext)
          .map(centralOrdering -> centralOrdering.map(Setting::getValue).orElse(null))
          .compose(orderingEnabled -> shouldFilterPiecesForTenant(configuration.centralTenantId(), Boolean.parseBoolean(orderingEnabled), requestContext)
            ? consortiumUserTenantsRetriever.getUserTenants(configuration.consortiumId(), configuration.centralTenantId(), requestContext)
            : Future.succeededFuture());
      });
  }

  public Future<List<Piece>> getPiecesByIds(List<String> pieceIds, RequestContext requestContext) {
    log.debug("getPiecesByIds:: start to retrieving pieces by ids: {}", pieceIds);
    var futures = ofSubLists(new ArrayList<>(pieceIds), MAX_IDS_FOR_GET_RQ_15)
      .map(QueryUtils::convertIdsToCqlQuery)
      .map(query -> getAllPieces(query, requestContext))
      .toList();
    return collectResultsOnSuccess(futures)
      .map(lists -> lists.stream()
        .map(PieceCollection::getPieces)
        .flatMap(Collection::stream)
        .toList())
      .onSuccess(v -> log.info("getPiecesByIds:: pieces by ids successfully retrieve: {}", pieceIds))
      .onFailure(t -> log.error("Failed to get pieces by ids", t));
  }

  public Future<List<Piece>> getPiecesByLineIdsByChunks(List<String> lineIds, RequestContext requestContext) {
    var futures = ofSubLists(new ArrayList<>(lineIds), MAX_IDS_FOR_GET_RQ_15)
      .map(ids -> getPieceChunkByLineIds(ids, requestContext))
      .toList();
    return collectResultsOnSuccess(futures)
      .map(lists -> lists.stream()
        .flatMap(Collection::stream)
        .toList());
  }

  private Future<List<Piece>> getPieceChunkByLineIds(Collection<String> poLineIds, RequestContext requestContext) {
    String query = convertIdsToCqlQuery(poLineIds, "poLineId");
    RequestEntry requestEntry = new RequestEntry(resourcesPath(PIECES_STORAGE)).withQuery(query)
      .withOffset(0)
      .withLimit(Integer.MAX_VALUE);
    return restClient.get(requestEntry, PieceCollection.class, requestContext)
      .map(PieceCollection::getPieces);
  }

  static String getQueryForUserTenants(List<String> userTenants, String query) {
    if (CollectionUtils.isEmpty(userTenants)) {
      return query;
    }
    String tenantCql = convertIdsToCqlQuery(userTenants, "receivingTenantId");
    String nullTenantCql = getCqlExpressionForFieldNullValue("receivingTenantId");
    String combinedTenantCql = combineCqlExpressions("or", tenantCql, nullTenantCql);
    return StringUtils.isNotBlank(query)
      ? combineCqlExpressions("and", combinedTenantCql, query)
      : combinedTenantCql;
  }

  private static boolean shouldFilterPiecesForTenant(String centralTenantId, boolean centralOrderingEnabled, RequestContext requestContext) {
    var requestTenantId = TenantTool.tenantId(requestContext.getHeaders());
    return centralOrderingEnabled && requestTenantId.equals(centralTenantId);
  }

}
