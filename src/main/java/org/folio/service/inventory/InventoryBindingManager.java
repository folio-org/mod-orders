package org.folio.service.inventory;

import static org.folio.service.inventory.InventoryUtils.INVENTORY_LOOKUP_ENDPOINTS;
import static org.folio.service.inventory.InventoryUtils.REQUESTS;

import java.util.List;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.CirculationRequestsRetriever;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

public class InventoryBindingManager {

  private static final Logger logger = LogManager.getLogger(InventoryBindingManager.class);

  private static final String REQUEST_CANCEL_MESSAGE = "Cancelling Request with id='{}'";
  private static final String REQUEST_MOVE_MESSAGE = "Moving Request with id='{}' to item with id='{}'";
  private static final String REQUEST_ENDPOINT = INVENTORY_LOOKUP_ENDPOINTS.get(REQUESTS) + "/%s";
  private static final String REQUEST_MOVE_ENDPOINT = REQUEST_ENDPOINT + "/move";
  private static final String REQUEST_JSON_KEY = "destinationItemId";

  private final CirculationRequestsRetriever circulationRequestsRetriever;
  private final RestClient restClient;

  public InventoryBindingManager(RestClient restClient, CirculationRequestsRetriever circulationRequestsRetriever) {
    this.restClient = restClient;
    this.circulationRequestsRetriever = circulationRequestsRetriever;
  }

  public Future<List<Piece>> getPiecesWithActiveRequests(List<Piece> pieces, RequestContext requestContext) {
    if (logger.isDebugEnabled()) {
      logger.debug("Filtering pieces with active requests: {}", pieces.stream().map(Piece::getId).toList());
    }
    var futures = pieces.stream()
      .map(piece -> getPieceWithRequests(piece, requestContext))
      .collect(Collectors.toList());
    return GenericCompositeFuture.all(futures)
      .map(f -> f.<Piece>list().stream()
        .filter(Objects::nonNull)
        .toList());
  }

  private Future<Piece> getPieceWithRequests(Piece piece, RequestContext requestContext) {
    return circulationRequestsRetriever.getNumberOfRequestsByItemId(piece.getItemId(), requestContext)
      .map(requests -> requests > 0 ? piece : null);
  }

  public Future<Void> cancelPieceItemRequests(Piece piece, RequestContext requestContext) {
    return handleBindingPieces(piece, requestContext, reqId -> cancelRequest(reqId, requestContext));
  }

  public Future<Void> transferPieceItemRequests(Piece piece, RequestContext requestContext) {
    return handleBindingPieces(piece, requestContext, reqId -> transferRequest(reqId, requestContext));
  }

  private Future<Void> handleBindingPieces(Piece piece, RequestContext requestContext, Function<String, Future<Void>> bindHandler) {
    return circulationRequestsRetriever.getRequestIdsByItemId(piece.getItemId(), requestContext)
      .compose(reqIds -> {
        var futures = reqIds.stream().map(bindHandler).toList();
        return GenericCompositeFuture.all(futures);
      })
      .mapEmpty();
  }

  private Future<Void> cancelRequest(String reqId, RequestContext requestContext) {
    logger.info(REQUEST_CANCEL_MESSAGE, reqId);
    RequestEntry requestEntry = new RequestEntry(String.format(REQUEST_ENDPOINT, reqId));
    return restClient.delete(requestEntry, requestContext);
  }

  private Future<Void> transferRequest(String reqId, RequestContext requestContext) {
    return createNewItemForRequest(reqId, requestContext)
      .compose(itemId -> {
        logger.info(REQUEST_MOVE_MESSAGE, reqId, itemId);
        JsonObject jsonObject =  JsonObject.of(REQUEST_JSON_KEY, itemId);
        RequestEntry requestEntry = new RequestEntry(String.format(REQUEST_MOVE_ENDPOINT, reqId));
        return restClient.postJsonObject(requestEntry, jsonObject, requestContext);
      })
      .mapEmpty();

  }

  private Future<String> createNewItemForRequest(String requestId, RequestContext requestContext) {
    throw new RuntimeException("Not implemented");
  }

}
