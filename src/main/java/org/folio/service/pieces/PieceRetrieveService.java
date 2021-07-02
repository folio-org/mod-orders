package org.folio.service.pieces;

import static org.folio.orders.utils.HelperUtils.buildQuery;
import static org.folio.orders.utils.ResourcePathResolver.PIECES;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.service.inventory.InventoryManager.SEARCH_PARAMS_WITHOUT_LANG;

import java.util.Map;
import java.util.concurrent.CompletableFuture;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.orders.utils.AsyncUtil;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PieceCollection;

public class PieceRetrieveService {
  private static final Logger logger = LogManager.getLogger(PieceRetrieveService.class);

  private static final String PIECES_BY_POL_ID_AND_STATUS_QUERY = "poLineId==%s and receivingStatus==%s";
  private static final String GET_PIECES_BY_QUERY = resourcesPath(PIECES) + SEARCH_PARAMS_WITHOUT_LANG;

  private final RestClient restClient;

  public PieceRetrieveService(RestClient restClient) {
    this.restClient = restClient;
  }

  public CompletableFuture<PieceCollection> getExpectedPiecesByLineId(String poLineId, RequestContext requestContext) {
    String query = String.format(PIECES_BY_POL_ID_AND_STATUS_QUERY, poLineId, Piece.ReceivingStatus.EXPECTED.value());
    return getPieces(Integer.MAX_VALUE, 0, query, requestContext);
  }

  public CompletableFuture<PieceCollection> getPieces(int limit, int offset, String query, RequestContext requestContext) {
    String endpoint = String.format(GET_PIECES_BY_QUERY, limit, offset, buildQuery(query, logger));
    Map<String, String> okapiHeaders = requestContext.getHeaders();
    return HelperUtils.handleGetRequest(endpoint, restClient.getHttpClient(okapiHeaders), okapiHeaders, logger)
      .thenCompose(json -> AsyncUtil.executeBlocking(requestContext.getContext(), false, () -> json.mapTo(PieceCollection.class)));
  }

}
