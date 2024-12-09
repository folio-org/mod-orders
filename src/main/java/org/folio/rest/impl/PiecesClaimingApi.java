package org.folio.rest.impl;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import org.folio.rest.annotations.Validate;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.ClaimingCollection;
import org.folio.rest.jaxrs.resource.PiecesClaim;
import org.folio.service.pieces.PiecesClaimingService;
import org.folio.spring.SpringContextUtil;
import org.springframework.beans.factory.annotation.Autowired;

import javax.ws.rs.core.Response;
import java.util.Map;

import static org.folio.orders.utils.ResourcePathResolver.PIECES_CLAIMING_BUSINESS;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.rest.RestConstants.OKAPI_URL;

public class PiecesClaimingApi extends BaseApi implements PiecesClaim {

  @Autowired
  private PiecesClaimingService pieceClaimingService;

  public PiecesClaimingApi() {
    SpringContextUtil.autowireDependencies(this, Vertx.currentContext());
  }

  @Override
  @Validate
  public void postPiecesClaim(ClaimingCollection claimingCollection, Map<String, String> okapiHeaders,
                              Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    var requestContext = new RequestContext(vertxContext, okapiHeaders);
    pieceClaimingService.sendClaims(claimingCollection, requestContext)
      .onSuccess(claimingResults -> {
        var okapiUrl = okapiHeaders.get(OKAPI_URL);
        var url = resourceByIdPath(PIECES_CLAIMING_BUSINESS);
        var response = buildResponseWithLocation(okapiUrl, url, claimingResults);
        asyncResultHandler.handle(Future.succeededFuture(response));
      })
      .onFailure(t -> handleErrorResponse(asyncResultHandler, t));
  }
}
