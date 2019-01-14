package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.orders.utils.SubObjects.PO_NUMBER;
import static org.folio.orders.utils.SubObjects.resourcesPath;
import static org.folio.rest.jaxrs.resource.Orders.PostOrdersPoNumberValidateResponse.respond204;
import static org.folio.rest.jaxrs.resource.Orders.PostOrdersPoNumberValidateResponse.respond400WithTextPlain;
import static org.folio.rest.jaxrs.resource.Orders.PostOrdersPoNumberValidateResponse.respond422WithApplicationJson;
import static org.folio.rest.jaxrs.resource.Orders.PostOrdersPoNumberValidateResponse.respond500WithTextPlain;
import static org.folio.rest.tools.client.Response.isSuccess;

import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

import javax.ws.rs.core.Response;

import me.escoffier.vertx.completablefuture.VertxCompletableFuture;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.PoNumber;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;

public class PoNumberHelper extends AbstractHelper{

  public PoNumberHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders,
                        Handler<AsyncResult<javax.ws.rs.core.Response>> asyncResultHandler, Context ctx, String lang) {
    super(httpClient, okapiHeaders, asyncResultHandler, ctx, lang);
  }

  void checkPONumberUnique(PoNumber poNumber,String lang)
  {
    HelperUtils.isPONumberUnique(poNumber.getPoNumber(), lang, httpClient, ctx, okapiHeaders, logger)
    .thenAccept(po->{
         asyncResultHandler.handle(succeededFuture(respond204()));
         httpClient.closeClient();
     })
    .exceptionally(this::handleError);
  }

  public CompletableFuture<PoNumber> generatePoNumber() {
    CompletableFuture<PoNumber> future = new VertxCompletableFuture<>(ctx);
    try {
      httpClient.request(resourcesPath(PO_NUMBER)).thenAccept(poNumberResp -> {
        if (!isSuccess(poNumberResp.getCode())) {
          throw new CompletionException(
            new HttpException(poNumberResp.getCode(), poNumberResp.getError().getString("errorMessage")));
        } else {
          PoNumber poNumber = new PoNumber();
          poNumber.setPoNumber(poNumberResp.getBody().getString(PO_NUMBER));
          future.complete(poNumber);
        }
      }).exceptionally(this::handleError);
    } catch (Exception e) {
      logger.error("Error retrieving po_number", e);
      future.completeExceptionally(e);
    }
    return future;
  }

  @Override
  Response buildErrorResponse(int code, Error error) {
    final Response result;
    switch (code) {
      case 400:
        result = respond400WithTextPlain(error.getMessage());
        break;
      case 422:
        result = respond422WithApplicationJson(withErrors(error));
        break;
      default:
        result = respond500WithTextPlain(error.getMessage());
    }
    return result;
  }
}
