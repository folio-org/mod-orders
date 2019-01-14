package org.folio.rest.impl;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.PoNumber;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import javax.ws.rs.core.Response;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.orders.utils.HelperUtils.PO_NUMBER_ALREADY_EXISTS;
import static org.folio.orders.utils.HelperUtils.getPurchaseOrderByPONumber;
import static org.folio.rest.jaxrs.resource.Orders.PostOrdersPoNumberValidateResponse.respond204;
import static org.folio.rest.jaxrs.resource.Orders.PostOrdersPoNumberValidateResponse.respond400WithTextPlain;
import static org.folio.rest.jaxrs.resource.Orders.PostOrdersPoNumberValidateResponse.respond422WithApplicationJson;
import static org.folio.rest.jaxrs.resource.Orders.PostOrdersPoNumberValidateResponse.respond500WithTextPlain;

public class ValidationHelper extends AbstractHelper{

  public ValidationHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders,
      Handler<AsyncResult<javax.ws.rs.core.Response>> asyncResultHandler, Context ctx, String lang) {
    super(httpClient, okapiHeaders, asyncResultHandler, ctx, lang);
  }

  void checkPONumberUnique(PoNumber poNumber, String lang) {
    try {
      checkPONumberUnique(poNumber.getPoNumber(), lang)
        .thenAccept(aVoid -> {
          asyncResultHandler.handle(succeededFuture(respond204()));
          httpClient.closeClient();
        })
        .exceptionally(this::handleError);
    } catch (Exception e) {
      handleError(e);
    }
  }

  CompletableFuture<Void> checkPONumberUnique(String poNumber, String lang) {
    return getPurchaseOrderByPONumber(poNumber, lang, httpClient, ctx, okapiHeaders, logger)
      .thenAccept(po -> {
         if (po.getInteger("total_records") != 0) {
           throw new CompletionException(new HttpException(400, PO_NUMBER_ALREADY_EXISTS));
         }
      });
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
