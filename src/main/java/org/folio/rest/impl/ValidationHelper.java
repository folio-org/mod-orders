package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.rest.jaxrs.resource.Orders.PostOrdersPoNumberValidateResponse.respond204;
import static org.folio.rest.jaxrs.resource.Orders.PostOrdersPoNumberValidateResponse.respond400WithTextPlain;
import static org.folio.rest.jaxrs.resource.Orders.PostOrdersPoNumberValidateResponse.respond422WithApplicationJson;
import static org.folio.rest.jaxrs.resource.Orders.PostOrdersPoNumberValidateResponse.respond500WithTextPlain;

import java.util.Map;

import javax.ws.rs.core.Response;

import org.folio.orders.utils.HelperUtils;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.PoNumber;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;

public class ValidationHelper extends AbstractHelper{

  public ValidationHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders,
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
