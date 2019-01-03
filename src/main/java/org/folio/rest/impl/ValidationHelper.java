package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.orders.utils.HelperUtils.getPurchaseOrderByPONumber;
import static org.folio.rest.jaxrs.resource.Orders.PostOrdersPoNumberValidateResponse.*;

import java.util.Map;

import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.PoNumber;
import org.folio.rest.jaxrs.resource.Orders.PostOrdersPoNumberValidateResponse;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

public class ValidationHelper {

  private static final Logger logger = LoggerFactory.getLogger(ValidationHelper.class);

  private final HttpClientInterface httpClient;
  private final Context ctx;
  private final Handler<AsyncResult<javax.ws.rs.core.Response>> asyncResultHandler;
  private final Map<String, String> okapiHeaders;

  public ValidationHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders,
      Handler<AsyncResult<javax.ws.rs.core.Response>> asyncResultHandler, Context ctx) {
    this.httpClient = httpClient;
    this.okapiHeaders = okapiHeaders;
    this.ctx = ctx;
    this.asyncResultHandler = asyncResultHandler;
  }

  void checkPONumberUnique(PoNumber poNumber,String lang)
  {
    getPurchaseOrderByPONumber(poNumber.getId(), lang, httpClient, ctx, okapiHeaders, logger).thenAccept(po->{
    if(po.getInteger("total_records")==0)
      asyncResultHandler.handle(succeededFuture(respond204()));
    else
      asyncResultHandler.handle(succeededFuture(respond400WithTextPlain("PO Number already exists")));
    httpClient.closeClient();
  })
  .exceptionally(this::handleResponse);
  }

  public Void handleResponse(Throwable throwable) {
    final Future<javax.ws.rs.core.Response> result;

    final Throwable t = throwable.getCause();
    if (t instanceof HttpException) {
      final int code = ((HttpException) t).getCode();
      final String message = t.getMessage();
      switch (code) {
      case 400:
        result = Future.succeededFuture(respond400WithTextPlain(message));
        break;
      case 422:
        Errors errors = new Errors();
        errors.getErrors().add(new Error().withMessage(message));
        result = Future.succeededFuture(respond422WithApplicationJson(errors));
        break;
      default:
        result = Future.succeededFuture(respond500WithTextPlain(message));
      }
    } else {
      result = Future.succeededFuture(respond500WithTextPlain(throwable.getMessage()));
    }

    httpClient.closeClient();
    asyncResultHandler.handle(result);

    return null;
  }
}
