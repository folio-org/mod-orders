package org.folio.rest.impl;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import org.folio.orders.rest.exceptions.CustomHttpException;
import org.folio.orders.utils.ErrorCodes;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.acq.model.SequenceNumber;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.PoNumber;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import javax.ws.rs.core.Response;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.orders.utils.HelperUtils.getPurchaseOrderByPONumber;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.jaxrs.resource.Orders.PostOrdersPoNumberValidateResponse.*;

public class PoNumberHelper extends AbstractHelper {

  public PoNumberHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders,
                        Handler<AsyncResult<javax.ws.rs.core.Response>> asyncResultHandler, Context ctx, String lang) {
    super(httpClient, okapiHeaders, asyncResultHandler, ctx, lang);
  }

  void checkPONumberUnique(PoNumber poNumber) {
    checkPONumberUnique(poNumber.getPoNumber())
      .thenAccept(aVoid -> {
        asyncResultHandler.handle(succeededFuture(respond204()));
        httpClient.closeClient();
      })
      .exceptionally(this::handleError);
  }

  CompletableFuture<Void> checkPONumberUnique(String poNumber) {
    return getPurchaseOrderByPONumber(poNumber, lang, httpClient, ctx, okapiHeaders, logger)
      .thenAccept(po -> {
         if (po.getInteger("total_records") != 0) {
           logger.error("Exception validating PO Number existence");
           throw new CompletionException(new CustomHttpException(400, ErrorCodes.PO_NUMBER_ALREADY_EXISTS));
         }
      });
  }

  public CompletableFuture<String> generatePoNumber() {
    return HelperUtils.handleGetRequest(resourcesPath(PO_NUMBER), httpClient, ctx, okapiHeaders, logger)
      .thenApply(seqNumber -> seqNumber.mapTo(SequenceNumber.class).getSequenceNumber());
  }


  @Override
  Response buildErrorResponse(int code, Error error) {
    final Response result;
    switch (code) {
      case 400:
        result = respond400WithApplicationJson(withErrors(error));
        break;
      case 422:
        result = respond422WithApplicationJson(withErrors(error));
        break;
      default:
        result = respond500WithApplicationJson(withErrors(error));
    }
    return result;
  }

}
