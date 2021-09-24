package org.folio.helper;

import static org.folio.orders.utils.HelperUtils.getPurchaseOrderByPONumber;
import static org.folio.orders.utils.ResourcePathResolver.PO_NUMBER;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

import java.util.Map;
import java.util.concurrent.CompletableFuture;

import javax.ws.rs.core.Response;

import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.acq.model.SequenceNumber;
import org.folio.rest.jaxrs.model.PoNumber;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import io.vertx.core.Context;

public class PoNumberHelper extends AbstractHelper {

  public PoNumberHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders, Context ctx, String lang) {
    super(httpClient, okapiHeaders, ctx, lang);
  }

  public PoNumberHelper(Map<String, String> okapiHeaders, Context ctx, String lang) {
    super(okapiHeaders, ctx, lang);
  }

  public CompletableFuture<Response> checkPONumberUnique(PoNumber poNumber) {
    return checkPONumberUnique(poNumber.getPoNumber())
      .thenApply(v -> {
        logger.info("The PO Number '{}' is not in use yet", poNumber.getPoNumber());
        return buildNoContentResponse();
      })
      .exceptionally(this::buildErrorResponse);
  }

  public CompletableFuture<Response> getPoNumber() {
    return generatePoNumber()
      .thenApply(number -> {
        logger.info("The PO Number '{}' is not in use yet", number);
        return buildOkResponse(new PoNumber().withPoNumber(number));
      })
      .exceptionally(this::buildErrorResponse);
  }

  CompletableFuture<Void> checkPONumberUnique(String poNumber) {
    return getPurchaseOrderByPONumber(poNumber, lang, httpClient, okapiHeaders, logger)
      .thenAccept(po -> {
         if (po.getInteger("totalRecords") != 0) {
           logger.error("Exception validating PO Number existence");
           throw new HttpException(400, ErrorCodes.PO_NUMBER_ALREADY_EXISTS);
         }
      });
  }

  CompletableFuture<String> generatePoNumber() {
    return HelperUtils.handleGetRequest(resourcesPath(PO_NUMBER), httpClient, okapiHeaders, logger)
      .thenApply(seqNumber -> seqNumber.mapTo(SequenceNumber.class).getSequenceNumber());
  }

}
