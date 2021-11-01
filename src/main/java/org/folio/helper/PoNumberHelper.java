package org.folio.helper;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.orders.utils.PoLineCommonUtil.DASH_SEPARATOR;
import static org.folio.orders.utils.ResourcePathResolver.PO_NUMBER;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;

import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.completablefuture.FolioVertxCompletableFuture;
import org.folio.rest.acq.model.SequenceNumber;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.PoNumber;
import org.folio.service.orders.PurchaseOrderStorageService;

public class PoNumberHelper {
  private static final Logger logger = LogManager.getLogger(PoNumberHelper.class);

  private final PurchaseOrderStorageService purchaseOrderStorageService;
  private final RestClient restClient;

  public PoNumberHelper(RestClient restClient, PurchaseOrderStorageService purchaseOrderStorageService) {
    this.restClient = restClient;
    this.purchaseOrderStorageService = purchaseOrderStorageService;
  }

  public CompletableFuture<Void> checkPONumberUnique(PoNumber poNumber, RequestContext requestContext) {
    FolioVertxCompletableFuture<Void> future = new FolioVertxCompletableFuture<>(requestContext.getContext());
    checkPONumberUnique(poNumber.getPoNumber(), requestContext)
      .thenAccept(v -> {
        logger.info("The PO Number '{}' is not in use yet", poNumber.getPoNumber());
        future.complete(null);
      })
      .exceptionally(t -> {
        future.completeExceptionally(t.getCause());
        return null;
      });
    return future;
  }

  public CompletableFuture<PoNumber> getPoNumber(RequestContext requestContext) {
    FolioVertxCompletableFuture<PoNumber> future = new FolioVertxCompletableFuture<>(requestContext.getContext());
    generatePoNumber(requestContext)
      .thenAccept(number -> {
        logger.info("The PO Number '{}' is not in use yet", number);
        future.complete(new PoNumber().withPoNumber(number));
      })
      .exceptionally(t -> {
        future.completeExceptionally(t.getCause());
        return null;
      });
    return future;
  }

  public CompletableFuture<Void> checkPONumberUnique(String poNumber, RequestContext requestContext) {
    return purchaseOrderStorageService.getPurchaseOrderByPONumber(poNumber, requestContext)
      .thenAccept(po -> {
         if (po.getInteger("totalRecords") != 0) {
           logger.error("Exception validating PO Number existence");
           throw new HttpException(400, ErrorCodes.PO_NUMBER_ALREADY_EXISTS);
         }
      });
  }

  public CompletableFuture<String> generatePoNumber(RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(resourcesPath(PO_NUMBER));
    return restClient.getAsJsonObject(requestEntry, requestContext)
                      .thenApply(seqNumber -> seqNumber.mapTo(SequenceNumber.class).getSequenceNumber());
  }

  public CompletionStage<Void> validatePoNumber(CompositePurchaseOrder poFromStorage, CompositePurchaseOrder updatedPo, RequestContext requestContext) {
    if (isPoNumberChanged(poFromStorage, updatedPo)) {
       return checkPONumberUnique(updatedPo.getPoNumber(), requestContext);
    }
    return completedFuture(null);
  }

  public static boolean isPoNumberChanged(CompositePurchaseOrder poFromStorage, CompositePurchaseOrder updatedPo) {
    return !StringUtils.equalsIgnoreCase(poFromStorage.getPoNumber(), updatedPo.getPoNumber());
  }

  public static String buildPoLineNumber(String poNumber, String sequence) {
    return poNumber + DASH_SEPARATOR + sequence;
  }
}
