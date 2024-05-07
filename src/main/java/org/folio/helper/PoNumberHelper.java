package org.folio.helper;

import static org.folio.orders.utils.ResourcePathResolver.PO_NUMBER;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.acq.model.SequenceNumber;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.PoNumber;
import org.folio.service.orders.PurchaseOrderStorageService;

import io.vertx.core.Future;
import io.vertx.core.Promise;

public class PoNumberHelper {
  private static final Logger logger = LogManager.getLogger(PoNumberHelper.class);

  private final PurchaseOrderStorageService purchaseOrderStorageService;
  private final RestClient restClient;

  public PoNumberHelper(RestClient restClient, PurchaseOrderStorageService purchaseOrderStorageService) {
    this.restClient = restClient;
    this.purchaseOrderStorageService = purchaseOrderStorageService;
  }

  public Future<Void> validatePoNumber(CompositePurchaseOrder poFromStorage, CompositePurchaseOrder updatedPo, RequestContext requestContext) {
    if (StringUtils.equalsIgnoreCase(poFromStorage.getPoNumber(), updatedPo.getPoNumber())) {
      return Future.succeededFuture();
    }
    return checkPONumberUnique(updatedPo.getPoNumber(), requestContext);
  }

  public Future<Void> checkPONumberUnique(PoNumber poNumber, RequestContext requestContext) {
    Promise<Void> promise = Promise.promise();
    checkPONumberUnique(poNumber.getPoNumber(), requestContext)
      .onSuccess(v -> {
        logger.info("The PO Number '{}' is not in use yet", poNumber.getPoNumber());
        promise.complete();
      })
      .onFailure(promise::fail);
    return promise.future();
  }

  public Future<Void> checkPONumberUnique(String poNumber, RequestContext requestContext) {
    if (StringUtils.isEmpty(poNumber)){
      return Future.succeededFuture();
    }
    return purchaseOrderStorageService.getPurchaseOrderByPONumber(poNumber, requestContext)
      .map(po -> {
        if (po.getInteger("totalRecords") != 0) {
          logger.error("Exception validating PO Number existence");
          throw new HttpException(400, ErrorCodes.PO_NUMBER_ALREADY_EXISTS);
        }
        return null;
      });
  }

  public Future<PoNumber> getPoNumber(RequestContext requestContext) {
    Promise<PoNumber> promise = Promise.promise();
    generatePoNumber(requestContext)
      .onSuccess(number -> {
        logger.info("The PO Number '{}' is not in use yet", number);
        promise.complete(new PoNumber().withPoNumber(number));
      })
      .onFailure(t -> promise.fail(t.getCause()));
    return promise.future();
  }

  public Future<String> generatePoNumber(RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(resourcesPath(PO_NUMBER));
    return restClient.getAsJsonObject(requestEntry, requestContext)
                      .map(seqNumber -> seqNumber.mapTo(SequenceNumber.class).getSequenceNumber());
  }

  public Future<Void> validatePoNumberPrefixAndSuffix(CompositePurchaseOrder updatedPo) {
    if (StringUtils.isNotEmpty(updatedPo.getPoNumberPrefix())) {
      String prefix = updatedPo.getPoNumberPrefix();
      if (!updatedPo.getPoNumber()
        .startsWith(prefix)) {
        logger.warn("Po Number {} is not starting with prefix {}", updatedPo.getPoNumber(), prefix);
        throw new HttpException(400, ErrorCodes.PO_NUMBER_PREFIX_REQUIRED);
      }
    }

    if (StringUtils.isNotEmpty(updatedPo.getPoNumberSuffix())) {
      String suffix = updatedPo.getPoNumberSuffix();
      if (!updatedPo.getPoNumber()
        .endsWith(suffix)) {
        logger.warn("Po Number {} is not ending with suffix {}", updatedPo.getPoNumber(), suffix);
        throw new HttpException(400, ErrorCodes.PO_NUMBER_SUFFIX_REQUIRED);
      }
    }
    return Future.succeededFuture();
  }

}
