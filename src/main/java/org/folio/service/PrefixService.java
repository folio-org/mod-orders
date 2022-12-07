package org.folio.service;

import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.folio.rest.core.exceptions.ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY;
import static org.folio.rest.core.exceptions.ErrorCodes.PREFIX_IS_USED;
import static org.folio.rest.core.exceptions.ErrorCodes.PREFIX_NOT_FOUND;

import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.Prefix;
import org.folio.rest.jaxrs.model.PrefixCollection;
import org.folio.service.orders.PurchaseOrderStorageService;

import io.vertx.core.Future;

public class PrefixService {

  private static final Logger logger = LogManager.getLogger();
  private static final String ENDPOINT = "/orders-storage/configuration/prefixes";
  private static final String BY_ID_ENDPOINT = ENDPOINT + "/{id}";


  private final RestClient restClient;
  private final PurchaseOrderStorageService purchaseOrderStorageService;

  public PrefixService(RestClient restClient, PurchaseOrderStorageService purchaseOrderStorageService) {
    this.restClient = restClient;
    this.purchaseOrderStorageService = purchaseOrderStorageService;
  }

  public Future<PrefixCollection> getPrefixes(String query, int offset, int limit, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT).withQuery(query).withOffset(offset).withLimit(limit);
    return restClient.get(requestEntry, PrefixCollection.class, requestContext);
  }

  public Future<Prefix> getPrefixById(String id, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(id);
    return restClient.get(requestEntry, Prefix.class, requestContext);
  }

  public Future<Prefix> createPrefix(Prefix prefix, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT);
    return restClient.post(requestEntry, prefix, Prefix.class, requestContext);
  }

  public Future<Void> updatePrefix(String id, Prefix prefix, RequestContext requestContext) {
    if (isEmpty(prefix.getId())) {
      prefix.setId(id);
    } else if (!id.equals(prefix.getId())) {
      return Future.failedFuture(new HttpException(422, MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY));
    }
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(id);
    return restClient.put(requestEntry, prefix, requestContext);
  }

  public Future<Void> deletePrefix(String id, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(id);
    return getPrefixById(id, requestContext)
      .compose(prefix -> checkPrefixNotUsed(prefix, requestContext))
      .compose(aVoid -> restClient.delete(requestEntry, requestContext));
  }

  private Future<Void> checkPrefixNotUsed(Prefix prefix, RequestContext requestContext) {
    String query = "poNumberPrefix==" + prefix.getName();
    return purchaseOrderStorageService.getPurchaseOrders(query, 0, 0, requestContext)
      .map(purchaseOrders -> {
        if (purchaseOrders.getTotalRecords() > 0) {
          logger.error("Prefix is used by {} orders", purchaseOrders.getTotalRecords());
          throw new HttpException(400, PREFIX_IS_USED);
        }
        return  null;
      });
  }

  public Future<Void> validatePrefixAvailability(String prefixName, RequestContext requestContext) {
    if(StringUtils.isNotEmpty(prefixName)) {
      String query = "name==" + prefixName;
      RequestEntry requestEntry = new RequestEntry(ENDPOINT).withQuery(query);
      return restClient.get(requestEntry, PrefixCollection.class, requestContext)
        .map(totalRecords -> {
          if(totalRecords.getTotalRecords() == 0) {
            logger.error("Prefix {} may not be available", prefixName);
            throw new HttpException(404, PREFIX_NOT_FOUND);
          }
          return null;
        });
    }
    return Future.succeededFuture();
  }
}
