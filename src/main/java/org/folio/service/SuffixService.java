package org.folio.service;

import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.folio.rest.core.exceptions.ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY;
import static org.folio.rest.core.exceptions.ErrorCodes.SUFFIX_IS_USED;

import java.util.concurrent.CompletableFuture;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.Suffix;
import org.folio.rest.jaxrs.model.SuffixCollection;
import org.folio.service.orders.PurchaseOrderStorageService;

public class SuffixService {

  private static final Logger logger = LogManager.getLogger();
  private static final String ENDPOINT = "/orders-storage/configuration/suffixes";
  private static final String BY_ID_ENDPOINT = ENDPOINT + "/{id}";

  private final RestClient restClient;
  private final PurchaseOrderStorageService purchaseOrderStorageService;

  public SuffixService(RestClient restClient, PurchaseOrderStorageService purchaseOrderStorageService) {
    this.restClient = restClient;
    this.purchaseOrderStorageService = purchaseOrderStorageService;
  }

  public CompletableFuture<SuffixCollection> getSuffixes(String query, int offset, int limit, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT).withQuery(query)
      .withOffset(offset)
      .withLimit(limit);
    return restClient.get(requestEntry, requestContext, SuffixCollection.class);
  }

  public CompletableFuture<Suffix> getSuffixById(String id, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(id);
    return restClient.get(requestEntry, requestContext, Suffix.class);
  }

  public CompletableFuture<Suffix> createSuffix(Suffix suffix, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT);
    return restClient.post(requestEntry, suffix, requestContext, Suffix.class);
  }

  public CompletableFuture<Void> updateSuffix(String id, Suffix suffix, RequestContext requestContext) {
    if (isEmpty(suffix.getId())) {
      suffix.setId(id);
    } else if (!id.equals(suffix.getId())) {
      CompletableFuture<Void> future = new CompletableFuture<>();
      future.completeExceptionally(new HttpException(422, MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY));
      return future;
    }
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(id);
    return restClient.put(requestEntry, suffix, requestContext);
  }

  public CompletableFuture<Void> deleteSuffix(String id, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(id);
    return getSuffixById(id, requestContext).thenCompose(suffix -> checkSuffixNotUsed(suffix, requestContext))
      .thenCompose(aVoid -> restClient.delete(requestEntry, requestContext));
  }

  private CompletableFuture<Void> checkSuffixNotUsed(Suffix suffix, RequestContext requestContext) {
    String query = "poNumberSuffix==" + suffix.getName();
    return purchaseOrderStorageService.getPurchaseOrders(query, 0, 0, requestContext)
      .thenAccept(purchaseOrders -> {
        if (purchaseOrders.getTotalRecords() > 0) {
          logger.error("Suffix is used by {} orders", purchaseOrders.getTotalRecords());
          throw new HttpException(400, SUFFIX_IS_USED);
        }
      });
  }
}
