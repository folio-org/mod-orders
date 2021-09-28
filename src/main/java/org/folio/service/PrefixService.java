package org.folio.service;

import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.folio.rest.core.exceptions.ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY;
import static org.folio.rest.core.exceptions.ErrorCodes.PREFIX_IS_USED;

import java.util.concurrent.CompletableFuture;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.Prefix;
import org.folio.rest.jaxrs.model.PrefixCollection;
import org.folio.service.orders.PurchaseOrderService;

public class PrefixService {

  private static final Logger logger = LogManager.getLogger();
  private static final String ENDPOINT = "/orders-storage/configuration/prefixes";
  private static final String BY_ID_ENDPOINT = ENDPOINT + "/{id}";


  private final RestClient restClient;
  private final PurchaseOrderService purchaseOrderService;

  public PrefixService(RestClient restClient, PurchaseOrderService purchaseOrderService) {
    this.restClient = restClient;
    this.purchaseOrderService = purchaseOrderService;
  }

  public CompletableFuture<PrefixCollection> getPrefixes(String query, int offset, int limit, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT).withQuery(query).withOffset(offset).withLimit(limit);
    return restClient.get(requestEntry, requestContext, PrefixCollection.class);
  }

  public CompletableFuture<Prefix> getPrefixById(String id, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(id);
    return restClient.get(requestEntry, requestContext, Prefix.class);
  }

  public CompletableFuture<Prefix> createPrefix(Prefix prefix, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT);
    return restClient.post(requestEntry, prefix, requestContext, Prefix.class);
  }

  public CompletableFuture<Void> updatePrefix(String id, Prefix prefix, RequestContext requestContext) {
    if (isEmpty(prefix.getId())) {
      prefix.setId(id);
    } else if (!id.equals(prefix.getId())) {
      CompletableFuture<Void> future = new CompletableFuture<>();
      future.completeExceptionally(new HttpException(422, MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY));
      return future;
    }
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(id);
    return restClient.put(requestEntry, prefix, requestContext);
  }

  public CompletableFuture<Void> deletePrefix(String id, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(id);
    return getPrefixById(id, requestContext)
      .thenCompose(prefix -> checkPrefixNotUsed(prefix, requestContext))
      .thenCompose(aVoid -> restClient.delete(requestEntry, requestContext));
  }

  private CompletableFuture<Void> checkPrefixNotUsed(Prefix prefix, RequestContext requestContext) {
    String query = "poNumberPrefix==" + prefix.getName();
    return purchaseOrderService.getPurchaseOrders(query, 0, 0, requestContext)
      .thenAccept(purchaseOrders -> {
        if (purchaseOrders.getTotalRecords() > 0) {
          logger.error("Prefix is used by {} orders", purchaseOrders.getTotalRecords());
          throw new HttpException(400, PREFIX_IS_USED);
        }
      });
  }
}
