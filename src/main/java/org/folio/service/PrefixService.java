package org.folio.service;

import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.folio.orders.utils.ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY;
import static org.folio.orders.utils.ErrorCodes.PREFIX_IS_USED;
import static org.folio.orders.utils.HelperUtils.buildQuery;

import java.util.Map;
import java.util.concurrent.CompletableFuture;

import org.folio.dao.PrefixDAO;
import org.folio.dao.PurchaseOrderDAO;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.jaxrs.model.Prefix;
import org.folio.rest.jaxrs.model.PrefixCollection;
import org.springframework.beans.factory.annotation.Autowired;

import io.vertx.core.Context;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

public class PrefixService {

  private static final Logger logger = LoggerFactory.getLogger(PrefixService.class);

  @Autowired
  private PrefixDAO prefixDAO;
  @Autowired
  private PurchaseOrderDAO purchaseOrderDAO;

  public CompletableFuture<PrefixCollection> getPrefixes(String query, int limit, int offset, Context context, Map<String, String> okapiHeaders) {
    return prefixDAO.get(query, limit, offset, context, okapiHeaders);
  }

  public CompletableFuture<Prefix> getPrefixById(String id, Context context, Map<String, String> okapiHeaders) {
    return prefixDAO.getById(id, context, okapiHeaders);
  }

  public CompletableFuture<Prefix> createPrefix(Prefix prefix, Context context, Map<String, String> okapiHeaders) {
    return prefixDAO.save(prefix, context, okapiHeaders);
  }

  public CompletableFuture<Void> updatePrefix(String id, Prefix prefix, Context context, Map<String, String> okapiHeaders) {
    if (isEmpty(prefix.getId())) {
      prefix.setId(id);
    } else if (!id.equals(prefix.getId())) {
      CompletableFuture<Void> future = new CompletableFuture<>();
      future.completeExceptionally(new HttpException(422, MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY));
      return future;
    }
    return prefixDAO.update(id, prefix, context, okapiHeaders);
  }

  public CompletableFuture<Void> deletePrefix(String id, Context context, Map<String, String> okapiHeaders) {
    return prefixDAO.getById(id, context, okapiHeaders)
      .thenCompose(prefix -> checkPrefixNotUsed(prefix, context, okapiHeaders))
      .thenCompose(aVoid -> prefixDAO.delete(id, context, okapiHeaders));
  }

  private CompletableFuture<Void> checkPrefixNotUsed(Prefix prefix, Context context, Map<String, String> okapiHeaders) {
    String query = "poNumberPrefix==" + prefix.getName();
    return purchaseOrderDAO.get(query, 0, 0, context, okapiHeaders)
      .thenAccept(purchaseOrders -> {
        if (purchaseOrders.getTotalRecords() > 0) {
          throw new HttpException(400, PREFIX_IS_USED);
        }
      });
  }
}
