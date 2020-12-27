package org.folio.service;

import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.folio.orders.utils.ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY;
import static org.folio.orders.utils.ErrorCodes.SUFFIX_IS_USED;

import java.util.Map;
import java.util.concurrent.CompletableFuture;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.dao.PurchaseOrderDAO;
import org.folio.dao.SuffixDAO;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.jaxrs.model.Suffix;
import org.folio.rest.jaxrs.model.SuffixCollection;
import org.springframework.beans.factory.annotation.Autowired;

import io.vertx.core.Context;

public class SuffixService {

  private static final Logger logger = LogManager.getLogger();

  @Autowired
  private SuffixDAO suffixDAO;

  @Autowired
  private PurchaseOrderDAO purchaseOrderDAO;

  public CompletableFuture<SuffixCollection> getSuffixes(String query, int offset, int limit, Context context, Map<String, String> okapiHeaders) {
    return suffixDAO.get(query, offset, limit, context, okapiHeaders);
  }

  public CompletableFuture<Suffix> getSuffixById(String id, Context context, Map<String, String> okapiHeaders) {
    return suffixDAO.getById(id, context, okapiHeaders);
  }

  public CompletableFuture<Suffix> createSuffix(Suffix suffix, Context context, Map<String, String> okapiHeaders) {
    return suffixDAO.save(suffix, context, okapiHeaders);
  }

  public CompletableFuture<Void> updateSuffix(String id, Suffix suffix, Context context, Map<String, String> okapiHeaders) {
    if (isEmpty(suffix.getId())) {
      suffix.setId(id);
    } else if (!id.equals(suffix.getId())) {
      CompletableFuture<Void> future = new CompletableFuture<>();
      future.completeExceptionally(new HttpException(422, MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY));
      return future;
    }
    return suffixDAO.update(id, suffix, context, okapiHeaders);
  }

  public CompletableFuture<Void> deleteSuffix(String id, Context context, Map<String, String> okapiHeaders) {
    return suffixDAO.getById(id, context, okapiHeaders)
      .thenCompose(suffix -> checkSuffixNotUsed(suffix, context, okapiHeaders))
      .thenCompose(aVoid -> suffixDAO.delete(id, context, okapiHeaders));
  }

  private CompletableFuture<Void> checkSuffixNotUsed(Suffix suffix, Context context, Map<String, String> okapiHeaders) {
    String query = "poNumberSuffix==" + suffix.getName();
    return purchaseOrderDAO.get(query, 0, 0, context, okapiHeaders)
      .thenAccept(purchaseOrders -> {
        if (purchaseOrders.getTotalRecords() > 0) {
          logger.error("Suffix is used by {} orders", purchaseOrders.getTotalRecords());
          throw new HttpException(400, SUFFIX_IS_USED);
        }
      });
  }
}
