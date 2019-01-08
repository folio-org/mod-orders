package org.folio.rest.impl;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.resource.Orders.PutOrdersByIdResponse;

import javax.ws.rs.core.Response;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import static org.folio.orders.utils.HelperUtils.deletePoLine;
import static org.folio.orders.utils.HelperUtils.getPoLines;
import static org.folio.orders.utils.HelperUtils.operateOnSubObj;
import static org.folio.orders.utils.SubObjects.PO_LINES;
import static org.folio.orders.utils.SubObjects.PURCHASE_ORDER;
import static org.folio.orders.utils.SubObjects.resourceByIdPath;

public class PutOrdersByIdHelper extends AbstractHelper {

  private String lang;

  private final PostOrdersHelper postHelper;
  private final PutOrderLineByIdHelper putLineHelper;
  private final PostOrderLineHelper postOrderLineHelper;

  public PutOrdersByIdHelper(String lang, Map<String, String> okapiHeaders,
                             Handler<AsyncResult<Response>> asyncResultHandler, Context ctx) {
    super(AbstractHelper.getHttpClient(okapiHeaders), okapiHeaders, asyncResultHandler, ctx);
    setDefaultHeaders(httpClient);
    this.lang = lang;
    postHelper = new PostOrdersHelper(httpClient, okapiHeaders, asyncResultHandler, ctx);
    putLineHelper = new PutOrderLineByIdHelper(lang, httpClient, okapiHeaders, asyncResultHandler, ctx);
    postOrderLineHelper = new PostOrderLineHelper(httpClient, okapiHeaders, asyncResultHandler, ctx);
  }

  public CompletableFuture<Void> updateOrderWithPoLines(String id, CompositePurchaseOrder compPO) {
    if (CollectionUtils.isEmpty(compPO.getPoLines())) {
      return updateOrder(id, compPO);
    }
    return getPoLines(id, lang, httpClient, ctx, okapiHeaders, logger)
      .thenCompose(jsonObject -> {
        JsonArray existedPoLinesArray = jsonObject.getJsonArray(PO_LINES);
        return handlePoLines(compPO, existedPoLinesArray);
      })
      .thenCompose(aVoid -> updateOrder(id, compPO));
  }

  private CompletableFuture<Void> updateOrder(String id, CompositePurchaseOrder compPO) {
    CompletableFuture<Void> future = new VertxCompletableFuture<>(ctx);

    logger.debug("Updating order...");
    JsonObject purchaseOrder = convertToPurchaseOrder(compPO);
    operateOnSubObj(HttpMethod.PUT, resourceByIdPath(PURCHASE_ORDER, id), purchaseOrder, httpClient, ctx, okapiHeaders, logger)
      .thenAccept(v -> {

        logger.info("Applying Funds...");
        postHelper.applyFunds(compPO)
          .thenAccept(withFunds -> {

            logger.info("Updating Inventory...");
            postHelper.updateInventory(withFunds)
              .thenAccept(withInventory -> {

                logger.info("Successfully Placed Order: " + JsonObject.mapFrom(compPO).encodePrettily());
                httpClient.closeClient();
                javax.ws.rs.core.Response response = PutOrdersByIdResponse.respond204();
                AsyncResult<javax.ws.rs.core.Response> result = Future.succeededFuture(response);
                asyncResultHandler.handle(result);
              })
              .exceptionally(this::handleError);
          })
          .exceptionally(this::handleError);
      })
      .exceptionally(this::handleError);

    return future;
  }

  private CompletableFuture<Void> handlePoLines(CompositePurchaseOrder compOrder, JsonArray poLinesFromStorage) {
    List<CompletableFuture<?>> futures = new ArrayList<>();
    List<PoLine> linesToCreate = new ArrayList<>();
    Map<PoLine, JsonObject> linesToUpdate = new HashMap<>();

    if (poLinesFromStorage.isEmpty()) {
      linesToCreate.addAll(compOrder.getPoLines());
    } else {
     sortOutPoLines(compOrder, poLinesFromStorage, linesToCreate, linesToUpdate);
    }

    boolean updateInventory = compOrder.getWorkflowStatus() == CompositePurchaseOrder.WorkflowStatus.OPEN;
    poLinesFromStorage.stream().forEach(poLine ->futures.add(deletePoLine((JsonObject) poLine, httpClient, ctx, okapiHeaders, logger)));
    linesToUpdate.forEach((poLine, lineFromStorage) -> futures.add(putLineHelper.updateOrderLine(poLine, lineFromStorage)));
    linesToCreate.forEach(poLine -> futures.add(postOrderLineHelper.createPoLine(poLine, updateInventory)));
    return VertxCompletableFuture.allOf(ctx, futures.toArray(new CompletableFuture[0]));
  }

  private void sortOutPoLines(CompositePurchaseOrder compOrder, JsonArray poLinesFromStorage, List<PoLine> linesToCreate, Map<PoLine, JsonObject> linesToUpdate) {

    for (PoLine line : compOrder.getPoLines()) {
      JsonObject existingLine = null;
      for (int i = 0; i < poLinesFromStorage.size(); i++) {
        JsonObject lineFromStorage = poLinesFromStorage.getJsonObject(i);
        if (StringUtils.equals(lineFromStorage.getString("id"), line.getId())) {
          existingLine = lineFromStorage;
          poLinesFromStorage.remove(i);
          break;
        }
      }
      if (existingLine == null ) {
        linesToCreate.add(line);
      } else {
        linesToUpdate.put(line, existingLine);
      }
    }
  }

  @Override
  Response buildErrorResponse(int code, Error error) {
    final Response result;
    switch (code) {
      case 404:
        result = PutOrdersByIdResponse.respond404WithTextPlain(error.getMessage());
        break;
      case 422:
        result = PutOrdersByIdResponse.respond422WithApplicationJson(withErrors(error));
        break;
      default:
        if (putLineHelper.getProcessingErrors().isEmpty()) {
          result = PutOrdersByIdResponse.respond500WithTextPlain(error.getMessage());
        } else {
          Errors processingErrors = new Errors();
          processingErrors.getErrors().addAll(putLineHelper.getProcessingErrors());
          processingErrors.getErrors().add(error);
          result = PutOrdersByIdResponse.respond500WithApplicationJson(processingErrors);
        }
    }
    return result;
  }
}
