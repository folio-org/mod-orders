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
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.function.BiFunction;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.folio.orders.utils.HelperUtils.URL_WITH_LANG_PARAM;
import static org.folio.orders.utils.HelperUtils.deletePoLine;
import static org.folio.orders.utils.HelperUtils.getPoLines;
import static org.folio.orders.utils.HelperUtils.getPurchaseOrderById;
import static org.folio.orders.utils.HelperUtils.operateOnSubObj;
import static org.folio.orders.utils.SubObjects.PO_LINES;
import static org.folio.orders.utils.SubObjects.PURCHASE_ORDER;
import static org.folio.orders.utils.SubObjects.resourceByIdPath;

public class PutOrdersByIdHelper extends AbstractHelper {

  private static final Pattern PO_LINE_NUMBER_PATTERN = Pattern.compile("([a-zA-Z0-9]{5,16})(-[0-9]{1,3})");

  private final PostOrdersHelper postHelper;
  private final PutOrderLineByIdHelper putLineHelper;
  private final PostOrderLineHelper postOrderLineHelper;
  private final ValidationHelper validationHelper;

  PutOrdersByIdHelper(Map<String, String> okapiHeaders,
                             Handler<AsyncResult<Response>> asyncResultHandler, Context ctx, String lang) {
    super(getHttpClient(okapiHeaders), okapiHeaders, asyncResultHandler, ctx, lang);
    setDefaultHeaders(httpClient);
    postHelper = new PostOrdersHelper(httpClient, okapiHeaders, asyncResultHandler, ctx, lang);
    putLineHelper = new PutOrderLineByIdHelper(httpClient, okapiHeaders, asyncResultHandler, ctx, lang);
    postOrderLineHelper = new PostOrderLineHelper(httpClient, okapiHeaders, asyncResultHandler, ctx, lang);
    validationHelper = new ValidationHelper(httpClient, okapiHeaders, asyncResultHandler, ctx, lang);
  }

  public void updateOrderWithPoLines(String orderId, CompositePurchaseOrder compPO) {
    getPurchaseOrderById(orderId, lang, httpClient, ctx, okapiHeaders, logger)
      .thenAccept(poFromStorage -> {
        boolean isPoNumberChanged = !StringUtils.equals(poFromStorage.getString(PO_NUMBER), compPO.getPoNumber());
        boolean isRequestContainsLines = !CollectionUtils.isEmpty(compPO.getPoLines());

        if (isPoNumberChanged) {
          validationHelper.checkPONumberUnique(compPO.getPoNumber(), lang)
            .thenAccept(aVoid -> {
              if (isRequestContainsLines) {
                updateOrderWithPoLines(poFromStorage, compPO, this::handlePoLines);
              } else {
                updateOrderWithPoLines(poFromStorage, compPO, this::updatePoLinesNumber);
              }
            })
            .exceptionally(this::handleError);
        } else {
          if (isRequestContainsLines) {
            updateOrderWithPoLines(poFromStorage, compPO, this::handlePoLines);
          } else {
            updateOrder(orderId, compPO);
          }
        }
      })
      .exceptionally(this::handleError);
  }

  private void updateOrderWithPoLines(JsonObject poFromStorage, CompositePurchaseOrder compPO,
                                      BiFunction<CompositePurchaseOrder, JsonArray, CompletableFuture<Void>> updatePoLines) {
    getPoLines(poFromStorage.getString(ID), lang, httpClient, ctx ,okapiHeaders, logger)
      .thenCompose(jsonObject -> {
        JsonArray existedPoLinesArray = jsonObject.getJsonArray(PO_LINES);
        return updatePoLines.apply(compPO, existedPoLinesArray);
      })
      .thenAccept(aVoid -> updateOrder(poFromStorage.getString(ID), compPO))
      .exceptionally(this::handleError);
  }

  private CompletableFuture<Void> updateOrder(String orderId, CompositePurchaseOrder compPO) {
    CompletableFuture<Void> future = new VertxCompletableFuture<>(ctx);

    logger.debug("Updating order...");
    JsonObject purchaseOrder = convertToPurchaseOrder(compPO);
    operateOnSubObj(HttpMethod.PUT, resourceByIdPath(PURCHASE_ORDER, orderId), purchaseOrder, httpClient, ctx, okapiHeaders, logger)
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

  private CompletableFuture<Void> updatePoLinesNumber(CompositePurchaseOrder compOrder, JsonArray poLinesFromStorage) {
    List<CompletableFuture<?>> futures = new ArrayList<>();
    poLinesFromStorage.forEach(object -> {
      JsonObject lineFromStorage = (JsonObject) object;
      lineFromStorage.put(PO_LINE_NUMBER, buildNewPoLineNumber(lineFromStorage.getString(PO_LINE_NUMBER), compOrder.getPoNumber()));
      String endpoint = String.format(URL_WITH_LANG_PARAM, resourceByIdPath(PO_LINES, lineFromStorage.getString(ID)), lang);
      futures.add(operateOnSubObj(HttpMethod.PUT, endpoint, lineFromStorage, httpClient, ctx, okapiHeaders, logger));
    });
    return VertxCompletableFuture.allOf(ctx, futures.toArray(new CompletableFuture[0]));
  }

  private CompletableFuture<Void> handlePoLines(CompositePurchaseOrder compOrder, JsonArray poLinesFromStorage) {
    List<CompletableFuture<?>> futures = new ArrayList<>();
    if (poLinesFromStorage.isEmpty()) {
      futures.addAll(processPoLinesCreation(compOrder, poLinesFromStorage));
    } else {
      futures.addAll(processPoLinesCreation(compOrder, poLinesFromStorage));
      futures.addAll(processPoLinesUpdate(compOrder.getPoLines(), poLinesFromStorage));
      // The remaining unprocessed PoLines should be removed
      poLinesFromStorage.stream().forEach(poLine -> futures.add(deletePoLine((JsonObject) poLine, httpClient, ctx, okapiHeaders, logger)));
    }
    return VertxCompletableFuture.allOf(ctx, futures.toArray(new CompletableFuture[0]));
  }

  private List<CompletableFuture<?>> processPoLinesUpdate(List<PoLine> poLines, JsonArray poLinesFromStorage) {
    List<CompletableFuture<?>> futures = new ArrayList<>();
    for (int i = 0; i < poLinesFromStorage.size(); i++) {
      JsonObject lineFromStorage = poLinesFromStorage.getJsonObject(i);
      for (PoLine line : poLines) {
        if (StringUtils.equals(lineFromStorage.getString(ID), line.getId())) {
          futures.add(putLineHelper.updateOrderLine(line, lineFromStorage));
          poLinesFromStorage.remove(i);
          break;
        }
      }
    }
    return futures;
  }

  private List<CompletableFuture<?>> processPoLinesCreation(CompositePurchaseOrder compOrder, JsonArray poLinesFromStorage) {
    List<CompletableFuture<?>> futures = new ArrayList<>();
    boolean updateInventory = compOrder.getWorkflowStatus() == CompositePurchaseOrder.WorkflowStatus.OPEN;
    for (PoLine line : compOrder.getPoLines()) {
      boolean isNew = true;
      for (int i = 0; i < poLinesFromStorage.size(); i++) {
        JsonObject lineFromStorage = poLinesFromStorage.getJsonObject(i);
        if (StringUtils.equals(lineFromStorage.getString(ID), line.getId())) {
          isNew = false;
          break;
        }
      }
      if (isNew) {
        futures.add(postOrderLineHelper.createPoLine(line, updateInventory));
      }
    }
    return futures;
  }

  private String buildNewPoLineNumber(String poLineFromStorage, String poNumber) {
    Matcher matcher = PO_LINE_NUMBER_PATTERN.matcher(poLineFromStorage);
    if (matcher.find()) {
      return poNumber + matcher.group(1);
    }
    return poLineFromStorage;
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
