package org.folio.rest.impl;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;
import org.apache.commons.lang3.StringUtils;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.resource.Orders.PutOrdersCompositeOrdersByIdResponse;

import javax.ws.rs.core.Response;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static java.util.stream.Collectors.toList;
import static org.apache.commons.collections4.CollectionUtils.isEmpty;
import static org.apache.commons.collections4.CollectionUtils.isNotEmpty;
import static org.folio.orders.utils.HelperUtils.deletePoLine;
import static org.folio.orders.utils.HelperUtils.getPoLines;
import static org.folio.orders.utils.HelperUtils.getPurchaseOrderById;
import static org.folio.orders.utils.HelperUtils.operateOnSubObj;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINE_NUMBER;
import static org.folio.orders.utils.ResourcePathResolver.PO_NUMBER;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.OPEN;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.PENDING;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES;
import static org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;

public class PutOrdersByIdHelper extends AbstractHelper {

  private static final Pattern PO_LINE_NUMBER_PATTERN = Pattern.compile("([a-zA-Z0-9]{5,16})(-[0-9]{1,3})");

  private final PutOrderLineByIdHelper putLineHelper;
  private final PostOrderLineHelper postOrderLineHelper;
  private final PoNumberHelper poNumberHelper;

  PutOrdersByIdHelper(Map<String, String> okapiHeaders,
                      Handler<AsyncResult<Response>> asyncResultHandler, Context ctx, String lang) {
    super(getHttpClient(okapiHeaders), okapiHeaders, asyncResultHandler, ctx, lang);
    setDefaultHeaders(httpClient);
    putLineHelper = new PutOrderLineByIdHelper(httpClient, okapiHeaders, asyncResultHandler, ctx, lang);
    postOrderLineHelper = new PostOrderLineHelper(httpClient, okapiHeaders, asyncResultHandler, ctx, lang);
    poNumberHelper = new PoNumberHelper(httpClient, okapiHeaders, asyncResultHandler, ctx, lang);

  }

  /**
   * Handles update of the order. First retrieve the PO from storage and depending on its content handle passed PO.
   */
  public void updateOrder(String orderId, CompositePurchaseOrder compPO) {
    compPO.setId(orderId);
    getPurchaseOrderById(orderId, lang, httpClient, ctx, okapiHeaders, logger)
      .thenCompose(poFromStorage -> validatePoNumber(compPO, poFromStorage))
      .thenCompose(poFromStorage -> {
        if (isTransitionToOpen(compPO, poFromStorage)) {
          return openOrder(compPO);
        } else {
          return updateOrderSummary(compPO);
        }
      })
      .thenCompose(v -> getPurchaseOrderById(orderId, lang, httpClient, ctx, okapiHeaders, logger))
      .thenCompose(poFromStorage -> updatePoLines(poFromStorage, compPO))
      .thenAccept(v -> {
        logger.info("Successfully Updated Order: " + JsonObject.mapFrom(compPO).encodePrettily());
        httpClient.closeClient();
        javax.ws.rs.core.Response response = PutOrdersCompositeOrdersByIdResponse.respond204();
        AsyncResult<javax.ws.rs.core.Response> result = Future.succeededFuture(response);
        asyncResultHandler.handle(result);
      })
      .exceptionally(this::handleError);
  }

  /**
   * Handles transition of given order to OPEN status.
   *
   * @param compPO Purchase Order to open
   * @return CompletableFuture that indicates when transition is completed
   */
  public CompletableFuture<Void> openOrder(CompositePurchaseOrder compPO) {
    compPO.setWorkflowStatus(OPEN);
    compPO.setDateOrdered(new Date());
    return updateInventory(compPO)
      .thenCompose(this::updateOrderSummary);
  }

  public CompletableFuture<CompositePurchaseOrder> updateInventory(CompositePurchaseOrder compPO) {
    CompletableFuture<List<CompositePoLine>> compositePoLines;
    if (isEmpty(compPO.getCompositePoLines())) {
      compositePoLines = HelperUtils.getCompositePoLines(compPO.getId(), lang, httpClient, ctx, okapiHeaders, logger);
    } else {
      compPO.getCompositePoLines().forEach(poLine -> poLine.setReceiptStatus(CompositePoLine.ReceiptStatus.AWAITING_RECEIPT));
      compositePoLines = completedFuture(compPO.getCompositePoLines());
    }

    return compositePoLines
      .thenApply(poLines -> poLines.stream().peek(poline -> {
        poline.setReceiptStatus(CompositePoLine.ReceiptStatus.AWAITING_RECEIPT);
      }))
      .thenCompose(poLines -> CompletableFuture.allOf(poLines.map(putLineHelper::updateInventory).toArray(CompletableFuture[]::new)))
      .thenApply(v -> compPO);
  }

  private CompletionStage<JsonObject> validatePoNumber(CompositePurchaseOrder compPO, JsonObject poFromStorage) {
    if (isPoNumberChanged(poFromStorage, compPO)) {
      return poNumberHelper
        .checkPONumberUnique(compPO.getPoNumber())
        .thenApply(v -> poFromStorage);
    }
    return completedFuture(poFromStorage);
  }

  private boolean isTransitionToOpen(CompositePurchaseOrder compPO, JsonObject poFromStorage) {
    WorkflowStatus currentStatus = WorkflowStatus.fromValue(poFromStorage.getString("workflow_status"));
    return currentStatus == PENDING && compPO.getWorkflowStatus() == OPEN;
  }

  private boolean isPoNumberChanged(JsonObject poFromStorage, CompositePurchaseOrder compPO) {
    String oldPoNumber = poFromStorage.getString(PO_NUMBER);
    return !StringUtils.equalsIgnoreCase(oldPoNumber, compPO.getPoNumber());
  }

  private CompletableFuture<Void> updatePoLines(JsonObject poFromStorage, CompositePurchaseOrder compPO) {
    if (isNotEmpty(compPO.getCompositePoLines()) || isPoNumberChanged(poFromStorage, compPO)) {
      return getPoLines(poFromStorage.getString(ID), lang, httpClient, ctx, okapiHeaders, logger)
        .thenCompose(jsonObject -> {
          JsonArray existedPoLinesArray = jsonObject.getJsonArray(PO_LINES);
          if (isNotEmpty(compPO.getCompositePoLines())) {
            return handlePoLines(compPO, existedPoLinesArray);
          } else {
            return updatePoLinesNumber(compPO, existedPoLinesArray);
          }
        });
    } else {
      return completedFuture(null);
    }
  }

  private CompletableFuture<Void> updateOrderSummary(CompositePurchaseOrder compPO) {
    logger.debug("Updating order...");
    JsonObject purchaseOrder = convertToPurchaseOrder(compPO);
    return operateOnSubObj(HttpMethod.PUT, resourceByIdPath(PURCHASE_ORDER, compPO.getId()), purchaseOrder, httpClient, ctx, okapiHeaders, logger)
      .thenAccept(json -> {});
  }

  private CompletableFuture<Void> updatePoLinesNumber(CompositePurchaseOrder compOrder, JsonArray poLinesFromStorage) {
    CompletableFuture[] futures = poLinesFromStorage.stream()
      .map(o -> {
        JsonObject lineFromStorage = (JsonObject) o;
        lineFromStorage.put(PO_LINE_NUMBER, buildNewPoLineNumber(lineFromStorage, compOrder.getPoNumber()));
        return putLineHelper
          .updateOrderLineSummary(lineFromStorage.getString(ID), lineFromStorage);
      })
      .toArray(CompletableFuture[]::new);

    return VertxCompletableFuture.allOf(ctx, futures);
  }

  private CompletableFuture<Void> handlePoLines(CompositePurchaseOrder compOrder, JsonArray poLinesFromStorage) {
    List<CompletableFuture<?>> futures = new ArrayList<>();
    if (poLinesFromStorage.isEmpty()) {
      futures.addAll(processPoLinesCreation(compOrder, poLinesFromStorage));
    } else {
      futures.addAll(processPoLinesCreation(compOrder, poLinesFromStorage));
      futures.addAll(processPoLinesUpdate(compOrder, poLinesFromStorage));
      // The remaining unprocessed PoLines should be removed
      poLinesFromStorage.stream().forEach(poLine -> futures.add(deletePoLine((JsonObject) poLine, httpClient, ctx, okapiHeaders, logger)));
    }
    return VertxCompletableFuture.allOf(ctx, futures.toArray(new CompletableFuture[0]));
  }

  private List<CompletableFuture<?>> processPoLinesUpdate(CompositePurchaseOrder compOrder, JsonArray poLinesFromStorage) {
    List<CompletableFuture<?>> futures = new ArrayList<>();
    Iterator<Object> iterator = poLinesFromStorage.iterator();
    while (iterator.hasNext()) {
      JsonObject lineFromStorage = (JsonObject) iterator.next();
      for (CompositePoLine line : compOrder.getCompositePoLines()) {
        if (StringUtils.equals(lineFromStorage.getString(ID), line.getId())) {
          line.setPoLineNumber(buildNewPoLineNumber(lineFromStorage, compOrder.getPoNumber()));
          futures.add(putLineHelper.updateOrderLine(line, lineFromStorage));
          iterator.remove();
          break;
        }
      }
    }
    return futures;
  }

  private List<CompletableFuture<?>> processPoLinesCreation(CompositePurchaseOrder compOrder, JsonArray poLinesFromStorage) {
    return  compOrder.getCompositePoLines().stream().filter(poLine ->
      poLinesFromStorage.stream()
        .map(o -> ((JsonObject) o).getString(ID))
        .noneMatch(s -> StringUtils.equals(s, poLine.getId()))
    ).map(compPOL -> {
      compPOL.setPoLineNumber(compOrder.getPoNumber());
      return postOrderLineHelper.createPoLine(compPOL);
    })
      .collect(toList());
  }

  private String buildNewPoLineNumber(JsonObject poLineFromStorage, String poNumber) {
    String oldPoLineNumber = poLineFromStorage.getString(PO_LINE_NUMBER);
    Matcher matcher = PO_LINE_NUMBER_PATTERN.matcher(oldPoLineNumber);
    if (matcher.find()) {
      return poNumber + matcher.group(2);
    }
    logger.error("PO Line - {} has invalid or missing number.", poLineFromStorage.getString(ID));
    return oldPoLineNumber;
  }

  @Override
  Response buildErrorResponse(int code, Error error) {
    final Response result;
    switch (code) {
      case 400:
        result = PutOrdersCompositeOrdersByIdResponse.respond400WithApplicationJson(withErrors(error));
        break;
      case 404:
        result = PutOrdersCompositeOrdersByIdResponse.respond404WithApplicationJson(withErrors(error));
        break;
      case 422:
        result = PutOrdersCompositeOrdersByIdResponse.respond422WithApplicationJson(withErrors(error));
        break;
      default:
        if (putLineHelper.getProcessingErrors().isEmpty()) {
          result = PutOrdersCompositeOrdersByIdResponse.respond500WithApplicationJson(withErrors(error));
        } else {
          Errors processingErrors = new Errors();
          processingErrors.getErrors().addAll(putLineHelper.getProcessingErrors());
          processingErrors.getErrors().add(error);
          result = PutOrdersCompositeOrdersByIdResponse.respond500WithApplicationJson(processingErrors);
        }
    }
    return result;
  }
}
