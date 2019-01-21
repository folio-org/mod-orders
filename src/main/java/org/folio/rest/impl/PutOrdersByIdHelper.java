package org.folio.rest.impl;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;
import org.apache.commons.lang.StringUtils;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.resource.Orders.PutOrdersCompositeOrdersByIdResponse;

import javax.ws.rs.core.Response;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static java.util.stream.Collectors.toList;
import static org.apache.commons.collections4.CollectionUtils.isEmpty;
import static org.apache.commons.collections4.CollectionUtils.isNotEmpty;
import static org.folio.orders.utils.HelperUtils.deletePoLine;
import static org.folio.orders.utils.HelperUtils.getPoLines;
import static org.folio.orders.utils.HelperUtils.getPurchaseOrderById;
import static org.folio.orders.utils.HelperUtils.operateOnSubObj;
import static org.folio.orders.utils.SubObjects.PO_LINES;
import static org.folio.orders.utils.SubObjects.PURCHASE_ORDER;
import static org.folio.orders.utils.SubObjects.resourceByIdPath;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.OPEN;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.PENDING;

public class PutOrdersByIdHelper extends AbstractHelper {

  private static final Pattern PO_LINE_NUMBER_PATTERN = Pattern.compile("([a-zA-Z0-9]{5,16})(-[0-9]{1,3})");

  private final PutOrderLineByIdHelper putLineHelper;
  private final PostOrderLineHelper postOrderLineHelper;
  private final ValidationHelper validationHelper;

  PutOrdersByIdHelper(Map<String, String> okapiHeaders,
                             Handler<AsyncResult<Response>> asyncResultHandler, Context ctx, String lang) {
    super(getHttpClient(okapiHeaders), okapiHeaders, asyncResultHandler, ctx, lang);
    setDefaultHeaders(httpClient);
    putLineHelper = new PutOrderLineByIdHelper(httpClient, okapiHeaders, asyncResultHandler, ctx, lang);
    postOrderLineHelper = new PostOrderLineHelper(httpClient, okapiHeaders, asyncResultHandler, ctx, lang);
    validationHelper = new ValidationHelper(httpClient, okapiHeaders, asyncResultHandler, ctx, lang);
  }

  /**
   * Handles update of the order. First retrieve the PO from storage and depending on its content handle passed PO.
   */
  public void updateOrder(String orderId, CompositePurchaseOrder compPO) {
    compPO.setId(orderId);
    getPurchaseOrderById(orderId, lang, httpClient, ctx, okapiHeaders, logger)
      .thenCompose(poFromStorage -> validatePoNumber(compPO, poFromStorage))
      .thenCompose(poFromStorage -> updatePoLines(poFromStorage, compPO).thenApply(v -> poFromStorage))
      .thenAccept(poFromStorage -> {
        CompletableFuture<Void> updatedPoFuture;
        if (isTransitionToOpen(compPO, poFromStorage)) {
          updatedPoFuture = openOrder(compPO);
        } else {
          updatedPoFuture = updateOrderSummary(compPO);
        }
        updatedPoFuture
          .thenAccept(v -> {
            logger.info("Successfully Updated Order: " + JsonObject.mapFrom(compPO).encodePrettily());
            httpClient.closeClient();
            javax.ws.rs.core.Response response = PutOrdersByIdResponse.respond204();
            AsyncResult<javax.ws.rs.core.Response> result = Future.succeededFuture(response);
            asyncResultHandler.handle(result);
          })
        .exceptionally(this::handleError);
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
    return updateInventory(compPO)
      .thenCompose(this::updateOrderSummary);
  }

  private CompletionStage<JsonObject> validatePoNumber(CompositePurchaseOrder compPO, JsonObject poFromStorage) {
    if (isPoNumberChanged(poFromStorage, compPO)) {
      return validationHelper
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
    if (isNotEmpty(compPO.getPoLines()) || isPoNumberChanged(poFromStorage, compPO)) {
      return getPoLines(poFromStorage.getString(ID), lang, httpClient, ctx, okapiHeaders, logger)
        .thenCompose(jsonObject -> {
          JsonArray existedPoLinesArray = jsonObject.getJsonArray(PO_LINES);
          if (isNotEmpty(compPO.getPoLines())) {
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

  public CompletableFuture<CompositePurchaseOrder> updateInventory(CompositePurchaseOrder compPO) {
    CompletableFuture<List<PoLine>> compositePoLines;
    if (isEmpty(compPO.getPoLines())) {
      compositePoLines = HelperUtils.getCompositePoLines(compPO.getId(), lang, httpClient, ctx, okapiHeaders, logger);
    } else {
      compositePoLines = completedFuture(compPO.getPoLines());
    }

    return compositePoLines
      .thenCompose(poLines -> {
        List<CompletableFuture<Void>> futures = poLines.stream()
          .map(putLineHelper::updateInventory)
          .collect(toList());
        return CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]))
          .thenApply(v -> compPO);
      });
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
      for (PoLine line : compOrder.getPoLines()) {
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
    return  compOrder.getPoLines().stream().filter(poLine ->
      poLinesFromStorage.stream()
        .map(o -> ((JsonObject) o).getString(ID))
        .noneMatch(s -> StringUtils.equals(s, poLine.getId()))
    ).map(postOrderLineHelper::createPoLine)
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
        result = PutOrdersCompositeOrdersByIdResponse.respond400WithTextPlain(error.getMessage());
        break;
      case 404:
        result = PutOrdersCompositeOrdersByIdResponse.respond404WithTextPlain(error.getMessage());
        break;
      case 422:
        result = PutOrdersCompositeOrdersByIdResponse.respond422WithApplicationJson(withErrors(error));
        break;
      default:
        if (putLineHelper.getProcessingErrors().isEmpty()) {
          result = PutOrdersCompositeOrdersByIdResponse.respond500WithTextPlain(error.getMessage());
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
