package org.folio.rest.impl;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static java.util.stream.Collectors.toList;
import static org.apache.commons.collections4.CollectionUtils.isEmpty;
import static org.apache.commons.collections4.CollectionUtils.isNotEmpty;
import static org.folio.orders.utils.HelperUtils.COMPOSITE_PO_LINES;
import static org.folio.orders.utils.HelperUtils.calculateTotalEstimatedPrice;
import static org.folio.orders.utils.HelperUtils.deletePoLine;
import static org.folio.orders.utils.HelperUtils.deletePoLines;
import static org.folio.orders.utils.HelperUtils.getCompositePoLines;
import static org.folio.orders.utils.HelperUtils.getEndpointWithQuery;
import static org.folio.orders.utils.HelperUtils.getPoLineLimit;
import static org.folio.orders.utils.HelperUtils.getPoLines;
import static org.folio.orders.utils.HelperUtils.getPurchaseOrderById;
import static org.folio.orders.utils.HelperUtils.handleGetRequest;
import static org.folio.orders.utils.HelperUtils.operateOnObject;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINE_NUMBER;
import static org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.OPEN;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.PENDING;

import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.ErrorCodes;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.jaxrs.model.*;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus;

import io.vertx.core.Context;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;
import org.folio.rest.jaxrs.model.Error;

public class PurchaseOrderHelper extends AbstractHelper {

  private static final String GET_PURCHASE_ORDERS_BY_QUERY = resourcesPath(PURCHASE_ORDER) + "?limit=%s&offset=%s%s&lang=%s";

  private final PoNumberHelper poNumberHelper;
  private final PurchaseOrderLineHelper orderLineHelper;

  PurchaseOrderHelper(Map<String, String> okapiHeaders, Context ctx, String lang) {
    super(getHttpClient(okapiHeaders), okapiHeaders, ctx, lang);

    poNumberHelper = new PoNumberHelper(httpClient, okapiHeaders, ctx, lang);
    orderLineHelper = new PurchaseOrderLineHelper(httpClient, okapiHeaders, ctx, lang);
  }

  /**
   * Retrieve a list of {@link PurchaseOrder} objects retrieved from storage by provided query.
   *
   * @param limit limit the number of elements returned in the response
   * @param offset skip over a number of elements by specifying an offset value for the query
   * @param query A query expressed as a CQL string using valid searchable fields.
   * @return completable future with {@link PurchaseOrders} object on success or an exception if processing fails
   */
  public CompletableFuture<PurchaseOrders> getPurchaseOrders(int limit, int offset, String query) {
    CompletableFuture<PurchaseOrders> future = new VertxCompletableFuture<>(ctx);

    try {
      String queryParam = getEndpointWithQuery(query, logger);
      String endpoint = String.format(GET_PURCHASE_ORDERS_BY_QUERY, limit, offset, queryParam, lang);
      handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
        .thenAccept(jsonOrders -> {
          logger.info("Successfully retrieved orders: " + jsonOrders.encodePrettily());
          future.complete(jsonOrders.mapTo(PurchaseOrders.class));
        })
        .exceptionally(t -> {
          logger.error("Error getting orders", t);
          future.completeExceptionally(t);
          return null;
        });
    } catch (Exception e) {
      future.completeExceptionally(e);
    }
    return future;
  }

  /**
   * Create a purchase order (PO) and a number of PO lines if provided.
   * @param compPO {@link CompositePurchaseOrder} object representing Purchase Order and optionally Purchase Order Line details.
   * @return completable future with {@link CompositePurchaseOrder} object with populated uuid on success or an exception if processing fails
   */
  public CompletableFuture<CompositePurchaseOrder> createPurchaseOrder(CompositePurchaseOrder compPO) {
    return setPoNumberIfMissing(compPO)
      .thenCompose(v -> poNumberHelper.checkPONumberUnique(compPO.getPoNumber()))
      .thenCompose(v -> createPOandPOLines(compPO))
      .thenApply(this::populateOrderSummary);
  }

  /**
   * Create fund transactions corresponding to the order
   * @param compPO {@link CompositePurchaseOrder} object representing Purchase Order and optionally Purchase Order Line details.
   * @return completable future with {@link CompositePurchaseOrder}
   */
  public CompletableFuture<CompositePurchaseOrder> applyFunds(CompositePurchaseOrder compPO) {
    CompletableFuture<CompositePurchaseOrder> future = new VertxCompletableFuture<>(ctx);
    future.complete(compPO);
    return future;
  }

  /**
   * Handles update of the order. First retrieve the PO from storage and depending on its content handle passed PO.
   * @param compPO updated {@link CompositePurchaseOrder} purchase order
   * @return completable future holding response indicating success (204 No Content) or error if failed
   */
  public CompletableFuture<Void> updateOrder(CompositePurchaseOrder compPO) {
    return getPurchaseOrderById(compPO.getId(), lang, httpClient, ctx, okapiHeaders, logger)
      .thenApply(HelperUtils::convertToCompositePurchaseOrder)
      .thenCompose(poFromStorage -> {
        logger.info("Order successfully retrieved from storage");
        return validatePoNumber(poFromStorage, compPO)
          .thenCompose(v -> updatePoLines(poFromStorage, compPO))
          .thenCompose(v -> {
            if (isTransitionToOpen(poFromStorage, compPO)) {
              return openOrder(compPO);
            } else {
              return updateOrderSummary(compPO);
            }
          });
        }
      );
  }

  /**
   * Delete a purchase order with given uuid. As a first step the logic deletes all associated PO Lines and then order.
   * @param id purchase order id
   * @return completable future which is just completed with nothing on success or an exception if processing fails
   */
  public CompletableFuture<Void> deleteOrder(String id) {
    CompletableFuture<Void> future = new VertxCompletableFuture<>(ctx);

    deletePoLines(id, lang, httpClient, ctx, okapiHeaders, logger)
      .thenRun(() -> {
        logger.info("Successfully deleted poLines, proceeding with purchase order");
        operateOnObject(HttpMethod.DELETE, resourceByIdPath(PURCHASE_ORDER, id), httpClient, ctx, okapiHeaders, logger)
          .thenAccept(rs -> {
            logger.info("Successfully deleted order with id={}", id);
            future.complete(null);
          })
          .exceptionally(t -> {
            logger.error("Failed to delete PO", t);
            future.completeExceptionally(t);
            return null;
          });
      })
      .exceptionally(t -> {
        logger.error("Failed to delete PO Lines", t);
        future.completeExceptionally(t);
        return null;
      });

    return future;
  }

  /**
   * Gets purchase order by id
   *
   * @param id purchase order uuid
   * @return completable future with {@link CompositePurchaseOrder} on success or an exception if processing fails
   */
  public CompletableFuture<CompositePurchaseOrder> getCompositeOrder(String id) {
    CompletableFuture<CompositePurchaseOrder> future = new VertxCompletableFuture<>(ctx);

    getPurchaseOrderById(id, lang, httpClient, ctx, okapiHeaders, logger)
      .thenAccept(po -> {
        if (logger.isInfoEnabled()) {
          logger.info("got: " + po.encodePrettily());
        }
        CompositePurchaseOrder compPO = HelperUtils.convertToCompositePurchaseOrder(po);

        getCompositePoLines(id, lang, httpClient, ctx, okapiHeaders, logger)
          .thenApply(compPO::withCompositePoLines)
          .thenApply(this::populateOrderSummary)
          .thenAccept(future::complete)
          .exceptionally(t -> {
            logger.error("Failed to get POLines", t);
            future.completeExceptionally(t);
            return null;
          });
      })
      .exceptionally(t -> {
        logger.error("Failed to build composite purchase order", t.getCause());
        future.completeExceptionally(t);
        return null;
      });

    return future;
  }

  private CompositePurchaseOrder populateOrderSummary(CompositePurchaseOrder compPO) {
    List<CompositePoLine> compositePoLines = compPO.getCompositePoLines();
    compPO.setTotalEstimatedPrice(calculateTotalEstimatedPrice(compositePoLines));
    compPO.setTotalItems(calculateTotalItemsQuantity(compositePoLines));

    return compPO;
  }

  private int calculateTotalItemsQuantity(List<CompositePoLine> poLines) {
    return poLines.stream().mapToInt(HelperUtils::calculateTotalQuantity).sum();
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
    return fetchCompositePoLines(compPO)
      .thenCompose(this::updateInventory)
      .thenCompose(v -> updateOrderSummary(compPO))
      .thenAccept(v -> changePoLineStatuses(compPO))
      .thenCompose(v -> updateCompositePoLines(compPO));
  }

  /**
   * Sets the tenant default values and validates the order. Checks if Orders has
   * PO Lines within limit and validates vendors and access providers.
   *
   * @param compPO
   *          Purchase Order to validate
   * @return completable future which might be completed with {@code true} if
   *         order is valid, {@code false} if not valid or an exception if
   *         processing fails
   */
  public CompletableFuture<Boolean> validateOrder(CompositePurchaseOrder compPO) {

    return setCreateInventoryDefaultValues(compPO)
      .thenAccept(v -> addProcessingErrors(HelperUtils.validateOrder(compPO)))
      .thenCompose(v -> validatePoLineLimit(compPO))
      .thenCompose(isLimitValid -> {
        if (!getErrors().isEmpty()) {
          return completedFuture(false);
        }
        if (isLimitValid) {
          return validateVendor(compPO);
        }

        return completedFuture(isLimitValid);
      });

  }

  CompletableFuture<Void> setCreateInventoryDefaultValues(CompositePurchaseOrder compPO) {
    CompletableFuture[] futures = compPO.getCompositePoLines()
      .stream()
      .map(orderLineHelper::setTenantDefaultCreateInventoryValues)
      .toArray(CompletableFuture[]::new);

    return VertxCompletableFuture.allOf(ctx, futures);
  }

  /**
   * Validates purchase order which already exists in the storage.
   * Checks PO Number presence, validates that provided order id corresponds to one set in order and its lines.
   * If all is okay, {@link #validateOrder(CompositePurchaseOrder)} is called afterwards.
   * @param orderId Purchase Order id
   * @param compPO Purchase Order to validate
   * @return completable future which might be completed with {@code true} if order is valid, {@code false} if not valid or an exception if processing fails
   */
  public CompletableFuture<Boolean> validateExistingOrder(String orderId, CompositePurchaseOrder compPO) {
    // The PO Number is required for existing orders
    if (StringUtils.isEmpty(compPO.getPoNumber())) {
      addProcessingError(ErrorCodes.PO_NUMBER_REQUIRED.toError());
    }

    // Validate order uuid
    if (!compPO.getId().equals(orderId)) {
      addProcessingError(ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY.toError());
    } else if (CollectionUtils.isNotEmpty(compPO.getCompositePoLines())) {
      // Validate that each PO Line has correct order id
      compPO.getCompositePoLines().forEach(poLine -> {
        if (!orderId.equals(poLine.getPurchaseOrderId())) {
          Error error = ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY.toError();
          if (StringUtils.isNotEmpty(poLine.getPoLineNumber())) {
            error.getParameters()
                 .add(new Parameter().withKey(PO_LINE_NUMBER)
                                     .withValue(poLine.getPoLineNumber()));
          }
          addProcessingError(error);
        }
      });
    }

    return validateOrder(compPO);
  }

  @Override
  protected Errors getProcessingErrors() {
    addProcessingErrors(orderLineHelper.getErrors());
    return super.getProcessingErrors();
  }

  private CompletableFuture<Void> setPoNumberIfMissing(CompositePurchaseOrder compPO) {
    if (null == compPO.getPoNumber()) {
      return poNumberHelper.generatePoNumber()
                           .thenAccept(compPO::setPoNumber);
    }
    return completedFuture(null);
  }

  private CompletableFuture<Boolean> validateVendor(CompositePurchaseOrder compPO) {
    if (compPO.getWorkflowStatus() == WorkflowStatus.OPEN) {
      VendorHelper vendorHelper = new VendorHelper(okapiHeaders, ctx, lang);
      return fetchCompositePoLines(compPO)
        .thenCompose(vendorHelper::validateVendor)
        .thenCompose(errors -> {
          addProcessingErrors(errors.getErrors());
          return vendorHelper.validateAccessProviders(compPO);
        })
        .thenApply(errors -> {
          addProcessingErrors(errors.getErrors());
          return getErrors().isEmpty();
        });
    }
    return completedFuture(true);
  }

  private CompletableFuture<Boolean> validatePoLineLimit(CompositePurchaseOrder compPO) {
    if (CollectionUtils.isNotEmpty(compPO.getCompositePoLines())) {
      CompletableFuture<Boolean> future = new VertxCompletableFuture<>(ctx);
      getTenantConfiguration()
        .thenAccept(config -> {
          int limit = getPoLineLimit(config);
          if (compPO.getCompositePoLines().size() > limit) {
            addProcessingError(ErrorCodes.POL_LINES_LIMIT_EXCEEDED.toError());
          }
          future.complete(getErrors().isEmpty());
        })
        .exceptionally(t -> {
          future.completeExceptionally(t);
          return null;
        });

      return future;
    }
    return completedFuture(true);
  }

  private CompletableFuture<CompositePurchaseOrder> createPOandPOLines(CompositePurchaseOrder compPO) {
    final WorkflowStatus finalStatus = compPO.getWorkflowStatus();

    // we should always create PO and PO lines in PENDING status and transition to OPEN only when it's all set
    // (e.g. PO lines are created, Inventory is updated, etc.)
    if (finalStatus == OPEN) {
      compPO.setWorkflowStatus(PENDING);
    }

    return createRecordInStorage(convertToPurchaseOrder(compPO), resourcesPath(PURCHASE_ORDER))
      .thenApply(compPO::withId)
      .thenCompose(this::createPoLines)
      .thenAccept(compPO::setCompositePoLines)
      .thenCompose(v -> {
        if (finalStatus == OPEN) {
          return openOrder(compPO);
        }
        return completedFuture(null);
      })
      .thenApply(v -> compPO);
  }

  private CompletableFuture<List<CompositePoLine>> createPoLines(CompositePurchaseOrder compPO) {
    List<CompletableFuture<CompositePoLine>> futures =
      compPO.getCompositePoLines()
            .stream()
            .map(compositePoLine -> orderLineHelper.createPoLine(compositePoLine, compPO))
            .collect(Collectors.toList());
    return HelperUtils.collectResultsOnSuccess(futures);
  }

  private CompletableFuture<CompositePurchaseOrder> fetchCompositePoLines(CompositePurchaseOrder compPO) {
    if (isEmpty(compPO.getCompositePoLines())) {
      return getCompositePoLines(compPO.getId(), lang, httpClient, ctx, okapiHeaders, logger)
        .thenApply(compPO::withCompositePoLines);
    }
    return completedFuture(compPO);
  }

  private void changePoLineStatuses(CompositePurchaseOrder compPO) {
    compPO.getCompositePoLines().forEach(poLine -> {
      changeReceiptStatus(poLine);
      changePaymentStatus(poLine);
    });
  }

  private void changePaymentStatus(CompositePoLine poLine) {
    if (poLine.getPaymentStatus() == CompositePoLine.PaymentStatus.PENDING) {
      poLine.setPaymentStatus(CompositePoLine.PaymentStatus.AWAITING_PAYMENT);
    }
  }

  private void changeReceiptStatus(CompositePoLine poLine) {
    if (poLine.getReceiptStatus() == CompositePoLine.ReceiptStatus.PENDING) {
      poLine.setReceiptStatus(CompositePoLine.ReceiptStatus.AWAITING_RECEIPT);
    }
  }

  private CompletionStage<Void> validatePoNumber(CompositePurchaseOrder poFromStorage, CompositePurchaseOrder updatedPo) {
    if (isPoNumberChanged(poFromStorage, updatedPo)) {
      return poNumberHelper.checkPONumberUnique(updatedPo.getPoNumber());
    }
    return completedFuture(null);
  }

  private boolean isTransitionToOpen(CompositePurchaseOrder poFromStorage, CompositePurchaseOrder compPO) {
    return poFromStorage.getWorkflowStatus() == PENDING && compPO.getWorkflowStatus() == OPEN;
  }

  private boolean isPoNumberChanged(CompositePurchaseOrder poFromStorage, CompositePurchaseOrder updatedPo) {
    return !StringUtils.equalsIgnoreCase(poFromStorage.getPoNumber(), updatedPo.getPoNumber());
  }

  private CompletableFuture<Void> updatePoLines(CompositePurchaseOrder poFromStorage, CompositePurchaseOrder compPO) {
    if (isPoLinesUpdateRequired(poFromStorage, compPO)) {
      return getPoLines(poFromStorage.getId(), lang, httpClient, ctx, okapiHeaders, logger)
        .thenCompose(existedPoLinesArray -> {
          if (isNotEmpty(compPO.getCompositePoLines())) {
            // New PO Line(s) can be added only to Pending order
            if (poFromStorage.getWorkflowStatus() != PENDING && hasNewPoLines(compPO, existedPoLinesArray)) {
              throw new HttpException(422, poFromStorage.getWorkflowStatus() == OPEN ? ErrorCodes.ORDER_OPEN : ErrorCodes.ORDER_CLOSED);
            }
            return handlePoLines(compPO, existedPoLinesArray);
          } else {
            return updatePoLinesNumber(compPO, existedPoLinesArray);
          }
        });
    }
    return completedFuture(null);
  }

  private boolean isPoLinesUpdateRequired(CompositePurchaseOrder poFromStorage, CompositePurchaseOrder compPO) {
    return isNotEmpty(compPO.getCompositePoLines()) || isPoNumberChanged(poFromStorage, compPO);
  }

  private CompletableFuture<Void> updateCompositePoLines(CompositePurchaseOrder compPO) {
    if (isNotEmpty(compPO.getCompositePoLines())) {
      return getPoLines(compPO.getId(), lang, httpClient, ctx, okapiHeaders, logger)
        .thenCompose(existedPoLinesArray -> handlePoLines(compPO, existedPoLinesArray));
    } else {
      return completedFuture(null);
    }
  }

  private CompletableFuture<Void> updateOrderSummary(CompositePurchaseOrder compPO) {
    logger.debug("Updating order...");
    JsonObject purchaseOrder = convertToPurchaseOrder(compPO);
    return HelperUtils.operateOnObject(HttpMethod.PUT, resourceByIdPath(PURCHASE_ORDER, compPO.getId()), purchaseOrder, httpClient, ctx, okapiHeaders, logger)
                      .thenAccept(json -> {
      });
  }

  /**
   * Convert {@link CompositePurchaseOrder} to Json representation of PurchaseOrder.
   * These objects are the same except PurchaseOrder doesn't contain poLines field.
   * @param compPO {@link CompositePurchaseOrder}
   * @return JsonObject representation of PurchaseOrder
   */
  private JsonObject convertToPurchaseOrder(CompositePurchaseOrder compPO) {
    // Remove dynamically calculated data
    compPO.setTotalEstimatedPrice(null);
    compPO.setTotalItems(null);

    JsonObject purchaseOrder = JsonObject.mapFrom(compPO);
    purchaseOrder.remove(COMPOSITE_PO_LINES);
    return purchaseOrder;
  }

  private CompletableFuture<Void> updatePoLinesNumber(CompositePurchaseOrder compOrder, JsonArray poLinesFromStorage) {
    CompletableFuture[] futures = poLinesFromStorage
      .stream()
      .map(o -> {
        JsonObject lineFromStorage = (JsonObject) o;
        lineFromStorage.put(PO_LINE_NUMBER, orderLineHelper.buildNewPoLineNumber(lineFromStorage, compOrder.getPoNumber()));
        return orderLineHelper
          .updateOrderLineSummary(lineFromStorage.getString(ID), lineFromStorage);
      })
       .toArray(CompletableFuture[]::new);

    return VertxCompletableFuture.allOf(ctx, futures);
  }

  private CompletableFuture<Void> updateInventory(CompositePurchaseOrder compPO) {
    return CompletableFuture.allOf(
      compPO.getCompositePoLines()
            .stream()
            .map(orderLineHelper::updateInventory)
            .toArray(CompletableFuture[]::new)
    );
  }

  private CompletableFuture<Void> handlePoLines(CompositePurchaseOrder compOrder, JsonArray poLinesFromStorage) {
    List<CompletableFuture<?>> futures = new ArrayList<>(processPoLinesCreation(compOrder, poLinesFromStorage));
    if (!poLinesFromStorage.isEmpty()) {
      futures.addAll(processPoLinesUpdate(compOrder, poLinesFromStorage));
      // The remaining unprocessed PoLines should be removed
      poLinesFromStorage.forEach(poLine -> futures.add(deletePoLine((JsonObject) poLine, httpClient, ctx, okapiHeaders, logger)));
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
          line.setPoLineNumber(orderLineHelper.buildNewPoLineNumber(lineFromStorage, compOrder.getPoNumber()));
          futures.add(orderLineHelper.updateOrderLine(line, lineFromStorage));
          iterator.remove();
          break;
        }
      }
    }
    return futures;
  }

  private List<CompletableFuture<CompositePoLine>> processPoLinesCreation(CompositePurchaseOrder compOrder, JsonArray poLinesFromStorage) {
    return getNewPoLines(compOrder, poLinesFromStorage)
      .map(compPOL -> orderLineHelper.createPoLine(compPOL, compOrder))
      .collect(toList());
  }

  private boolean hasNewPoLines(CompositePurchaseOrder compPO, JsonArray poLinesFromStorage) {
    return getNewPoLines(compPO, poLinesFromStorage).count() > 0;
  }

  private Stream<CompositePoLine> getNewPoLines(CompositePurchaseOrder compPO, JsonArray poLinesFromStorage) {
    List<String> lineIdsInStorage = poLinesFromStorage
      .stream()
      .map(o -> ((JsonObject) o).getString(ID))
      .collect(toList());

    return compPO.getCompositePoLines()
      .stream()
      .filter(poLine -> !lineIdsInStorage.contains(poLine.getId()));
  }
}
