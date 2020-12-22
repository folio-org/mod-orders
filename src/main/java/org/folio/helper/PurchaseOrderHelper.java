package org.folio.helper;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static java.util.stream.Collectors.toList;
import static org.apache.commons.collections4.CollectionUtils.isEmpty;
import static org.apache.commons.collections4.CollectionUtils.isNotEmpty;
import static org.folio.orders.utils.AcqDesiredPermissions.ASSIGN;
import static org.folio.orders.utils.AcqDesiredPermissions.MANAGE;
import static org.folio.orders.utils.ErrorCodes.APPROVAL_REQUIRED_TO_OPEN;
import static org.folio.orders.utils.ErrorCodes.MISSING_ONGOING;
import static org.folio.orders.utils.ErrorCodes.ONGOING_NOT_ALLOWED;
import static org.folio.orders.utils.ErrorCodes.USER_HAS_NO_ACQ_PERMISSIONS;
import static org.folio.orders.utils.ErrorCodes.USER_HAS_NO_APPROVAL_PERMISSIONS;
import static org.folio.orders.utils.ErrorCodes.USER_HAS_NO_UNOPEN_PERMISSIONS;
import static org.folio.orders.utils.HelperUtils.COMPOSITE_PO_LINES;
import static org.folio.orders.utils.HelperUtils.ORDER_CONFIG_MODULE_NAME;
import static org.folio.orders.utils.HelperUtils.WORKFLOW_STATUS;
import static org.folio.orders.utils.HelperUtils.buildQuery;
import static org.folio.orders.utils.HelperUtils.changeOrderStatus;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.HelperUtils.combineCqlExpressions;
import static org.folio.orders.utils.HelperUtils.convertIdsToCqlQuery;
import static org.folio.orders.utils.HelperUtils.convertToCompositePurchaseOrder;
import static org.folio.orders.utils.HelperUtils.deletePoLine;
import static org.folio.orders.utils.HelperUtils.deletePoLines;
import static org.folio.orders.utils.HelperUtils.encodeQuery;
import static org.folio.orders.utils.HelperUtils.getCompositePoLines;
import static org.folio.orders.utils.HelperUtils.getPoLineLimit;
import static org.folio.orders.utils.HelperUtils.getPoLines;
import static org.folio.orders.utils.HelperUtils.getPurchaseOrderById;
import static org.folio.orders.utils.HelperUtils.handleDeleteRequest;
import static org.folio.orders.utils.HelperUtils.handleGetRequest;
import static org.folio.orders.utils.HelperUtils.handlePutRequest;
import static org.folio.orders.utils.HelperUtils.verifyLocationsAndPiecesConsistency;
import static org.folio.orders.utils.HelperUtils.verifyProtectedFieldsChanged;
import static org.folio.orders.utils.OrderStatusTransitionUtil.isOrderClosing;
import static org.folio.orders.utils.OrderStatusTransitionUtil.isOrderReopening;
import static org.folio.orders.utils.OrderStatusTransitionUtil.isTransitionToApproved;
import static org.folio.orders.utils.OrderStatusTransitionUtil.isTransitionToOpen;
import static org.folio.orders.utils.OrderStatusTransitionUtil.isTransitionToPending;
import static org.folio.orders.utils.POProtectedFields.getFieldNames;
import static org.folio.orders.utils.POProtectedFields.getFieldNamesForOpenOrder;
import static org.folio.orders.utils.ProtectedOperationType.CREATE;
import static org.folio.orders.utils.ProtectedOperationType.DELETE;
import static org.folio.orders.utils.ProtectedOperationType.UPDATE;
import static org.folio.orders.utils.ResourcePathResolver.ENCUMBRANCES;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINE_NUMBER;
import static org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER;
import static org.folio.orders.utils.ResourcePathResolver.SEARCH_ORDERS;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_PERMISSIONS;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.OPEN;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.PENDING;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.CompletionStage;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.money.convert.ConversionQuery;
import javax.money.convert.ConversionQueryBuilder;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.ListUtils;
import org.apache.commons.lang3.StringUtils;
import org.folio.HttpStatus;
import org.folio.models.EncumbranceRelationsHolder;
import org.folio.models.EncumbrancesProcessingHolder;
import org.folio.models.PoLineFundHolder;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.AcqDesiredPermissions;
import org.folio.orders.utils.ErrorCodes;
import org.folio.orders.utils.HelperUtils;
import org.folio.orders.utils.POLineProtectedFields;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.orders.utils.validators.CompositePoLineValidationUtil;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.jaxrs.model.PurchaseOrderCollection;
import org.folio.rest.jaxrs.model.Title;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import org.folio.service.exchange.ExchangeRateProviderResolver;
import org.javamoney.moneta.Money;

import io.vertx.core.Context;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;
import one.util.streamex.StreamEx;

public class PurchaseOrderHelper extends AbstractHelper {
  static final String ENCUMBRANCE_POST_ENDPOINT = resourcesPath(ENCUMBRANCES) + "?lang=%s";
  private static final String PERMISSION_ORDER_APPROVE = "orders.item.approve";
  private static final String PERMISSION_ORDER_UNOPEN = "orders.item.unopen";
  private static final String SEARCH_ORDERS_BY_LINES_DATA = resourcesPath(SEARCH_ORDERS) + SEARCH_PARAMS;
  public static final String GET_PURCHASE_ORDERS = resourcesPath(PURCHASE_ORDER) + SEARCH_PARAMS;
  public static final String EMPTY_ARRAY = "[]";

  // Using variable to "cache" lines for particular order base on assumption that the helper is stateful and new instance is used
  private List<PoLine> orderLines;

  private final PoNumberHelper poNumberHelper;
  private final PurchaseOrderLineHelper orderLineHelper;
  private final ProtectionHelper protectionHelper;
  private final TitlesHelper titlesHelper;
  private final FinanceHelper financeHelper;
  private final InventoryHelper inventoryHelper;
  private final ExchangeRateProviderResolver exchangeRateProviderResolver;


  public PurchaseOrderHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders, Context ctx, String lang) {
    super(httpClient, okapiHeaders, ctx, lang);
    this.financeHelper = new FinanceHelper(getHttpClient(okapiHeaders), okapiHeaders, ctx, lang);
    this.poNumberHelper = new PoNumberHelper(httpClient, okapiHeaders, ctx, lang);
    this.orderLineHelper = new PurchaseOrderLineHelper(getHttpClient(okapiHeaders), okapiHeaders, ctx, lang);
    this.protectionHelper = new ProtectionHelper(httpClient, okapiHeaders, ctx, lang);
    this.titlesHelper = new TitlesHelper(httpClient, okapiHeaders, ctx, lang);
    this.inventoryHelper = new InventoryHelper(httpClient, okapiHeaders, ctx, lang);
    this.exchangeRateProviderResolver = new ExchangeRateProviderResolver();
  }

  public PurchaseOrderHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders, Context ctx, String lang,
      PoNumberHelper poNumberHelper, PurchaseOrderLineHelper orderLineHelper, FinanceHelper financeHelper) {
    super(httpClient, okapiHeaders, ctx, lang);
    this.financeHelper = financeHelper;
    this.poNumberHelper = poNumberHelper;
    this.orderLineHelper = orderLineHelper;
    this.protectionHelper =  new ProtectionHelper(httpClient, okapiHeaders, ctx, lang);
    this.titlesHelper = new TitlesHelper(httpClient, okapiHeaders, ctx, lang);
    this.exchangeRateProviderResolver = new ExchangeRateProviderResolver();
    this.inventoryHelper = new InventoryHelper(httpClient, okapiHeaders, ctx, lang);
  }

  public PurchaseOrderHelper(Map<String, String> okapiHeaders, Context ctx, String lang) {
    this(getHttpClient(okapiHeaders), okapiHeaders, ctx, lang);
  }

  /**
   * Retrieve a list of {@link PurchaseOrder} objects retrieved from storage by provided query.
   *
   * @param limit limit the number of elements returned in the response
   * @param offset skip over a number of elements by specifying an offset value for the query
   * @param query A query expressed as a CQL string using valid searchable fields.
   * @return completable future with {@link PurchaseOrderCollection} object on success or an exception if processing fails
   */
  public CompletableFuture<PurchaseOrderCollection> getPurchaseOrders(int limit, int offset, String query) {
    CompletableFuture<PurchaseOrderCollection> future = new VertxCompletableFuture<>(ctx);

    try {
      buildGetOrdersPath(limit, offset, query)
        .thenCompose(endpoint -> handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger))
        .thenAccept(jsonOrders -> future.complete(jsonOrders.mapTo(PurchaseOrderCollection.class)))
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

  private CompletableFuture<String> buildGetOrdersPath(int limit, int offset, String query) {
    AcquisitionsUnitsHelper acqUnitsHelper = new AcquisitionsUnitsHelper(httpClient, okapiHeaders, ctx, lang);
    return acqUnitsHelper.buildAcqUnitsCqlExprToSearchRecords()
      .thenApply(acqUnitsCqlExpr -> {
        if (StringUtils.isEmpty(query)) {
          String queryParam = buildQuery(acqUnitsCqlExpr, logger);
          return String.format(GET_PURCHASE_ORDERS, limit, offset, queryParam, lang);
        } else {
          String queryParam = buildQuery(combineCqlExpressions("and", acqUnitsCqlExpr, query), logger);
          return String.format(SEARCH_ORDERS_BY_LINES_DATA, limit, offset, queryParam, lang);
        }
      });
  }

  /**
   * Create a purchase order (PO) and a number of PO lines if provided.
   * @param compPO {@link CompositePurchaseOrder} object representing Purchase Order and optionally Purchase Order Line details.
   * @return completable future with {@link CompositePurchaseOrder} object with populated uuid on success or an exception if processing fails
   */
  public CompletableFuture<CompositePurchaseOrder> createPurchaseOrder(CompositePurchaseOrder compPO) {

    return validateAcqUnitsOnCreate(compPO.getAcqUnitIds())
        .thenCompose(ok -> checkOrderApprovalPermissions(compPO))
        .thenCompose(ok -> setPoNumberIfMissing(compPO)
        .thenCompose(v -> poNumberHelper.checkPONumberUnique(compPO.getPoNumber()))
        .thenCompose(v -> createPOandPOLines(compPO))
        .thenCompose(this::populateOrderSummary));
  }

  /**
   * @param acqUnitIds acquisitions units assigned to purchase order from request
   * @return completable future completed successfully if all checks pass or exceptionally in case of error/restriction
   *         caused by acquisitions units
   */
  private CompletableFuture<Void> validateAcqUnitsOnCreate(List<String> acqUnitIds) {
    if (acqUnitIds.isEmpty()) {
      return completedFuture(null);
    }

    return VertxCompletableFuture.runAsync(ctx, () -> verifyUserHasAssignPermission(acqUnitIds))
      .thenCompose(ok -> protectionHelper.verifyIfUnitsAreActive(acqUnitIds))
      .thenCompose(ok -> protectionHelper.isOperationRestricted(acqUnitIds, ProtectedOperationType.CREATE));
  }

  /**
   * Handles update of the order. First retrieve the PO from storage and depending on its content handle passed PO.
   * @param compPO updated {@link CompositePurchaseOrder} purchase order
   * @return completable future holding response indicating success (204 No Content) or error if failed
   */
  public CompletableFuture<Void> updateOrder(CompositePurchaseOrder compPO) {
    return getPurchaseOrderById(compPO.getId(), lang, httpClient, ctx, okapiHeaders, logger)
      .thenCompose(jsonPoFromStorage -> validateIfPOProtectedFieldsChanged(compPO, jsonPoFromStorage))
      .thenApply(HelperUtils::convertToCompositePurchaseOrder)
      .thenCompose(poFromStorage -> {
        boolean isTransitionToOpen = isTransitionToOpen(poFromStorage, compPO);
        return validateAcqUnitsOnUpdate(compPO, poFromStorage)
          .thenCompose(ok -> validatePoNumber(poFromStorage, compPO))
          .thenCompose(ok -> {
            if (isTransitionToApproved(poFromStorage, compPO)) {
              return checkOrderApprovalPermissions(compPO);
            }
            return completedFuture(null);
          })
          .thenCompose(ok -> {
            if (isTransitionToPending(poFromStorage, compPO)) {
              checkOrderUnopenPermissions();
              return unOpenOrder(compPO);
            }
            return CompletableFuture.completedFuture(null);
          })
          .thenCompose(v -> {
            if (isTransitionToOpen) {
              return updateAndGetOrderWithLines(compPO)
                .thenCompose(order -> {
                  compPO.getCompositePoLines().forEach(poLine -> orderLineHelper.updateLocationsQuantity(poLine.getLocations()));
                  return checkLocationsAndPiecesConsistency(compPO.getCompositePoLines());
                });
            } else {
              return CompletableFuture.completedFuture(null);
            }
          })
          .thenCompose(v -> updatePoLines(poFromStorage, compPO))
          .thenCompose(v -> {
            if (isTransitionToOpen) {
              return checkOrderApprovalRequired(compPO).thenCompose(ok -> openOrder(compPO));
            } else {
              return CompletableFuture.completedFuture(null);
            }
          })
          .thenCompose(ok -> handleFinalOrderStatus(compPO, poFromStorage.getWorkflowStatus().value()));

      });
  }

  private CompletableFuture<Void> checkLocationsAndPiecesConsistency(List<CompositePoLine> poLines) {
    List<CompositePoLine> linesWithId = poLines.stream().filter(compositePoLine -> StringUtils.isNotEmpty(compositePoLine.getId())).collect(Collectors.toList());
    String query = convertIdsToCqlQuery(linesWithId.stream().map(CompositePoLine::getId).collect(toList()), "poLineId");
    return inventoryHelper.getPieces(Integer.MAX_VALUE, 0, query)
      .thenAccept(pieces -> verifyLocationsAndPiecesConsistency(linesWithId, pieces));
  }

  public CompletableFuture<Void> handleFinalOrderStatus(CompositePurchaseOrder compPO, String initialOrdersStatus) {
    PurchaseOrder purchaseOrder = convertToPurchaseOrder(compPO).mapTo(PurchaseOrder.class);
    CompletableFuture<List<PoLine>> future;

    if (isEmpty(compPO.getCompositePoLines())) {
      future = fetchOrderLinesByOrderId(compPO.getId());
    } else {
      future = VertxCompletableFuture.supplyBlockingAsync(ctx, () -> {
        List<PoLine> poLines = HelperUtils.convertToPoLines(compPO.getCompositePoLines());
        changeOrderStatus(purchaseOrder, poLines);
        return poLines;
      });
    }

    return future.thenCompose(poLines -> handleFinalOrderStatus(purchaseOrder, poLines, initialOrdersStatus))
      .thenAccept(aVoid -> {
        compPO.setWorkflowStatus(WorkflowStatus.fromValue(purchaseOrder.getWorkflowStatus().value()));
        compPO.setCloseReason(purchaseOrder.getCloseReason());
      })
      .thenCompose(aVoid -> updateOrderSummary(purchaseOrder));
  }

  public CompletableFuture<Void> handleFinalOrderStatus(PurchaseOrder purchaseOrder, List<PoLine> poLines, String initialOrdersStatus) {
    if (isOrderClosing(purchaseOrder.getWorkflowStatus(), initialOrdersStatus)) {
      return closeOrder(poLines);
    } else if (isOrderReopening(purchaseOrder.getWorkflowStatus(), initialOrdersStatus)) {
      return reopenOrder(poLines);
    }
    return CompletableFuture.completedFuture(null);
  }

  /**
   * Handles transition of related order to CLOSED status.
   *
   * @param poLines PO Lines associated with closing order
   * @return CompletableFuture that indicates when transition is completed
   */
  private CompletableFuture<Void> closeOrder(List<PoLine> poLines) {
    return updateItemsStatusInInventory(poLines, "On order", "Order closed");
  }

  private List<JsonObject> updateStatusName(List<JsonObject> items, String s) {
    items.forEach(item -> item.getJsonObject("status").put("name", s));
    return items;
  }

  private CompletableFuture<List<JsonObject>> getItemsByStatus(List<PoLine> compositePoLines, String itemStatus) {
    List<String> lineIds = compositePoLines.stream().map(PoLine::getId).collect(toList());
    // Split all id's by maximum number of id's for get query
    List<CompletableFuture<List<JsonObject>>> futures = StreamEx
      .ofSubLists(lineIds, MAX_IDS_FOR_GET_RQ)
      // Get item records from Inventory storage
      .map(ids -> {
        String query = encodeQuery(String.format("status.name==%s and %s", itemStatus, convertIdsToCqlQuery(ids, InventoryHelper.ITEM_PURCHASE_ORDER_LINE_IDENTIFIER, true)), logger);
        return inventoryHelper.getItemRecordsByQuery(query);
      })
      .toList();

    return collectResultsOnSuccess(futures)
      .thenApply(lists -> StreamEx.of(lists).toFlatList(jsonObjects -> jsonObjects));
  }

  /**
   * Handles transition of related order from CLOSED to OPEN status.
   *
   * @param poLines PO Lines associated with closing order
   * @return CompletableFuture that indicates when transition is completed
   */
  private CompletableFuture<Void> reopenOrder(List<PoLine> poLines) {
    return updateItemsStatusInInventory(poLines, "Order closed", "On order");
  }

  private CompletableFuture<Void> updateItemsStatusInInventory(List<PoLine> poLines, String currentStatus, String newStatus) {
    return getItemsByStatus(poLines, currentStatus)
      .thenApply(items -> updateStatusName(items, newStatus))
      .thenCompose(this::updateItemsInInventory);
  }

  private CompletableFuture<Set<ProtectedOperationType>> getInvolvedOperations(CompositePurchaseOrder compPO) {
    if (CollectionUtils.isEmpty(compPO.getCompositePoLines())) {
      return CompletableFuture.completedFuture(Collections.singleton(UPDATE));

    }
    return fetchOrderLinesByOrderId(compPO.getId())
      .thenApply(poLines -> {
        Set<String> newIds = compPO.getCompositePoLines().stream().map(CompositePoLine::getId).collect(Collectors.toSet());
        Set<String> storageIds = poLines.stream().map(PoLine::getId).collect(Collectors.toSet());
        Set<ProtectedOperationType> operations = new HashSet<>();
        operations.add(UPDATE);
        operations.addAll(getInvolvedOperations(newIds, storageIds));
        return operations;
      });
  }

  private Set<ProtectedOperationType> getInvolvedOperations(Set<String> newIds, Set<String> storageIds) {
    if (CollectionUtils.isEqualCollection(newIds, storageIds)) {
      return Collections.emptySet();
    } else if (CollectionUtils.isSubCollection(storageIds, newIds)) {
      return Collections.singleton(CREATE);
    } else if (CollectionUtils.isSubCollection(newIds, storageIds)) {
      return Collections.singleton(DELETE);
    }
    Set<ProtectedOperationType> operations = new HashSet<>();
    operations.add(CREATE);
    operations.add(DELETE);
    return operations;
  }


  public CompletableFuture<JsonObject> validateIfPOProtectedFieldsChanged(CompositePurchaseOrder compPO,
      JsonObject compPOFromStorageJson) {
    WorkflowStatus storagePOWorkflowStatus = WorkflowStatus.fromValue(compPOFromStorageJson.getString(WORKFLOW_STATUS));
    if (!PENDING.equals(storagePOWorkflowStatus)) {
      List<String> fieldNames = OPEN.equals(storagePOWorkflowStatus) ? getFieldNamesForOpenOrder() : getFieldNames();
      // MODORDERS-429 double conversion required until HttpClient returns e.g. 'ongoing.renewalDate' in different format
      CompositePurchaseOrder storageCompPO = compPOFromStorageJson.mapTo(CompositePurchaseOrder.class);
      verifyProtectedFieldsChanged(fieldNames, JsonObject.mapFrom(storageCompPO), JsonObject.mapFrom(compPO));
    }
    return completedFuture(compPOFromStorageJson);
  }

  private JsonObject findCorrespondingCompositePoLine(CompositePoLine poLine, List<PoLine> poLinesFromStorage) {
    return poLinesFromStorage.stream()
      .filter(line -> line.getId()
        .equals(poLine.getId()))
      .findFirst()
      .map(JsonObject::mapFrom)
      .orElse(null);
  }

  /**
   * Delete a purchase order with given uuid. As a first step the logic deletes all associated PO Lines and then order.
   * @param id purchase order id
   * @return completable future which is just completed with nothing on success or an exception if processing fails
   */
  public CompletableFuture<Void> deleteOrder(String id) {
    CompletableFuture<Void> future = new VertxCompletableFuture<>(ctx);

    getPurchaseOrderById(id, lang, httpClient, ctx, okapiHeaders, logger)
      .thenAccept(purchaseOrder -> {
        CompositePurchaseOrder compPo = convertToCompositePurchaseOrder(purchaseOrder);
        protectionHelper.isOperationRestricted(compPo.getAcqUnitIds(), DELETE)
          .thenAccept(aVoid -> financeHelper.deleteOrderEncumbrances(id)
            .thenCompose(v -> deletePoLines(id, lang, httpClient, ctx, okapiHeaders, logger))
            .thenRun(() -> {
              logger.info("Successfully deleted poLines, proceeding with purchase order");
              handleDeleteRequest(resourceByIdPath(PURCHASE_ORDER, id), httpClient, ctx, okapiHeaders, logger)
                .thenAccept(rs -> {
                  logger.info("Successfully deleted order with id={}", id);
                  future.complete(null);
                })
                .exceptionally(t -> {
                  logger.error("Failed to delete the order with id={}", t.getCause(), id);
                  future.completeExceptionally(t);
                  return null;
                });
            })
            .exceptionally(t -> {
              logger.error("Failed to delete PO Lines of the order with id={}", t.getCause(), id);
              future.completeExceptionally(t);
              return null;
            })
          )
          .exceptionally(t -> {
            logger.error("User with id={} is forbidden to view delete with id={}", t.getCause(), getCurrentUserId(), id);
            future.completeExceptionally(t);
            return null;
          });
      })
    .exceptionally(t ->{
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
      .thenApply(HelperUtils::convertToCompositePurchaseOrder)
      .thenAccept(compPO -> protectionHelper.isOperationRestricted(compPO.getAcqUnitIds(), ProtectedOperationType.READ)
        .thenAccept(ok -> updateAndGetOrderWithLines(compPO)
          .thenCompose(this::fetchNonPackageTitles)
          .thenAccept(linesIdTitles -> populateInstanceId(linesIdTitles, compPO.getCompositePoLines()))
          .thenCompose(v -> populateOrderSummary(compPO))
          .thenAccept(future::complete)
          .exceptionally(t -> {
            logger.error("Failed to get lines for order with id={}", t.getCause(), id);
            future.completeExceptionally(t);
            return null;
          }))
        .exceptionally(t -> {
          logger.error("User with id={} is forbidden to view order with id={}", t.getCause(), getCurrentUserId(), id);
          future.completeExceptionally(t);
          return null;
        }))
      .exceptionally(t -> {
        logger.error("Failed to build composite purchase order with id={}", t.getCause(), id);
        future.completeExceptionally(t);
        return null;
      });

    return future;
  }

  private CompletableFuture<CompositePurchaseOrder> populateOrderSummary(CompositePurchaseOrder compPO) {
    List<CompositePoLine> compositePoLines = compPO.getCompositePoLines();
    return calculateTotalEstimatedPrice(compositePoLines).thenApply(totalAmount -> {
      compPO.setTotalEstimatedPrice(totalAmount);
      compPO.setTotalItems(calculateTotalItemsQuantity(compositePoLines));
      return compPO;
    });
  }

  /**
   * Calculates PO's estimated price by summing the Estimated Price of the associated PO Lines. See MODORDERS-181 for more details.
   * At the moment assumption is that all prices could be in the different currency.
   *
   * @param compositePoLines list of composite PO Lines
   * @return estimated purchase order's total price
   */
  public CompletableFuture<Double> calculateTotalEstimatedPrice(List<CompositePoLine> compositePoLines) {
    return getSystemCurrency().thenApply(toCurrency -> compositePoLines.stream()
      .map(CompositePoLine::getCost)
      .map(cost -> Money.of(cost.getPoLineEstimatedPrice(), cost.getCurrency()))
      .map(money -> {
        if (money.getCurrency().getCurrencyCode().equals(toCurrency)) {
          return money;
        }
        ConversionQuery conversionQuery = ConversionQueryBuilder.of()
          .setBaseCurrency(money.getCurrency())
          .setTermCurrency(toCurrency)
          .build();
        var exchangeRateProvider = exchangeRateProviderResolver.resolve(conversionQuery, new RequestContext(ctx, okapiHeaders));
        var conversion = exchangeRateProvider.getCurrencyConversion(conversionQuery);

        return money.with(conversion);
      })
      .reduce(Money.of(0, toCurrency), Money::add)
      .getNumber()
      .doubleValue());
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
    return financeHelper.validateExpenseClasses(compPO.getCompositePoLines())
      .thenAccept(v -> orderLineHelper.validateFundDistributionTotal(compPO.getCompositePoLines()))
      .thenApply(v -> this.validateMaterialTypes(compPO))
      .thenCompose(this::fetchNonPackageTitles)
      .thenCompose(linesIdTitles -> {
          populateInstanceId(linesIdTitles, compPO.getCompositePoLines());
          return openOrderUpdateInventory(linesIdTitles, compPO);
        })
      .thenCompose(ok -> processEncumbrances(compPO))
      .thenAccept(ok -> changePoLineStatuses(compPO.getCompositePoLines()))
      .thenCompose(ok -> orderLineHelper.updatePoLinesSummary(compPO.getCompositePoLines()));
  }

  private CompletableFuture<Void> updateItemsInInventory(List<JsonObject> items) {
    return VertxCompletableFuture.allOf(ctx, items.stream()
      .map(inventoryHelper::updateItem)
      .toArray(CompletableFuture[]::new));
  }


  private CompletableFuture<Map<String, List<Title>>> fetchNonPackageTitles(CompositePurchaseOrder compPO) {
    List<String> lineIds = getNonPackageLineIds(compPO.getCompositePoLines());
    return titlesHelper.getTitlesByPoLineIds(lineIds);
  }

  private List<CompositePoLine> getNonPackageLines(List<CompositePoLine> compositePoLines) {
    return compositePoLines.stream().filter(line -> !line.getIsPackage()).collect(toList());
  }

  private List<String> getNonPackageLineIds(List<CompositePoLine> compositePoLines) {
    return compositePoLines.stream().filter(line -> !line.getIsPackage()).map(CompositePoLine::getId).collect(toList());
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
      .thenAccept(v -> validateOrderPoLines(compPO))
      .thenCompose(v -> validateIsbnValues(compPO))
      .thenCompose(v -> validatePoLineLimit(compPO))
      .thenCompose(v -> validateVendor(compPO))
      .thenAccept(v -> validateRenewalInfo(compPO))
      .thenApply(v -> getErrors().isEmpty());
  }

  public void validateOrderPoLines(CompositePurchaseOrder compositeOrder) {
    List<Error> errors = new ArrayList<>();
    for (CompositePoLine compositePoLine : compositeOrder.getCompositePoLines()) {
      errors.addAll(CompositePoLineValidationUtil.validatePoLine(compositeOrder.getOrderType().value(), compositePoLine));
    }
    addProcessingErrors(errors);
  }

  private CompletableFuture<Void> validateIsbnValues(CompositePurchaseOrder compPO) {
    CompletableFuture<?>[] futures = compPO.getCompositePoLines()
      .stream()
      .map(orderLineHelper::validateAndNormalizeISBN)
      .toArray(CompletableFuture[]::new);

    return VertxCompletableFuture.allOf(ctx, futures);
  }

  private CompletableFuture<Void> setCreateInventoryDefaultValues(CompositePurchaseOrder compPO) {
    CompletableFuture<?>[] futures = compPO.getCompositePoLines()
      .stream()
      .map(orderLineHelper::setTenantDefaultCreateInventoryValues)
      .toArray(CompletableFuture[]::new);

    return VertxCompletableFuture.allOf(ctx, futures);
  }

  /**
   * Validates purchase order which already exists in the storage.
   * Checks PO Number presence, validates that provided order id corresponds to one set in order and its lines.
   * If all is okay, {@link #validateOrderPoLines(CompositePurchaseOrder)} is called afterwards.
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

  public CompletableFuture<Void> unOpenOrder(CompositePurchaseOrder compPO) {
    CompletableFuture<Void> future = new CompletableFuture<>();
    updateAndGetOrderWithLines(compPO)
      .thenCompose(compositePO -> financeHelper.getOrderEncumbrances(compPO.getId()))
      .thenApply(financeHelper::makeEncumbrancesPending)
      .thenCompose(encumbrances -> financeHelper.updateOrderTransactionSummary(compPO.getId(), encumbrances.size())
                                                .thenApply(v -> encumbrances))
      .thenCompose(financeHelper::updateTransactions)
      .thenAccept(ok -> orderLineHelper.makePoLinesPending(compPO.getCompositePoLines()))
      .thenCompose(ok -> orderLineHelper.updatePoLinesSummary(compPO.getCompositePoLines()))
      .thenAccept(v-> future.complete(null))
      .exceptionally(t -> {
        future.completeExceptionally(t);
        return null;
      });
    return future;
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

  private CompletableFuture<Void> validateVendor(CompositePurchaseOrder compPO) {
    if (compPO.getWorkflowStatus() == WorkflowStatus.OPEN) {
      VendorHelper vendorHelper = new VendorHelper(httpClient, okapiHeaders, ctx, lang);
      return vendorHelper.validateVendor(compPO)
        .thenCompose(errors -> {
          addProcessingErrors(errors.getErrors());
          return fetchCompositePolLines(compPO)
            .thenCompose(vendorHelper::validateAccessProviders);
        })
        .thenAccept(errors -> addProcessingErrors(errors.getErrors()));
    }
    return completedFuture(null);
  }

  private void validateRenewalInfo(CompositePurchaseOrder compPO) {
    if (compPO.getOrderType() == CompositePurchaseOrder.OrderType.ONGOING && Objects.isNull(compPO.getOngoing())) {
      addProcessingError(MISSING_ONGOING.toError());
    } else if (compPO.getOrderType() == CompositePurchaseOrder.OrderType.ONE_TIME && Objects.nonNull(compPO.getOngoing())) {
      addProcessingError(ONGOING_NOT_ALLOWED.toError());
    }
  }

  private CompletableFuture<Void> validatePoLineLimit(CompositePurchaseOrder compPO) {
    if (CollectionUtils.isNotEmpty(compPO.getCompositePoLines())) {
       return getTenantConfiguration(ORDER_CONFIG_MODULE_NAME)
        .thenAccept(config -> {
          int limit = getPoLineLimit(config);
          if (compPO.getCompositePoLines().size() > limit) {
            addProcessingError(ErrorCodes.POL_LINES_LIMIT_EXCEEDED.toError());
          }
        });
    }
    return completedFuture(null);
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
          return checkOrderApprovalRequired(compPO)
            .thenCompose(ok ->
                updateAndGetOrderWithLines(compPO)
              .thenCompose(this::openOrder))
            .thenCompose(ok -> handleFinalOrderStatus(compPO, finalStatus.value()));
        }
        return completedFuture(null);
      })
      .thenApply(v -> compPO);
  }


  /**
   * Checks the value of "isApprovalRequired" in configurations, if the value is set to true, and order is being approved, verifies
   * if the user has required permissions to approve order
   *
   * @param compPO composite purchase order for checking permissions
   */
  private CompletableFuture<Void> checkOrderApprovalPermissions(CompositePurchaseOrder compPO) {
    return getTenantConfiguration(ORDER_CONFIG_MODULE_NAME).thenAccept(config -> {
      boolean isApprovalRequired = isApprovalRequiredConfiguration(config);
      if (isApprovalRequired && compPO.getApproved()) {
        if (isUserNotHaveApprovePermission()) {
          throw new HttpException(HttpStatus.HTTP_FORBIDDEN.toInt(), USER_HAS_NO_APPROVAL_PERMISSIONS);
        }
        compPO.setApprovalDate(new Date());
        compPO.setApprovedById(getCurrentUserId());
      }
    });
  }

  private void checkOrderUnopenPermissions() {
    if (isUserNotHaveUnopenPermission()) {
      throw new HttpException(HttpStatus.HTTP_FORBIDDEN.toInt(), USER_HAS_NO_UNOPEN_PERMISSIONS);
    }
  }

  private boolean isApprovalRequiredConfiguration(JsonObject config) {
    return Optional.ofNullable(config.getString("approvals"))
      .map(approval -> new JsonObject(approval).getBoolean("isApprovalRequired"))
      .orElse(false);
  }

  /**
   * If an order is transitioning to OPEN, checks if approval is required and throws an error if it is not approved
   *
   * @param compPO composite purchase order
   */
  private CompletableFuture<Void> checkOrderApprovalRequired(CompositePurchaseOrder compPO) {
    return getTenantConfiguration(ORDER_CONFIG_MODULE_NAME).thenAccept(config -> {
      boolean isApprovalRequired = isApprovalRequiredConfiguration(config);
      if (isApprovalRequired && !compPO.getApproved()) {
        throw new HttpException(400, APPROVAL_REQUIRED_TO_OPEN);
      }
      compPO.setApprovedById(getCurrentUserId());
      compPO.setApprovalDate(new Date());
    });
  }

  private CompletableFuture<List<CompositePoLine>> createPoLines(CompositePurchaseOrder compPO) {
    List<CompletableFuture<CompositePoLine>> futures =
      compPO.getCompositePoLines()
            .stream()
            .map(compositePoLine -> orderLineHelper.saveValidPoLine(compositePoLine, compPO))
            .collect(Collectors.toList());
    return HelperUtils.collectResultsOnSuccess(futures);
  }

  private CompletableFuture<List<CompositePoLine>> fetchCompositePolLines(CompositePurchaseOrder compPO) {
    if (CollectionUtils.isEmpty(compPO.getCompositePoLines())) {
      return  getCompositePoLines(compPO.getId(), lang, httpClient, ctx, okapiHeaders, logger)
        .thenApply(poLines -> {
          orderLineHelper.sortPoLinesByPoLineNumber(poLines);
          return poLines;
        });
    } else {
      return completedFuture(compPO.getCompositePoLines());
    }
  }

  public CompletableFuture<CompositePurchaseOrder> updateAndGetOrderWithLines(CompositePurchaseOrder compPO) {
    if (CollectionUtils.isEmpty(compPO.getCompositePoLines())) {
      return getCompositePoLines(compPO.getId(), lang, httpClient, ctx, okapiHeaders, logger)
        .thenApply(poLines -> {
          orderLineHelper.sortPoLinesByPoLineNumber(poLines);
          return compPO.withCompositePoLines(poLines);
        })
        .thenApply(v -> compPO);
    } else {
      return completedFuture(compPO);
    }
  }

  private void populateInstanceId(Map<String, List<Title>> lineIdsTitles, List<CompositePoLine> lines) {
    getNonPackageLines(lines).forEach(line -> {
      if (lineIdsTitles.containsKey(line.getId())) {
        line.setInstanceId(lineIdsTitles.get(line.getId()).get(0).getInstanceId());
      }
    });
  }

  private void changePoLineStatuses(List<CompositePoLine> compositePoLines) {
    compositePoLines.forEach(poLine -> {
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

  private boolean isPoNumberChanged(CompositePurchaseOrder poFromStorage, CompositePurchaseOrder updatedPo) {
    return !StringUtils.equalsIgnoreCase(poFromStorage.getPoNumber(), updatedPo.getPoNumber());
  }

  private CompletableFuture<Void> updatePoLines(CompositePurchaseOrder poFromStorage, CompositePurchaseOrder compPO) {
    if (isPoLinesUpdateRequired(poFromStorage, compPO)) {
      return fetchOrderLinesByOrderId(compPO.getId())
        .thenCompose(existingPoLines -> {
          if (isNotEmpty(compPO.getCompositePoLines())) {
            // New PO Line(s) can be added only to Pending order
            if (poFromStorage.getWorkflowStatus() != PENDING && hasNewPoLines(compPO, existingPoLines)) {
              throw new HttpException(422, poFromStorage.getWorkflowStatus() == OPEN ? ErrorCodes.ORDER_OPEN : ErrorCodes.ORDER_CLOSED);
            }
            validatePOLineProtectedFieldsChangedInPO(poFromStorage, compPO, existingPoLines);
            //check if the order is in open status and the fields are being changed
            return handlePoLines(compPO, existingPoLines);
          } else {
            return updatePoLinesNumber(compPO, existingPoLines);
          }
        });
    }
    return completedFuture(null);
  }

  private void validatePOLineProtectedFieldsChangedInPO(CompositePurchaseOrder poFromStorage, CompositePurchaseOrder compPO,
                                                        List<PoLine> existingPoLines) {
    if (poFromStorage.getWorkflowStatus() != PENDING) {
      compPO.getCompositePoLines()
        .forEach(poLine -> verifyProtectedFieldsChanged(POLineProtectedFields.getFieldNames(),
            findCorrespondingCompositePoLine(poLine, existingPoLines), JsonObject.mapFrom(poLine)));
    }
  }

  private boolean isPoLinesUpdateRequired(CompositePurchaseOrder poFromStorage, CompositePurchaseOrder compPO) {
    return isNotEmpty(compPO.getCompositePoLines()) || isPoNumberChanged(poFromStorage, compPO);
  }

  public CompletableFuture<Void> updateOrderSummary(PurchaseOrder purchaseOrder) {
    logger.debug("Updating order...");
    return handlePutRequest(resourceByIdPath(PURCHASE_ORDER, purchaseOrder.getId()), JsonObject.mapFrom(purchaseOrder), httpClient, ctx, okapiHeaders, logger);
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

  private CompletableFuture<Void> updatePoLinesNumber(CompositePurchaseOrder compOrder, List<PoLine> poLinesFromStorage) {
    CompletableFuture<?>[] futures = poLinesFromStorage
      .stream()
      .map(lineFromStorage -> {
        lineFromStorage.setPoLineNumber(orderLineHelper.buildNewPoLineNumber(lineFromStorage, compOrder.getPoNumber()));
        return orderLineHelper
          .updateOrderLineSummary(lineFromStorage.getId(), JsonObject.mapFrom(lineFromStorage));
      })
       .toArray(CompletableFuture[]::new);

    return VertxCompletableFuture.allOf(ctx, futures);
  }

  public CompletableFuture<EncumbrancesProcessingHolder> processEncumbrances(CompositePurchaseOrder compPO) {
    EncumbrancesProcessingHolder holder = new EncumbrancesProcessingHolder();
    if (isFundDistributionsPresent(compPO.getCompositePoLines())) {
      return financeHelper.getOrderEncumbrances(compPO.getId())
        .thenAccept(holder::withEncumbrancesFromStorage)
        .thenCompose(v -> financeHelper.buildNewEncumbrances(compPO, compPO.getCompositePoLines(), holder.getEncumbrancesFromStorage()))
        .thenAccept(holder::withEncumbrancesForCreate)
        .thenCompose(v -> financeHelper.buildEncumbrancesForUpdate(compPO.getCompositePoLines(), holder.getEncumbrancesFromStorage()))
        .thenAccept(holder::withEncumbrancesForUpdate)
        .thenApply(v -> financeHelper.findNeedReleaseEncumbrances(compPO.getCompositePoLines(), holder.getEncumbrancesFromStorage()))
        .thenAccept(holder::withEncumbrancesForRelease)
        .thenCompose(v -> orderLineHelper.createOrUpdateOrderTransactionSummary(compPO.getId(), holder))
        .thenCompose(v -> orderLineHelper.createOrUpdateEncumbrances(holder))
        .thenApply(v -> holder);

    }
    return CompletableFuture.completedFuture(null);
  }


  private boolean isFundDistributionsPresent(List<CompositePoLine> compositePoLines) {
    return compositePoLines.stream().mapToLong(compositePoLine -> compositePoLine.getFundDistribution().size()).sum() >= 1;
  }

  private CompletableFuture<Void> openOrderUpdateInventory(Map<String, List<Title>> lineIdsTitles, CompositePurchaseOrder compPO) {
    return CompletableFuture.allOf(
      compPO.getCompositePoLines()
        .stream()
        .map(poLine -> orderLineHelper.updateInventory(poLine, getFirstTitleIdIfExist(lineIdsTitles, poLine), true))
        .toArray(CompletableFuture[]::new)
    );
  }

  private String getFirstTitleIdIfExist(Map<String, List<Title>> lineIdsTitles, CompositePoLine poLine) {
    return Optional.ofNullable(lineIdsTitles.get(poLine.getId()))
      .map(titles -> titles.get(0))
      .map(Title::getId)
      .orElse(null);
  }

  private CompletableFuture<Void> handlePoLines(CompositePurchaseOrder compOrder, List<PoLine> poLinesFromStorage) {
    List<CompletableFuture<?>> futures = new ArrayList<>(processPoLinesCreation(compOrder, poLinesFromStorage));
    if (!poLinesFromStorage.isEmpty()) {
      futures.addAll(processPoLinesUpdate(compOrder, poLinesFromStorage));
      // The remaining unprocessed PoLines should be removed
      poLinesFromStorage
        .forEach(poLine -> futures.add(financeHelper.deletePoLineEncumbrances(poLine.getId())
          .thenCompose(v -> deletePoLine(JsonObject.mapFrom(poLine), httpClient, ctx, okapiHeaders, logger))));
    }
    return VertxCompletableFuture.allOf(ctx, futures.toArray(new CompletableFuture[0]));
  }

  private List<CompletableFuture<?>> processPoLinesUpdate(CompositePurchaseOrder compOrder, List<PoLine> poLinesFromStorage) {
    List<CompletableFuture<?>> futures = new ArrayList<>();
    Iterator<PoLine> iterator = poLinesFromStorage.iterator();
    while (iterator.hasNext()) {
      PoLine lineFromStorage = iterator.next();
      for (CompositePoLine line : compOrder.getCompositePoLines()) {
        if (StringUtils.equals(lineFromStorage.getId(), line.getId())) {
          line.setPoLineNumber(orderLineHelper.buildNewPoLineNumber(lineFromStorage, compOrder.getPoNumber()));
          orderLineHelper.updateLocationsQuantity(line.getLocations());
          orderLineHelper.updateEstimatedPrice(compOrder.getOrderType().value(), line);

          futures.add(orderLineHelper.updateOrderLine(line, JsonObject.mapFrom(lineFromStorage)));
          iterator.remove();
          break;
        }
      }
    }
    return futures;
  }

  private List<CompletableFuture<CompositePoLine>> processPoLinesCreation(CompositePurchaseOrder compOrder, List<PoLine> poLinesFromStorage) {
    return getNewPoLines(compOrder, poLinesFromStorage)
      .map(compPOL -> orderLineHelper.saveValidPoLine(compPOL, compOrder))
      .collect(toList());
  }

  private boolean hasNewPoLines(CompositePurchaseOrder compPO, List<PoLine> poLinesFromStorage) {
    return getNewPoLines(compPO, poLinesFromStorage).count() > 0;
  }

  private Stream<CompositePoLine> getNewPoLines(CompositePurchaseOrder compPO, List<PoLine> poLinesFromStorage) {
    List<String> lineIdsInStorage = poLinesFromStorage
      .stream()
      .map(PoLine::getId)
      .collect(toList());

    return compPO.getCompositePoLines()
      .stream()
      .filter(poLine -> !lineIdsInStorage.contains(poLine.getId()));
  }

  /**
   * The method checks if the order is assigned to acquisition unit, if yes,
   * then check that if the user has desired permission to assign the record to acquisition unit
   *
   * @throws HttpException if user does not have assign permission
   * @param acqUnitIds acquisitions units assigned to purchase order from request
   */
  private void verifyUserHasAssignPermission(List<String> acqUnitIds) {
    if (CollectionUtils.isNotEmpty(acqUnitIds) && isUserDoesNotHaveDesiredPermission(ASSIGN)){
      throw new HttpException(HttpStatus.HTTP_FORBIDDEN.toInt(), USER_HAS_NO_ACQ_PERMISSIONS);
    }
  }

  /**
   * @param updatedOrder purchase order from request
   * @param persistedOrder purchase order from storage
   * @return completable future completed successfully if all checks pass or exceptionally in case of error/restriction
   *         caused by acquisitions units
   */
  private CompletableFuture<Void> validateAcqUnitsOnUpdate(CompositePurchaseOrder updatedOrder,
      CompositePurchaseOrder persistedOrder) {
    List<String> updatedAcqUnitIds = updatedOrder.getAcqUnitIds();
    List<String> currentAcqUnitIds = persistedOrder.getAcqUnitIds();

    return VertxCompletableFuture.runAsync(ctx, () -> verifyUserHasManagePermission(updatedAcqUnitIds, currentAcqUnitIds))
      // Check that all newly assigned units are active/exist
      .thenCompose(ok -> protectionHelper.verifyIfUnitsAreActive(ListUtils.subtract(updatedAcqUnitIds, currentAcqUnitIds)))
      .thenCompose(ok -> getInvolvedOperations(updatedOrder))
      // The check should be done against currently assigned (persisted in storage) units
      .thenCompose(protectedOperationTypes -> protectionHelper.isOperationRestricted(currentAcqUnitIds, protectedOperationTypes));
  }

  /**
   * The method checks if list of acquisition units to which the order is assigned is changed, if yes,
   * then check that if the user has desired permission to manage acquisition units assignments
   *
   * @throws HttpException if user does not have manage permission
   * @param newAcqUnitIds acquisitions units assigned to purchase order from request
   * @param currentAcqUnitIds acquisitions units assigned to purchase order from storage
   */
  private void verifyUserHasManagePermission(List<String> newAcqUnitIds, List<String> currentAcqUnitIds) {
    Set<String> newAcqUnits = new HashSet<>(CollectionUtils.emptyIfNull(newAcqUnitIds));
    Set<String> acqUnitsFromStorage = new HashSet<>(CollectionUtils.emptyIfNull(currentAcqUnitIds));

    if (isManagePermissionRequired(newAcqUnits, acqUnitsFromStorage) && isUserDoesNotHaveDesiredPermission(MANAGE)){
      throw new HttpException(HttpStatus.HTTP_FORBIDDEN.toInt(), USER_HAS_NO_ACQ_PERMISSIONS);
    }
  }

  private boolean isUserDoesNotHaveDesiredPermission(AcqDesiredPermissions acqPerm) {
    return !getProvidedPermissions().contains(acqPerm.getPermission());
  }

  private boolean isManagePermissionRequired(Set<String> newAcqUnits, Set<String> acqUnitsFromStorage) {
    return !CollectionUtils.isEqualCollection(newAcqUnits, acqUnitsFromStorage);
  }

  private List<String> getProvidedPermissions() {
    return new JsonArray(okapiHeaders.getOrDefault(OKAPI_HEADER_PERMISSIONS, EMPTY_ARRAY)).stream().
      map(Object::toString)
      .collect(Collectors.toList());
  }

  private CompletableFuture<List<PoLine>> fetchOrderLinesByOrderId(String orderId) {
    if (orderLines != null) {
      return CompletableFuture.completedFuture(new ArrayList<>(orderLines));
    }

    return getPoLines(orderId, lang, httpClient, ctx, okapiHeaders, logger)
      .thenApply(linesJsonArray -> {
        orderLines = HelperUtils.convertJsonToPoLines(linesJsonArray);
        return new ArrayList<>(orderLines);
      });
  }

  private boolean isUserNotHaveApprovePermission() {
    return !getProvidedPermissions().contains(PERMISSION_ORDER_APPROVE);
  }

  private boolean isUserNotHaveUnopenPermission() {
    return !getProvidedPermissions().contains(PERMISSION_ORDER_UNOPEN);
  }

  private CompositePurchaseOrder validateMaterialTypes(CompositePurchaseOrder purchaseOrder){
    if (purchaseOrder.getWorkflowStatus() != PENDING) {
        List<Error> errors = CompositePoLineValidationUtil.checkMaterialsAvailability(purchaseOrder.getCompositePoLines());
        if (!errors.isEmpty()) {
          throw new HttpException(422, errors.get(0));
        }
    }
    return purchaseOrder;
  }

  public CompletableFuture<Void> createEncumbrancesAndUpdatePoLines(List<EncumbranceRelationsHolder> relationsHolders) {
    return VertxCompletableFuture.allOf(ctx, relationsHolders.stream()
        .map(holder -> createRecordInStorage(JsonObject.mapFrom(holder.getTransaction()), String.format(ENCUMBRANCE_POST_ENDPOINT, lang))
                              .thenCompose(id -> {
                                PoLineFundHolder poLineFundHolder = holder.getPoLineFundHolder();
                                poLineFundHolder.getFundDistribution().setEncumbrance(id);
                                return orderLineHelper.updatePoLinesSummary(Collections.singletonList(poLineFundHolder.getPoLine()));
                              })
                              .exceptionally(fail -> {
                                checkCustomTransactionError(fail);
                                throw new CompletionException(fail);
                              })
        )
      .toArray(CompletableFuture[]::new)
    );
  }

}
