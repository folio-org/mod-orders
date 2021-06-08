package org.folio.helper;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static java.util.stream.Collectors.toList;
import static one.util.streamex.StreamEx.ofSubLists;
import static org.apache.commons.collections4.CollectionUtils.isEmpty;
import static org.apache.commons.collections4.CollectionUtils.isNotEmpty;
import static org.folio.orders.utils.AcqDesiredPermissions.ASSIGN;
import static org.folio.orders.utils.AcqDesiredPermissions.MANAGE;
import static org.folio.orders.utils.ErrorCodes.APPROVAL_REQUIRED_TO_OPEN;
import static org.folio.orders.utils.ErrorCodes.MISSING_ONGOING;
import static org.folio.orders.utils.ErrorCodes.ONGOING_NOT_ALLOWED;
import static org.folio.orders.utils.ErrorCodes.USER_HAS_NO_ACQ_PERMISSIONS;
import static org.folio.orders.utils.ErrorCodes.USER_HAS_NO_APPROVAL_PERMISSIONS;
import static org.folio.orders.utils.ErrorCodes.USER_HAS_NO_REOPEN_PERMISSIONS;
import static org.folio.orders.utils.ErrorCodes.USER_HAS_NO_UNOPEN_PERMISSIONS;
import static org.folio.orders.utils.HelperUtils.COMPOSITE_PO_LINES;
import static org.folio.orders.utils.HelperUtils.WORKFLOW_STATUS;
import static org.folio.orders.utils.HelperUtils.buildQuery;
import static org.folio.orders.utils.HelperUtils.changeOrderStatus;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.HelperUtils.combineCqlExpressions;
import static org.folio.orders.utils.HelperUtils.convertIdsToCqlQuery;
import static org.folio.orders.utils.HelperUtils.convertToCompositePurchaseOrder;
import static org.folio.orders.utils.HelperUtils.deletePoLine;
import static org.folio.orders.utils.HelperUtils.deletePoLines;
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
import static org.folio.orders.utils.OrderStatusTransitionUtil.isTransitionToClosed;
import static org.folio.orders.utils.OrderStatusTransitionUtil.isTransitionToOpen;
import static org.folio.orders.utils.OrderStatusTransitionUtil.isTransitionToPending;
import static org.folio.orders.utils.OrderStatusTransitionUtil.isTransitionToReopen;
import static org.folio.orders.utils.POProtectedFields.getFieldNames;
import static org.folio.orders.utils.POProtectedFields.getFieldNamesForOpenOrder;
import static org.folio.orders.utils.ProtectedOperationType.CREATE;
import static org.folio.orders.utils.ProtectedOperationType.DELETE;
import static org.folio.orders.utils.ProtectedOperationType.UPDATE;
import static org.folio.orders.utils.ResourcePathResolver.ALERTS;
import static org.folio.orders.utils.ResourcePathResolver.PIECES;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINE_NUMBER;
import static org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER;
import static org.folio.orders.utils.ResourcePathResolver.REPORTING_CODES;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.OPEN;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.PENDING;

import java.util.ArrayList;
import java.util.Collection;
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
import java.util.concurrent.CompletionStage;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.ListUtils;
import org.apache.commons.lang3.StringUtils;
import org.folio.HttpStatus;
import org.folio.completablefuture.FolioVertxCompletableFuture;
import org.folio.models.CompositeOrderRetrieveHolder;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.AcqDesiredPermissions;
import org.folio.orders.utils.ErrorCodes;
import org.folio.orders.utils.FundDistributionUtils;
import org.folio.orders.utils.HelperUtils;
import org.folio.orders.utils.POLineProtectedFields;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.orders.utils.validators.CompositePoLineValidationUtil;
import org.folio.orders.utils.validators.OngoingOrderValidator;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.Alert;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PieceCollection;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.jaxrs.model.PurchaseOrderCollection;
import org.folio.rest.jaxrs.model.ReportingCode;
import org.folio.rest.jaxrs.model.Title;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import org.folio.service.AcquisitionsUnitsService;
import org.folio.service.ProtectionService;
import org.folio.service.TagService;
import org.folio.service.finance.expenceclass.ExpenseClassValidationService;
import org.folio.service.finance.transaction.EncumbranceService;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategy;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategyFactory;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.orders.CompositeOrderDynamicDataPopulateService;
import org.folio.service.orders.OrderInvoiceRelationService;
import org.folio.service.orders.OrderLinesSummaryPopulateService;
import org.folio.service.orders.OrderWorkflowType;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.titles.TitlesService;
import org.springframework.beans.factory.annotation.Autowired;

import io.vertx.core.Context;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import one.util.streamex.StreamEx;

public class PurchaseOrderHelper extends AbstractHelper {

  private static final String PERMISSION_ORDER_APPROVE = "orders.item.approve";
  private static final String PERMISSION_ORDER_UNOPEN = "orders.item.unopen";
  private static final String PERMISSION_ORDER_REOPEN = "orders.item.reopen";
  private static final String SEARCH_ORDERS_BY_LINES_DATA = resourcesPath(PURCHASE_ORDER) + SEARCH_PARAMS;
  public static final String GET_PURCHASE_ORDERS = resourcesPath(PURCHASE_ORDER) + SEARCH_PARAMS;
  public static final String EMPTY_ARRAY = "[]";
  public static final String OKAPI_HEADER_PERMISSIONS = "X-Okapi-Permissions";


  // Using variable to "cache" lines for particular order base on assumption that the helper is stateful and new instance is used
  private List<PoLine> orderLines;

  private final PoNumberHelper poNumberHelper;
  private final PurchaseOrderLineHelper orderLineHelper;

  @Autowired
  private OrderLinesSummaryPopulateService orderLinesSummaryPopulateService;
  @Autowired
  private EncumbranceService encumbranceService;
  @Autowired
  private CompositeOrderDynamicDataPopulateService combinedPopulateService;
  @Autowired
  private ExpenseClassValidationService expenseClassValidationService;
  @Autowired
  private EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory;
  @Autowired
  private OrderInvoiceRelationService orderInvoiceRelationService;
  @Autowired
  private TagService tagService;
  @Autowired
  private RestClient restClient;
  @Autowired
  private PurchaseOrderLineService purchaseOrderLineService;
  @Autowired
  private TitlesService titlesService;
  @Autowired
  private AcquisitionsUnitsService acquisitionsUnitsService;
  @Autowired
  private ProtectionService protectionService;
  @Autowired
  private InventoryManager inventoryManager;

  public PurchaseOrderHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders, Context ctx, String lang) {
    super(httpClient, okapiHeaders, ctx, lang);

    this.poNumberHelper = new PoNumberHelper(httpClient, okapiHeaders, ctx, lang);
    this.orderLineHelper = new PurchaseOrderLineHelper(getHttpClient(okapiHeaders), okapiHeaders, ctx, lang);
  }

  public PurchaseOrderHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders, Context ctx, String lang,
      PurchaseOrderLineHelper orderLineHelper) {
    super(httpClient, okapiHeaders, ctx, lang);
    this.poNumberHelper = new PoNumberHelper(httpClient, okapiHeaders, ctx, lang);
    this.orderLineHelper = orderLineHelper;
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
    CompletableFuture<PurchaseOrderCollection> future = new CompletableFuture<>();

    try {
      buildGetOrdersPath(limit, offset, query)
        .thenCompose(endpoint -> handleGetRequest(endpoint, httpClient, okapiHeaders, logger))
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
    return acquisitionsUnitsService.buildAcqUnitsCqlExprToSearchRecords(getRequestContext(), StringUtils.EMPTY)
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
  public CompletableFuture<CompositePurchaseOrder> createPurchaseOrder(CompositePurchaseOrder compPO, RequestContext requestContext) {

    return validateAcqUnitsOnCreate(compPO.getAcqUnitIds(), requestContext)
        .thenCompose(ok -> checkOrderApprovalPermissions(compPO))
        .thenCompose(ok -> setPoNumberIfMissing(compPO)
        .thenCompose(v -> poNumberHelper.checkPONumberUnique(compPO.getPoNumber()))
        .thenCompose(v -> processPoLineTags(compPO))
        .thenCompose(v -> createPOandPOLines(compPO, requestContext))
        .thenCompose(this::populateOrderSummary))
        .thenCompose(compOrder -> encumbranceService.updateEncumbrancesOrderStatus(compOrder.getId(), compOrder.getWorkflowStatus(), requestContext)
                .thenApply(v -> compOrder));
  }

  private CompletionStage<Void> processPoLineTags(CompositePurchaseOrder compPO) {
    // MODORDERS-470 - new tags are all lower-case and no spaces
    for (CompositePoLine line : compPO.getCompositePoLines()) {
      if (line.getTags() != null) {
        var processedTagList = line.getTags()
          .getTagList()
          .stream()
          .filter(StringUtils::isNotBlank)
          .map(tag -> StringUtils.deleteWhitespace(tag).toLowerCase())
          .collect(toList());

        line.getTags().setTagList(processedTagList);
      }
    }

    Set<String> tagLabels = compPO.getCompositePoLines().stream()
      .filter(line -> Objects.nonNull(line.getTags()))
      .flatMap(line -> line.getTags().getTagList().stream())
      .collect(Collectors.toSet());

    if (tagLabels.isEmpty()) {
      return CompletableFuture.completedFuture(null);
    }
    return tagService.createTagsIfMissing(tagLabels, getRequestContext());
  }

  /**
   * @param acqUnitIds acquisitions units assigned to purchase order from request
   * @return completable future completed successfully if all checks pass or exceptionally in case of error/restriction
   *         caused by acquisitions units
   */
  private CompletableFuture<Void> validateAcqUnitsOnCreate(List<String> acqUnitIds, RequestContext requestContext) {
    if (acqUnitIds.isEmpty()) {
      return completedFuture(null);
    }

    return FolioVertxCompletableFuture.runAsync(ctx, () -> verifyUserHasAssignPermission(acqUnitIds))
      .thenCompose(ok -> protectionService.verifyIfUnitsAreActive(acqUnitIds, requestContext))
      .thenCompose(ok -> protectionService.isOperationRestricted(acqUnitIds, ProtectedOperationType.CREATE, requestContext));
  }

  /**
   * Handles update of the order. First retrieve the PO from storage and depending on its content handle passed PO.
   * @param compPO updated {@link CompositePurchaseOrder} purchase order
   * @return completable future holding response indicating success (204 No Content) or error if failed
   */
  public CompletableFuture<Void> updateOrder(CompositePurchaseOrder compPO, RequestContext requestContext) {
    return getPurchaseOrderById(compPO.getId(), lang, httpClient, okapiHeaders, logger)
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
              return unOpenOrder(compPO, requestContext);
            }
            return CompletableFuture.completedFuture(null);
          })
          .thenCompose(ok -> {
            if (isTransitionToClosed(poFromStorage, compPO)) {
              return closeOrder(compPO);
            }
            return CompletableFuture.completedFuture(null);
          })
          .thenCompose(v -> {
            if (isTransitionToOpen) {
              logger.info("updateAndGetOrderWithLines");
              return updateAndGetOrderWithLines(compPO)
                .thenCompose(order -> {
                  compPO.getCompositePoLines().forEach(poLine -> orderLineHelper.updateLocationsQuantity(poLine.getLocations()));
                  return checkLocationsAndPiecesConsistency(compPO.getCompositePoLines(), requestContext);
                });
            } else {
              return CompletableFuture.completedFuture(null);
            }
          })
          .thenCompose(ok -> {
            if (isTransitionToReopen(poFromStorage, compPO)) {
              checkOrderReopenPermissions();
              return reopenOrder(compPO);
            }
            return CompletableFuture.completedFuture(null);
          })
          .thenCompose(v -> updatePoLines(poFromStorage, compPO))
          .thenCompose(v -> {
            if (isTransitionToOpen) {
              return checkOrderApprovalRequired(compPO).thenCompose(ok -> openOrder(compPO, requestContext));
            } else {
              return CompletableFuture.completedFuture(null);
            }
          })
          .thenCompose(ok -> handleFinalOrderStatus(compPO, poFromStorage.getWorkflowStatus().value(), requestContext))
          .thenCompose(v -> encumbranceService.updateEncumbrancesOrderStatus(compPO.getId(), compPO.getWorkflowStatus(), getRequestContext()));
      });
  }

  private CompletionStage<Void> closeOrder(CompositePurchaseOrder compPO) {
    return fetchCompositePoLines(compPO).thenCompose(lines -> {
      EncumbranceWorkflowStrategy strategy = encumbranceWorkflowStrategyFactory.getStrategy(OrderWorkflowType.OPEN_TO_CLOSED);
      CompositePurchaseOrder cloneCompPO = JsonObject.mapFrom(compPO).mapTo(CompositePurchaseOrder.class);
      return strategy.processEncumbrances(cloneCompPO.withCompositePoLines(lines), getRequestContext());
    });
  }

  private CompletableFuture<Void> checkLocationsAndPiecesConsistency(List<CompositePoLine> poLines, RequestContext requestContext) {
    logger.info("checkLocationsAndPiecesConsistency start");
    List<CompositePoLine> linesWithIdWithoutManualPieceReceived = poLines.stream().filter(
      compositePoLine -> StringUtils.isNotEmpty(compositePoLine.getId()) && Boolean.FALSE.equals(compositePoLine.getCheckinItems()))
      .collect(Collectors.toList());
    List<String> lineIds = linesWithIdWithoutManualPieceReceived.stream().map(CompositePoLine::getId).collect(toList());
    return getPiecesByLineIdsByChunks(lineIds, requestContext)
                  .thenApply(pieces -> new PieceCollection().withPieces(pieces).withTotalRecords(pieces.size()))
                  .thenAccept(pieces -> verifyLocationsAndPiecesConsistency(linesWithIdWithoutManualPieceReceived, pieces));
  }

  public CompletableFuture<List<Piece>> getPiecesByLineIdsByChunks(List<String> lineIds, RequestContext requestContext) {
    logger.info("getPiecesByLineIdsByChunks start");
    return collectResultsOnSuccess(
      ofSubLists(new ArrayList<>(lineIds), MAX_IDS_FOR_GET_RQ).map(ids -> getPieceChunkByLineIds(ids, requestContext))
        .toList()).thenApply(
      lists -> lists.stream()
        .flatMap(Collection::stream)
        .collect(Collectors.toList()));
  }

  private CompletableFuture<List<Piece>> getPieceChunkByLineIds(Collection<String> poLineIds, RequestContext requestContext) {
    String query = convertIdsToCqlQuery(poLineIds, "poLineId");
    RequestEntry requestEntry = new RequestEntry(resourcesPath(PIECES)).withQuery(query)
      .withOffset(0)
      .withLimit(poLineIds.size());
    return restClient.get(requestEntry, requestContext, PieceCollection.class)
                     .thenApply(PieceCollection::getPieces);
  }

  private CompletionStage<Void> reopenOrder(CompositePurchaseOrder compPO) {
    return fetchCompositePoLines(compPO).thenCompose(lines -> {
      EncumbranceWorkflowStrategy strategy = encumbranceWorkflowStrategyFactory.getStrategy(OrderWorkflowType.CLOSED_TO_OPEN);
      CompositePurchaseOrder cloneCompPO = JsonObject.mapFrom(compPO).mapTo(CompositePurchaseOrder.class);
      return strategy.processEncumbrances(cloneCompPO.withCompositePoLines(lines), getRequestContext());
    });
  }

  public CompletableFuture<Void> handleFinalOrderStatus(CompositePurchaseOrder compPO, String initialOrdersStatus,
                                                        RequestContext requestContext) {
    PurchaseOrder purchaseOrder = convertToPurchaseOrder(compPO).mapTo(PurchaseOrder.class);
    CompletableFuture<List<PoLine>> future;

    if (isEmpty(compPO.getCompositePoLines())) {
      future = fetchOrderLinesByOrderId(compPO.getId());
    } else {
      future = FolioVertxCompletableFuture.supplyBlockingAsync(ctx, () -> {
        List<PoLine> poLines = HelperUtils.convertToPoLines(compPO.getCompositePoLines());
        changeOrderStatus(purchaseOrder, poLines);
        return poLines;
      });
    }

    return future.thenCompose(poLines -> handleFinalOrderItemsStatus(purchaseOrder, poLines, initialOrdersStatus, requestContext))
      .thenAccept(aVoid -> {
        compPO.setWorkflowStatus(WorkflowStatus.fromValue(purchaseOrder.getWorkflowStatus().value()));
        compPO.setCloseReason(purchaseOrder.getCloseReason());
      })
      .thenCompose(aVoid -> updateOrderSummary(purchaseOrder));
  }

  public CompletableFuture<Void> handleFinalOrderItemsStatus(PurchaseOrder purchaseOrder, List<PoLine> poLines, String initialOrdersStatus,
                                                             RequestContext requestContext) {
    if (isOrderClosing(purchaseOrder.getWorkflowStatus(), initialOrdersStatus)) {
      return updateItemsStatusInInventory(poLines, "On order", "Order closed", requestContext);
    } else if (isOrderReopening(purchaseOrder.getWorkflowStatus(), initialOrdersStatus)) {
      return updateItemsStatusInInventory(poLines, "Order closed", "On order", requestContext);
    }
    return CompletableFuture.completedFuture(null);
  }

  private List<JsonObject> updateStatusName(List<JsonObject> items, String s) {
    items.forEach(item -> item.getJsonObject("status").put("name", s));
    return items;
  }

  private CompletableFuture<List<JsonObject>> getItemsByStatus(List<PoLine> compositePoLines, String itemStatus, RequestContext requestContext) {
    logger.info("org.folio.helper.PurchaseOrderHelper.getItemsByStatus start");
    List<String> lineIds = compositePoLines.stream().map(PoLine::getId).collect(toList());
    // Split all id's by maximum number of id's for get query
    List<CompletableFuture<List<JsonObject>>> futures = StreamEx
      .ofSubLists(lineIds, MAX_IDS_FOR_GET_RQ)
      // Get item records from Inventory storage
      .map(ids -> {
        String query = String.format("status.name==%s and %s", itemStatus, HelperUtils.convertFieldListToCqlQuery(ids, InventoryManager.ITEM_PURCHASE_ORDER_LINE_IDENTIFIER, true));
        return inventoryManager.getItemRecordsByQuery(query, requestContext);
      })
      .toList();

    return collectResultsOnSuccess(futures)
      .thenApply(lists -> StreamEx.of(lists).toFlatList(jsonObjects -> jsonObjects));
  }

  private CompletableFuture<Void> updateItemsStatusInInventory(List<PoLine> poLines, String currentStatus, String newStatus, RequestContext requestContext) {
    return getItemsByStatus(poLines, currentStatus, requestContext)
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
    CompletableFuture<Void> future = new CompletableFuture<>();

    getPurchaseOrderById(id, lang, httpClient, okapiHeaders, logger)
      .thenAccept(purchaseOrder -> {
        CompositePurchaseOrder compPo = convertToCompositePurchaseOrder(purchaseOrder);
        protectionService.isOperationRestricted(compPo.getAcqUnitIds(), DELETE, getRequestContext())
          .thenCompose(v -> orderInvoiceRelationService.checkOrderInvoiceRelationship(id, getRequestContext()))
          .thenAccept(aVoid -> encumbranceService.deleteOrderEncumbrances(id, getRequestContext())
            .thenCompose(v -> deletePoLines(id, lang, httpClient, okapiHeaders, logger))
            .thenRun(() -> {
              logger.info("Successfully deleted poLines, proceeding with purchase order");
              handleDeleteRequest(resourceByIdPath(PURCHASE_ORDER, id), httpClient, okapiHeaders, logger)
                .thenAccept(rs -> {
                  logger.info("Successfully deleted order with id={}", id);
                  future.complete(null);
                })
                .exceptionally(t -> {
                  logger.error("Failed to delete the order with id={}", id, t.getCause());
                  future.completeExceptionally(t);
                  return null;
                });
            })
            .exceptionally(t -> {
              logger.error("Failed to delete PO Lines of the order with id={}", id, t.getCause());
              future.completeExceptionally(t);
              return null;
            })
          )
          .exceptionally(t -> {
            logger.error("User with id={} is forbidden to view delete with id={}", getCurrentUserId(), id, t.getCause());
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

    CompletableFuture<CompositePurchaseOrder> future = new CompletableFuture<>();
    getPurchaseOrderById(id, lang, httpClient, okapiHeaders, logger)
      .thenApply(HelperUtils::convertToCompositePurchaseOrder)
      .thenAccept(compPO -> protectionService.isOperationRestricted(compPO.getAcqUnitIds(), ProtectedOperationType.READ, getRequestContext())
        .thenAccept(ok -> updateAndGetOrderWithLines(compPO)
          .thenCompose(this::fetchNonPackageTitles)
          .thenAccept(linesIdTitles -> populateInstanceId(linesIdTitles, compPO.getCompositePoLines()))
          .thenCompose(po -> combinedPopulateService.populate(new CompositeOrderRetrieveHolder(compPO), getRequestContext()))
          .thenApply(CompositeOrderRetrieveHolder::getOrder)
          .thenAccept(future::complete)
          .exceptionally(t -> {
            logger.error("Failed to get lines for order with id={}", id, t.getCause());
            future.completeExceptionally(t);
            return null;
          }))
        .exceptionally(t -> {
          logger.error("User with id={} is forbidden to view order with id={}", getCurrentUserId(), id, t.getCause());
          future.completeExceptionally(t);
          return null;
        }))
      .exceptionally(t -> {
        logger.error("Failed to build composite purchase order with id={}", id, t.getCause());
        future.completeExceptionally(t);
        return null;
      });

    return future;
  }

  private CompletableFuture<CompositePurchaseOrder> populateOrderSummary(CompositePurchaseOrder order) {
    return orderLinesSummaryPopulateService.populate(new CompositeOrderRetrieveHolder(order), getRequestContext())
            .thenApply(CompositeOrderRetrieveHolder::getOrder);
  }

  /**
   * Handles transition of given order to OPEN status.
   *
   * @param compPO Purchase Order to open
   * @return CompletableFuture that indicates when transition is completed
   */
  public CompletableFuture<Void> openOrder(CompositePurchaseOrder compPO, RequestContext requestContext) {
    compPO.setWorkflowStatus(OPEN);
    compPO.setDateOrdered(new Date());
    return expenseClassValidationService.validateExpenseClasses(compPO.getCompositePoLines(), requestContext)
      .thenAccept(v -> FundDistributionUtils.validateFundDistributionTotal(compPO.getCompositePoLines()))
      .thenAccept(v -> OngoingOrderValidator.validate(compPO))
      .thenApply(v -> this.validateMaterialTypes(compPO))
      .thenCompose(this::fetchNonPackageTitles)
      .thenCompose(linesIdTitles -> {
          populateInstanceId(linesIdTitles, compPO.getCompositePoLines());
          return openOrderUpdateInventory(linesIdTitles, compPO, requestContext);
        })
      .thenApply(aVoid -> encumbranceWorkflowStrategyFactory.getStrategy(OrderWorkflowType.PENDING_TO_OPEN))
      .thenCompose(strategy -> strategy.processEncumbrances(compPO, requestContext))
      .thenAccept(ok -> changePoLineStatuses(compPO))
      .thenCompose(ok -> openOrderUpdatePoLinesSummary(compPO.getCompositePoLines()));
  }

  public CompletableFuture<Void> openOrderUpdatePoLinesSummary(List<CompositePoLine> compositePoLines) {
    return CompletableFuture.allOf( compositePoLines.stream()
      .map(this::openOrderConvertToPoLine)
      .map(line -> purchaseOrderLineService.updateOrderLine(line, getRequestContext()))
      .toArray(CompletableFuture[]::new));
  }

  public PoLine openOrderConvertToPoLine(CompositePoLine compPoLine) {
    JsonObject pol = JsonObject.mapFrom(compPoLine);
    pol.remove(ALERTS);
    pol.remove(REPORTING_CODES);
    PoLine poLine = pol.mapTo(PoLine.class);
    poLine.setAlerts(compPoLine.getAlerts().stream().map(Alert::getId).collect(toList()));
    poLine.setReportingCodes(compPoLine.getReportingCodes().stream().map(ReportingCode::getId).collect(toList()));
    return poLine;
  }

  private CompletableFuture<Void> updateItemsInInventory(List<JsonObject> items) {
    return CompletableFuture.allOf(items.stream()
                                        .map(item -> inventoryManager.updateItem(item, getRequestContext()))
                                        .toArray(CompletableFuture[]::new));
  }

  private CompletableFuture<Map<String, List<Title>>> fetchNonPackageTitles(CompositePurchaseOrder compPO) {
    List<String> lineIds = getNonPackageLineIds(compPO.getCompositePoLines());
    return titlesService.getTitlesByPoLineIds(lineIds, getRequestContext());
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
  public CompletableFuture<Boolean> validateOrder(CompositePurchaseOrder compPO, RequestContext requestContext) {

    return setCreateInventoryDefaultValues(compPO)
      .thenAccept(v -> validateOrderPoLines(compPO))
      .thenCompose(v -> validateIsbnValues(compPO, requestContext))
      .thenCompose(v -> validatePoLineLimit(compPO))
      .thenCompose(v -> validateVendor(compPO))
      .thenAccept(v -> validateRenewalInfo(compPO))
      .thenApply(v -> getErrors().isEmpty());
  }

  public void validateOrderPoLines(CompositePurchaseOrder compositeOrder) {
    List<Error> errors = new ArrayList<>();
    for (CompositePoLine compositePoLine : compositeOrder.getCompositePoLines()) {
      errors.addAll(CompositePoLineValidationUtil.validatePoLine(compositePoLine));
    }
    addProcessingErrors(errors);
  }

  private CompletableFuture<Void> validateIsbnValues(CompositePurchaseOrder compPO, RequestContext requestContext) {
    CompletableFuture<?>[] futures = compPO.getCompositePoLines()
      .stream()
      .map(line -> orderLineHelper.validateAndNormalizeISBN(line, requestContext))
      .toArray(CompletableFuture[]::new);

    return CompletableFuture.allOf(futures);
  }

  private CompletableFuture<Void> setCreateInventoryDefaultValues(CompositePurchaseOrder compPO) {
    CompletableFuture<?>[] futures = compPO.getCompositePoLines()
      .stream()
      .map(orderLineHelper::setTenantDefaultCreateInventoryValues)
      .toArray(CompletableFuture[]::new);

    return CompletableFuture.allOf(futures);
  }

  /**
   * Validates purchase order which already exists in the storage.
   * Checks PO Number presence, validates that provided order id corresponds to one set in order and its lines.
   * If all is okay, {@link #validateOrderPoLines(CompositePurchaseOrder)} is called afterwards.
   * @param orderId Purchase Order id
   * @param compPO Purchase Order to validate
   * @return completable future which might be completed with {@code true} if order is valid, {@code false} if not valid or an exception if processing fails
   */
  public CompletableFuture<Boolean> validateExistingOrder(String orderId, CompositePurchaseOrder compPO, RequestContext requestContext) {
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

    return validateOrder(compPO, requestContext);
  }

  public CompletableFuture<Void> unOpenOrder(CompositePurchaseOrder compPO, RequestContext requestContext) {
    CompletableFuture<Void> future = new CompletableFuture<>();
    updateAndGetOrderWithLines(compPO)
      .thenApply(aVoid -> encumbranceWorkflowStrategyFactory.getStrategy(OrderWorkflowType.OPEN_TO_PENDING))
      .thenCompose(strategy -> strategy.processEncumbrances(compPO, getRequestContext()))
      .thenAccept(ok -> orderLineHelper.makePoLinesPending(compPO.getCompositePoLines()))
      .thenCompose(ok -> unOpenOrderUpdatePoLinesSummary(compPO.getCompositePoLines(), requestContext))
      .thenAccept(v-> future.complete(null))
      .exceptionally(t -> {
        future.completeExceptionally(t);
        return null;
      });
    return future;
  }

  public CompletableFuture<Void> unOpenOrderUpdatePoLinesSummary(List<CompositePoLine> compositePoLines, RequestContext requestContext) {
    return CompletableFuture.allOf( compositePoLines.stream()
      .map(HelperUtils::convertToPoLine)
      .map(line -> purchaseOrderLineService.updateOrderLine(line, requestContext))
      .toArray(CompletableFuture[]::new));
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
          return fetchCompositePoLines(compPO)
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
       return getTenantConfiguration()
        .thenAccept(config -> {
          int limit = getPoLineLimit(config);
          if (compPO.getCompositePoLines().size() > limit) {
            addProcessingError(ErrorCodes.POL_LINES_LIMIT_EXCEEDED.toError());
          }
        });
    }
    return completedFuture(null);
  }

  private CompletableFuture<CompositePurchaseOrder> createPOandPOLines(CompositePurchaseOrder compPO, RequestContext requestContext) {
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
              .thenCompose(po -> openOrder(po, requestContext)))
            .thenCompose(ok -> handleFinalOrderStatus(compPO, finalStatus.value(), requestContext));
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
    return getTenantConfiguration().thenAccept(config -> {
      boolean isApprovalRequired = isApprovalRequiredConfiguration(config);
      if (isApprovalRequired && compPO.getApproved().equals(Boolean.TRUE)) {
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

  private void checkOrderReopenPermissions() {
    if (isUserNotHaveReopenPermission()) {
      throw new HttpException(HttpStatus.HTTP_FORBIDDEN.toInt(), USER_HAS_NO_REOPEN_PERMISSIONS);
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
    return getTenantConfiguration().thenAccept(config -> {
      boolean isApprovalRequired = isApprovalRequiredConfiguration(config);
      if (isApprovalRequired && !compPO.getApproved().equals(Boolean.TRUE)) {
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
            .map(compositePoLine -> orderLineHelper.createPoLine(compositePoLine, compPO))
            .collect(Collectors.toList());
    return HelperUtils.collectResultsOnSuccess(futures);
  }

  private CompletableFuture<List<CompositePoLine>> fetchCompositePoLines(CompositePurchaseOrder compPO) {
    if (CollectionUtils.isEmpty(compPO.getCompositePoLines())) {
      return  purchaseOrderLineService.getCompositePoLinesByOrderId(compPO.getId(), getRequestContext())
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
      return purchaseOrderLineService.getCompositePoLinesByOrderId(compPO.getId(), getRequestContext())
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

  private void changePoLineStatuses(CompositePurchaseOrder compPO) {
    compPO.getCompositePoLines().forEach(poLine -> {
      changeReceiptStatus(compPO, poLine);
      changePaymentStatus(compPO, poLine);
    });
  }

  private void changePaymentStatus(CompositePurchaseOrder compPO, CompositePoLine poLine) {
    if (compPO.getOrderType().equals(CompositePurchaseOrder.OrderType.ONGOING)) {
      poLine.setPaymentStatus(CompositePoLine.PaymentStatus.ONGOING);
    }
    else if (poLine.getPaymentStatus() == CompositePoLine.PaymentStatus.PENDING) {
      poLine.setPaymentStatus(CompositePoLine.PaymentStatus.AWAITING_PAYMENT);
    }
  }

  private void changeReceiptStatus(CompositePurchaseOrder compPO, CompositePoLine poLine) {
    if (compPO.getOrderType().equals(CompositePurchaseOrder.OrderType.ONGOING)) {
      poLine.setReceiptStatus(CompositePoLine.ReceiptStatus.ONGOING);
    }
    else if (poLine.getReceiptStatus() == CompositePoLine.ReceiptStatus.PENDING) {
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
    logger.info("updatePoLines start");
    if (isPoLinesUpdateRequired(poFromStorage, compPO)) {
      return fetchOrderLinesByOrderId(compPO.getId())
        .thenCompose(existingPoLines -> {
          if (isNotEmpty(compPO.getCompositePoLines())) {
            // New PO Line(s) can be added only to Pending order
            if (poFromStorage.getWorkflowStatus() != PENDING && hasNewPoLines(compPO, existingPoLines)) {
              throw new HttpException(422, poFromStorage.getWorkflowStatus() == OPEN ? ErrorCodes.ORDER_OPEN : ErrorCodes.ORDER_CLOSED);
            }
            validatePOLineProtectedFieldsChangedInPO(poFromStorage, compPO, existingPoLines);
            logger.info("updatePoLines start");
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
    return handlePutRequest(resourceByIdPath(PURCHASE_ORDER, purchaseOrder.getId()), JsonObject.mapFrom(purchaseOrder), httpClient, okapiHeaders, logger);
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
    compPO.setTotalEncumbered(null);
    compPO.setTotalExpended(null);
    compPO.setNeedReEncumber(null);
    compPO.setDateOrdered(null);

    JsonObject purchaseOrder = JsonObject.mapFrom(compPO);
    purchaseOrder.remove(COMPOSITE_PO_LINES);
    return purchaseOrder;
  }

  private CompletableFuture<Void> updatePoLinesNumber(CompositePurchaseOrder compOrder, List<PoLine> poLinesFromStorage) {
    CompletableFuture<?>[] futures = poLinesFromStorage
      .stream()
      .map(lineFromStorage -> {
        lineFromStorage.setPoLineNumber(orderLineHelper.buildNewPoLineNumber(lineFromStorage, compOrder.getPoNumber()));
        return purchaseOrderLineService.updateOrderLine(lineFromStorage, getRequestContext());
      })
       .toArray(CompletableFuture[]::new);

    return CompletableFuture.allOf(futures);
  }

  private CompletableFuture<Void> openOrderUpdateInventory(Map<String, List<Title>> lineIdsTitles, CompositePurchaseOrder compPO,
                                                           RequestContext requestContext) {
    return CompletableFuture.allOf(
      compPO.getCompositePoLines()
        .stream()
        .map(poLine -> orderLineHelper.openOrderUpdateInventory(poLine, getFirstTitleIdIfExist(lineIdsTitles, poLine), requestContext))
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
    logger.info("handlePoLines start");
    List<CompletableFuture<?>> futures = new ArrayList<>(processPoLinesCreation(compOrder, poLinesFromStorage));
    if (!poLinesFromStorage.isEmpty()) {
      futures.addAll(processPoLinesUpdate(compOrder, poLinesFromStorage));
      // The remaining unprocessed PoLines should be removed
      poLinesFromStorage
        .forEach(poLine -> futures.add(orderInvoiceRelationService.checkOrderInvoiceRelationship(compOrder.getId(), getRequestContext())
            .thenCompose(v -> encumbranceService.deletePoLineEncumbrances(poLine.getId(), getRequestContext())
              .thenCompose(ok -> deletePoLine(JsonObject.mapFrom(poLine), httpClient, okapiHeaders, logger)))));

    }
    return CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]));
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
          orderLineHelper.updateEstimatedPrice(line);

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
      .map(compPOL -> orderLineHelper.createPoLine(compPOL, compOrder))
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

    return FolioVertxCompletableFuture.runAsync(ctx, () -> verifyUserHasManagePermission(updatedAcqUnitIds, currentAcqUnitIds))
      // Check that all newly assigned units are active/exist
      .thenCompose(ok -> protectionService.verifyIfUnitsAreActive(ListUtils.subtract(updatedAcqUnitIds, currentAcqUnitIds), getRequestContext()))
      .thenCompose(ok -> getInvolvedOperations(updatedOrder))
      // The check should be done against currently assigned (persisted in storage) units
      .thenCompose(protectedOperationTypes -> protectionService.isOperationRestricted(currentAcqUnitIds, protectedOperationTypes, getRequestContext()));
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
    logger.info("fetchOrderLinesByOrderId start");
    if (orderLines != null) {
      return CompletableFuture.completedFuture(new ArrayList<>(orderLines));
    }

    return getPoLines(orderId, lang, httpClient, okapiHeaders, logger)
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

  private boolean isUserNotHaveReopenPermission() {
    return !getProvidedPermissions().contains(PERMISSION_ORDER_REOPEN);
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


}
