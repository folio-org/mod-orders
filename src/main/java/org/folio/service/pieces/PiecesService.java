package org.folio.service.pieces;

import static java.util.concurrent.CompletableFuture.allOf;
import static java.util.concurrent.CompletableFuture.completedFuture;
import static java.util.stream.Collectors.collectingAndThen;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.summingInt;
import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.HelperUtils.ID;
import static org.folio.orders.utils.HelperUtils.calculateInventoryItemsQuantity;
import static org.folio.orders.utils.HelperUtils.calculatePiecesQuantityWithoutLocation;
import static org.folio.orders.utils.HelperUtils.groupLocationsById;
import static org.folio.orders.utils.HelperUtils.isItemsUpdateRequired;
import static org.folio.orders.utils.ProtectedOperationType.DELETE;
import static org.folio.orders.utils.ResourcePathResolver.PIECES;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.stream.Collectors;

import javax.ws.rs.core.Response;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.service.inventory.InventoryManager;
import org.folio.orders.events.handlers.MessageAddress;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.rest.exceptions.InventoryException;
import org.folio.orders.utils.ErrorCodes;
import org.folio.orders.utils.HelperUtils;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.Piece.ReceivingStatus;
import org.folio.rest.jaxrs.model.PieceCollection;
import org.folio.rest.jaxrs.model.Title;
import org.folio.service.ProtectionService;
import org.folio.service.orders.CompositePurchaseOrderService;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.titles.TitlesService;

import io.vertx.core.json.JsonObject;
import one.util.streamex.StreamEx;

public class PiecesService {
  private static final Logger logger = LogManager.getLogger(PiecesService.class);
  private static final String INSTANCES = "instances";
  private static final String ENDPOINT = resourcesPath(PIECES);
  private static final String BY_ID_ENDPOINT = ENDPOINT + "/{id}";

  private final TitlesService titlesService;
  private final ProtectionService protectionService;
  private final CompositePurchaseOrderService compositePurchaseOrderService;
  private final PurchaseOrderLineService purchaseOrderLineService;
  private final InventoryManager inventoryManager;
  private final RestClient restClient;
  private final PieceChangeReceiptStatusPublisher receiptStatusPublisher;

  public PiecesService(RestClient restClient, TitlesService titlesService, ProtectionService protectionService,
                       CompositePurchaseOrderService compositePurchaseOrderService,
                       PurchaseOrderLineService purchaseOrderLineService,
                       InventoryManager inventoryManager, PieceChangeReceiptStatusPublisher receiptStatusPublisher) {

    this.titlesService = titlesService;
    this.protectionService = protectionService;
    this.compositePurchaseOrderService = compositePurchaseOrderService;
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.inventoryManager = inventoryManager;
    this.restClient = restClient;
    this.receiptStatusPublisher = receiptStatusPublisher;
  }

  public CompletableFuture<Piece> createPiece(Piece piece, RequestContext requestContext) {
     logger.info("createPiece start");
      return getCompositeOrderByPoLineId(piece.getPoLineId(), requestContext)
        .thenCompose(order -> protectionService.isOperationRestricted(order.getAcqUnitIds(), ProtectedOperationType.CREATE, requestContext)
                                               .thenApply(v -> order))
        .thenCompose(order -> updateInventory(order.getCompositePoLines().get(0), piece, requestContext))
        .thenCompose(v -> {
          RequestEntry requestEntry = new RequestEntry(ENDPOINT);
          return restClient.post(requestEntry, piece, requestContext, Piece.class);
        });
  }

  // Flow to update piece
  // 1. Before update, get piece by id from storage and store receiving status
  // 2. Update piece with new content and complete future
  // 3. Create a message and check if receivingStatus is not consistent with storage; if yes - send a message to event bus
  public CompletableFuture<Void> updatePieceRecord(Piece piece, RequestContext requestContext) {
    CompletableFuture<Void> future = new CompletableFuture<>();
    getOrderByPoLineId(piece.getPoLineId(), requestContext)
      .thenCompose(order -> protectionService.isOperationRestricted(order.getAcqUnitIds(), ProtectedOperationType.UPDATE, requestContext))
      .thenCompose(v -> inventoryManager.updateItemWithPoLineId(piece.getItemId(), piece.getPoLineId(), requestContext))
      .thenAccept(vVoid ->
        getPieceById(piece.getId(), requestContext).thenAccept(pieceStorage -> {
          ReceivingStatus receivingStatusStorage = pieceStorage.getReceivingStatus();
          updatePiece(piece, requestContext)
            .thenAccept(future::complete)
            .thenAccept(afterUpdate -> {

              JsonObject messageToEventBus = new JsonObject();
              messageToEventBus.put("poLineIdUpdate", piece.getPoLineId());

              ReceivingStatus receivingStatusUpdate = piece.getReceivingStatus();
              logger.debug("receivingStatusStorage -- {}", receivingStatusStorage);
              logger.debug("receivingStatusUpdate -- {}", receivingStatusUpdate);

              if (receivingStatusStorage.compareTo(receivingStatusUpdate) != 0) {
                receiptConsistencyPiecePoLine(messageToEventBus, requestContext);
              }
            })
            .exceptionally(e -> {
              logger.error("Error updating piece by id to storage {}", piece.getId(), e);
              future.completeExceptionally(e);
              return null;
            });
        })
        .exceptionally(e -> {
          logger.error("Error getting piece by id from storage {}", piece.getId(), e);
          future.completeExceptionally(e);
          return null;
        })
    )
      .exceptionally(t -> {
        logger.error("User to update piece with id={}", piece.getId(), t.getCause());
        future.completeExceptionally(t);
        return null;
    });
    return future;
  }

  public CompletableFuture<PieceCollection> getPieces(int limit, int offset, String query, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT).withQuery(query)
      .withOffset(offset)
      .withLimit(limit);
    return restClient.get(requestEntry, requestContext, PieceCollection.class);
  }

  public CompletableFuture<Piece> getPieceById(String pieceId, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(pieceId);
    return restClient.get(requestEntry, requestContext, Piece.class);
  }

  public CompletableFuture<Void> updatePiece(Piece piece, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(piece.getId());
    return restClient.put(requestEntry, piece, requestContext);
  }

  public CompletableFuture<Void> deletePiece(String pieceId, RequestContext requestContext) {
    return getPieceById(pieceId, requestContext)
      .thenCompose(piece -> getCompositeOrderByPoLineId(piece.getPoLineId(), requestContext)
        .thenCompose(purchaseOrder -> protectionService.isOperationRestricted(purchaseOrder.getAcqUnitIds(), DELETE, requestContext)
          .thenCompose(vVoid -> inventoryManager.getNumberOfRequestsByItemId(piece.getItemId(), requestContext))
          .thenAccept(numOfRequests -> {
            if (numOfRequests > 0) {
              throw new HttpException(422, ErrorCodes.REQUEST_FOUND.toError());
            }
          })
          .thenCompose(aVoid -> {
            RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(pieceId);
            return restClient.delete(requestEntry, requestContext);
          })
          .thenCompose(aVoid -> {
            if (StringUtils.isNotEmpty(piece.getItemId())) {
              // Attempt to delete item
              return inventoryManager.deleteItem(piece.getItemId(), requestContext)
                .exceptionally(t -> {
                  // Skip error processing if item has already deleted
                  if (t instanceof HttpException && ((HttpException) t).getCode() == 404) {
                    return null;
                  } else {
                    throw new CompletionException(t);
                  }
                });
            } else {
              return CompletableFuture.completedFuture(null);
            }
          })
        )
      );
  }

  public CompletableFuture<CompositePurchaseOrder> getOrderByPoLineId(String poLineId, RequestContext requestContext) {
    return purchaseOrderLineService.getOrderLineById(poLineId, requestContext)
      .thenCompose(poLine -> compositePurchaseOrderService.getCompositeOrderById(poLine.getPurchaseOrderId(), requestContext));
  }

  private CompletableFuture<CompositePurchaseOrder> getCompositeOrderByPoLineId(String poLineId, RequestContext requestContext) {
    logger.info("getCompositeOrderByPoLineId start");
    return purchaseOrderLineService.getOrderLineById(poLineId, requestContext)
                                  .thenCompose(poLine -> getCompositePurchaseOrder(poLine.getPurchaseOrderId(), requestContext));
  }

  public CompletableFuture<CompositePurchaseOrder> getCompositePurchaseOrder(String purchaseOrderId, RequestContext requestContext) {
    logger.info("getCompositePurchaseOrder start");
    return compositePurchaseOrderService.getCompositeOrderById(purchaseOrderId, requestContext)
      .exceptionally(t -> {
        Throwable cause = t.getCause();
        // The case when specified order does not exist
        if (cause instanceof HttpException && ((HttpException) cause).getCode() == Response.Status.NOT_FOUND.getStatusCode()) {
          throw new HttpException(404, ErrorCodes.ORDER_NOT_FOUND);
        }
        throw t instanceof CompletionException ? (CompletionException) t : new CompletionException(cause);
      });
  }

  /**
   * Creates Inventory records associated with given PO line and updates PO line with corresponding links.
   *
   * @param compPOL Composite PO line to update Inventory for
   * @return CompletableFuture with void.
   */
  public CompletableFuture<Piece> updateInventory(CompositePoLine compPOL, Piece piece, RequestContext requestContext) {
    if (Boolean.TRUE.equals(compPOL.getIsPackage())) {
      return titlesService.getTitleById(piece.getTitleId(), requestContext)
        .thenCompose(title -> handleInstanceRecord(title, requestContext))
        .thenCompose(title -> titlesService.updateTitle(title, requestContext).thenApply(json -> title))
        .thenCompose(title -> handleHoldingsRecord(compPOL, piece.getLocationId(), title.getInstanceId(), requestContext))
        .thenCompose(holdingId -> createItemRecord(compPOL, holdingId, requestContext))
        .thenApply(itemId -> itemId != null ? piece.withItemId(itemId) : piece);
    }
    else
    {
      return inventoryManager.updateItemWithPoLineId(piece.getItemId(), piece.getPoLineId(), requestContext)
                            .thenApply(v -> piece);
    }
  }

  public CompletableFuture<Title> handleInstanceRecord(Title title, RequestContext requestContext) {
    if (title.getInstanceId() != null) {
      return CompletableFuture.completedFuture(title);
    } else {
      return getOrCreateInstanceRecord(title, requestContext).thenApply(title::withInstanceId);
    }
  }

  public CompletableFuture<String> getOrCreateInstanceRecord(Title title, RequestContext requestContext) {
    // proceed with new Instance Record creation if no productId is provided
    if (!CollectionUtils.isNotEmpty(title.getProductIds())) {
      return inventoryManager.createInstanceRecord(title, requestContext);
    }

    return inventoryManager.searchInstancesByProducts(title.getProductIds(), requestContext)
      .thenCompose(instances -> {
        if (!instances.getJsonArray(INSTANCES).isEmpty()) {
          String instanceId = inventoryManager.getFirstObjectFromResponse(instances, INSTANCES).getString(ID);
          return completedFuture(instanceId);
        }
        return inventoryManager.createInstanceRecord(title, requestContext);
      });
  }

  /**
   * Return id of created  Holding
   */
  public CompletableFuture<String> handleHoldingsRecord(final CompositePoLine compPOL, String locationId, String instanceId,
                                                        RequestContext requestContext) {
    try {
      if (HelperUtils.isHoldingsUpdateRequired(compPOL.getEresource(), compPOL.getPhysical())) {
        return inventoryManager.getOrCreateHoldingsRecord(instanceId, locationId, requestContext);
      } else {
        return CompletableFuture.completedFuture(null);
      }
    }
    catch (Exception e) {
      return CompletableFuture.failedFuture(e);
    }

  }

  /**
   * Return id of created  Item
   */
  public CompletableFuture<String> createItemRecord(CompositePoLine compPOL, String holdingId, RequestContext requestContext) {
    final int ITEM_QUANTITY = 1;
    logger.debug("Handling {} items for PO Line and holdings with id={}", ITEM_QUANTITY, holdingId);
    CompletableFuture<String> itemFuture = new CompletableFuture<>();
    try {
      if (isItemsUpdateRequired(compPOL)) {
        if (compPOL.getOrderFormat() == ELECTRONIC_RESOURCE) {
           inventoryManager.createMissingElectronicItems(compPOL, holdingId, ITEM_QUANTITY, requestContext)
                          .thenApply(idS -> itemFuture.complete(idS.get(0)))
                          .exceptionally(itemFuture::completeExceptionally);
        } else {
          inventoryManager.createMissingPhysicalItems(compPOL, holdingId, ITEM_QUANTITY, requestContext)
                         .thenApply(idS -> itemFuture.complete(idS.get(0)))
                         .exceptionally(itemFuture::completeExceptionally);
        }
      }
      else {
        itemFuture.complete(null);
      }
    } catch (Exception e) {
       itemFuture.completeExceptionally(e);
    }
    return itemFuture;
  }

  /**
   * Creates pieces that are not yet in storage
   *
   * @param compPOL PO line to create Pieces Records for
   * @param expectedPiecesWithItem expected Pieces to create with created associated Items records
   * @return void future
   */
  public CompletableFuture<Void> createPieces(CompositePoLine compPOL, String titleId,
                                               List<Piece> expectedPiecesWithItem, boolean isOpenOrderFlow, RequestContext requestContext) {
    int createdItemsQuantity = expectedPiecesWithItem.size();
    // do not create pieces in case of check-in flow
    if (compPOL.getCheckinItems() != null && compPOL.getCheckinItems()) {
      return completedFuture(null);
    }
    logger.info("Get pieces by poLine ID");
    return getPiecesByPoLineId(compPOL, requestContext)
      .thenCompose(existingPieces -> {
        List<Piece> piecesToCreate;
        List<Piece> piecesWithLocationToProcess = createPiecesByLocationId(compPOL, expectedPiecesWithItem, existingPieces);
        List<Piece> onlyLocationChangedPieces = getPiecesWithChangedLocation(compPOL, piecesWithLocationToProcess, existingPieces);
        if ((onlyLocationChangedPieces.size() == piecesWithLocationToProcess.size()) && !isOpenOrderFlow) {
          return allOf(onlyLocationChangedPieces.stream()
                    .map(piece -> updatePieceRecord(piece, requestContext)).toArray(CompletableFuture[]::new));
        } else {
          piecesToCreate = new ArrayList<>(piecesWithLocationToProcess);
        }
        piecesToCreate.addAll(createPiecesWithoutLocationId(compPOL, existingPieces));
        piecesToCreate.forEach(piece -> piece.setTitleId(titleId));
        logger.info("Trying to create pieces");
        return allOf(piecesToCreate.stream()
                  .map(piece -> createPiece(piece, requestContext)).toArray(CompletableFuture[]::new));
      })
      .thenAccept(v -> validateItemsCreation(compPOL, createdItemsQuantity));
  }

  private List<Piece> createPiecesByLocationId(CompositePoLine compPOL, List<Piece> expectedPiecesWithItem, List<Piece> existingPieces) {
    List<Piece> piecesToCreate = new ArrayList<>();
    // For each location collect pieces that need to be created.
    groupLocationsById(compPOL)
      .forEach((existingPieceLocationId, existingPieceLocations) -> {
        List<Piece> filteredExistingPieces = filterByLocationId(existingPieces, existingPieceLocationId);
        List<Piece> createdPiecesWithItem = processPiecesWithItem(expectedPiecesWithItem, filteredExistingPieces, existingPieceLocationId);
        piecesToCreate.addAll(createdPiecesWithItem);
        List<Piece> piecesWithoutItem = processPiecesWithoutItem(compPOL, filteredExistingPieces, existingPieceLocationId, existingPieceLocations);
        piecesToCreate.addAll(piecesWithoutItem);
      });
    return piecesToCreate;
  }

  /**
   * Search for pieces which might be already created for the PO line
   * @param compPOL PO line to retrieve Piece Records for
   * @return future with list of Pieces
   */
  private CompletableFuture<List<Piece>> getPiecesByPoLineId(CompositePoLine compPOL, RequestContext requestContext) {
    String query = String.format("poLineId==%s", compPOL.getId());
    RequestEntry requestEntry = new RequestEntry(resourcesPath(PIECES)).withQuery(query)
      .withLimit(Integer.MAX_VALUE)
      .withOffset(0);

    return restClient.get(requestEntry, requestContext, PieceCollection.class)
                     .thenApply(PieceCollection::getPieces);
  }

  private List<Piece> getPiecesWithChangedLocation(CompositePoLine compPOL, List<Piece> needProcessPieces, List<Piece> existingPieces) {
    Map<String, Map<Piece.Format, Integer>> existingPieceMap = numOfPiecesByFormatAndLocationId(existingPieces, compPOL.getId());
    Map<String, Map<Piece.Format, Integer>> needProcessPiecesMap = numOfPiecesByFormatAndLocationId(needProcessPieces, compPOL.getId());

    List<Piece> piecesForLocationUpdate = new ArrayList<>();
    for (Map.Entry<String, Map<Piece.Format, Integer>> entry : existingPieceMap.entrySet()) {
      String existingPieceLocationId = entry.getKey();
      Map<Piece.Format, Integer> existingPieceQtyMap = entry.getValue();
      for (Map.Entry<Piece.Format, Integer> existPieceFormatQty : existingPieceQtyMap.entrySet()) {
        Map<Piece.Format, Integer> pieceLocationMap = needProcessPiecesMap.get(existingPieceLocationId);
        if (pieceLocationMap == null) {
          needProcessPiecesMap.forEach((newLocationId, value) -> {
            Integer pieceQty = value.get(existPieceFormatQty.getKey());
            if (pieceQty != null && pieceQty.equals(existPieceFormatQty.getValue())) {
              List<Piece> piecesWithUpdatedLocation = existingPieces.stream()
                .filter(piece -> existingPieceLocationId.equals(piece.getLocationId())
                  && existPieceFormatQty.getKey() == piece.getFormat())
                .map(piece -> piece.withLocationId(newLocationId))
                .collect(Collectors.toList());
              piecesForLocationUpdate.addAll(piecesWithUpdatedLocation);
            }
          });
        }
      }
    }
    return piecesForLocationUpdate;
  }

  private List<Piece> createPiecesWithoutLocationId(CompositePoLine compPOL, List<Piece> existingPieces) {
    List<Piece> piecesToCreate = new ArrayList<>();
    Map<Piece.Format, Integer> expectedQuantitiesWithoutLocation = calculatePiecesQuantityWithoutLocation(compPOL);
    Map<Piece.Format, Integer> existingPiecesQuantities = calculateQuantityOfExistingPiecesWithoutLocation(existingPieces);
    expectedQuantitiesWithoutLocation.forEach((format, expectedQty) -> {
      int remainingPiecesQuantity = expectedQty - existingPiecesQuantities.getOrDefault(format, 0);
      if (remainingPiecesQuantity > 0) {
        for (int i = 0; i < remainingPiecesQuantity; i++) {
          piecesToCreate.add(new Piece().withFormat(format).withPoLineId(compPOL.getId()));
        }
      }
    });
    return piecesToCreate;
  }

  private List<Piece> processPiecesWithoutItem(CompositePoLine compPOL, List<Piece> existedPieces, String existingPieceLocationId, List<Location> existingPieceLocations) {
    List<Piece> piecesToCreate = new ArrayList<>();
    Map<Piece.Format, Integer> expectedQuantitiesWithoutItem = HelperUtils.calculatePiecesWithoutItemIdQuantity(compPOL, existingPieceLocations);
    Map<Piece.Format, Integer> existedQuantityWithoutItem = calculateQuantityOfExistingPiecesWithoutItem(existedPieces);
    expectedQuantitiesWithoutItem.forEach((format, expectedQty) -> {
      int remainingPiecesQuantity = expectedQty - existedQuantityWithoutItem.getOrDefault(format, 0);
      if (remainingPiecesQuantity > 0) {
        for (int i = 0; i < remainingPiecesQuantity; i++) {
          piecesToCreate.add(new Piece().withFormat(format).withLocationId(existingPieceLocationId).withPoLineId(compPOL.getId()));
        }
      }
    });
    return piecesToCreate;
  }

  private List<Piece> processPiecesWithItem(List<Piece> piecesWithItem, List<Piece> existedPieces, String existingPieceLocationId) {
    List<Piece> expectedPiecesWithItem = filterByLocationId(piecesWithItem, existingPieceLocationId);
    return collectMissingPiecesWithItem(expectedPiecesWithItem, existedPieces);
  }

  private void validateItemsCreation(CompositePoLine compPOL, int itemsSize) {
    int expectedItemsQuantity = calculateInventoryItemsQuantity(compPOL);
    if (itemsSize != expectedItemsQuantity) {
      String message = String.format("Error creating items for PO Line with '%s' id. Expected %d but %d created",
        compPOL.getId(), expectedItemsQuantity, itemsSize);
      throw new InventoryException(message);
    }
  }

  private Map<Piece.Format, Integer> calculateQuantityOfExistingPiecesWithoutItem(List<Piece> pieces) {
    return StreamEx.of(pieces)
      .filter(piece -> StringUtils.isEmpty(piece.getItemId()))
      .groupingBy(Piece::getFormat, collectingAndThen(toList(), List::size));
  }

  private Map<Piece.Format, Integer> calculateQuantityOfExistingPiecesWithoutLocation(List<Piece> pieces) {
    return StreamEx.of(pieces)
      .filter(piece -> StringUtils.isEmpty(piece.getLocationId()))
      .groupingBy(Piece::getFormat, collectingAndThen(toList(), List::size));
  }

  private List<Piece> filterByLocationId(List<Piece> pieces, String locationId) {
    return pieces.stream()
      .filter(piece -> locationId.equals(piece.getLocationId()))
      .collect(Collectors.toList());
  }

  /**
   * Find pieces for which created items, but which are not yet in the storage.
   *
   * @param piecesWithItem pieces for which created items
   * @param existingPieces pieces from storage
   * @return List of Pieces with itemId that are not in storage.
   */
  private List<Piece> collectMissingPiecesWithItem(List<Piece> piecesWithItem, List<Piece> existingPieces) {
    return piecesWithItem.stream()
      .filter(pieceWithItem -> existingPieces.stream()
        .noneMatch(existingPiece -> pieceWithItem.getItemId().equals(existingPiece.getItemId())))
      .collect(Collectors.toList());
  }

  public static Map<String, Map<Piece.Format, Integer>> numOfPiecesByFormatAndLocationId(List<Piece> pieces, String poLineId) {
    return pieces.stream()
      .filter(piece -> Objects.nonNull(piece.getPoLineId())
        && Objects.nonNull(piece.getLocationId())
        && piece.getPoLineId().equals(poLineId))
      .collect(groupingBy(Piece::getLocationId, groupingBy(Piece::getFormat, summingInt(q -> 1))));
  }

  private void receiptConsistencyPiecePoLine(JsonObject jsonObj, RequestContext requestContext) {
    logger.debug("Sending event to verify receipt status");

    receiptStatusPublisher.sendEvent(MessageAddress.RECEIPT_STATUS, jsonObj, requestContext);

    logger.debug("Event to verify receipt status - sent");
  }
}
