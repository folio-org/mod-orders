package org.folio.service.orders.flows.update.unopen;

import static java.util.stream.Collectors.toList;
import static org.apache.commons.collections4.CollectionUtils.isNotEmpty;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import io.vertx.core.Promise;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.orders.utils.HelperUtils;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategyFactory;
import org.folio.service.orders.OrderWorkflowType;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.pieces.PieceStorageService;

import io.vertx.core.Future;
import org.folio.service.pieces.flows.delete.PieceDeleteFlowManager;

public class UnOpenCompositeOrderManager {
  private static final Logger logger = LogManager.getLogger(UnOpenCompositeOrderManager.class);

  private final PurchaseOrderLineService purchaseOrderLineService;
  private final EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory;
  private final PieceStorageService pieceStorageService;
  private final PieceDeleteFlowManager pieceDeleteFlowManager;

  public UnOpenCompositeOrderManager(PurchaseOrderLineService purchaseOrderLineService,
                                     EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory,
                                     PieceStorageService pieceStorageService,
                                     PieceDeleteFlowManager pieceDeleteFlowManager) {
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.encumbranceWorkflowStrategyFactory = encumbranceWorkflowStrategyFactory;
    this.pieceStorageService = pieceStorageService;
    this.pieceDeleteFlowManager = pieceDeleteFlowManager;
  }


  public Future<Void> process(CompositePurchaseOrder compPO, boolean deleteHolding, CompositePurchaseOrder poFromStorage, RequestContext requestContext) {
    return updateAndGetOrderWithLines(compPO, requestContext)
      .map(aVoid -> encumbranceWorkflowStrategyFactory.getStrategy(OrderWorkflowType.OPEN_TO_PENDING))
      .compose(strategy -> strategy.processEncumbrances(compPO, poFromStorage, requestContext))
      .map(ok -> {
        PoLineCommonUtil.makePoLinesPending(compPO.getCompositePoLines());
        return null;
      })
      .compose(ok -> updatePoLinesSummary(compPO.getCompositePoLines(), requestContext))
      .compose(ok -> processInventory(compPO.getCompositePoLines(), deleteHolding, requestContext));

  }

  public Future<Void> updatePoLinesSummary(List<CompositePoLine> compositePoLines, RequestContext requestContext) {
    return GenericCompositeFuture.join(compositePoLines.stream()
      .map(HelperUtils::convertToPoLine)
      .map(line -> purchaseOrderLineService.saveOrderLine(line, requestContext))
      .collect(toList()))
      .map(ok -> null);
  }

  private Future<Void> processInventory(List<CompositePoLine> compositePoLines, boolean deleteHolding, RequestContext requestContext) {
    return GenericCompositeFuture.join(compositePoLines.stream()
      .map(line -> processInventory(line, deleteHolding, requestContext))
        .collect(toList()))
      .mapEmpty();
  }

  private Future<Void> processInventory(CompositePoLine compPOL, boolean deleteHolding, RequestContext requestContext) {
    if (Boolean.TRUE.equals(compPOL.getIsPackage())) {
      return Future.succeededFuture();
    }
    return deleteExpectedPieces(compPOL, deleteHolding, requestContext).onSuccess(pieces -> {
      if (logger.isDebugEnabled()) {
        String deletedIds = pieces.stream().map(Piece::getId).collect(Collectors.joining(","));
        logger.info(String.format("Pieces were removed : %s", deletedIds));
      }
    }).mapEmpty();
  }

  private Future<List<Piece>> deleteExpectedPieces(CompositePoLine compPOL, boolean deleteHolding, RequestContext requestContext) {
    Promise<List<Piece>> promise = Promise.promise();
    if (!PoLineCommonUtil.isReceiptNotRequired(compPOL.getReceiptStatus()) && Boolean.FALSE.equals(compPOL.getCheckinItems())) {
      return pieceStorageService.getExpectedPiecesByLineId(compPOL.getId(), requestContext)
        .compose(pieceCollection -> {

          if (isNotEmpty(pieceCollection.getPieces())) {
            List<Piece> pieces = pieceCollection.getPieces();

            List<Future<Void>> futures = pieces.stream().map(Piece::getId)
              .map(pieceId -> pieceDeleteFlowManager.deletePiece(pieceId, deleteHolding, requestContext)).collect(toList());
            GenericCompositeFuture.join(futures)
              .onComplete(asyncResult -> {
                if (asyncResult.succeeded()) {
                  promise.complete(pieces);
                } else {
                  promise.fail(asyncResult.cause());
                }
              });
          }
          promise.complete(Collections.emptyList());
          return promise.future();
        });
    }
    return promise.future();
  }

  private Future<CompositePurchaseOrder> updateAndGetOrderWithLines(CompositePurchaseOrder compPO, RequestContext requestContext) {
    if (CollectionUtils.isEmpty(compPO.getCompositePoLines())) {
      return purchaseOrderLineService.getCompositePoLinesByOrderId(compPO.getId(), requestContext)
        .map(poLines -> {
          PoLineCommonUtil.sortPoLinesByPoLineNumber(poLines);
          return compPO.withCompositePoLines(poLines);
        })
        .map(v -> compPO);
    } else {
      return Future.succeededFuture(compPO);
    }
  }

}
