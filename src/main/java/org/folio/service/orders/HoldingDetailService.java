package org.folio.service.orders;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.folio.models.HoldingDetailHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.HoldingDetailResults;
import org.folio.rest.jaxrs.model.HoldingDetailResultsProperty;
import org.folio.rest.jaxrs.model.ItemsDetail;
import org.folio.rest.jaxrs.model.ItemsDetailCollection;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PiecesDetail;
import org.folio.rest.jaxrs.model.PiecesDetailCollection;
import org.folio.rest.jaxrs.model.PoLinesDetail;
import org.folio.rest.jaxrs.model.PoLinesDetailCollection;
import org.folio.service.inventory.InventoryItemManager;
import org.folio.service.pieces.PieceStorageService;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.RequestContextUtil.createContextWithNewTenantId;
import static org.folio.service.inventory.InventoryItemManager.ID;

@Log4j2
public class HoldingDetailService {

  private final PieceStorageService pieceStorageService;
  private final InventoryItemManager inventoryItemManager;

  public HoldingDetailService(PieceStorageService pieceStorageService, InventoryItemManager inventoryItemManager) {
    this.pieceStorageService = pieceStorageService;
    this.inventoryItemManager = inventoryItemManager;
  }

  public Future<HoldingDetailResults> postOrdersHoldingDetail(List<String> holdingIds, RequestContext requestContext) {
    if (CollectionUtils.isEmpty(holdingIds)) {
      log.info("postOrdersHoldingDetail:: No holding ids were passed");
      return Future.succeededFuture(new HoldingDetailResults());
    }
    return pieceStorageService.getPiecesByHoldingIds(holdingIds, requestContext)
      .compose(pieces -> {
        if (CollectionUtils.isEmpty(pieces)) {
          log.info("postOrdersHoldingDetail:: No pieces were found by holding ids={}", holdingIds);
          return Future.succeededFuture(new HoldingDetailResults());
        }
        var holdersFutures = new ArrayList<Future<HoldingDetailHolder>>();
        groupPiecesByTenantIdAndHoldingId(pieces)
          .forEach((tenantId, groupedPiecesByHoldingId) -> {
            if (Objects.nonNull(groupedPiecesByHoldingId)) {
              groupedPiecesByHoldingId.forEach((holdingId, groupedPieces) -> {
                log.info("postOrdersHoldingDetail:: Processing holding detail by tenant={}, holding id={}, pieces={}", getTenantId(tenantId), holdingId, groupedPieces.size());
                var poLinesDetails = createPoLineDetail(groupedPieces);
                var piecesDetail = createPieceDetail(groupedPieces);
                var holdersFuture = getHolderFuture(tenantId, holdingId, poLinesDetails, piecesDetail, requestContext);
                holdersFutures.add(holdersFuture);
              });
            }
          });
        return collectResultsOnSuccess(holdersFutures)
          .map(this::createHoldingDetailResults);
      })
      .recover(throwable -> {
        log.error("postOrdersHoldingDetail:: Error processing holding details for holding ids={}", holdingIds, throwable);
        return Future.failedFuture(throwable);
      });
  }

  protected Map<String, Map<String, List<Piece>>> groupPiecesByTenantIdAndHoldingId(List<Piece> pieces) {
    if (Objects.isNull(pieces)) {
      return Collections.emptyMap();
    }
    return pieces.stream()
      .filter(Objects::nonNull)
      .filter(piece -> Objects.nonNull(piece.getHoldingId()))
      .map(this::getPieceWithNonNullReceivingTenant)
      .collect(Collectors.groupingBy(Piece::getReceivingTenantId, Collectors.groupingBy(Piece::getHoldingId)));
  }

  private Piece getPieceWithNonNullReceivingTenant(Piece piece) {
    return Objects.nonNull(piece.getReceivingTenantId()) ? piece : piece.withReceivingTenantId("");
  }

  protected Future<HoldingDetailHolder> getHolderFuture(String tenantId, String holdingId, List<PoLinesDetail> poLinesDetails,
                                                        List<PiecesDetail> piecesDetail, RequestContext requestContext) {
    var localRequestContext = StringUtils.isNotBlank(tenantId) ? createContextWithNewTenantId(requestContext, tenantId) : requestContext;
    return inventoryItemManager.getItemsByHoldingId(holdingId, localRequestContext)
      .map(items -> {
        if (Objects.isNull(items)) {
          return Collections.<ItemsDetail>emptyList();
        }
        return items.stream()
          .filter(Objects::nonNull)
          .map(item -> createItemDetail(tenantId, item))
          .toList();
      })
      .map(itemsDetail -> {
        var finalPoLinesDetail = Objects.nonNull(poLinesDetails) ? poLinesDetails : Collections.<PoLinesDetail>emptyList();
        var finalPiecesDetail = Objects.nonNull(piecesDetail) ? piecesDetail : Collections.<PiecesDetail>emptyList();
        return new HoldingDetailHolder(holdingId, finalPoLinesDetail, finalPiecesDetail, itemsDetail);
      });
  }

  private List<PoLinesDetail> createPoLineDetail(List<Piece> groupedPieces) {
    if (CollectionUtils.isEmpty(groupedPieces)) {
      return Collections.emptyList();
    }
    return groupedPieces.stream()
      .filter(Objects::nonNull)
      .map(Piece::getPoLineId)
      .distinct()
      .map(poLineId -> new PoLinesDetail().withId(poLineId))
      .toList();
  }

  private List<PiecesDetail> createPieceDetail(List<Piece> groupedPieces) {
    if (CollectionUtils.isEmpty(groupedPieces)) {
      return Collections.emptyList();
    }
    return groupedPieces.stream()
      .filter(Objects::nonNull)
      .map(piece -> new PiecesDetail()
        .withId(piece.getId())
        .withPoLineId(piece.getPoLineId())
        .withItemId(piece.getItemId())
        .withTenantId(getTenantId(piece.getReceivingTenantId())))
      .toList();
  }

  private ItemsDetail createItemDetail(String tenantId, JsonObject item) {
    return new ItemsDetail()
      .withId(item.getString(ID))
      .withTenantId(getTenantId(tenantId));
  }

  private String getTenantId(String tenantId) {
    return StringUtils.isNotBlank(tenantId) ? tenantId : null;
  }

  private HoldingDetailResults createHoldingDetailResults(List<HoldingDetailHolder> holders) {
    if (CollectionUtils.isEmpty(holders)) {
      log.info("createHoldingDetailResults:: No detail holders were generated");
      return new HoldingDetailResults();
    }
    var holdingDetailResults = new HoldingDetailResults();
    holders.forEach(holder -> {
      var poLinesDetailCollection = new PoLinesDetailCollection();
      var piecesDetailCollection = new PiecesDetailCollection();
      var itemsDetailCollection = new ItemsDetailCollection();
      if (Objects.nonNull(holder.poLines())) {
        poLinesDetailCollection.withPoLinesDetail(holder.poLines())
          .withTotalRecords(holder.poLines().size());
      }
      if (Objects.nonNull(holder.pieces())) {
        piecesDetailCollection.withPiecesDetail(holder.pieces())
          .withTotalRecords(holder.pieces().size());
      }
      if (Objects.nonNull(holder.items())) {
        itemsDetailCollection.withItemsDetail(holder.items())
          .withTotalRecords(holder.items().size());
      }
      var holdingDetailProperty = new HoldingDetailResultsProperty()
        .withPoLinesDetailCollection(poLinesDetailCollection)
        .withPiecesDetailCollection(piecesDetailCollection)
        .withItemsDetailCollection(itemsDetailCollection);
      holdingDetailResults.withAdditionalProperty(holder.holdingId(), holdingDetailProperty);
    });
    return holdingDetailResults;
  }
}
