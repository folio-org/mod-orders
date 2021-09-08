package org.folio.service.pieces;

import org.folio.completablefuture.FolioVertxCompletableFuture;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;

import java.util.Objects;
import java.util.concurrent.CompletableFuture;

import static java.util.concurrent.CompletableFuture.completedFuture;

public final class PieceFlowUpdatePoLineUtil {
  private PieceFlowUpdatePoLineUtil() {

  }

  public static CompletableFuture<Void> updatePoLineLocationAndCostQuantity(int qty, UpdateQuantityType updateQuantityType, Piece piece, CompositePoLine lineToSave,
                                                                            RequestContext requestContext) {
    return FolioVertxCompletableFuture.from(requestContext.getContext(), completedFuture(lineToSave))
      .thenAccept(poLine -> {
        poLine.getLocations().stream()
          .filter(loc -> (Objects.nonNull(piece.getLocationId()) && piece.getLocationId().equals(loc.getLocationId()))
            || (Objects.nonNull(piece.getHoldingId()) && piece.getHoldingId().equals(loc.getHoldingId())))
          .findAny().ifPresent(loc -> {
            updateQuantityType.updateQuantity(qty, piece, loc, poLine.getCost());
          });
      });
  }

  public enum UpdateQuantityType {
    DELETE() {
      @Override void updateQuantity(int qty, Piece piece, Location loc, Cost cost) {
        if (piece.getFormat() == Piece.Format.ELECTRONIC) {
          loc.setQuantityElectronic(loc.getQuantityElectronic() - qty);
          loc.setQuantity(loc.getQuantity() + qty);
          cost.setQuantityElectronic(cost.getQuantityElectronic() - qty);
        } else {
          loc.setQuantityPhysical(loc.getQuantityPhysical() - qty);
          loc.setQuantity(loc.getQuantity() + qty);
          cost.setQuantityPhysical(cost.getQuantityPhysical() - qty);
        }
      }
    },
    ADD() {
      @Override void updateQuantity(int qty, Piece piece, Location loc, Cost cost) {
        if (piece.getFormat() == Piece.Format.ELECTRONIC) {
          loc.setQuantityElectronic(loc.getQuantityElectronic() + qty);
          loc.setQuantity(loc.getQuantity() + qty);
          cost.setQuantityElectronic(cost.getQuantityElectronic() + qty);
        } else {
          loc.setQuantityPhysical(loc.getQuantityPhysical() + qty);
          loc.setQuantity(loc.getQuantity() + qty);
          cost.setQuantityPhysical(cost.getQuantityPhysical() + qty);
        }
      }
    };

    abstract void updateQuantity(int qty, Piece piece, Location loc, Cost cost);
  }
}
