package org.folio.service.pieces;

import static java.util.concurrent.CompletableFuture.completedFuture;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;

import org.folio.completablefuture.FolioVertxCompletableFuture;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;

public enum PieceFlowUpdatePoLineStrategies {
    DELETE() {
      @Override
      void updateQuantity(int qty, Piece piece, CompositePoLine lineToSave) {
        Optional<Location> locationToUpdate = lineToSave.getLocations().stream()
          .filter(loc -> (Objects.nonNull(piece.getLocationId()) && piece.getLocationId().equals(loc.getLocationId()))
            || (Objects.nonNull(piece.getHoldingId()) && piece.getHoldingId().equals(loc.getHoldingId())))
          .findAny();
          List<Location> locationToDelete = new ArrayList<>();
          if (locationToUpdate.isPresent()) {
            Location loc = locationToUpdate.get();
            Cost cost = lineToSave.getCost();
            if (piece.getFormat() == Piece.Format.ELECTRONIC) {
              loc.setQuantityElectronic(loc.getQuantityElectronic() - qty);
              loc.setQuantity(loc.getQuantity() - qty);
              cost.setQuantityElectronic(cost.getQuantityElectronic() - qty);
            } else {
              loc.setQuantityPhysical(loc.getQuantityPhysical() - qty);
              loc.setQuantity(loc.getQuantity() - qty);
              cost.setQuantityPhysical(cost.getQuantityPhysical() - qty);
            }
            if (loc.getQuantity() != null && loc.getQuantity() == 0) {
              locationToDelete.add(loc);
            }
          }
        lineToSave.getLocations().removeAll(locationToDelete);
      }
    },
    ADD() {
      @Override
      void updateQuantity(int qty, Piece piece, CompositePoLine lineToSave) {
        Optional<Location> locationToUpdate = lineToSave.getLocations().stream()
          .filter(loc -> (Objects.nonNull(piece.getLocationId()) && piece.getLocationId().equals(loc.getLocationId()))
            || (Objects.nonNull(piece.getHoldingId()) && piece.getHoldingId().equals(loc.getHoldingId())))
          .findAny();
        if (locationToUpdate.isPresent()) {
          Location loc = locationToUpdate.get();
          Cost cost = lineToSave.getCost();
          if (piece.getFormat() == Piece.Format.ELECTRONIC) {
            loc.setQuantityElectronic(loc.getQuantityElectronic() + qty);
            loc.setQuantity(loc.getQuantity() + qty);
            cost.setQuantityElectronic(cost.getQuantityElectronic() + qty);
          } else {
            loc.setQuantityPhysical(loc.getQuantityPhysical() + qty);
            loc.setQuantity(loc.getQuantity() + qty);
            cost.setQuantityPhysical(cost.getQuantityPhysical() + qty);
          }
        } else {
          Location locationToAdd = new Location().withLocationId(piece.getLocationId()).withHoldingId(piece.getHoldingId())
                                                  .withQuantity(qty);
          Cost cost = lineToSave.getCost();
          if (piece.getFormat() == Piece.Format.ELECTRONIC) {
            locationToAdd.withQuantityElectronic(qty);
            cost.setQuantityElectronic(cost.getQuantityElectronic() + qty);
          } else {
            locationToAdd.withQuantityPhysical(qty);
            cost.setQuantityPhysical(cost.getQuantityPhysical() + qty);
          }
          List<Location> locations = lineToSave.getLocations();
          locations.add(locationToAdd);
        }
      }
    };

    abstract void updateQuantity(int qty, Piece piece, CompositePoLine poLine);
}
