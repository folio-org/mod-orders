package org.folio.service.pieces;

import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

public class PieceUtil {
  public static List<Location> findOrderPieceLineLocation(Piece piece, CompositePoLine compPoLine) {
    if (piece.getFormat() == Piece.Format.ELECTRONIC) {
      return compPoLine.getLocations().stream()
        .filter(loc -> Objects.nonNull(loc.getQuantityElectronic()))
        .filter(loc -> isLocationMatch(piece, loc)).collect(Collectors.toList());
    }
    return compPoLine.getLocations().stream()
      .filter(loc -> Objects.nonNull(loc.getQuantityPhysical()))
      .filter(loc -> isLocationMatch(piece, loc)).collect(Collectors.toList());
  }

  private static boolean isLocationMatch(Piece piece, Location loc) {
    return (Objects.nonNull(piece.getLocationId()) && piece.getLocationId().equals(loc.getLocationId())) ||
                      (Objects.nonNull(piece.getHoldingId()) && piece.getHoldingId().equals(loc.getHoldingId()));
  }
}
