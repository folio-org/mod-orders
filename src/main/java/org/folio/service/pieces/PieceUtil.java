package org.folio.service.pieces;

import static org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat.OTHER;

import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.Piece;

public class PieceUtil {
  public static List<Location> findOrderPieceLineLocation(Piece piece, CompositePoLine compPoLine) {
    if ((piece.getFormat() == Piece.Format.ELECTRONIC || piece.getFormat() == Piece.Format.PHYSICAL) &&
      (CompositePoLine.OrderFormat.P_E_MIX == compPoLine.getOrderFormat())) {
      return compPoLine.getLocations().stream()
        .filter(loc -> isLocationMatch(piece, loc)).collect(Collectors.toList());
    } else if (piece.getFormat() == Piece.Format.ELECTRONIC && CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE == compPoLine.getOrderFormat()) {
      return compPoLine.getLocations().stream()
        .filter(loc -> Objects.nonNull(loc.getQuantityElectronic()))
        .filter(loc -> isLocationMatch(piece, loc)).collect(Collectors.toList());
    }
    return compPoLine.getLocations().stream()
      .filter(loc -> Objects.nonNull(loc.getQuantityPhysical()))
      .filter(loc -> isLocationMatch(piece, loc))
      .collect(Collectors.toList());
  }

  public static Map<Piece.Format, Integer> calculatePiecesQuantityWithoutLocation(CompositePoLine compPOL) {
    EnumMap<Piece.Format, Integer> quantities = new EnumMap<>(Piece.Format.class);

    if (compPOL.getOrderFormat() == OTHER && (compPOL.getPhysical().getCreateInventory() == Physical.CreateInventory.NONE ||
      compPOL.getPhysical().getCreateInventory() == Physical.CreateInventory.INSTANCE)) {
      Physical.CreateInventory physicalCreateInventory = compPOL.getPhysical().getCreateInventory();
      if (physicalCreateInventory == Physical.CreateInventory.NONE || physicalCreateInventory == Physical.CreateInventory.INSTANCE) {
        quantities.put(Piece.Format.OTHER, PoLineCommonUtil.getPhysicalCostQuantity(compPOL));
      }
    } else {
      quantities.putAll(calculatePhysicalPiecesQuantityWithoutLocation(compPOL));
      quantities.putAll(calculateElectronicPiecesQuantityWithoutLocation(compPOL));
    }
    return quantities;
  }

  private static EnumMap<Piece.Format, Integer> calculatePhysicalPiecesQuantityWithoutLocation(CompositePoLine compPOL) {
    EnumMap<Piece.Format, Integer> quantities = new EnumMap<>(Piece.Format.class);
    Physical.CreateInventory physicalCreateInventory = Optional.ofNullable(compPOL.getPhysical()).map(Physical::getCreateInventory).orElse(null);
    if (physicalCreateInventory == Physical.CreateInventory.NONE || physicalCreateInventory == Physical.CreateInventory.INSTANCE) {
      quantities.put(Piece.Format.PHYSICAL, PoLineCommonUtil.getPhysicalCostQuantity(compPOL));
    }
    return quantities;
  }

  private static EnumMap<Piece.Format, Integer> calculateElectronicPiecesQuantityWithoutLocation(CompositePoLine compPOL) {
    EnumMap<Piece.Format, Integer> quantities = new EnumMap<>(Piece.Format.class);
    Eresource.CreateInventory eresourceCreateInventory = Optional.ofNullable(compPOL.getEresource()).map(Eresource::getCreateInventory).orElse(null);
    if (eresourceCreateInventory == Eresource.CreateInventory.NONE || eresourceCreateInventory == Eresource.CreateInventory.INSTANCE) {
      quantities.put(Piece.Format.ELECTRONIC, PoLineCommonUtil.getElectronicCostQuantity(compPOL));
    }
    return quantities;
  }

  private static boolean isLocationMatch(Piece piece, Location loc) {
    return (Objects.nonNull(piece.getLocationId()) && piece.getLocationId().equals(loc.getLocationId())) ||
                      (Objects.nonNull(piece.getHoldingId()) && piece.getHoldingId().equals(loc.getHoldingId()));
  }
}
