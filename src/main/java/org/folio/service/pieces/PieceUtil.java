package org.folio.service.pieces;

import static org.folio.rest.jaxrs.model.PoLine.OrderFormat.OTHER;

import java.util.Date;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import org.folio.models.pieces.PieceBatchStatusUpdateHolder;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.Piece;

public class PieceUtil {
  public static List<Location> findOrderPieceLineLocation(Piece piece, PoLine poLine) {
    if ((piece.getFormat() == Piece.Format.ELECTRONIC || piece.getFormat() == Piece.Format.PHYSICAL) &&
      (PoLine.OrderFormat.P_E_MIX == poLine.getOrderFormat())) {
      return poLine.getLocations().stream()
        .filter(loc -> isLocationMatch(piece, loc)).collect(Collectors.toList());
    } else if (piece.getFormat() == Piece.Format.ELECTRONIC && PoLine.OrderFormat.ELECTRONIC_RESOURCE == poLine.getOrderFormat()) {
      return poLine.getLocations().stream()
        .filter(loc -> Objects.nonNull(loc.getQuantityElectronic()))
        .filter(loc -> isLocationMatch(piece, loc)).collect(Collectors.toList());
    }
    return poLine.getLocations().stream()
      .filter(loc -> Objects.nonNull(loc.getQuantityPhysical()))
      .filter(loc -> isLocationMatch(piece, loc))
      .collect(Collectors.toList());
  }

  public static Map<Piece.Format, Integer> calculatePiecesQuantityWithoutLocation(PoLine poLine) {
    EnumMap<Piece.Format, Integer> quantities = new EnumMap<>(Piece.Format.class);

    if (poLine.getOrderFormat() == OTHER && (poLine.getPhysical().getCreateInventory() == Physical.CreateInventory.NONE ||
      poLine.getPhysical().getCreateInventory() == Physical.CreateInventory.INSTANCE)) {
      Physical.CreateInventory physicalCreateInventory = poLine.getPhysical().getCreateInventory();
      if (physicalCreateInventory == Physical.CreateInventory.NONE || physicalCreateInventory == Physical.CreateInventory.INSTANCE) {
        quantities.put(Piece.Format.OTHER, PoLineCommonUtil.getPhysicalCostQuantity(poLine));
      }
    } else {
      quantities.putAll(calculatePhysicalPiecesQuantityWithoutLocation(poLine));
      quantities.putAll(calculateElectronicPiecesQuantityWithoutLocation(poLine));
    }
    return quantities;
  }

  private static EnumMap<Piece.Format, Integer> calculatePhysicalPiecesQuantityWithoutLocation(PoLine poLine) {
    EnumMap<Piece.Format, Integer> quantities = new EnumMap<>(Piece.Format.class);
    Physical.CreateInventory physicalCreateInventory = Optional.ofNullable(poLine.getPhysical()).map(Physical::getCreateInventory).orElse(null);
    if (physicalCreateInventory == Physical.CreateInventory.NONE || physicalCreateInventory == Physical.CreateInventory.INSTANCE) {
      quantities.put(Piece.Format.PHYSICAL, PoLineCommonUtil.getPhysicalCostQuantity(poLine));
    }
    return quantities;
  }

  private static EnumMap<Piece.Format, Integer> calculateElectronicPiecesQuantityWithoutLocation(PoLine poLine) {
    EnumMap<Piece.Format, Integer> quantities = new EnumMap<>(Piece.Format.class);
    Eresource.CreateInventory eresourceCreateInventory = Optional.ofNullable(poLine.getEresource()).map(Eresource::getCreateInventory).orElse(null);
    if (eresourceCreateInventory == Eresource.CreateInventory.NONE || eresourceCreateInventory == Eresource.CreateInventory.INSTANCE) {
      quantities.put(Piece.Format.ELECTRONIC, PoLineCommonUtil.getElectronicCostQuantity(poLine));
    }
    return quantities;
  }

  private static boolean isLocationMatch(Piece piece, Location loc) {
    return (Objects.nonNull(piece.getLocationId()) && piece.getLocationId().equals(loc.getLocationId()))
      || (Objects.nonNull(piece.getHoldingId()) && piece.getHoldingId().equals(loc.getHoldingId()));
  }

  public static boolean updatePieceStatus(Piece piece, Piece.ReceivingStatus oldStatus, Piece.ReceivingStatus newStatus) {
    var isStatusChanged = !oldStatus.equals(newStatus);
    if (isStatusChanged) {
      piece.setStatusUpdatedDate(new Date());
    }
    piece.setReceivingStatus(newStatus);
    return isStatusChanged;
  }

  public static boolean updatePieceStatus(Piece piece, PieceBatchStatusUpdateHolder holder) {
    var isStatusChanged = !piece.getReceivingStatus().equals(holder.getReceivingStatus());
    if (isStatusChanged) {
      piece.setStatusUpdatedDate(new Date());
    }
    piece.setReceivingStatus(holder.getReceivingStatus());
    piece.setClaimingInterval(holder.getClaimingInterval());
    piece.setInternalNote(holder.getInternalNote());
    piece.setExternalNote(holder.getExternalNote());
    return isStatusChanged;
  }

}
