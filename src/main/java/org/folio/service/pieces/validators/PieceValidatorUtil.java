package org.folio.service.pieces.validators;

import static java.util.stream.Collectors.toList;
import static org.folio.rest.core.exceptions.ErrorCodes.PIECE_FORMAT_IS_NOT_VALID_ERROR;
import static org.folio.rest.jaxrs.model.PoLine.OrderFormat.ELECTRONIC_RESOURCE;
import static org.folio.rest.jaxrs.model.PoLine.OrderFormat.OTHER;
import static org.folio.rest.jaxrs.model.PoLine.OrderFormat.PHYSICAL_RESOURCE;
import static org.folio.rest.jaxrs.model.PoLine.OrderFormat.P_E_MIX;
import static org.folio.rest.jaxrs.model.Piece.Format.ELECTRONIC;
import static org.folio.rest.jaxrs.model.Piece.Format.PHYSICAL;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.Piece;

public class PieceValidatorUtil {

  public static List<Error> validatePieceLocation(Piece piece, PoLine originPoLine) {
    List<ErrorCodes> errors = new ArrayList<>();
    if (isLocationRequired(piece.getFormat(), originPoLine) && piece.getHoldingId() == null && piece.getLocationId() == null) {
     errors.add(ErrorCodes.HOLDINGS_ID_AND_LOCATION_ID_IS_NULL_ERROR);
    } else if (piece.getHoldingId() != null && piece.getLocationId() != null) {
     errors.add(ErrorCodes.MAY_BE_LINK_TO_EITHER_HOLDING_OR_LOCATION_ERROR);
    }
    return errors.stream().map(ErrorCodes::toError).collect(toList());
  }

  public static List<Error> validatePieceFormat(Piece piece, PoLine poLine) {
    if (!isPieceFormatValid(piece, poLine)) {
      String msg = String.format(PIECE_FORMAT_IS_NOT_VALID_ERROR.getDescription(), piece.getFormat(), poLine.getOrderFormat());
      return List.of(new Error().withCode(PIECE_FORMAT_IS_NOT_VALID_ERROR.getCode()).withMessage(msg));
    }
    return Collections.emptyList();
  }

  public static List<Error> validatePieceRelatedOrder(CompositePurchaseOrder order, PoLine poLine) {
    if (order == null || poLine == null) {
      return Collections.emptyList();
    }
    if (WorkflowStatus.PENDING == order.getWorkflowStatus() && Boolean.FALSE.equals(poLine.getCheckinItems())) {
      return List.of(new Error().withCode(ErrorCodes.PIECE_RELATED_ORDER_DATA_IS_NOT_VALID.getCode())
        .withMessage(ErrorCodes.PIECE_RELATED_ORDER_DATA_IS_NOT_VALID.getDescription()));
    }
    return Collections.emptyList();
  }

  public static boolean isLocationRequired(Piece.Format pieceFormat, PoLine poLine) {
    if (ELECTRONIC.equals(pieceFormat))  {
      return poLine.getEresource() != null &&
          (poLine.getEresource().getCreateInventory() != Eresource.CreateInventory.NONE &&
            poLine.getEresource().getCreateInventory() != Eresource.CreateInventory.INSTANCE);
    } else {
      return poLine.getPhysical() != null && (poLine.getPhysical().getCreateInventory() != Physical.CreateInventory.NONE &&
            poLine.getPhysical().getCreateInventory() != Physical.CreateInventory.INSTANCE);
    }
  }

  private static boolean isPieceFormatValid(Piece piece, PoLine poLine) {
    if ((piece.getFormat() == ELECTRONIC && !isElectronicPieceApplicable(poLine)) ||
      (piece.getFormat() == PHYSICAL && !isPhysicalPieceApplicable(poLine)) ||
      (piece.getFormat() == Piece.Format.OTHER && !isOtherPieceApplicable(poLine))) {
      return false;
    }
    return true;
  }

  private static boolean isElectronicPieceApplicable(PoLine poLine) {
    return poLine.getOrderFormat() == ELECTRONIC_RESOURCE || poLine.getOrderFormat() == P_E_MIX;
  }

  private static boolean isPhysicalPieceApplicable(PoLine poLine) {
    return poLine.getOrderFormat() == PHYSICAL_RESOURCE || poLine.getOrderFormat() == P_E_MIX;
  }

  private static boolean isOtherPieceApplicable(PoLine poLine) {
    return poLine.getOrderFormat() == OTHER;
  }
}
