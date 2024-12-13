package org.folio.service.pieces.validators;

import static java.util.stream.Collectors.toList;
import static org.folio.rest.core.exceptions.ErrorCodes.PIECE_FORMAT_IS_NOT_VALID_ERROR;
import static org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE;
import static org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat.OTHER;
import static org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat.PHYSICAL_RESOURCE;
import static org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat.P_E_MIX;
import static org.folio.rest.jaxrs.model.Piece.Format.ELECTRONIC;
import static org.folio.rest.jaxrs.model.Piece.Format.PHYSICAL;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.Piece;

public class PieceValidatorUtil {

  public static List<Error> validatePieceLocation(Piece piece, CompositePoLine originPoLine) {
    List<ErrorCodes> errors = new ArrayList<>();
    if (isLocationRequired(piece.getFormat(), originPoLine) && piece.getHoldingId() == null && piece.getLocationId() == null) {
     errors.add(ErrorCodes.HOLDINGS_ID_AND_LOCATION_ID_IS_NULL_ERROR);
    } else if (piece.getHoldingId() != null && piece.getLocationId() != null) {
     errors.add(ErrorCodes.MAY_BE_LINK_TO_EITHER_HOLDING_OR_LOCATION_ERROR);
    }
    return errors.stream().map(ErrorCodes::toError).collect(toList());
  }

  public static List<Error> validatePieceFormat(Piece piece, CompositePoLine compositePoLine) {
    if (!isPieceFormatValid(piece, compositePoLine)) {
      String msg = String.format(PIECE_FORMAT_IS_NOT_VALID_ERROR.getDescription(), piece.getFormat(), compositePoLine.getOrderFormat());
      return List.of(new Error().withCode(PIECE_FORMAT_IS_NOT_VALID_ERROR.getCode()).withMessage(msg));
    }
    return Collections.emptyList();
  }

  public static List<Error> validatePieceRelatedOrder(CompositePurchaseOrder order, CompositePoLine poLine) {
    if (order == null || poLine == null) {
      return Collections.emptyList();
    }
    if (WorkflowStatus.PENDING == order.getWorkflowStatus() && Boolean.FALSE.equals(poLine.getCheckinItems())) {
      return List.of(new Error().withCode(ErrorCodes.PIECE_RELATED_ORDER_DATA_IS_NOT_VALID.getCode())
        .withMessage(ErrorCodes.PIECE_RELATED_ORDER_DATA_IS_NOT_VALID.getDescription()));
    }
    return Collections.emptyList();
  }

  public static boolean isLocationRequired(Piece.Format pieceFormat, CompositePoLine compPOL) {
    if (ELECTRONIC.equals(pieceFormat))  {
      return compPOL.getEresource() != null &&
          (compPOL.getEresource().getCreateInventory() != Eresource.CreateInventory.NONE &&
            compPOL.getEresource().getCreateInventory() != Eresource.CreateInventory.INSTANCE);
    } else {
      return compPOL.getPhysical() != null && (compPOL.getPhysical().getCreateInventory() != Physical.CreateInventory.NONE &&
            compPOL.getPhysical().getCreateInventory() != Physical.CreateInventory.INSTANCE);
    }
  }

  private static boolean isPieceFormatValid(Piece piece, CompositePoLine compositePoLine) {
    if ((piece.getFormat() == ELECTRONIC && !isElectronicPieceApplicable(compositePoLine)) ||
      (piece.getFormat() == PHYSICAL && !isPhysicalPieceApplicable(compositePoLine)) ||
      (piece.getFormat() == Piece.Format.OTHER && !isOtherPieceApplicable(compositePoLine))) {
      return false;
    }
    return true;
  }

  private static boolean isElectronicPieceApplicable(CompositePoLine compositePoLine) {
    return compositePoLine.getOrderFormat() == ELECTRONIC_RESOURCE || compositePoLine.getOrderFormat() == P_E_MIX;
  }

  private static boolean isPhysicalPieceApplicable(CompositePoLine compositePoLine) {
    return compositePoLine.getOrderFormat() == PHYSICAL_RESOURCE || compositePoLine.getOrderFormat() == P_E_MIX;
  }

  private static boolean isOtherPieceApplicable(CompositePoLine compositePoLine) {
    return compositePoLine.getOrderFormat() == OTHER;
  }
}
