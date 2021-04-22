package org.folio.orders.utils.validators;

import org.folio.HttpStatus;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;

import static org.folio.orders.utils.ErrorCodes.RENEWAL_DATE_IS_NOT_SET;
import static org.folio.orders.utils.ErrorCodes.RENEWAL_INTERVAL_IS_NOT_SET;

/**
 * Class for composite orders validation routines
 */
public final class OngoingOrderValidator {

  private OngoingOrderValidator() {
    throw new IllegalStateException(this.getClass().getName() + " is utility class");
  }

  public static void validate(CompositePurchaseOrder compositePurchaseOrder) {
    if (compositePurchaseOrder.getOrderType() == CompositePurchaseOrder.OrderType.ONGOING) {
      validateRenewalDate(compositePurchaseOrder);
      validateRenewalInterval(compositePurchaseOrder);
    }
  }

  private static void validateRenewalDate(CompositePurchaseOrder compositePurchaseOrder) {
    if (compositePurchaseOrder.getOngoing().getRenewalDate() == null) {
      throw new HttpException(HttpStatus.HTTP_UNPROCESSABLE_ENTITY.toInt(), RENEWAL_DATE_IS_NOT_SET);
    }
  }

  private static void validateRenewalInterval(CompositePurchaseOrder compositePurchaseOrder) {
    if (compositePurchaseOrder.getOngoing().getInterval() == null) {
      throw new HttpException(HttpStatus.HTTP_UNPROCESSABLE_ENTITY.toInt(), RENEWAL_INTERVAL_IS_NOT_SET);
    }
  }

}