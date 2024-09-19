package org.folio.service.orders.utils;

import io.vertx.core.Future;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import org.apache.commons.lang3.StringUtils;
import org.folio.orders.events.handlers.MessageAddress;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePoLine.PaymentStatus;
import org.folio.rest.jaxrs.model.CompositePoLine.ReceiptStatus;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.PoLine;

import java.util.List;
import java.util.Set;

import static org.folio.helper.BaseHelper.EVENT_PAYLOAD;
import static org.folio.helper.BaseHelper.ORDER_ID;

public class StatusUtils {

  private static final Set<PaymentStatus> resolutionPaymentStatus = Set.of(PaymentStatus.CANCELLED, PaymentStatus.FULLY_PAID, PaymentStatus.PAYMENT_NOT_REQUIRED);
  private static final Set<ReceiptStatus> resolutionReceiptStatus = Set.of(ReceiptStatus.CANCELLED, ReceiptStatus.FULLY_RECEIVED, ReceiptStatus.RECEIPT_NOT_REQUIRED);

  public static Future<Void> updateOrderStatusIfNeeded(CompositePurchaseOrder compositePurchaseOrder, CompositePoLine compOrderLine,
                                                       PoLine poLineFromStorage, RequestContext requestContext) {
    // See MODORDERS-218
    if (isStatusChanged(compOrderLine, poLineFromStorage) && shouldUpdateOrderStatus(compositePurchaseOrder, compOrderLine)) {
      var updateOrderMessage = JsonObject.of(EVENT_PAYLOAD, JsonArray.of(JsonObject.of(ORDER_ID, compOrderLine.getPurchaseOrderId())));
      HelperUtils.sendEvent(MessageAddress.RECEIVE_ORDER_STATUS_UPDATE, updateOrderMessage, requestContext);
    }
    return Future.succeededFuture();
  }

  public static boolean isStatusChanged(CompositePoLine compOrderLine, PoLine lineFromStorage) {
    return !StringUtils.equals(lineFromStorage.getReceiptStatus().value(), compOrderLine.getReceiptStatus().value()) ||
      !StringUtils.equals(lineFromStorage.getPaymentStatus().value(), compOrderLine.getPaymentStatus().value());
  }

  private static boolean shouldUpdateOrderStatus(CompositePurchaseOrder compositePurchaseOrder, CompositePoLine compOrderLine) {
    return compositePurchaseOrder.getWorkflowStatus() != CompositePurchaseOrder.WorkflowStatus.CLOSED
      && !resolutionPaymentStatus.contains(compOrderLine.getPaymentStatus())
      && !resolutionReceiptStatus.contains(compOrderLine.getReceiptStatus());
  }

  public static boolean areAllPoLinesCanceled(List<PoLine> poLines) {
    return poLines.stream().allMatch(StatusUtils::isPoLineStatusCanceled);
  }

  private static boolean isPoLineStatusCanceled(PoLine poLine) {
    return PoLine.PaymentStatus.CANCELLED.equals(poLine.getPaymentStatus()) ||
      PoLine.ReceiptStatus.CANCELLED.equals(poLine.getReceiptStatus());
  }

  public static boolean isCompositePoLineStatusCanceled(CompositePoLine compOrderLine) {
    return CompositePoLine.ReceiptStatus.CANCELLED.equals(compOrderLine.getReceiptStatus()) ||
      CompositePoLine.PaymentStatus.CANCELLED.equals(compOrderLine.getPaymentStatus());
  }

}
