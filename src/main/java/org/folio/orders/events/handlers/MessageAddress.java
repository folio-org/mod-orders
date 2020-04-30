package org.folio.orders.events.handlers;

public enum MessageAddress {
  CHECKIN_ORDER_STATUS_UPDATE("org.folio.orders.checkin.order.update.status"),
  RECEIVE_ORDER_STATUS_UPDATE("org.folio.orders.receive.order.update.status"),
  RECEIPT_STATUS("org.folio.orders.po-line.update.receipt-status");

  MessageAddress(String address) {
    this.address = address;
  }

  public final String address;
}
