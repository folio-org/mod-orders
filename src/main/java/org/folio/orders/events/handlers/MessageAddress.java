package org.folio.orders.events.handlers;

public enum MessageAddress {

  ORDER_STATUS("org.folio.orders.order.update.status"),
  RECEIPT_STATUS("org.folio.orders.po-line.update.receipt-status");

  MessageAddress(String address) {
    this.address = address;
  }

  public final String address;
}
