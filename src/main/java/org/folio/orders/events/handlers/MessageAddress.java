package org.folio.orders.events.handlers;

public enum MessageAddress {

  ORDER_STATUS("org.folio.orders.order.update.status");

  MessageAddress(String address) {
    this.address = address;
  }

  public final String address;
}
