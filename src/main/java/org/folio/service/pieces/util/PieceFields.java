package org.folio.service.pieces.util;

import lombok.Getter;

@Getter
public enum PieceFields {

  HOLDING_ID("holdingId"),
  PO_LINE_ID("poLineId"),
  RECEIVING_TENANT_ID("receivingTenantId");

  private final String value;

  PieceFields(String value) {
    this.value = value;
  }

}
