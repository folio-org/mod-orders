package org.folio.models.claiming;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum IntegrationDetailField {
  JOB_ID("id"),
  TENANT("tenant"),
  EXPORT_TYPE_SPECIFIC_PARAMETERS("exportTypeSpecificParameters"),
  VENDOR_EDI_ORDERS_EXPORT_CONFIG("vendorEdiOrdersExportConfig"),
  CLAIM_PIECE_IDS("claimPieceIds");

  private final String value;
}
