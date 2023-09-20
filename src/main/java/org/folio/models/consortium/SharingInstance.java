package org.folio.models.consortium;


import java.util.UUID;

public record SharingInstance(UUID id,
                              UUID instanceIdentifier,
                              String sourceTenantId,
                              String targetTenantId,
                              SharingStatus status,
                              String error) {
  public SharingInstance(UUID instanceIdentifier, String sourceTenantId, String targetTenantId) {
    this(null, instanceIdentifier, sourceTenantId, targetTenantId, null, null);
  }

  public SharingInstance(UUID id,
                         UUID instanceIdentifier,
                         String sourceTenantId,
                         String targetTenantId,
                         SharingStatus status,
                         String error) {
    this.id = id;
    this.instanceIdentifier = instanceIdentifier;
    this.sourceTenantId = sourceTenantId;
    this.targetTenantId = targetTenantId;
    this.status = status;
    this.error = error;
  }
}
