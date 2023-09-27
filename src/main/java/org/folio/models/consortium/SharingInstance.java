package org.folio.models.consortium;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import java.util.UUID;

@JsonIgnoreProperties(ignoreUnknown = true)
public record SharingInstance(UUID id,
                              UUID instanceIdentifier,
                              String sourceTenantId,
                              String targetTenantId,
                              SharingStatus status,
                              String error) {
  public SharingInstance(UUID instanceIdentifier, String sourceTenantId, String targetTenantId) {
    this(null, instanceIdentifier, sourceTenantId, targetTenantId, null, null);
  }
}
