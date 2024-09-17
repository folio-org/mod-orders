package org.folio.service.pieces.util;

public enum UserTenantFields {

  USER_ID("userId"),
  TENANT_ID("tenantId"),
  COLLECTION_USER_TENANTS("userTenants");

  private final String value;

  UserTenantFields(String value) {
    this.value = value;
  }

  public String getValue() {
    return value;
  }

}
