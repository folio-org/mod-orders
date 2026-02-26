package org.folio.orders.utils;

import java.util.Arrays;
import java.util.List;

public enum AcqDesiredPermissions {
  ASSIGN("orders.acquisitions-units-assignments.create.execute"),
  MANAGE("orders.acquisitions-units-assignments.manage.execute"),
  TITLES_ASSIGN("titles.acquisitions-units-assignments.create.execute"),
  TITLES_MANAGE("titles.acquisitions-units-assignments.manage.execute"),
  BYPASS_ACQ_UNITS("orders.acquisition-units.bypass.execute");

  private final String permission;
  private static final List<String> values;
  static {
    values = Arrays.stream(AcqDesiredPermissions.values())
      .map(AcqDesiredPermissions::getPermission)
      .toList();
  }

  AcqDesiredPermissions(String permission) {
    this.permission = permission;
  }

  public String getPermission() {
    return permission;
  }

  public static List<String> getValuesExceptBypass() {
    return values.stream().filter(v -> !BYPASS_ACQ_UNITS.getPermission().equals(v)).toList();
  }
}
