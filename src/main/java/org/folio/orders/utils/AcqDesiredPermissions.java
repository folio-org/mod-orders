package org.folio.orders.utils;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public enum AcqDesiredPermissions {
  ASSIGN("orders.acquisitions-units-assignments.assign"),
  MANAGE("orders.acquisitions-units-assignments.manage");

  private final String permission;
  private static final List<String> values;
  static {
    values = Arrays.stream(AcqDesiredPermissions.values())
      .map(AcqDesiredPermissions::getPermission)
      .collect(Collectors.toUnmodifiableList());
  }

  AcqDesiredPermissions(String permission) {
    this.permission = permission;
  }

  public String getPermission() {
    return permission;
  }

  public static List<String> getValues() {
    return values;
  }
}
