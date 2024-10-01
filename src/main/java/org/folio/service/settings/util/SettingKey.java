package org.folio.service.settings.util;

public enum SettingKey {

  CENTRAL_ORDERING_ENABLED("ALLOW_ORDERING_WITH_AFFILIATED_LOCATIONS");

  private final String name;

  SettingKey(String name) {
    this.name = name;
  }

  public String getName() {
    return name;
  }

}
