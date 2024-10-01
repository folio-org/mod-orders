package org.folio.service.settings.util;

public enum SettingFields {

  COLLECTION_SETTINGS("settings"), KEY("key"), VALUE("value");

  private final String value;

  SettingFields(String value) {
    this.value = value;
  }

  public String getValue() {
    return value;
  }

}
