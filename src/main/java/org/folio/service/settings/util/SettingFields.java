package org.folio.service.settings.util;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
@Getter
public enum SettingFields {

  COLLECTION_SETTINGS("settings"), KEY("key"), VALUE("value");

  private final String value;

}
