package org.folio.rest.impl.crud.models;

public interface GenericCrudObject<T> {
  T withId(T object, String id);
  String getName();
  String getId(T object);
  void setId(T object, String id);
}
