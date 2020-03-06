package org.folio.rest.impl.crud.models;

import org.folio.rest.jaxrs.model.Suffix;

import static org.folio.orders.utils.ResourcePathResolver.SUFFIXES;

public class SuffixCrudObject implements GenericCrudObject<Suffix> {

  @Override
  public Suffix withId(Suffix object, String id) {
    return object.withId(id);
  }

  @Override
  public String getName() {
    return SUFFIXES;
  }

  @Override
  public String getId(Suffix object) {
    return object.getId();
  }

  @Override
  public void setId(Suffix object, String id) {
    object.setId(id);
  }
}
