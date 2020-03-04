package org.folio.rest.impl.crud.models;

import org.folio.rest.jaxrs.model.Prefix;

import static org.folio.orders.utils.ResourcePathResolver.PREFIXES;

public class PrefixCrudObject implements GenericCrudObject<Prefix> {

  @Override
  public Prefix withId(Prefix object, String id) {
    return object.withId(id);
  }

  @Override
  public String getName() {
    return PREFIXES;
  }

  @Override
  public String getId(Prefix object) {
    return object.getId();
  }

  @Override
  public void setId(Prefix object, String id) {
    object.setId(id);
  }
}
