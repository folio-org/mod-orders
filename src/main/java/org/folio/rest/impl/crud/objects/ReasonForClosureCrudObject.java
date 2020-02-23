package org.folio.rest.impl.crud.objects;

import org.folio.rest.jaxrs.model.ReasonForClosure;

import static org.folio.orders.utils.ResourcePathResolver.REASONS_FOR_CLOSURE;

public class ReasonForClosureCrudObject implements GenericCrudObject<ReasonForClosure> {

  @Override
  public ReasonForClosure withId(ReasonForClosure object, String id) {
    return object.withId(id);
  }

  @Override
  public String getName() {
    return REASONS_FOR_CLOSURE;
  }

  @Override
  public String getId(ReasonForClosure object) {
    return object.getId();
  }

  @Override
  public void setId(ReasonForClosure object, String id) {
    object.setId(id);
  }
}
