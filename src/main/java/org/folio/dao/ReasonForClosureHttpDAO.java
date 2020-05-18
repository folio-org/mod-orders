package org.folio.dao;

import static org.folio.orders.utils.ResourcePathResolver.REASONS_FOR_CLOSURE;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

import org.folio.rest.jaxrs.model.ReasonForClosure;
import org.folio.rest.jaxrs.model.ReasonForClosureCollection;

public class ReasonForClosureHttpDAO extends AbstractHttpDAO<ReasonForClosure, ReasonForClosureCollection> implements ReasonForClosureDAO {

  @Override
  protected String getByIdEndpoint(String id) {
    return resourceByIdPath(REASONS_FOR_CLOSURE, id);
  }

  @Override
  protected String getEndpoint() {
    return resourcesPath(REASONS_FOR_CLOSURE);
  }

  @Override
  protected Class<ReasonForClosure> getClazz() {
    return ReasonForClosure.class;
  }

  @Override
  protected Class<ReasonForClosureCollection> getCollectionClazz() {
    return ReasonForClosureCollection.class;
  }
}
