package org.folio.dao;

import static org.folio.orders.utils.ResourcePathResolver.PREFIXES;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

import org.folio.rest.jaxrs.model.Prefix;
import org.folio.rest.jaxrs.model.PrefixCollection;

public class PrefixHttpDAO extends AbstractHttpDAO<Prefix, PrefixCollection> implements PrefixDAO {

  @Override
  protected String getByIdEndpoint(String id) {
    return resourceByIdPath(PREFIXES, id);
  }

  @Override
  protected String getEndpoint() {
    return resourcesPath(PREFIXES);
  }

  @Override
  protected Class<Prefix> getClazz() {
    return Prefix.class;
  }

  @Override
  protected Class<PrefixCollection> getCollectionClazz() {
    return PrefixCollection.class;
  }
}
