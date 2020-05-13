package org.folio.dao;

import static org.folio.orders.utils.ResourcePathResolver.SUFFIXES;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

import java.util.Map;

import org.folio.rest.jaxrs.model.Suffix;
import org.folio.rest.jaxrs.model.SuffixCollection;

import io.vertx.core.Context;

public class SuffixHttpDAO extends AbstractHttpDAO<Suffix, SuffixCollection> implements SuffixDAO {

  @Override
  protected String getByIdEndpoint(String id) {
    return resourceByIdPath(SUFFIXES, id);
  }

  @Override
  protected String getEndpoint() {
    return resourcesPath(SUFFIXES);
  }

  @Override
  protected Class<Suffix> getClazz() {
    return Suffix.class;
  }

  @Override
  protected Class<SuffixCollection> getCollectionClazz() {
    return SuffixCollection.class;
  }
}
