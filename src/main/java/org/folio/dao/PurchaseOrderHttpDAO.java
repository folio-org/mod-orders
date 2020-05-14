package org.folio.dao;

import static org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.jaxrs.model.PurchaseOrderCollection;

public class PurchaseOrderHttpDAO extends AbstractHttpDAO<PurchaseOrder, PurchaseOrderCollection> implements PurchaseOrderDAO {
  @Override
  protected String getByIdEndpoint(String id) {
    return resourceByIdPath(PURCHASE_ORDER, id);
  }

  @Override
  protected String getEndpoint() {
    return resourcesPath(PURCHASE_ORDER);
  }

  @Override
  protected Class<PurchaseOrder> getClazz() {
    return PurchaseOrder.class;
  }

  @Override
  protected Class<PurchaseOrderCollection> getCollectionClazz() {
    return PurchaseOrderCollection.class;
  }
}
