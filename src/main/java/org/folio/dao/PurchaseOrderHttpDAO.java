package org.folio.dao;

import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.jaxrs.model.PurchaseOrders;

import static org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

public class PurchaseOrderHttpDAO extends AbstractHttpDAO<PurchaseOrder, PurchaseOrders> implements PurchaseOrderDAO {
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
  protected Class<PurchaseOrders> getCollectionClazz() {
    return PurchaseOrders.class;
  }
}
