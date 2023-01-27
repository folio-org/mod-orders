package org.folio.service.dataimport;

import io.vertx.core.Future;
import org.folio.dao.PoLinesImportProgressDao;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class PoLineImportProgressServiceImpl implements PoLineImportProgressService {

  private final PoLinesImportProgressDao poLinesImportProgressDao;

  @Autowired
  public PoLineImportProgressServiceImpl(PoLinesImportProgressDao poLinesImportProgressDao) {
    this.poLinesImportProgressDao = poLinesImportProgressDao;
  }

  @Override
  public Future<Void> savePoLinesAmountPerOrder(String orderId, int totalPoLines, String tenantId) {
    return poLinesImportProgressDao.savePoLinesAmountPerOrder(orderId, totalPoLines, tenantId);
  }

  @Override
  public Future<Void> trackImportedPoLine(String orderId, String tenantId) {
    return poLinesImportProgressDao.trackImportedPoLine(orderId, tenantId);
  }

  @Override
  public Future<Boolean> isPoLinesImported(String orderId, String tenantId) {
    return poLinesImportProgressDao.isPoLinesImported(orderId, tenantId);
  }
}
