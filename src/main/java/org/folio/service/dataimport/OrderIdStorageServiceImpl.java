package org.folio.service.dataimport;

import io.vertx.core.Future;
import org.folio.dao.IdStorageDao;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class OrderIdStorageServiceImpl implements IdStorageService {

  private IdStorageDao orderIdStorageDao;

  @Autowired
  public OrderIdStorageServiceImpl(IdStorageDao orderIdStorageDao) {
    this.orderIdStorageDao = orderIdStorageDao;
  }

  @Override
  public Future<String> store(String recordId, String orderId, String tenantId) {
    return orderIdStorageDao.store(recordId, orderId, tenantId);
  }
}
