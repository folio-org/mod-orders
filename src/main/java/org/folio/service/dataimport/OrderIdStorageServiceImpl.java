package org.folio.service.dataimport;

import io.vertx.core.Future;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.dao.IdStorageDao;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class OrderIdStorageServiceImpl implements IdStorageService {

  private final static Logger LOGGER = LogManager.getLogger();

  private IdStorageDao orderIdStorageDao;

  @Autowired
  public OrderIdStorageServiceImpl(IdStorageDao orderIdStorageDao) {
    this.orderIdStorageDao = orderIdStorageDao;
  }

  @Override
  public Future<String> store(String recordId, String tenantId) {
    LOGGER.debug("store :: recordId: {}, tenantId: {}", recordId, tenantId);
    return orderIdStorageDao.store(recordId, tenantId);
  }

  @Override
  public Future<String> get(String recordId, String tenantId) {
    LOGGER.debug("get :: recordId: {}, tenantId: {}", recordId, tenantId);
    return orderIdStorageDao.get(recordId, tenantId);
  }
}
