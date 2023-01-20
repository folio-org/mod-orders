package org.folio.service.dataimport;

import io.vertx.core.Future;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.dao.SequentialOrderIdStorageDao;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class SequentialOrderIdServiceImpl implements SequentialOrderIdService {

  private final static Logger LOGGER = LogManager.getLogger();

  private SequentialOrderIdStorageDao sequenceIdStorageDao;

  @Autowired
  public SequentialOrderIdServiceImpl(SequentialOrderIdStorageDao sequenceIdStorageDao) {
    this.sequenceIdStorageDao = sequenceIdStorageDao;
  }

  @Override
  public Future<String> store(String jobExecutionId, Integer sequenceNo, String orderId, String tenantId) {
    LOGGER.debug("store :: jobExecutionId: {}, sequenceNo: {}, orderId: {}, tenantId: {}",
      jobExecutionId, sequenceNo, orderId, tenantId);
    return sequenceIdStorageDao.store(jobExecutionId, sequenceNo, orderId, tenantId);
  }
}
