package org.folio.service.dataimport;

import io.vertx.core.Future;
import io.vertx.pgclient.PgException;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.DataImportEventPayload;
import org.folio.dao.IdStorageDao;
import org.folio.kafka.exception.DuplicateEventException;
import org.folio.processing.exceptions.EventProcessingException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Objects;
import java.util.concurrent.CompletableFuture;

@Service
public class OrderIdStorageServiceImpl implements IdStorageService {

  private final static Logger LOGGER = LogManager.getLogger();
  public static final String PG_CONSTRAINT_ERROR_CODE = "23505";

  private IdStorageDao orderIdStorageDao;

  @Autowired
  public OrderIdStorageServiceImpl(IdStorageDao orderIdStorageDao) {
    this.orderIdStorageDao = orderIdStorageDao;
  }

  @Override
  public Future<String> store(String recordId, String tenantId) {
    LOGGER.debug("get :: recordId: {}, tenantId: {}", recordId, tenantId);

    CompletableFuture<String> future = new CompletableFuture<>();

    orderIdStorageDao.store(recordId, tenantId)
      .onSuccess(future::complete)
      .onFailure(ex -> {
        if (ex instanceof PgException) {
          PgException currentException = (PgException) ex;
          if (StringUtils.equals(currentException.getCode(), PG_CONSTRAINT_ERROR_CODE)) {
            LOGGER.info("handle:: Source record with {} id is already exists: {}",
              recordId, DuplicateEventException.class);
            future.completeExceptionally(new DuplicateEventException(String.format("Source record with %s id is already exists", recordId)));
            return;
          }
        }
        future.completeExceptionally(ex);
      });
    return Future.fromCompletionStage(future);
  }
}
