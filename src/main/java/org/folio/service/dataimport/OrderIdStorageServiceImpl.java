package org.folio.service.dataimport;

import io.vertx.core.Future;
import io.vertx.pgclient.PgException;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang3.StringUtils;
import org.folio.dao.RecordIdStorageDao;
import org.folio.kafka.exception.DuplicateEventException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.concurrent.CompletableFuture;

@Log4j2
@Service
public class OrderIdStorageServiceImpl implements IdStorageService {

  public static final String PG_CONSTRAINT_ERROR_CODE = "23505";

  private final RecordIdStorageDao orderRecordIdStorageDao;

  @Autowired
  public OrderIdStorageServiceImpl(RecordIdStorageDao orderRecordIdStorageDao) {
    this.orderRecordIdStorageDao = orderRecordIdStorageDao;
  }

  @Override
  public Future<String> store(String recordId, String tenantId) {
    log.debug("store :: recordId: {}, tenantId: {}", recordId, tenantId);

    CompletableFuture<String> future = new CompletableFuture<>();

    orderRecordIdStorageDao.store(recordId, tenantId)
      .onSuccess(future::complete)
      .onFailure(ex -> {
        if (ex instanceof PgException currentException) {
          if (StringUtils.equals(currentException.getSqlState(), PG_CONSTRAINT_ERROR_CODE)) {
            log.info("store:: Source record with {} id is already exists: {}", recordId, DuplicateEventException.class);
            future.completeExceptionally(new DuplicateEventException(String.format("Source record with %s id is already exists", recordId)));
            return;
          }
        }
        future.completeExceptionally(ex);
      });
    return Future.fromCompletionStage(future);
  }
}
