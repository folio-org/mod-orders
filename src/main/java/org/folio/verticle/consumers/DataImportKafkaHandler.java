package org.folio.verticle.consumers;

import java.util.Map;

import io.vertx.core.Promise;
import io.vertx.core.json.Json;
import io.vertx.core.json.jackson.DatabindCodec;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.DataImportEventPayload;
import org.folio.kafka.AsyncRecordHandler;
import org.folio.kafka.KafkaHeaderUtils;
import org.folio.processing.events.EventManager;
import org.folio.processing.events.services.handler.EventHandler;
import org.folio.processing.exceptions.EventProcessingException;
import org.folio.processing.mapping.MappingManager;
import org.folio.processing.mapping.mapper.reader.record.marc.MarcBibReaderFactory;
import org.folio.rest.jaxrs.model.Event;
import org.folio.rest.util.OkapiConnectionParams;
import org.folio.service.caches.JobProfileSnapshotCache;
import org.folio.service.dataimport.OrderWriterFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.kafka.client.consumer.KafkaConsumerRecord;

import static java.lang.String.format;
import static org.folio.DataImportEventTypes.DI_ERROR;

@Component
@Qualifier("DataImportKafkaHandler")
public class DataImportKafkaHandler implements AsyncRecordHandler<String, String> {

  private static final Logger LOGGER = LogManager.getLogger();

  private static final String PROFILE_SNAPSHOT_NOT_FOUND_MSG = "JobProfileSnapshot was not found by id '%s'";
  private static final String RECORD_ID_HEADER = "recordId";
  private static final String CHUNK_ID_HEADER = "chunkId";
  private static final String JOB_PROFILE_SNAPSHOT_ID_KEY = "JOB_PROFILE_SNAPSHOT_ID";

  private final Vertx vertx;
  private JobProfileSnapshotCache profileSnapshotCache;
  private final EventHandler createOrderEventHandler;

  @Autowired
  public DataImportKafkaHandler(Vertx vertx, JobProfileSnapshotCache profileSnapshotCache,
                                EventHandler createOrderEventHandler) {
    this.vertx = vertx;
    this.createOrderEventHandler = createOrderEventHandler;
    this.profileSnapshotCache = profileSnapshotCache;
    MappingManager.registerReaderFactory(new MarcBibReaderFactory());
    MappingManager.registerWriterFactory(new OrderWriterFactory());
    EventManager.registerEventHandler(this.createOrderEventHandler);
  }

  @Override
  public Future<String> handle(KafkaConsumerRecord<String, String> record) {
    try {
      Promise<String> promise = Promise.promise();
      Map<String, String> headersMap = KafkaHeaderUtils.kafkaHeadersToMap(record.headers());
      String chunkId = headersMap.get(CHUNK_ID_HEADER);
      String recordId = headersMap.get(RECORD_ID_HEADER);
      Event event = DatabindCodec.mapper().readValue(record.value(), Event.class);
      DataImportEventPayload eventPayload = Json.decodeValue(event.getEventPayload(), DataImportEventPayload.class);
      String jobExecutionId = eventPayload.getJobExecutionId();
      LOGGER.debug("handle:: Data import event payload has been received with event type: {}, jobExecutionId: {}, recordId: {}, chunkId: {}",
        eventPayload.getEventType(), jobExecutionId, recordId, chunkId); // todo: info or debug?

      OkapiConnectionParams okapiParams = new OkapiConnectionParams(headersMap, vertx);
      eventPayload.getContext().put(RECORD_ID_HEADER, recordId);
      String profileSnapshotId = eventPayload.getContext().get(JOB_PROFILE_SNAPSHOT_ID_KEY);

      profileSnapshotCache.get(profileSnapshotId, okapiParams)
        .compose(snapshotOptional -> snapshotOptional
          .map(profileSnapshot -> Future.fromCompletionStage(EventManager.handleEvent(eventPayload, profileSnapshot)))
          .orElse(Future.failedFuture(new EventProcessingException(format(PROFILE_SNAPSHOT_NOT_FOUND_MSG, profileSnapshotId)))))
        .onComplete(payloadAr -> {
          if (payloadAr.failed() || DI_ERROR.value().equals(payloadAr.result().getEventType())) {
            promise.fail(payloadAr.cause());
          } else {
            promise.complete(record.key());
          }
        });
      return Future.succeededFuture();
    } catch (Exception e) {
      LOGGER.error("Can't process kafka record: ", e);
      return Future.failedFuture(e);
    }
  }
}
