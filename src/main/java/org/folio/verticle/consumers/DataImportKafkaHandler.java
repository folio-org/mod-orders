package org.folio.verticle.consumers;

import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.Vertx;
import io.vertx.core.json.Json;
import io.vertx.core.json.jackson.DatabindCodec;
import io.vertx.kafka.client.consumer.KafkaConsumerRecord;
import io.vertx.kafka.client.producer.KafkaHeader;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.DataImportEventPayload;
import org.folio.kafka.AsyncRecordHandler;
import org.folio.processing.events.EventManager;
import org.folio.processing.events.services.handler.EventHandler;
import org.folio.processing.exceptions.EventProcessingException;
import org.folio.processing.mapping.MappingManager;
import org.folio.processing.mapping.mapper.reader.record.marc.MarcBibReaderFactory;
import org.folio.rest.jaxrs.model.Event;
import org.folio.rest.util.OkapiConnectionParams;
import org.folio.service.caches.CancelledJobsIdsCache;
import org.folio.service.caches.JobProfileSnapshotCache;
import org.folio.service.dataimport.OrderWriterFactory;
import org.folio.service.dataimport.utils.DataImportUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;

import static java.lang.String.format;
import static org.folio.DataImportEventTypes.DI_ERROR;
import static org.folio.rest.util.OkapiConnectionParams.OKAPI_REQUEST_ID_HEADER;
import static org.folio.rest.util.OkapiConnectionParams.USER_ID_HEADER;
import static org.folio.service.dataimport.utils.DataImportUtils.OKAPI_PERMISSIONS_HEADER;

@Component
@Qualifier("DataImportKafkaHandler")
public class DataImportKafkaHandler implements AsyncRecordHandler<String, String> {

  private static final Logger LOGGER = LogManager.getLogger();

  private static final String PROFILE_SNAPSHOT_NOT_FOUND_MSG = "JobProfileSnapshot was not found by id '%s'";
  private static final String RECORD_ID_HEADER = "recordId";
  private static final String CHUNK_ID_HEADER = "chunkId";
  private static final String JOB_PROFILE_SNAPSHOT_ID_KEY = "JOB_PROFILE_SNAPSHOT_ID";

  private final Vertx vertx;
  private final JobProfileSnapshotCache profileSnapshotCache;
  private final CancelledJobsIdsCache cancelledJobsIdsCache;

  @Autowired
  public DataImportKafkaHandler(Vertx vertx, JobProfileSnapshotCache profileSnapshotCache,
                                CancelledJobsIdsCache cancelledJobsIdsCache,
                                List<EventHandler> eventHandlers) {
    this.vertx = vertx;
    this.profileSnapshotCache = profileSnapshotCache;
    this.cancelledJobsIdsCache = cancelledJobsIdsCache;
    MappingManager.registerReaderFactory(new MarcBibReaderFactory());
    MappingManager.registerWriterFactory(new OrderWriterFactory());
    eventHandlers.forEach(EventManager::registerEventHandler);
  }

  @Override
  @SuppressWarnings("squid:S2629")
  public Future<String> handle(KafkaConsumerRecord<String, String> kafkaRecord) {
    try {
      Promise<String> promise = Promise.promise();
      Map<String, String> headersMap = DataImportUtils.kafkaHeadersToMap(kafkaRecord);
      String chunkId = headersMap.get(CHUNK_ID_HEADER);
      String recordId = headersMap.get(RECORD_ID_HEADER);
      Event event = DatabindCodec.mapper().readValue(kafkaRecord.value(), Event.class);
      DataImportEventPayload eventPayload = Json.decodeValue(event.getEventPayload(), DataImportEventPayload.class);
      String jobExecutionId = eventPayload.getJobExecutionId();
      LOGGER.debug("handle:: Data import event payload has been received with event type: {}, jobExecutionId: {}, recordId: {}, chunkId: {}",
        eventPayload.getEventType(), jobExecutionId, recordId, chunkId);

      if (cancelledJobsIdsCache.contains(eventPayload.getJobExecutionId())) {
        LOGGER.info("handle:: Skipping processing of event, topic: '{}', jobExecutionId: '{}' because the job has been cancelled",
          kafkaRecord.topic(), eventPayload.getJobExecutionId());
        return Future.succeededFuture(kafkaRecord.key());
      }

      OkapiConnectionParams okapiParams = new OkapiConnectionParams(headersMap, vertx);
      eventPayload.getContext().put(RECORD_ID_HEADER, recordId);
      populatePayloadWithUserIdAndPermissions(kafkaRecord, eventPayload);
      String profileSnapshotId = eventPayload.getContext().get(JOB_PROFILE_SNAPSHOT_ID_KEY);

      profileSnapshotCache.get(profileSnapshotId, okapiParams)
        .compose(snapshotOptional -> snapshotOptional
          .map(profileSnapshot -> Future.fromCompletionStage(EventManager.handleEvent(eventPayload, profileSnapshot)))
          .orElse(Future.failedFuture(new EventProcessingException(format(PROFILE_SNAPSHOT_NOT_FOUND_MSG, profileSnapshotId)))))
        .onComplete(payloadAr -> {
          if (payloadAr.failed() || DI_ERROR.value().equals(payloadAr.result().getEventType())) {
            promise.fail(payloadAr.cause());
          } else {
            promise.complete(kafkaRecord.key());
          }
        });
      return promise.future();
    } catch (Exception e) {
      LOGGER.error("handle:: Failed to process kafka record from topic {}", kafkaRecord.topic(), e);
      return Future.failedFuture(e);
    }
  }

  private void populatePayloadWithUserIdAndPermissions(KafkaConsumerRecord<String, String> kafkaRecord,
                                                       DataImportEventPayload eventPayload) {
    for (KafkaHeader header: kafkaRecord.headers()) {
      if (OKAPI_PERMISSIONS_HEADER.equalsIgnoreCase(header.key())) {
        String permissions = header.value().toString();
        eventPayload.getContext().put(OKAPI_PERMISSIONS_HEADER, permissions);
      } else if (USER_ID_HEADER.equalsIgnoreCase(header.key())) {
        String userId = header.value().toString();
        eventPayload.getContext().put(USER_ID_HEADER, userId);
      } else if (OKAPI_REQUEST_ID_HEADER.equalsIgnoreCase(header.key())) {
        String requestId = header.value().toString();
        eventPayload.getContext().put(OKAPI_REQUEST_ID_HEADER, requestId);
      }
    }
  }
}
