package org.folio.verticle;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.json.Json;
import io.vertx.kafka.client.consumer.KafkaConsumerRecord;
import io.vertx.kafka.client.producer.KafkaHeader;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.kafka.GlobalLoadSensor;
import org.folio.kafka.KafkaConfig;
import org.folio.kafka.KafkaConsumerWrapper;
import org.folio.kafka.KafkaTopicNameHelper;
import org.folio.kafka.SubscriptionDefinition;
import org.folio.processing.events.utils.PomReaderUtil;
import org.folio.rest.jaxrs.model.Event;
import org.folio.rest.tools.utils.ModuleName;
import org.folio.service.caches.CancelledJobsIdsCache;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.UUID;

import static org.folio.DataImportEventTypes.DI_JOB_CANCELLED;
import static org.folio.kafka.headers.FolioKafkaHeaders.TENANT_ID;
import static org.springframework.beans.factory.config.BeanDefinition.SCOPE_PROTOTYPE;

@Component
@Scope(SCOPE_PROTOTYPE)
public class CancelledJobExecutionConsumerVerticle extends AbstractVerticle {

  private static final Logger LOGGER = LogManager.getLogger(CancelledJobExecutionConsumerVerticle.class);

  private final KafkaConfig kafkaConfig;
  private final CancelledJobsIdsCache cancelledJobsIdsCache;
  private final int loadLimit;
  private KafkaConsumerWrapper<String, String> consumerWrapper;

  @Autowired
  public CancelledJobExecutionConsumerVerticle(
    CancelledJobsIdsCache cancelledJobsIdsCache,
    @Qualifier("newKafkaConfig") KafkaConfig kafkaConfig,
    @Value("${orders.kafka.CancelledJobExecutionConsumer.loadLimit:1000}") int loadLimit
  ) {
    this.cancelledJobsIdsCache = cancelledJobsIdsCache;
    this.kafkaConfig = kafkaConfig;
    this.loadLimit = loadLimit;
  }

  @Override
  public void start(Promise<Void> startPromise) {
    LOGGER.debug("CancelledJobExecutionConsumerVerticle:: start");
    String moduleName = getModuleName();
    String groupName = KafkaTopicNameHelper.formatGroupName(DI_JOB_CANCELLED.value(), moduleName);

    SubscriptionDefinition subscriptionDefinition = KafkaTopicNameHelper.createSubscriptionDefinition(
      kafkaConfig.getEnvId(),
      KafkaTopicNameHelper.getDefaultNameSpace(),
      DI_JOB_CANCELLED.value());

    consumerWrapper = KafkaConsumerWrapper.<String, String>builder()
      .context(context)
      .vertx(vertx)
      .kafkaConfig(kafkaConfig)
      .loadLimit(loadLimit)
      .globalLoadSensor(new GlobalLoadSensor())
      .subscriptionDefinition(subscriptionDefinition)
      .groupInstanceId(getGroupInstanceId())
      .build();

    consumerWrapper.start(this::handle, moduleName)
      .onSuccess(v ->
        LOGGER.info("start:: CancelledJobExecutionConsumerVerticle verticle was started, consumer group: '{}'", groupName))
      .onFailure(e -> LOGGER.error("start:: Failed to start CancelledJobExecutionConsumerVerticle verticle", e))
      .onComplete(startPromise);
  }

  /**
   * Constructs a unique module name with pseudo-random suffix.
   * This ensures that each instance of the module will have own consumer group
   * and will consume all messages from the topic.
   *
   * @return unique module name string.
   */
  private String getModuleName() {
    return PomReaderUtil.INSTANCE
      .constructModuleVersionAndVersion(ModuleName.getModuleName(), ModuleName.getModuleVersion())
      + "-" + UUID.randomUUID();
  }

  private String getGroupInstanceId() {
    return ModuleName.getModuleName() + "-" + getClass().getSimpleName() + "-" + UUID.randomUUID();
  }

  private Future<String> handle(KafkaConsumerRecord<String, String> kafkaRecord) {
    try {
      String tenantId = extractHeader(kafkaRecord.headers(), TENANT_ID);
      LOGGER.debug("handle:: Received cancelled job event, key: '{}', tenantId: '{}'", kafkaRecord.key(), tenantId);

      String jobId = Json.decodeValue(kafkaRecord.value(), Event.class).getEventPayload();
      cancelledJobsIdsCache.put(UUID.fromString(jobId));
      LOGGER.info("handle:: Processed cancelled job, jobId: '{}', tenantId: '{}', topic: '{}'",
        jobId, tenantId, kafkaRecord.topic());
      return Future.succeededFuture(kafkaRecord.key());
    } catch (Exception e) {
      LOGGER.warn("handle:: Failed to process cancelled job, key: '{}', from topic: '{}'",
        kafkaRecord.key(), kafkaRecord.topic(), e);
     return Future.failedFuture(e);
    }
  }

  private String extractHeader(List<KafkaHeader> headers, String headerName) {
    return headers.stream()
      .filter(header -> header.key().equals(headerName))
      .findAny()
      .map(header -> header.value().toString())
      .orElse(null);
  }

  @Override
  public void stop(Promise<Void> stopPromise) {
    consumerWrapper.stop().onComplete(stopPromise);
  }
}
