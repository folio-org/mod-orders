package org.folio.verticle;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.Future;
import io.vertx.core.Promise;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.kafka.AsyncRecordHandler;
import org.folio.kafka.GlobalLoadSensor;
import org.folio.kafka.KafkaConfig;
import org.folio.kafka.KafkaConsumerWrapper;
import org.folio.kafka.KafkaTopicNameHelper;
import org.folio.kafka.ProcessRecordErrorHandler;
import org.folio.kafka.SubscriptionDefinition;
import org.folio.processing.events.EventManager;
import org.folio.processing.events.utils.PomReaderUtil;
import org.folio.rest.tools.utils.ModuleName;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import static org.folio.DataImportEventTypes.DI_INCOMING_MARC_BIB_FOR_ORDER_PARSED;
import static org.folio.DataImportEventTypes.DI_INVENTORY_INSTANCE_MATCHED;
import static org.folio.DataImportEventTypes.DI_ORDER_CREATED_READY_FOR_POST_PROCESSING;
import static org.springframework.beans.factory.config.BeanDefinition.SCOPE_PROTOTYPE;

@Component
@Scope(SCOPE_PROTOTYPE)
public class DataImportConsumerVerticle extends AbstractVerticle {

  private static final GlobalLoadSensor globalLoadSensor = new GlobalLoadSensor();

  private static final Logger LOGGER = LogManager.getLogger();

  @Autowired
  @Qualifier("newKafkaConfig")
  private KafkaConfig kafkaConfig;

  @Value("${orders.kafka.OrderConsumer.loadLimit:5}")
  private int loadLimit;

  @Value("${orders.dataimport.kafka.max.distribution.number:100}")
  private int maxDistributionNumber;

  @Autowired
  @Qualifier("DataImportKafkaHandler")
  private AsyncRecordHandler<String, String> dataImportKafkaHandler;

  @Autowired
  @Qualifier("DataImportErrorHandler")
  private ProcessRecordErrorHandler<String, String> errorHandler;

  public List<String> getEvents() {
    return List.of(
      DI_INCOMING_MARC_BIB_FOR_ORDER_PARSED.value(),
      DI_INVENTORY_INSTANCE_MATCHED.value(),
      DI_ORDER_CREATED_READY_FOR_POST_PROCESSING.value());
  }

  @Override
  public void start(Promise<Void> startPromise) {
    LOGGER.debug("DataImportConsumerVerticle :: start");
    EventManager.registerKafkaEventPublisher(kafkaConfig, vertx, maxDistributionNumber);

    List<Future<Void>> futures = new ArrayList<>();

    getEvents().forEach(event -> {
      SubscriptionDefinition subscriptionDefinition = KafkaTopicNameHelper
        .createSubscriptionDefinition(kafkaConfig.getEnvId(),
          KafkaTopicNameHelper.getDefaultNameSpace(),
          event);
      KafkaConsumerWrapper<String, String> consumerWrapper = KafkaConsumerWrapper.<String, String>builder()
        .context(context)
        .vertx(vertx)
        .kafkaConfig(kafkaConfig)
        .loadLimit(loadLimit)
        .globalLoadSensor(globalLoadSensor)
        .subscriptionDefinition(subscriptionDefinition)
        .processRecordErrorHandler(getErrorHandler())
        .groupInstanceId(getClass().getSimpleName() + "-" + UUID.randomUUID())
        .build();

      futures.add(consumerWrapper.start(getHandler(),
        constructModuleName() + "_" + getClass().getSimpleName()));
    });

    Future.all(futures).onComplete(ar -> startPromise.complete());
  }

  public static String constructModuleName() {
    return PomReaderUtil.INSTANCE.constructModuleVersionAndVersion(ModuleName.getModuleName(),
      ModuleName.getModuleVersion());
  }

  public AsyncRecordHandler<String, String> getHandler() {
    return this.dataImportKafkaHandler;
  }

  public ProcessRecordErrorHandler<String, String> getErrorHandler() {
    return this.errorHandler;
  }
}
