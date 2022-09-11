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
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.processing.events.utils.PomReaderUtil;
import org.folio.rest.tools.utils.ModuleName;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

import static org.folio.DataImportEventTypes.DI_COMPLETED;
import static org.folio.DataImportEventTypes.DI_ERROR;
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

  @Autowired
  @Qualifier("OrdersKafkaHandler")
  private AsyncRecordHandler<String, String> ordersKafkaHandler;

  @Autowired
  @Qualifier("OrdersErrorHandler")
  private ProcessRecordErrorHandler<String, String> errorHandler;

  public List<String> getEvents() {
    return List.of(DI_COMPLETED.value(), DI_ERROR.value());
  }

  @Override
  public void start(Promise<Void> startPromise) {

    LOGGER.debug("OrderConsumerVerticle :: start");

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
        .build();

      futures.add(consumerWrapper.start(getHandler(),
        constructModuleName() + "_" + getClass().getSimpleName()));
    });

    GenericCompositeFuture.all(futures).onComplete(ar -> startPromise.complete());
  }

  public static String constructModuleName() {
    return PomReaderUtil.INSTANCE.constructModuleVersionAndVersion(ModuleName.getModuleName(),
      ModuleName.getModuleVersion());
  }

  public AsyncRecordHandler<String, String> getHandler() {
    return this.ordersKafkaHandler;
  }

  public ProcessRecordErrorHandler<String, String> getErrorHandler() {
    return this.errorHandler;
  }
}
