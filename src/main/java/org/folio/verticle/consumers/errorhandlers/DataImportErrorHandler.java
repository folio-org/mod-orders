package org.folio.verticle.consumers.errorhandlers;

import io.vertx.kafka.client.consumer.KafkaConsumerRecord;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.kafka.ProcessRecordErrorHandler;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

@Component
@Qualifier("DataImportErrorHandler")
public class DataImportErrorHandler implements ProcessRecordErrorHandler<String, String> {
  private static final Logger LOGGER = LogManager.getLogger();

  @Override
  public void handle(Throwable throwable, KafkaConsumerRecord<String, String> kafkaConsumerRecord) {
    LOGGER.error("Error handling package from Kafka", throwable);
  }
}
