package org.folio.service;

import org.folio.kafka.services.KafkaTopic;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.PropertySource;
import org.springframework.stereotype.Service;

@Service
@PropertySource(value = "kafka.properties")
public class OrdersKafkaTopicService {

  @Value("${order_created_ready_for_post_processing.partitions}")
  private Integer orderCreatedReadyForPostProcessingPartitions;

  @Value("${order_created.partitions}")
  private Integer orderCreatedPartitions;

  @Value("${pending_order_created.partitions}")
  private Integer pendingOrderCreatedPartitions;

  public KafkaTopic[] createTopicObjects() {
    return new OrdersKafkaTopic[] {
      new OrdersKafkaTopic("DI_ORDER_CREATED_READY_FOR_POST_PROCESSING", orderCreatedReadyForPostProcessingPartitions),
      new OrdersKafkaTopic("DI_ORDER_CREATED", orderCreatedPartitions),
      new OrdersKafkaTopic("DI_PENDING_ORDER_CREATED", pendingOrderCreatedPartitions),
    };
  }
}
