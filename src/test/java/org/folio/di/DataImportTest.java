package org.folio.di;

import static org.folio.DataImportEventTypes.DI_COMPLETED;
import static org.folio.DataImportEventTypes.DI_ERROR;
import static org.junit.Assert.assertTrue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;

import java.util.UUID;
import org.apache.kafka.clients.producer.ProducerRecord;
import org.folio.DataImportEventPayload;
import org.folio.verticle.consumers.DataImportKafkaHandler;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;

import io.vertx.core.json.Json;
import io.vertx.ext.unit.TestContext;
import io.vertx.ext.unit.junit.VertxUnitRunner;
import io.vertx.kafka.client.consumer.KafkaConsumerRecord;

// TODO: tests will be enabled in scope of the https://issues.folio.org/browse/MODORDERS-773
@Ignore()
@RunWith(VertxUnitRunner.class)
public class DataImportTest extends DiAbstractRestTest {

  DataImportKafkaHandler dataImportKafkaHandler;

  @Before
  public void setUp(TestContext context) {
    super.setUp(context);

    dataImportKafkaHandler = getBeanFromSpringContext(vertx, org.folio.verticle.consumers.DataImportKafkaHandler.class);
    Assert.assertNotNull(dataImportKafkaHandler);
  }

  @Test
  public void sendAndReceiveEvent() throws InterruptedException {

    String message = "MESSAGE";
    ProducerRecord<String, String> request =
      prepareWithSpecifiedEventPayload(DI_ERROR.value(), Json.encode(message));

    // when
    send(request);

    //then
    var value = observeTopic(DI_ERROR.value());
    assertThat(value, containsString(message));
  }

  @Test
  public void handleEvent() {

    //given
    DataImportEventPayload dataImportEventPayload = new DataImportEventPayload()
      .withEventType(DI_COMPLETED.value())
      .withJobExecutionId(UUID.randomUUID().toString())
      .withOkapiUrl(OKAPI_URL)
      .withTenant(TENANT_ID)
      .withToken(TOKEN);

    //when
    KafkaConsumerRecord<String, String> kafkaConsumerRecord = buildKafkaConsumerRecord(dataImportEventPayload);

    //then
    assertTrue(dataImportKafkaHandler.handle(kafkaConsumerRecord).succeeded());
  }
}
