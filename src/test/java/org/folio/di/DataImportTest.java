package org.folio.di;

import static org.folio.DataImportEventTypes.DI_COMPLETED;
import static org.folio.DataImportEventTypes.DI_ERROR;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.util.List;
import java.util.UUID;

import org.folio.DataImportEventPayload;
import org.folio.rest.jaxrs.model.Event;
import org.folio.verticle.consumers.DataImportKafkaHandler;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.jupiter.api.Disabled;
import org.junit.runner.RunWith;

import io.vertx.core.json.Json;
import io.vertx.ext.unit.TestContext;
import io.vertx.ext.unit.junit.VertxUnitRunner;
import io.vertx.kafka.client.consumer.KafkaConsumerRecord;
import net.mguenther.kafka.junit.SendKeyValues;

// TODO: tests will be enabled in scope of the https://issues.folio.org/browse/MODORDERS-773
@Disabled
@RunWith(VertxUnitRunner.class)
public class DataImportTest extends DiAbstractRestTest {

  DataImportKafkaHandler dataImportKafkaHandler;

  @Before
  public void setUp(TestContext context) throws IOException {
    super.setUp(context);

    dataImportKafkaHandler = getBeanFromSpringContext(vertx, org.folio.verticle.consumers.DataImportKafkaHandler.class);
    Assert.assertNotNull(dataImportKafkaHandler);
  }

  @Test
  public void sendAndReceiveEvent() throws InterruptedException {

    String message = "MESSAGE";
    SendKeyValues<String, String> request =
      prepareWithSpecifiedEventPayload(DI_ERROR.value(), Json.encode(message));

    // when
    kafkaCluster.send(request);

    //then
    List<String> obtainedValues =
      observeValuesAndFilterByPartOfMessage(message, DI_ERROR.value(), 1);
    Event obtainedEvent = Json.decodeValue(obtainedValues.get(0), Event.class);
    assertTrue(obtainedEvent.getEventPayload().contains(message));
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
