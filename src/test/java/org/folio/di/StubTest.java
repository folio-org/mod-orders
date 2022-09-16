package org.folio.di;

import io.vertx.core.json.Json;
import io.vertx.ext.unit.TestContext;
import io.vertx.ext.unit.junit.VertxUnitRunner;
import net.mguenther.kafka.junit.KeyValue;
import net.mguenther.kafka.junit.SendKeyValues;
import org.folio.DataImportEventPayload;
import org.folio.rest.jaxrs.model.Event;
import org.folio.verticle.consumers.DataImportKafkaHandler;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

import java.io.IOException;
import java.util.List;
import java.util.UUID;

import static java.nio.charset.StandardCharsets.UTF_8;
import static java.util.Collections.singletonList;
import static org.folio.DataImportEventTypes.DI_COMPLETED;
import static org.folio.DataImportEventTypes.DI_ERROR;
import static org.folio.dataimport.util.RestUtil.OKAPI_URL_HEADER;
import static org.folio.rest.util.OkapiConnectionParams.OKAPI_TENANT_HEADER;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

@RunWith(VertxUnitRunner.class)
public class StubTest extends DiAbstractRestTest {

  private static final String JOB_EXECUTION_ID_HEADER = "jobExecutionId";

  private String handlerId;
  private String eventId;

  DataImportKafkaHandler dataImportKafkaHandler;

  @Before
  public void setUp(TestContext context) throws IOException {
    super.setUp(context);
    handlerId = UUID.randomUUID().toString();
    eventId = UUID.randomUUID().toString();

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
}
