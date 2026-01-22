package org.folio.service.dataimport.utils;

import io.vertx.core.buffer.Buffer;
import io.vertx.kafka.client.consumer.KafkaConsumerRecord;
import io.vertx.kafka.client.producer.KafkaHeader;
import org.folio.CopilotGenerated;
import org.folio.DataImportEventPayload;
import org.folio.rest.RestConstants;
import org.folio.rest.RestVerticle;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@CopilotGenerated(model = "Claude Sonnet 4.5")
class DataImportUtilsTest {

  private static final String TENANT = "test-tenant";
  private static final String OKAPI_URL = "http://localhost:9130";
  private static final String TOKEN = "test-token";
  private static final String USER_ID = "user-123";
  private static final String REQUEST_ID = "request-456";
  private static final String PERMISSIONS = "[\"orders.all\"]";

  @Test
  void shouldExtractOkapiHeadersFromEventPayload() {
    // Given
    HashMap<String, String> context = new HashMap<>();
    context.put(DataImportUtils.OKAPI_PERMISSIONS_HEADER, PERMISSIONS);
    context.put(RestVerticle.OKAPI_USERID_HEADER, USER_ID);
    context.put(RestVerticle.OKAPI_REQUESTID_HEADER, REQUEST_ID);

    DataImportEventPayload eventPayload = new DataImportEventPayload()
      .withTenant(TENANT)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN)
      .withContext(context);

    // When
    Map<String, String> headers = DataImportUtils.extractOkapiHeaders(eventPayload);

    // Then
    assertEquals(TENANT, headers.get(RestVerticle.OKAPI_HEADER_TENANT));
    assertEquals(OKAPI_URL, headers.get(RestConstants.OKAPI_URL));
    assertEquals(PERMISSIONS, headers.get(DataImportUtils.OKAPI_PERMISSIONS_HEADER));
    assertEquals(USER_ID, headers.get(RestVerticle.OKAPI_USERID_HEADER));
    assertEquals(REQUEST_ID, headers.get(RestVerticle.OKAPI_REQUESTID_HEADER));
    assertTrue(headers.size() >= 5);
  }

  @Test
  void shouldNotIncludeBlankContextValues() {
    // Given
    HashMap<String, String> context = new HashMap<>();
    context.put(DataImportUtils.OKAPI_PERMISSIONS_HEADER, "");
    context.put(RestVerticle.OKAPI_USERID_HEADER, "   ");
    context.put(RestVerticle.OKAPI_REQUESTID_HEADER, null);

    DataImportEventPayload eventPayload = new DataImportEventPayload()
      .withTenant(TENANT)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN)
      .withContext(context);

    // When
    Map<String, String> headers = DataImportUtils.extractOkapiHeaders(eventPayload);

    // Then
    assertFalse(headers.containsKey(DataImportUtils.OKAPI_PERMISSIONS_HEADER));
    assertFalse(headers.containsKey(RestVerticle.OKAPI_USERID_HEADER));
    assertFalse(headers.containsKey(RestVerticle.OKAPI_REQUESTID_HEADER));
  }

  @Test
  @SuppressWarnings("unchecked")
  void shouldConvertKafkaHeadersToMap() {
    // Given
    KafkaConsumerRecord<String, String> kafkaRecord = mock(KafkaConsumerRecord.class);
    KafkaHeader tenantHeader = mockKafkaHeader(RestVerticle.OKAPI_HEADER_TENANT, TENANT);
    KafkaHeader urlHeader = mockKafkaHeader(RestConstants.OKAPI_URL, OKAPI_URL);
    KafkaHeader nullValueHeader = mockKafkaHeader("null-header", null);

    when(kafkaRecord.headers()).thenReturn(List.of(tenantHeader, urlHeader, nullValueHeader));

    // When
    Map<String, String> result = DataImportUtils.kafkaHeadersToMap(kafkaRecord);

    // Then
    assertEquals(TENANT, result.get(RestVerticle.OKAPI_HEADER_TENANT));
    assertEquals(OKAPI_URL, result.get(RestConstants.OKAPI_URL));
    assertEquals("", result.get("null-header"));
  }

  @Test
  @SuppressWarnings("unchecked")
  void shouldHandleDuplicateKafkaHeaders() {
    // Given
    KafkaConsumerRecord<String, String> kafkaRecord = mock(KafkaConsumerRecord.class);
    KafkaHeader first = mockKafkaHeader(RestVerticle.OKAPI_HEADER_TENANT, TENANT);
    KafkaHeader duplicate = mockKafkaHeader(RestVerticle.OKAPI_HEADER_TENANT, "different-tenant");
    KafkaHeader blankFirst = mockKafkaHeader("test-header", "");
    KafkaHeader nonBlankSecond = mockKafkaHeader("test-header", "actual-value");

    when(kafkaRecord.headers()).thenReturn(List.of(first, duplicate, blankFirst, nonBlankSecond));

    // When
    Map<String, String> result = DataImportUtils.kafkaHeadersToMap(kafkaRecord);

    // Then - preserves first non-blank value, replaces blank with non-blank
    assertEquals(TENANT, result.get(RestVerticle.OKAPI_HEADER_TENANT));
    assertEquals("actual-value", result.get("test-header"));
    assertEquals(2, result.size());
  }

  private KafkaHeader mockKafkaHeader(String key, String value) {
    KafkaHeader header = mock(KafkaHeader.class);
    when(header.key()).thenReturn(key);
    when(header.value()).thenReturn(value != null ? Buffer.buffer(value) : null);
    return header;
  }
}
