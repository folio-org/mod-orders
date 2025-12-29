package org.folio.service.dataimport.utils;

import io.vertx.core.buffer.Buffer;
import io.vertx.kafka.client.consumer.KafkaConsumerRecord;
import io.vertx.kafka.client.producer.KafkaHeader;
import org.folio.CopilotGenerated;
import org.folio.DataImportEventPayload;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.folio.rest.util.OkapiConnectionParams.OKAPI_REQUEST_ID_HEADER;
import static org.folio.rest.util.OkapiConnectionParams.OKAPI_TENANT_HEADER;
import static org.folio.rest.util.OkapiConnectionParams.OKAPI_URL_HEADER;
import static org.folio.rest.util.OkapiConnectionParams.USER_ID_HEADER;
import static org.folio.service.dataimport.utils.DataImportUtils.OKAPI_PERMISSIONS_HEADER;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@CopilotGenerated(model = "Claude Sonnet 4.5")
class DataImportUtilsTest {

  private static final String TENANT = "diku";
  private static final String OKAPI_URL = "http://localhost:9130";
  private static final String TOKEN = "test-token";
  private static final String PERMISSIONS = "orders.all";
  private static final String USER_ID = "user-123";
  private static final String REQUEST_ID = "request-456";

  @Test
  void shouldExtractBasicOkapiHeadersFromEventPayload() {
    // Given
    DataImportEventPayload eventPayload = new DataImportEventPayload()
      .withTenant(TENANT)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN);
    eventPayload.setContext(new HashMap<>());

    // When
    Map<String, String> headers = DataImportUtils.extractOkapiHeaders(eventPayload);

    // Then
    assertNotNull(headers);
    assertEquals(TENANT, headers.get(OKAPI_TENANT_HEADER));
    assertEquals(OKAPI_URL, headers.get(OKAPI_URL_HEADER));
  }

  @Test
  void shouldExtractPermissionsHeaderFromEventPayloadContext() {
    // Given
    HashMap<String, String> context = new HashMap<>();
    context.put(OKAPI_PERMISSIONS_HEADER, PERMISSIONS);

    DataImportEventPayload eventPayload = new DataImportEventPayload()
      .withTenant(TENANT)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN);
    eventPayload.setContext(context);

    // When
    Map<String, String> headers = DataImportUtils.extractOkapiHeaders(eventPayload);

    // Then
    assertNotNull(headers);
    assertEquals(PERMISSIONS, headers.get(OKAPI_PERMISSIONS_HEADER));
  }

  @Test
  void shouldExtractUserIdHeaderFromEventPayloadContext() {
    // Given
    HashMap<String, String> context = new HashMap<>();
    context.put(USER_ID_HEADER, USER_ID);

    DataImportEventPayload eventPayload = new DataImportEventPayload()
      .withTenant(TENANT)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN);
    eventPayload.setContext(context);

    // When
    Map<String, String> headers = DataImportUtils.extractOkapiHeaders(eventPayload);

    // Then
    assertNotNull(headers);
    assertEquals(USER_ID, headers.get(USER_ID_HEADER));
  }

  @Test
  void shouldExtractRequestIdHeaderFromEventPayloadContext() {
    // Given
    HashMap<String, String> context = new HashMap<>();
    context.put(OKAPI_REQUEST_ID_HEADER, REQUEST_ID);

    DataImportEventPayload eventPayload = new DataImportEventPayload()
      .withTenant(TENANT)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN);
    eventPayload.setContext(context);

    // When
    Map<String, String> headers = DataImportUtils.extractOkapiHeaders(eventPayload);

    // Then
    assertNotNull(headers);
    assertEquals(REQUEST_ID, headers.get(OKAPI_REQUEST_ID_HEADER));
  }

  @Test
  void shouldExtractAllHeadersFromEventPayloadContext() {
    // Given
    HashMap<String, String> context = new HashMap<>();
    context.put(OKAPI_PERMISSIONS_HEADER, PERMISSIONS);
    context.put(USER_ID_HEADER, USER_ID);
    context.put(OKAPI_REQUEST_ID_HEADER, REQUEST_ID);

    DataImportEventPayload eventPayload = new DataImportEventPayload()
      .withTenant(TENANT)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN);
    eventPayload.setContext(context);

    // When
    Map<String, String> headers = DataImportUtils.extractOkapiHeaders(eventPayload);

    // Then
    assertNotNull(headers);
    assertEquals(TENANT, headers.get(OKAPI_TENANT_HEADER));
    assertEquals(OKAPI_URL, headers.get(OKAPI_URL_HEADER));
    assertEquals(PERMISSIONS, headers.get(OKAPI_PERMISSIONS_HEADER));
    assertEquals(USER_ID, headers.get(USER_ID_HEADER));
    assertEquals(REQUEST_ID, headers.get(OKAPI_REQUEST_ID_HEADER));
  }

  @Test
  void shouldNotIncludeBlankPermissionsHeader() {
    // Given
    HashMap<String, String> context = new HashMap<>();
    context.put(OKAPI_PERMISSIONS_HEADER, "");

    DataImportEventPayload eventPayload = new DataImportEventPayload()
      .withTenant(TENANT)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN);
    eventPayload.setContext(context);

    // When
    Map<String, String> headers = DataImportUtils.extractOkapiHeaders(eventPayload);

    // Then
    assertFalse(headers.containsKey(OKAPI_PERMISSIONS_HEADER));
  }

  @Test
  void shouldNotIncludeBlankUserIdHeader() {
    // Given
    HashMap<String, String> context = new HashMap<>();
    context.put(USER_ID_HEADER, "   ");

    DataImportEventPayload eventPayload = new DataImportEventPayload()
      .withTenant(TENANT)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN);
    eventPayload.setContext(context);

    // When
    Map<String, String> headers = DataImportUtils.extractOkapiHeaders(eventPayload);

    // Then
    assertFalse(headers.containsKey(USER_ID_HEADER));
  }

  @Test
  void shouldNotIncludeBlankRequestIdHeader() {
    // Given
    HashMap<String, String> context = new HashMap<>();
    context.put(OKAPI_REQUEST_ID_HEADER, null);

    DataImportEventPayload eventPayload = new DataImportEventPayload()
      .withTenant(TENANT)
      .withOkapiUrl(OKAPI_URL)
      .withToken(TOKEN);
    eventPayload.setContext(context);

    // When
    Map<String, String> headers = DataImportUtils.extractOkapiHeaders(eventPayload);

    // Then
    assertFalse(headers.containsKey(OKAPI_REQUEST_ID_HEADER));
  }

  @Test
  void shouldConvertKafkaHeadersToMap() {
    // Given
    KafkaHeader header1 = createKafkaHeader(OKAPI_TENANT_HEADER, TENANT);
    KafkaHeader header2 = createKafkaHeader(OKAPI_URL_HEADER, OKAPI_URL);
    KafkaHeader header3 = createKafkaHeader(USER_ID_HEADER, USER_ID);

    KafkaConsumerRecord<String, String> kafkaRecord = mock(KafkaConsumerRecord.class);
    when(kafkaRecord.headers()).thenReturn(List.of(header1, header2, header3));

    // When
    Map<String, String> headers = DataImportUtils.kafkaHeadersToMap(kafkaRecord);

    // Then
    assertNotNull(headers);
    assertEquals(TENANT, headers.get(OKAPI_TENANT_HEADER));
    assertEquals(OKAPI_URL, headers.get(OKAPI_URL_HEADER));
    assertEquals(USER_ID, headers.get(USER_ID_HEADER));
  }

  @Test
  void shouldHandleNullHeaderValues() {
    // Given
    KafkaHeader header1 = createKafkaHeader(OKAPI_TENANT_HEADER, TENANT);
    KafkaHeader header2 = createKafkaHeaderWithNullValue(USER_ID_HEADER);

    KafkaConsumerRecord<String, String> kafkaRecord = mock(KafkaConsumerRecord.class);
    when(kafkaRecord.headers()).thenReturn(List.of(header1, header2));

    // When
    System.setProperty("SYSTEM_USER_ENABLED", "false");
    Map<String, String> headers = DataImportUtils.kafkaHeadersToMap(kafkaRecord);

    // Then
    assertNotNull(headers);
    assertEquals(TENANT, headers.get(OKAPI_TENANT_HEADER));
    assertEquals("", headers.get(USER_ID_HEADER));
  }

  @Test
  void shouldHandleDuplicateHeaderKeysKeepingNonBlankValue() {
    // Given
    KafkaHeader header1 = createKafkaHeader(OKAPI_TENANT_HEADER, TENANT);
    KafkaHeader header2 = createKafkaHeaderWithNullValue(OKAPI_TENANT_HEADER);

    KafkaConsumerRecord<String, String> kafkaRecord = mock(KafkaConsumerRecord.class);
    when(kafkaRecord.headers()).thenReturn(List.of(header1, header2));

    // When
    System.setProperty("SYSTEM_USER_ENABLED", "false");
    Map<String, String> headers = DataImportUtils.kafkaHeadersToMap(kafkaRecord);

    // Then
    assertNotNull(headers);
    assertEquals(TENANT, headers.get(OKAPI_TENANT_HEADER));
  }

  @Test
  void shouldHandleEmptyKafkaHeaders() {
    // Given
    KafkaConsumerRecord<String, String> kafkaRecord = mock(KafkaConsumerRecord.class);
    when(kafkaRecord.headers()).thenReturn(List.of());

    // When
    System.setProperty("SYSTEM_USER_ENABLED", "false");
    Map<String, String> headers = DataImportUtils.kafkaHeadersToMap(kafkaRecord);

    // Then
    assertNotNull(headers);
    assertTrue(headers.isEmpty());
  }


  private KafkaHeader createKafkaHeader(String key, String value) {
    KafkaHeader header = mock(KafkaHeader.class);
    when(header.key()).thenReturn(key);
    when(header.value()).thenReturn(Buffer.buffer(value));
    return header;
  }

  private KafkaHeader createKafkaHeaderWithNullValue(String key) {
    KafkaHeader header = mock(KafkaHeader.class);
    when(header.key()).thenReturn(key);
    when(header.value()).thenReturn(null);
    return header;
  }
}
