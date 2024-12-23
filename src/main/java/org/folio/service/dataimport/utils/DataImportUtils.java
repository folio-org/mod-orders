package org.folio.service.dataimport.utils;

import io.vertx.core.buffer.Buffer;
import io.vertx.kafka.client.consumer.KafkaConsumerRecord;
import io.vertx.kafka.client.producer.KafkaHeader;
import org.apache.commons.lang3.StringUtils;
import org.folio.DataImportEventPayload;
import org.folio.rest.RestConstants;
import org.folio.rest.RestVerticle;

import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

public class DataImportUtils {

  public static final String PERMISSIONS_KEY = "USER_PERMISSIONS";
  public static final String USER_ID_KEY = "USER_ID";
  public static final String OKAPI_PERMISSIONS_HEADER = "X-Okapi-Permissions";

  private DataImportUtils() {
  }

  public static Map<String, String> extractOkapiHeaders(DataImportEventPayload eventPayload) {
    Map<String, String> headers = new HashMap<>();
    headers.put(RestVerticle.OKAPI_HEADER_TENANT, eventPayload.getTenant());
    headers.put(RestConstants.OKAPI_URL, eventPayload.getOkapiUrl());
    if (!isSystemUserEnabled()) {
      headers.put(RestVerticle.OKAPI_HEADER_TOKEN, eventPayload.getToken());
    }

    String permissionsHeader = eventPayload.getContext().get(PERMISSIONS_KEY);
    if (StringUtils.isNotBlank(permissionsHeader)) {
      headers.put(OKAPI_PERMISSIONS_HEADER, permissionsHeader);
    }
    String userId = eventPayload.getContext().get(USER_ID_KEY);
    if (StringUtils.isNotBlank(userId)) {
      headers.put(RestVerticle.OKAPI_USERID_HEADER, userId);
    }
    return headers;
  }

  public static Map<String, String> kafkaHeadersToMap(KafkaConsumerRecord<String, String> kafkaRecord) {
    var headerStream = kafkaRecord.headers().stream();
    if (isSystemUserEnabled()) {
      headerStream = headerStream.filter(header -> !StringUtils.equalsIgnoreCase(RestVerticle.OKAPI_HEADER_TOKEN, header.key()));
    }

    return headerStream.collect(Collectors.toMap(
      KafkaHeader::key,
      header -> {
        Buffer value = header.value();
        return value == null ? "" : value.toString();
      },
      (existing, replacement) -> StringUtils.isNotBlank(existing) ? existing : replacement
    ));
  }

  /**
   * Checks if the system user is enabled based on a system property.
   * <p>
   * This method reads the `SYSTEM_USER_ENABLED` system property and parses
   * its value as a boolean. If the property is not found or cannot be parsed,
   * it defaults to `true`. The method then negates the parsed value and returns it.
   * <p>
   * Note: This functionality is specific to the Eureka environment.
   *
   * @return {@code true} if the system user is set for Eureka env; otherwise {@code false}.
   */
  private static boolean isSystemUserEnabled() {
    return !Boolean.parseBoolean(System.getProperty("SYSTEM_USER_ENABLED", "true"));
  }

}
