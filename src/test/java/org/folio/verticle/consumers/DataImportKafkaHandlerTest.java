package org.folio.verticle.consumers;

import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.json.Json;
import io.vertx.core.json.JsonObject;
import io.vertx.kafka.client.consumer.KafkaConsumerRecord;
import io.vertx.kafka.client.producer.KafkaHeader;
import org.folio.ActionProfile;
import org.folio.DataImportEventPayload;
import org.folio.JobProfile;
import org.folio.processing.events.EventManager;
import org.folio.processing.events.services.handler.EventHandler;
import org.folio.rest.RestVerticle;
import org.folio.rest.jaxrs.model.Event;
import org.folio.rest.jaxrs.model.ProfileSnapshotWrapper;
import org.folio.rest.jaxrs.model.ProfileType;
import org.folio.rest.util.OkapiConnectionParams;
import org.folio.service.dataimport.handlers.CreateOrderEventHandler;
import org.folio.service.caches.CancelledJobsIdsCache;
import org.folio.service.caches.JobProfileSnapshotCache;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.MockitoAnnotations;

import java.util.HashMap;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

import static org.folio.DataImportEventTypes.DI_INCOMING_MARC_BIB_FOR_ORDER_PARSED;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class DataImportKafkaHandlerTest {

  private static final String TENANT_ID = "diku";
  private DataImportKafkaHandler dataImportKafkaHandler;

  private static Vertx vertx;

  @Mock
  private JobProfileSnapshotCache profileSnapshotCache;
  @Mock
  private EventHandler mockedEventHandler;
  private CancelledJobsIdsCache cancelledJobsIdsCache;
  private AutoCloseable mocksCloseable;

  private final ProfileSnapshotWrapper jobProfileSnapshotWrapper = new ProfileSnapshotWrapper()
    .withId(UUID.randomUUID().toString())
    .withContentType(ProfileType.JOB_PROFILE)
    .withContent(JsonObject.mapFrom(new JobProfile()
      .withId(UUID.randomUUID().toString())).getMap())
    .withChildSnapshotWrappers(List.of(new ProfileSnapshotWrapper()
      .withId(UUID.randomUUID().toString())
      .withContentType(ProfileType.ACTION_PROFILE)
      .withContent(JsonObject.mapFrom(new ActionProfile()
        .withAction(ActionProfile.Action.CREATE)
        .withFolioRecord(ActionProfile.FolioRecord.ORDER)).getMap())));

  @BeforeAll
  static void setUpClass() {
    vertx = Vertx.vertx();
  }

  @BeforeEach
  void setUp() {
    mocksCloseable = MockitoAnnotations.openMocks(this);
    cancelledJobsIdsCache = new CancelledJobsIdsCache(5);
    dataImportKafkaHandler = new DataImportKafkaHandler(vertx, profileSnapshotCache, cancelledJobsIdsCache, List.of(mockedEventHandler));

    when(profileSnapshotCache.get(anyString(), any(OkapiConnectionParams.class)))
      .thenReturn(Future.succeededFuture(Optional.of(jobProfileSnapshotWrapper)));
    when(mockedEventHandler.isEligible(any(DataImportEventPayload.class)))
      .thenReturn(true);
    doAnswer(invocation -> CompletableFuture.completedFuture(invocation.<DataImportEventPayload>getArgument(0)))
      .when(mockedEventHandler).handle(any(DataImportEventPayload.class));
  }

  @AfterEach
  void tearDown() throws Exception {
    mocksCloseable.close();
  }

  @Test
  void shouldReturnSucceededFutureAndSkipEventProcessingIfEventPayloadContainsCancelledJobExecutionId() {
    // Given
    String cancelledJobId = UUID.randomUUID().toString();
    cancelledJobsIdsCache.put(cancelledJobId);

    DataImportEventPayload eventPayload = new DataImportEventPayload()
      .withJobExecutionId(cancelledJobId)
      .withEventType(DI_INCOMING_MARC_BIB_FOR_ORDER_PARSED.value())
      .withCurrentNode(jobProfileSnapshotWrapper.getChildSnapshotWrappers().getFirst())
      .withTenant(TENANT_ID);

    Event event = new Event().withEventPayload(Json.encode(eventPayload));
    KafkaConsumerRecord<String, String> kafkaRecord = mock(KafkaConsumerRecord.class);
    when(kafkaRecord.value()).thenReturn(Json.encode(event));
    when(kafkaRecord.headers()).thenReturn(List.of());

    try (MockedStatic<EventManager> mockedStatic = mockStatic(EventManager.class)) {
    // When
      Future<String> future = dataImportKafkaHandler.handle(kafkaRecord);

    // Then
      assertTrue(future.succeeded());
      verify(mockedEventHandler, never()).isEligible(any(DataImportEventPayload.class));
      verify(mockedEventHandler, never()).handle(any(DataImportEventPayload.class));
      mockedStatic.verify(() -> EventManager.handleEvent(any(DataImportEventPayload.class), any()), never());
    }
  }

  @Test
  void shouldPopulateEventPayloadWithUserIdPermissionsAndRequestId() {
    // Given
    String userId = "user-123";
    String requestId = "request-456";
    String permissions = "[\"orders.all\"]";
    String profileSnapshotId = UUID.randomUUID().toString();

    HashMap<String, String> context = new HashMap<>();
    context.put("JOB_PROFILE_SNAPSHOT_ID", profileSnapshotId);

    DataImportEventPayload eventPayload = new DataImportEventPayload()
      .withJobExecutionId(UUID.randomUUID().toString())
      .withEventType(DI_INCOMING_MARC_BIB_FOR_ORDER_PARSED.value())
      .withCurrentNode(jobProfileSnapshotWrapper.getChildSnapshotWrappers().getFirst())
      .withTenant(TENANT_ID)
      .withContext(context);

    Event event = new Event().withEventPayload(Json.encode(eventPayload));

    KafkaHeader userIdHeader = mockKafkaHeader(RestVerticle.OKAPI_USERID_HEADER, userId);
    KafkaHeader requestIdHeader = mockKafkaHeader(RestVerticle.OKAPI_REQUESTID_HEADER, requestId);
    KafkaHeader permissionsHeader = mockKafkaHeader(CreateOrderEventHandler.OKAPI_PERMISSIONS_HEADER, permissions);

    KafkaConsumerRecord<String, String> kafkaRecord = mock(KafkaConsumerRecord.class);
    when(kafkaRecord.value()).thenReturn(Json.encode(event));
    when(kafkaRecord.headers()).thenReturn(List.of(userIdHeader, requestIdHeader, permissionsHeader));

    try (MockedStatic<EventManager> mockedStatic = mockStatic(EventManager.class)) {
      mockedStatic.when(() -> EventManager.handleEvent(any(DataImportEventPayload.class), any()))
        .thenAnswer(invocation -> {
          DataImportEventPayload payload = invocation.getArgument(0);
          // Verify headers were populated into context
          assertEquals(userId, payload.getContext().get(RestVerticle.OKAPI_USERID_HEADER));
          assertEquals(requestId, payload.getContext().get(RestVerticle.OKAPI_REQUESTID_HEADER));
          assertEquals(permissions, payload.getContext().get(CreateOrderEventHandler.OKAPI_PERMISSIONS_HEADER));
          return CompletableFuture.completedFuture(payload);
        });

      // When
      Future<String> future = dataImportKafkaHandler.handle(kafkaRecord);

      // Then
      assertTrue(future.succeeded());
      mockedStatic.verify(() -> EventManager.handleEvent(any(DataImportEventPayload.class), any()));
    }
  }

  private KafkaHeader mockKafkaHeader(String key, String value) {
    KafkaHeader header = mock(KafkaHeader.class);
    when(header.key()).thenReturn(key);
    when(header.value()).thenReturn(Buffer.buffer(value));
    return header;
  }
}
