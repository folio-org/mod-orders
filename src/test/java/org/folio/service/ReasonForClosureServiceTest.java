package org.folio.service;

import static org.folio.rest.core.exceptions.ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

import io.vertx.core.Future;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.ReasonForClosure;
import org.folio.rest.jaxrs.model.ReasonForClosureCollection;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

@ExtendWith(VertxExtension.class)
public class ReasonForClosureServiceTest {

  @InjectMocks
  private ReasonForClosureService reasonForClosureService;

  @Mock
  private RestClient restClient;


  @Mock
  private RequestContext requestContext;


  @BeforeEach
  public void initMocks(){
      MockitoAnnotations.openMocks(this);
  }

  @Test
  void testDeleteReasonForClosureSuccessIfNotUsed(VertxTestContext vertxTestContext) {
    //given
    when(restClient.delete(any(RequestEntry.class), any())).thenReturn(Future.succeededFuture(null));

    String id = UUID.randomUUID().toString();
    Future<Void> future = reasonForClosureService.deleteReasonForClosure(id, requestContext);
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        verify(restClient).delete(any(RequestEntry.class), any());
        vertxTestContext.completeNow();
      });

  }

  @Test
  void testGetReasonForClosureById(VertxTestContext vertxTestContext) {
    ReasonForClosure reasonForClosure = new ReasonForClosure()
      .withReason("suf")
      .withId(UUID.randomUUID().toString())
      .withSource(ReasonForClosure.Source.SYSTEM);
    when(restClient.get(any(RequestEntry.class), any(), any())).thenReturn(Future.succeededFuture(reasonForClosure));

    Future<ReasonForClosure> future = reasonForClosureService.getReasonForClosureById(reasonForClosure.getId(), requestContext);
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());

        ReasonForClosure resultReasonForClosure = result.result();
        assertEquals(reasonForClosure, resultReasonForClosure);

        verify(restClient).get(any(RequestEntry.class), any(), any());
        vertxTestContext.completeNow();
      });

  }

  @Test
  void testGetReasonsForClosureByQuery(VertxTestContext vertxTestContext) {
    String query = "reason==reason";
    ReasonForClosure reasonForClosure = new ReasonForClosure()
      .withReason("reason")
      .withId(UUID.randomUUID().toString())
      .withSource(ReasonForClosure.Source.SYSTEM);
    ReasonForClosureCollection suffixCollection = new ReasonForClosureCollection().withTotalRecords(1).withReasonsForClosure(Collections.singletonList(reasonForClosure));
    when(restClient.get(any(RequestEntry.class), any(), any()))
      .thenReturn(Future.succeededFuture(suffixCollection));

    Future<ReasonForClosureCollection> future = reasonForClosureService.getReasonsForClosure(query, 1, 0, requestContext);
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());

        ReasonForClosureCollection resultReasonForClosureCollection = result.result();

        assertEquals(suffixCollection, resultReasonForClosureCollection);
        verify(restClient).get(any(RequestEntry.class), any(), any());
        vertxTestContext.completeNow();
      });

  }

  @Test
  void testUpdateReasonForClosure(VertxTestContext vertxTestContext) {
    ReasonForClosure reasonForClosure = new ReasonForClosure()
      .withReason("reason")
      .withId(UUID.randomUUID().toString())
      .withSource(ReasonForClosure.Source.SYSTEM);

    when(restClient.put(any(RequestEntry.class), eq(reasonForClosure), any())).thenReturn(Future.succeededFuture(null));

    Future<Void> future = reasonForClosureService.updateReasonForClosure(reasonForClosure.getId(), reasonForClosure, requestContext);
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        result.result();

        verify(restClient).put(any(RequestEntry.class), eq(reasonForClosure), any());
        vertxTestContext.completeNow();
      });

  }

  @Test
  void testUpdateReasonForClosureWithoutIdInBody(VertxTestContext vertxTestContext) {
    ReasonForClosure reasonForClosure = new ReasonForClosure()
      .withReason("reason")
      .withSource(ReasonForClosure.Source.SYSTEM);

    String id = UUID.randomUUID().toString();
    when(restClient.put(any(RequestEntry.class), eq(reasonForClosure), any())).thenReturn(Future.succeededFuture(null));

    Future<Void> future = reasonForClosureService.updateReasonForClosure(id, reasonForClosure, requestContext);
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        assertEquals(id, reasonForClosure.getId());
        verify(restClient).put(any(RequestEntry.class), eq(reasonForClosure), any());
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testUpdateReasonForClosureWithIdMismatchFails(VertxTestContext vertxTestContext) {
    ReasonForClosure suffix = new ReasonForClosure().withId(UUID.randomUUID().toString());
    Future<Void> future = reasonForClosureService.updateReasonForClosure(UUID.randomUUID().toString(), suffix, requestContext);
    vertxTestContext.assertFailure(future)
      .onComplete(result -> {
        HttpException httpException = (HttpException) result.cause();
        assertEquals(422, httpException.getCode());
        assertEquals(MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY.toError(), httpException.getError());
        vertxTestContext.completeNow();
      });

  }

  @Test
  void testCreateReasonForClosure(VertxTestContext vertxTestContext) {
    ReasonForClosure reasonForClosure = new ReasonForClosure()
      .withReason("reason");

    when(restClient.post(any(RequestEntry.class), any(), any(), any())).thenReturn(Future.succeededFuture(reasonForClosure));

    Future<ReasonForClosure> future = reasonForClosureService.createReasonForClosure(reasonForClosure, requestContext);
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        ReasonForClosure resultReasonForClosure = result.result();
        assertEquals(reasonForClosure, resultReasonForClosure);
        assertEquals(ReasonForClosure.Source.USER, reasonForClosure.getSource());
        verify(restClient).post(any(RequestEntry.class), any(), any(), any());
        vertxTestContext.completeNow();
      });

  }

}
