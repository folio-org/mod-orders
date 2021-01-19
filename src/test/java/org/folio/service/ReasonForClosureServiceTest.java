package org.folio.service;

import static org.folio.orders.utils.ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.ReasonForClosure;
import org.folio.rest.jaxrs.model.ReasonForClosureCollection;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

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
  void testDeleteReasonForClosureSuccessIfNotUsed() {
    //given
    when(restClient.delete(any(), any())).thenReturn(CompletableFuture.completedFuture(null));

    String id = UUID.randomUUID().toString();
    CompletableFuture<Void> result = reasonForClosureService.deleteReasonForClosure(id, requestContext);
    assertFalse(result.isCompletedExceptionally());
    result.join();

    verify(restClient).delete(any(), any());
  }

  @Test
  void testGetReasonForClosureById() {
    ReasonForClosure reasonForClosure = new ReasonForClosure()
      .withReason("suf")
      .withId(UUID.randomUUID().toString())
      .withSource(ReasonForClosure.Source.SYSTEM);
    when(restClient.get(any(), any(), any())).thenReturn(CompletableFuture.completedFuture(reasonForClosure));

    CompletableFuture<ReasonForClosure> result = reasonForClosureService.getReasonForClosureById(reasonForClosure.getId(), requestContext);
    assertFalse(result.isCompletedExceptionally());

    ReasonForClosure resultReasonForClosure = result.join();
    assertEquals(reasonForClosure, resultReasonForClosure);

    verify(restClient).get(any(), any(), any());
  }

  @Test
  void testGetReasonsForClosureByQuery() {
    String query = "reason==reason";
    ReasonForClosure reasonForClosure = new ReasonForClosure()
      .withReason("reason")
      .withId(UUID.randomUUID().toString())
      .withSource(ReasonForClosure.Source.SYSTEM);
    ReasonForClosureCollection suffixCollection = new ReasonForClosureCollection().withTotalRecords(1).withReasonsForClosure(Collections.singletonList(reasonForClosure));
    when(restClient.get(any(), any(), any()))
      .thenReturn(CompletableFuture.completedFuture(suffixCollection));

    CompletableFuture<ReasonForClosureCollection> result = reasonForClosureService.getReasonsForClosure(query, 1, 0, requestContext);
    assertFalse(result.isCompletedExceptionally());

    ReasonForClosureCollection resultReasonForClosureCollection = result.join();

    assertEquals(suffixCollection, resultReasonForClosureCollection);
    verify(restClient).get(any(), any(), any());
  }

  @Test
  void testUpdateReasonForClosure() {
    ReasonForClosure reasonForClosure = new ReasonForClosure()
      .withReason("reason")
      .withId(UUID.randomUUID().toString())
      .withSource(ReasonForClosure.Source.SYSTEM);

    when(restClient.put(any(), eq(reasonForClosure), any())).thenReturn(CompletableFuture.completedFuture(null));

    CompletableFuture<Void> result = reasonForClosureService.updateReasonForClosure(reasonForClosure.getId(), reasonForClosure, requestContext);
    assertFalse(result.isCompletedExceptionally());
    result.join();

    verify(restClient).put(any(), eq(reasonForClosure), any());
  }

  @Test
  void testUpdateReasonForClosureWithoutIdInBody() {
    ReasonForClosure reasonForClosure = new ReasonForClosure()
      .withReason("reason")
      .withSource(ReasonForClosure.Source.SYSTEM);

    String id = UUID.randomUUID().toString();
    when(restClient.put(any(), eq(reasonForClosure), any())).thenReturn(CompletableFuture.completedFuture(null));

    CompletableFuture<Void> result = reasonForClosureService.updateReasonForClosure(id, reasonForClosure, requestContext);
    assertFalse(result.isCompletedExceptionally());
    result.join();

    assertEquals(id, reasonForClosure.getId());
    verify(restClient).put(any(), eq(reasonForClosure), any());
  }

  @Test
  void testUpdateReasonForClosureWithIdMismatchFails() {
    ReasonForClosure suffix = new ReasonForClosure().withId(UUID.randomUUID().toString());
    CompletableFuture<Void> result = reasonForClosureService.updateReasonForClosure(UUID.randomUUID().toString(),
      suffix, requestContext);
    CompletionException expectedException = assertThrows(CompletionException.class, result::join);

    HttpException httpException = (HttpException) expectedException.getCause();
    assertEquals(422, httpException.getCode());
    assertEquals(MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY.toError(), httpException.getError());

  }

  @Test
  void testCreateReasonForClosure() {
    ReasonForClosure reasonForClosure = new ReasonForClosure()
      .withReason("reason");

    when(restClient.post(any(), any(), any(), any())).thenReturn(CompletableFuture.completedFuture(reasonForClosure));

    CompletableFuture<ReasonForClosure> result = reasonForClosureService.createReasonForClosure(reasonForClosure, requestContext);
    assertFalse(result.isCompletedExceptionally());
    ReasonForClosure resultReasonForClosure = result.join();
    assertEquals(reasonForClosure, resultReasonForClosure);

    assertEquals(ReasonForClosure.Source.USER, reasonForClosure.getSource());
    verify(restClient).post(any(), any(), any(), any());
  }

}
