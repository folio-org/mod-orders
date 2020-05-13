package org.folio.service;

import static org.folio.orders.utils.ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY;
import static org.hamcrest.core.Is.isA;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

import org.folio.dao.ReasonForClosureDAO;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.utils.HttpExceptionCodeMatcher;
import org.folio.utils.HttpExceptionErrorMatcher;
import org.folio.rest.jaxrs.model.ReasonForClosure;
import org.folio.rest.jaxrs.model.ReasonForClosureCollection;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import io.vertx.core.impl.EventLoopContext;

public class ReasonForClosureServiceTest {

  @Rule
  public ExpectedException expectedException = ExpectedException.none();

  @InjectMocks
  private ReasonForClosureService reasonForClosureService;

  @Mock
  private ReasonForClosureDAO reasonForClosureDAO;

  @Mock
  private Map<String, String> okapiHeadersMock;

  @Mock
  private EventLoopContext ctxMock;


  @Before
  public void initMocks(){
      MockitoAnnotations.initMocks(this);
  }

  @Test
  public void testDeleteReasonForClosureSuccessIfNotUsed() {
    //given
    when(reasonForClosureDAO.delete(anyString(), any(), anyMap())).thenReturn(CompletableFuture.completedFuture(null));

    String id = UUID.randomUUID().toString();
    CompletableFuture<Void> result = reasonForClosureService.deleteReasonForClosure(id, ctxMock, okapiHeadersMock);
    assertFalse(result.isCompletedExceptionally());
    result.join();

    verify(reasonForClosureDAO).delete(eq(id), any(), anyMap());
  }

  @Test
  public void testGetReasonForClosureById() {
    ReasonForClosure reasonForClosure = new ReasonForClosure()
      .withReason("suf")
      .withId(UUID.randomUUID().toString())
      .withSource(ReasonForClosure.Source.SYSTEM);
    when(reasonForClosureDAO.getById(Mockito.anyString(), any(), anyMap())).thenReturn(CompletableFuture.completedFuture(reasonForClosure));

    CompletableFuture<ReasonForClosure> result = reasonForClosureService.getReasonForClosureById(reasonForClosure.getId(), ctxMock, okapiHeadersMock);
    assertFalse(result.isCompletedExceptionally());

    ReasonForClosure resultReasonForClosure = result.join();
    assertEquals(reasonForClosure, resultReasonForClosure);

    verify(reasonForClosureDAO).getById(eq(reasonForClosure.getId()), any(), anyMap());
  }

  @Test
  public void testGetReasonsForClosureByQuery() {
    String query = "reason==reason";
    ReasonForClosure reasonForClosure = new ReasonForClosure()
      .withReason("reason")
      .withId(UUID.randomUUID().toString())
      .withSource(ReasonForClosure.Source.SYSTEM);
    ReasonForClosureCollection suffixCollection = new ReasonForClosureCollection().withTotalRecords(1).withReasonsForClosure(Collections.singletonList(reasonForClosure));
    when(reasonForClosureDAO.get(eq(query), eq(1), eq(0), any(), anyMap())).thenReturn(CompletableFuture.completedFuture(suffixCollection));

    CompletableFuture<ReasonForClosureCollection> result = reasonForClosureService.getReasonsForClosure(query, 1, 0, ctxMock, okapiHeadersMock);
    assertFalse(result.isCompletedExceptionally());

    ReasonForClosureCollection resultReasonForClosureCollection = result.join();

    assertEquals(suffixCollection, resultReasonForClosureCollection);
    verify(reasonForClosureDAO).get(eq(query), eq(1), eq(0), any(), anyMap());
  }

  @Test
  public void testUpdateReasonForClosure() {
    ReasonForClosure reasonForClosure = new ReasonForClosure()
      .withReason("reason")
      .withId(UUID.randomUUID().toString())
      .withSource(ReasonForClosure.Source.SYSTEM);

    when(reasonForClosureDAO.update(eq(reasonForClosure.getId()), eq(reasonForClosure), any(), anyMap())).thenReturn(CompletableFuture.completedFuture(null));

    CompletableFuture<Void> result = reasonForClosureService.updateReasonForClosure(reasonForClosure.getId(), reasonForClosure, ctxMock, okapiHeadersMock);
    assertFalse(result.isCompletedExceptionally());
    result.join();

    verify(reasonForClosureDAO).update(eq(reasonForClosure.getId()), eq(reasonForClosure), any(), anyMap());
  }

  @Test
  public void testUpdateReasonForClosureWithoutIdInBody() {
    ReasonForClosure reasonForClosure = new ReasonForClosure()
      .withReason("reason")
      .withSource(ReasonForClosure.Source.SYSTEM);

    String id = UUID.randomUUID().toString();
    when(reasonForClosureDAO.update(eq(id), eq(reasonForClosure), any(), anyMap())).thenReturn(CompletableFuture.completedFuture(null));

    CompletableFuture<Void> result = reasonForClosureService.updateReasonForClosure(id, reasonForClosure, ctxMock, okapiHeadersMock);
    assertFalse(result.isCompletedExceptionally());
    result.join();

    assertEquals(id, reasonForClosure.getId());
    verify(reasonForClosureDAO).update(eq(id), eq(reasonForClosure), any(), anyMap());
  }

  @Test
  public void testUpdateReasonForClosureWithIdMismatchFails() {
    ReasonForClosure suffix = new ReasonForClosure().withId(UUID.randomUUID().toString());

    expectedException.expectCause(isA(HttpException.class));
    expectedException.expectCause(HttpExceptionCodeMatcher.hasCode(422));
    expectedException.expectCause(HttpExceptionErrorMatcher.hasError(MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY.toError()));
    CompletableFuture<Void> result = reasonForClosureService.updateReasonForClosure(UUID.randomUUID().toString(), suffix, ctxMock, okapiHeadersMock);
    assertTrue(result.isCompletedExceptionally());
    result.join();
  }

  @Test
  public void testCreateReasonForClosure() {
    ReasonForClosure reasonForClosure = new ReasonForClosure()
      .withReason("reason");

    when(reasonForClosureDAO.save(eq(reasonForClosure), any(), anyMap())).thenReturn(CompletableFuture.completedFuture(reasonForClosure));

    CompletableFuture<ReasonForClosure> result = reasonForClosureService.createReasonForClosure(reasonForClosure, ctxMock, okapiHeadersMock);
    assertFalse(result.isCompletedExceptionally());
    ReasonForClosure resultReasonForClosure = result.join();
    assertEquals(reasonForClosure, resultReasonForClosure);

    assertEquals(ReasonForClosure.Source.USER, reasonForClosure.getSource());
    verify(reasonForClosureDAO).save(eq(reasonForClosure), any(), anyMap());
  }

}
