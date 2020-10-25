package org.folio.service;

import static org.folio.orders.utils.ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY;
import static org.folio.orders.utils.ErrorCodes.SUFFIX_IS_USED;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

import org.folio.dao.PurchaseOrderDAO;
import org.folio.dao.SuffixDAO;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.jaxrs.model.PurchaseOrderCollection;
import org.folio.rest.jaxrs.model.Suffix;
import org.folio.rest.jaxrs.model.SuffixCollection;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import io.vertx.core.impl.EventLoopContext;

public class SuffixServiceTest {

  @InjectMocks
  private SuffixService suffixService;

  @Mock
  private SuffixDAO suffixDAO;

  @Mock
  private PurchaseOrderDAO purchaseOrderDAO;

  @Mock
  private Map<String, String> okapiHeadersMock;

  @Mock
  private EventLoopContext ctxMock;


  @BeforeEach
  public void initMocks(){
      MockitoAnnotations.openMocks(this);
  }

  @Test
  public void testDeleteSuffixFailedIfSuffixUsedByOrder() {
    //given
    when(suffixDAO.getById(Mockito.anyString(), any(), anyMap()))
      .thenReturn(CompletableFuture.completedFuture(new Suffix().withName("test")));
    when(suffixDAO.delete(anyString(), any(), anyMap())).thenReturn(CompletableFuture.completedFuture(null));
    when(purchaseOrderDAO.get(anyString(), anyInt(), anyInt(), any(), anyMap()))
      .thenReturn(CompletableFuture.completedFuture(new PurchaseOrderCollection().withTotalRecords(1)));

    String id = UUID.randomUUID().toString();
    CompletableFuture<Void> result = suffixService.deleteSuffix(id, ctxMock, okapiHeadersMock);
    CompletionException expectedException = assertThrows(CompletionException.class, result::join);

    HttpException httpException = (HttpException) expectedException.getCause();
    assertEquals(400, httpException.getCode());
    assertEquals(SUFFIX_IS_USED.toError(), httpException.getError());

    verify(suffixDAO).getById(eq(id), any(), anyMap());
    verify(suffixDAO, never()).delete(eq(id), any(), anyMap());
    verify(purchaseOrderDAO).get(eq("poNumberSuffix==test"), eq(0), eq(0), any(), anyMap());
  }

  @Test
  public void testDeleteSuffixSuccessIfNotUsed() {
    //given
    when(suffixDAO.getById(anyString(), any(), anyMap())).thenReturn(CompletableFuture.completedFuture(new Suffix().withName("test")));
    when(suffixDAO.delete(anyString(), any(), anyMap())).thenReturn(CompletableFuture.completedFuture(null));
    when(purchaseOrderDAO.get(anyString(), anyInt(), anyInt(), any(), anyMap()))
      .thenReturn(CompletableFuture.completedFuture(new PurchaseOrderCollection().withTotalRecords(0)));

    String id = UUID.randomUUID().toString();
    CompletableFuture<Void> result = suffixService.deleteSuffix(id, ctxMock, okapiHeadersMock);
    assertFalse(result.isCompletedExceptionally());
    result.join();

    verify(suffixDAO).getById(eq(id), any(), anyMap());
    verify(suffixDAO).delete(eq(id), any(), anyMap());
    verify(purchaseOrderDAO).get(eq("poNumberSuffix==test"), eq(0), eq(0), any(), anyMap());
  }

  @Test
  public void testGetSuffixById() {
    Suffix suffix = new Suffix().withName("suf").withId(UUID.randomUUID().toString()).withDescription("Test suffix");
    when(suffixDAO.getById(Mockito.anyString(), any(), anyMap())).thenReturn(CompletableFuture.completedFuture(suffix));

    CompletableFuture<Suffix> result = suffixService.getSuffixById(suffix.getId(), ctxMock, okapiHeadersMock);
    assertFalse(result.isCompletedExceptionally());

    Suffix resultSuffix = result.join();
    assertEquals(suffix, resultSuffix);

    verify(suffixDAO).getById(eq(suffix.getId()), any(), anyMap());
  }

  @Test
  public void testGetSuffixesByQuery() {
    String query = "name==suf";
    Suffix suffix = new Suffix().withName("suf").withId(UUID.randomUUID().toString()).withDescription("Test suffix");
    SuffixCollection suffixCollection = new SuffixCollection().withTotalRecords(1).withSuffixes(Collections.singletonList(suffix));
    when(suffixDAO.get(eq(query), eq(1), eq(0), any(), anyMap())).thenReturn(CompletableFuture.completedFuture(suffixCollection));

    CompletableFuture<SuffixCollection> result = suffixService.getSuffixes(query, 1, 0, ctxMock, okapiHeadersMock);
    assertFalse(result.isCompletedExceptionally());

    SuffixCollection resultSuffixCollection = result.join();

    assertEquals(suffixCollection, resultSuffixCollection);
    verify(suffixDAO).get(eq(query), eq(1), eq(0), any(), anyMap());
  }

  @Test
  public void testUpdateSuffix() {
    Suffix suffix = new Suffix().withId(UUID.randomUUID().toString())
      .withName("suff");

    when(suffixDAO.update(eq(suffix.getId()), eq(suffix), any(), anyMap())).thenReturn(CompletableFuture.completedFuture(null));

    CompletableFuture<Void> result = suffixService.updateSuffix(suffix.getId(), suffix, ctxMock, okapiHeadersMock);
    assertFalse(result.isCompletedExceptionally());
    result.join();

    verify(suffixDAO).update(eq(suffix.getId()), eq(suffix), any(), anyMap());
  }

  @Test
  public void testUpdateSuffixWithoutIdInBody() {
    Suffix suffix = new Suffix()
      .withName("suff");

    String id = UUID.randomUUID().toString();
    when(suffixDAO.update(eq(id), eq(suffix), any(), anyMap())).thenReturn(CompletableFuture.completedFuture(null));

    CompletableFuture<Void> result = suffixService.updateSuffix(id, suffix, ctxMock, okapiHeadersMock);
    assertFalse(result.isCompletedExceptionally());
    result.join();

    assertEquals(id, suffix.getId());
    verify(suffixDAO).update(eq(id), eq(suffix), any(), anyMap());
  }

  @Test
  public void testUpdateSuffixWithIdMismatchFails() {
    Suffix suffix = new Suffix().withId(UUID.randomUUID().toString());

    CompletionException expectedException = assertThrows(CompletionException.class, () -> {
      CompletableFuture<Void> result = suffixService.updateSuffix(UUID.randomUUID().toString(), suffix, ctxMock, okapiHeadersMock);
      assertTrue(result.isCompletedExceptionally());
      result.join();
    });
    HttpException httpException = (HttpException) expectedException.getCause();
    assertEquals(422, httpException.getCode());
    assertEquals(MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY.toError(), httpException.getError());
  }

  @Test
  public void testCreateSuffix() {
    Suffix suffix = new Suffix().withName("suf").withId(UUID.randomUUID().toString()).withDescription("Test suffix");

    when(suffixDAO.save(eq(suffix), any(), anyMap())).thenReturn(CompletableFuture.completedFuture(suffix));

    CompletableFuture<Suffix> result = suffixService.createSuffix(suffix, ctxMock, okapiHeadersMock);
    assertFalse(result.isCompletedExceptionally());
    Suffix resultSuffix = result.join();
    assertEquals(suffix, resultSuffix);

    verify(suffixDAO).save(eq(suffix), any(), anyMap());


  }

}
