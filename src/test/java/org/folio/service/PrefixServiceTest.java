package org.folio.service;

import static org.folio.orders.utils.ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY;
import static org.folio.orders.utils.ErrorCodes.PREFIX_IS_USED;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
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

import org.folio.dao.PrefixDAO;
import org.folio.dao.PurchaseOrderDAO;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.jaxrs.model.Prefix;
import org.folio.rest.jaxrs.model.PrefixCollection;
import org.folio.rest.jaxrs.model.PurchaseOrderCollection;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import io.vertx.core.impl.EventLoopContext;

public class PrefixServiceTest {

  @InjectMocks
  private PrefixService prefixService;

  @Mock
  private PrefixDAO prefixDAO;

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
  public void testDeletePrefixFailedIfSuffixUsedByOrder() {
    //given
    when(prefixDAO.getById(Mockito.anyString(), any(), anyMap())).thenReturn(CompletableFuture.completedFuture(new Prefix().withName("test")));
    when(prefixDAO.delete(anyString(), any(), anyMap())).thenReturn(CompletableFuture.completedFuture(null));
    when(purchaseOrderDAO.get(anyString(), anyInt(), anyInt(), any(), anyMap())).thenReturn(CompletableFuture.completedFuture(new PurchaseOrderCollection().withTotalRecords(1)));

    String id = UUID.randomUUID().toString();
    CompletableFuture<Void> result = prefixService.deletePrefix(id, ctxMock, okapiHeadersMock);
    CompletionException expectedException = assertThrows(CompletionException.class, result::join);

    HttpException httpException = (HttpException) expectedException.getCause();
    assertEquals(400, httpException.getCode());
    assertEquals(PREFIX_IS_USED.toError(), httpException.getError());

    verify(prefixDAO).getById(eq(id), any(), anyMap());
    verify(prefixDAO, never()).delete(eq(id), any(), anyMap());
    verify(purchaseOrderDAO).get(eq("poNumberPrefix==test"), eq(0), eq(0), any(), anyMap());
  }

  @Test
  public void testDeletePrefixSuccessIfNotUsed() {
    //given
    when(prefixDAO.getById(anyString(), any(), anyMap())).thenReturn(CompletableFuture.completedFuture(new Prefix().withName("test")));
    when(prefixDAO.delete(anyString(), any(), anyMap())).thenReturn(CompletableFuture.completedFuture(null));
    when(purchaseOrderDAO.get(anyString(), anyInt(), anyInt(), any(), anyMap())).thenReturn(CompletableFuture.completedFuture(new PurchaseOrderCollection().withTotalRecords(0)));

    String id = UUID.randomUUID().toString();
    CompletableFuture<Void> result = prefixService.deletePrefix(id, ctxMock, okapiHeadersMock);
    assertFalse(result.isCompletedExceptionally());
    result.join();

    verify(prefixDAO).getById(eq(id), any(), anyMap());
    verify(prefixDAO).delete(eq(id), any(), anyMap());
    verify(purchaseOrderDAO).get(eq("poNumberPrefix==test"), eq(0), eq(0), any(), anyMap());
  }

  @Test
  public void testGetPrefixById() {
    Prefix prefix = new Prefix().withName("suf").withId(UUID.randomUUID().toString()).withDescription("Test prefix");
    when(prefixDAO.getById(Mockito.anyString(), any(), anyMap())).thenReturn(CompletableFuture.completedFuture(prefix));

    CompletableFuture<Prefix> result = prefixService.getPrefixById(prefix.getId(), ctxMock, okapiHeadersMock);
    assertFalse(result.isCompletedExceptionally());

    Prefix resultPrefix = result.join();
    assertEquals(prefix, resultPrefix);

    verify(prefixDAO).getById(eq(prefix.getId()), any(), anyMap());
  }

  @Test
  public void testGetSuffixesByQuery() {
    String query = "name==pref";
    Prefix prefix = new Prefix().withName("suf").withId(UUID.randomUUID().toString()).withDescription("Test prefix");
    PrefixCollection prefixCollection = new PrefixCollection().withTotalRecords(1).withPrefixes(Collections.singletonList(prefix));
    when(prefixDAO.get(eq(query), eq(1), eq(0), any(), anyMap())).thenReturn(CompletableFuture.completedFuture(prefixCollection));

    CompletableFuture<PrefixCollection> result = prefixService.getPrefixes(query, 1, 0, ctxMock, okapiHeadersMock);
    assertFalse(result.isCompletedExceptionally());

    PrefixCollection resultPrefixCollection = result.join();

    assertEquals(prefixCollection, resultPrefixCollection);
    verify(prefixDAO).get(eq(query), eq(1), eq(0), any(), anyMap());
  }

  @Test
  public void testUpdateSuffix() {
    Prefix prefix = new Prefix().withId(UUID.randomUUID().toString())
      .withName("pref");

    when(prefixDAO.update(eq(prefix.getId()), eq(prefix), any(), anyMap())).thenReturn(CompletableFuture.completedFuture(null));

    CompletableFuture<Void> result = prefixService.updatePrefix(prefix.getId(), prefix, ctxMock, okapiHeadersMock);
    assertFalse(result.isCompletedExceptionally());
    result.join();

    verify(prefixDAO).update(eq(prefix.getId()), eq(prefix), any(), anyMap());
  }

  @Test
  public void testUpdateSuffixWithoutIdInBody() {
    Prefix prefix = new Prefix()
      .withName("pref");

    String id = UUID.randomUUID().toString();
    when(prefixDAO.update(eq(id), eq(prefix), any(), anyMap())).thenReturn(CompletableFuture.completedFuture(null));

    CompletableFuture<Void> result = prefixService.updatePrefix(id, prefix, ctxMock, okapiHeadersMock);
    assertFalse(result.isCompletedExceptionally());
    result.join();

    assertEquals(id, prefix.getId());
    verify(prefixDAO).update(eq(id), eq(prefix), any(), anyMap());
  }

  @Test
  public void testUpdateSuffixWithIdMismatchFails() {
    Prefix prefix = new Prefix().withId(UUID.randomUUID().toString());

    CompletableFuture<Void> result = prefixService.updatePrefix(UUID.randomUUID().toString(), prefix, ctxMock, okapiHeadersMock);
    CompletionException expectedException = assertThrows(CompletionException.class, result::join);

    HttpException httpException = (HttpException) expectedException.getCause();
    assertEquals(422, httpException.getCode());
    assertEquals(MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY.toError(), httpException.getError());
  }

  @Test
  public void testCreateSuffix() {
    Prefix prefix = new Prefix().withName("suf").withId(UUID.randomUUID().toString()).withDescription("Test prefix");

    when(prefixDAO.save(eq(prefix), any(), anyMap())).thenReturn(CompletableFuture.completedFuture(prefix));

    CompletableFuture<Prefix> result = prefixService.createPrefix(prefix, ctxMock, okapiHeadersMock);
    assertFalse(result.isCompletedExceptionally());
    Prefix resultPrefix = result.join();
    assertEquals(prefix, resultPrefix);

    verify(prefixDAO).save(eq(prefix), any(), anyMap());

  }

}
