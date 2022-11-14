package org.folio.service;

import static org.folio.rest.core.exceptions.ErrorCodes.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Prefix;
import org.folio.rest.jaxrs.model.PrefixCollection;
import org.folio.rest.jaxrs.model.PurchaseOrderCollection;
import org.folio.rest.jaxrs.model.SuffixCollection;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class PrefixServiceTest {

  @InjectMocks
  private PrefixService prefixService;

  @Mock
  private RestClient restClient;

  @Mock
  private PurchaseOrderStorageService purchaseOrderStorageService;

  @Mock
  private RequestContext requestContext;

  @BeforeEach
  public void initMocks(){
      MockitoAnnotations.openMocks(this);
  }

  @Test
  void testDeletePrefixFailedIfSuffixUsedByOrder() {
    //given
    when(restClient.get(any(), any(), any())).thenReturn(CompletableFuture.completedFuture(new Prefix().withName("test")));
    when(restClient.delete(any(), any())).thenReturn(CompletableFuture.completedFuture(null));
    when(purchaseOrderStorageService.getPurchaseOrders(anyString(), anyInt(), anyInt(), any())).thenReturn(CompletableFuture.completedFuture(new PurchaseOrderCollection().withTotalRecords(1)));

    String id = UUID.randomUUID().toString();
    CompletableFuture<Void> result = prefixService.deletePrefix(id, requestContext);
    CompletionException expectedException = assertThrows(CompletionException.class, result::join);

    HttpException httpException = (HttpException) expectedException.getCause();
    assertEquals(400, httpException.getCode());
    assertEquals(PREFIX_IS_USED.toError(), httpException.getError());

    verify(restClient).get(any(), any(), any());
    verify(restClient, never()).delete(any(), any());
    verify(purchaseOrderStorageService).getPurchaseOrders(eq("poNumberPrefix==test"), eq(0), eq(0), any());
  }
  @Test
  void testSetPrefixFailedIfSuffixIsNotAvailable() {
    //given
    when(restClient.get(any(), any(), any())).thenReturn(CompletableFuture.completedFuture(new PrefixCollection().withTotalRecords(0)));

    String id = UUID.randomUUID().toString();
    CompletableFuture<Void> result = prefixService.isPrefixAvailable("Test", requestContext);
    CompletionException expectedException = assertThrows(CompletionException.class, result::join);

    HttpException httpException = (HttpException) expectedException.getCause();
    assertEquals(404, httpException.getCode());
    assertEquals(PREFIX_NOT_FOUND.toError(), httpException.getError());

    verify(restClient).get(any(), any(), any());
  }

  @Test
  void testDeletePrefixSuccessIfNotUsed() {
    //given
    when(restClient.get(any(), any(), any())).thenReturn(CompletableFuture.completedFuture(new Prefix().withName("test")));
    when(restClient.delete(any(), any())).thenReturn(CompletableFuture.completedFuture(null));
    when(purchaseOrderStorageService.getPurchaseOrders(anyString(), anyInt(), anyInt(), any())).thenReturn(CompletableFuture.completedFuture(new PurchaseOrderCollection().withTotalRecords(0)));

    String id = UUID.randomUUID().toString();
    CompletableFuture<Void> result = prefixService.deletePrefix(id, requestContext);
    assertFalse(result.isCompletedExceptionally());
    result.join();

    verify(restClient).get(any(), any(), any());
    verify(restClient).delete(any(), any());
    verify(purchaseOrderStorageService).getPurchaseOrders(eq("poNumberPrefix==test"), eq(0), eq(0), any());
  }

  @Test
  void testGetPrefixById() {
    Prefix prefix = new Prefix().withName("suf").withId(UUID.randomUUID().toString()).withDescription("Test prefix");
    when(restClient.get(any(), any(), any())).thenReturn(CompletableFuture.completedFuture(prefix));

    CompletableFuture<Prefix> result = prefixService.getPrefixById(prefix.getId(), requestContext);
    assertFalse(result.isCompletedExceptionally());

    Prefix resultPrefix = result.join();
    assertEquals(prefix, resultPrefix);

    verify(restClient).get(any(), any(), any());
  }

  @Test
  void testGetSuffixesByQuery() {
    String query = "name==pref";
    Prefix prefix = new Prefix().withName("suf").withId(UUID.randomUUID().toString()).withDescription("Test prefix");
    PrefixCollection prefixCollection = new PrefixCollection().withTotalRecords(1).withPrefixes(Collections.singletonList(prefix));
    when(restClient.get(any(), any(), any())).thenReturn(CompletableFuture.completedFuture(prefixCollection));

    CompletableFuture<PrefixCollection> result = prefixService.getPrefixes(query, 1, 0, requestContext);
    assertFalse(result.isCompletedExceptionally());

    PrefixCollection resultPrefixCollection = result.join();

    assertEquals(prefixCollection, resultPrefixCollection);
    verify(restClient).get(any(), any(), any());
  }

  @Test
  void testUpdateSuffix() {
    Prefix prefix = new Prefix().withId(UUID.randomUUID().toString())
      .withName("pref");

    when(restClient.put(any(), eq(prefix), any())).thenReturn(CompletableFuture.completedFuture(null));

    CompletableFuture<Void> result = prefixService.updatePrefix(prefix.getId(), prefix, requestContext);
    assertFalse(result.isCompletedExceptionally());
    result.join();

    verify(restClient).put(any(), eq(prefix), any());
  }

  @Test
  void testUpdateSuffixWithoutIdInBody() {
    Prefix prefix = new Prefix()
      .withName("pref");

    String id = UUID.randomUUID().toString();
    when(restClient.put(any(), eq(prefix), any())).thenReturn(CompletableFuture.completedFuture(null));

    CompletableFuture<Void> result = prefixService.updatePrefix(id, prefix, requestContext);
    assertFalse(result.isCompletedExceptionally());
    result.join();

    assertEquals(id, prefix.getId());
    verify(restClient).put(any(), eq(prefix), any());
  }

  @Test
  void testUpdateSuffixWithIdMismatchFails() {
    Prefix prefix = new Prefix().withId(UUID.randomUUID().toString());

    CompletableFuture<Void> result = prefixService.updatePrefix(UUID.randomUUID().toString(), prefix, requestContext);
    CompletionException expectedException = assertThrows(CompletionException.class, result::join);

    HttpException httpException = (HttpException) expectedException.getCause();
    assertEquals(422, httpException.getCode());
    assertEquals(MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY.toError(), httpException.getError());
  }

  @Test
  void testCreateSuffix() {
    Prefix prefix = new Prefix().withName("suf").withId(UUID.randomUUID().toString()).withDescription("Test prefix");

    when(restClient.post(any(), any(), any(), any())).thenReturn(CompletableFuture.completedFuture(prefix));

    CompletableFuture<Prefix> result = prefixService.createPrefix(prefix, requestContext);
    assertFalse(result.isCompletedExceptionally());
    Prefix resultPrefix = result.join();
    assertEquals(prefix, resultPrefix);

    verify(restClient).post(any(), any(), any(), any());

  }

}
