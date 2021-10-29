package org.folio.service;

import static org.folio.rest.core.exceptions.ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY;
import static org.folio.rest.core.exceptions.ErrorCodes.SUFFIX_IS_USED;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
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

import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PurchaseOrderCollection;
import org.folio.rest.jaxrs.model.Suffix;
import org.folio.rest.jaxrs.model.SuffixCollection;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class SuffixServiceTest {

  @InjectMocks
  private SuffixService suffixService;

  @Mock
  private RestClient restClient;

  @Mock
  private PurchaseOrderStorageService purchaseOrderStorageService;

  @Mock
  private Map<String, String> okapiHeadersMock;

  @Mock
  private RequestContext requestContext;


  @BeforeEach
  public void initMocks(){
      MockitoAnnotations.openMocks(this);
  }

  @Test
  void testDeleteSuffixFailedIfSuffixUsedByOrder() {
    //given
    when(restClient.get(any(), any(), any()))
      .thenReturn(CompletableFuture.completedFuture(new Suffix().withName("test")));
    when(restClient.delete(any(), any())).thenReturn(CompletableFuture.completedFuture(null));
    when(purchaseOrderStorageService.getPurchaseOrders(anyString(), anyInt(), anyInt(), any()))
      .thenReturn(CompletableFuture.completedFuture(new PurchaseOrderCollection().withTotalRecords(1)));

    String id = UUID.randomUUID().toString();
    CompletableFuture<Void> result = suffixService.deleteSuffix(id, requestContext);
    CompletionException expectedException = assertThrows(CompletionException.class, result::join);

    HttpException httpException = (HttpException) expectedException.getCause();
    assertEquals(400, httpException.getCode());
    assertEquals(SUFFIX_IS_USED.toError(), httpException.getError());

    verify(restClient).get(any(), any(), any());
    verify(restClient, never()).delete(any(), any());
    verify(purchaseOrderStorageService).getPurchaseOrders(eq("poNumberSuffix==test"), eq(0), eq(0), any());
  }

  @Test
  void testDeleteSuffixSuccessIfNotUsed() {
    //given
    when(restClient.get(any(), any(), any())).thenReturn(CompletableFuture.completedFuture(new Suffix().withName("test")));
    when(restClient.delete(any(), any())).thenReturn(CompletableFuture.completedFuture(null));
    when(purchaseOrderStorageService.getPurchaseOrders(anyString(), anyInt(), anyInt(), any()))
      .thenReturn(CompletableFuture.completedFuture(new PurchaseOrderCollection().withTotalRecords(0)));

    String id = UUID.randomUUID().toString();
    CompletableFuture<Void> result = suffixService.deleteSuffix(id, requestContext);
    assertFalse(result.isCompletedExceptionally());
    result.join();

    verify(restClient).get(any(), any(), any());
    verify(restClient).delete(any(), any());
    verify(purchaseOrderStorageService).getPurchaseOrders(eq("poNumberSuffix==test"), eq(0), eq(0), any());
  }

  @Test
  void testGetSuffixById() {
    Suffix suffix = new Suffix().withName("suf").withId(UUID.randomUUID().toString()).withDescription("Test suffix");
    when(restClient.get(any(), any(), any())).thenReturn(CompletableFuture.completedFuture(suffix));

    CompletableFuture<Suffix> result = suffixService.getSuffixById(suffix.getId(), requestContext);
    assertFalse(result.isCompletedExceptionally());

    Suffix resultSuffix = result.join();
    assertEquals(suffix, resultSuffix);

    verify(restClient).get(any(), any(), any());
  }

  @Test
  void testGetSuffixesByQuery() {
    String query = "name==suf";
    Suffix suffix = new Suffix().withName("suf").withId(UUID.randomUUID().toString()).withDescription("Test suffix");
    SuffixCollection suffixCollection = new SuffixCollection().withTotalRecords(1).withSuffixes(Collections.singletonList(suffix));
    when(restClient.get(any(), any(), any())).thenReturn(CompletableFuture.completedFuture(suffixCollection));

    CompletableFuture<SuffixCollection> result = suffixService.getSuffixes(query, 1, 0, requestContext);
    assertFalse(result.isCompletedExceptionally());

    SuffixCollection resultSuffixCollection = result.join();

    assertEquals(suffixCollection, resultSuffixCollection);
    verify(restClient).get(any(), any(), any());
  }

  @Test
  void testUpdateSuffix() {
    Suffix suffix = new Suffix().withId(UUID.randomUUID().toString())
      .withName("suff");

    when(restClient.put(any(), eq(suffix), any())).thenReturn(CompletableFuture.completedFuture(null));

    CompletableFuture<Void> result = suffixService.updateSuffix(suffix.getId(), suffix, requestContext);
    assertFalse(result.isCompletedExceptionally());
    result.join();

    verify(restClient).put(any(), eq(suffix), any());
  }

  @Test
  void testUpdateSuffixWithoutIdInBody() {
    Suffix suffix = new Suffix()
      .withName("suff");

    String id = UUID.randomUUID().toString();
    when(restClient.put(any(), eq(suffix), any())).thenReturn(CompletableFuture.completedFuture(null));

    CompletableFuture<Void> result = suffixService.updateSuffix(id, suffix, requestContext);
    assertFalse(result.isCompletedExceptionally());
    result.join();

    assertEquals(id, suffix.getId());
    verify(restClient).put(any(), eq(suffix), any());
  }

  @Test
  void testUpdateSuffixWithIdMismatchFails() {
    Suffix suffix = new Suffix().withId(UUID.randomUUID().toString());

    CompletionException expectedException = assertThrows(CompletionException.class, () -> {
      CompletableFuture<Void> result = suffixService.updateSuffix(UUID.randomUUID().toString(), suffix, requestContext);
      assertTrue(result.isCompletedExceptionally());
      result.join();
    });
    HttpException httpException = (HttpException) expectedException.getCause();
    assertEquals(422, httpException.getCode());
    assertEquals(MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY.toError(), httpException.getError());
  }

  @Test
  void testCreateSuffix() {
    Suffix suffix = new Suffix().withName("suf").withId(UUID.randomUUID().toString()).withDescription("Test suffix");

    when(restClient.post(any(), eq(suffix), any(), any())).thenReturn(CompletableFuture.completedFuture(suffix));

    CompletableFuture<Suffix> result = suffixService.createSuffix(suffix, requestContext);
    assertFalse(result.isCompletedExceptionally());
    Suffix resultSuffix = result.join();
    assertEquals(suffix, resultSuffix);

    verify(restClient).post(any(), eq(suffix), any(), any());


  }

}
