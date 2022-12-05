package org.folio.service;

import static org.folio.rest.core.exceptions.ErrorCodes.*;
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
import java.util.concurrent.CompletionException;

import io.vertx.core.Future;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.PurchaseOrderCollection;
import org.folio.rest.jaxrs.model.Suffix;
import org.folio.rest.jaxrs.model.SuffixCollection;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

@ExtendWith(VertxExtension.class)
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
  void testDeleteSuffixFailedIfSuffixUsedByOrder(VertxTestContext vertxTestContext) {
    //given
    when(restClient.get(any(RequestEntry.class), any(), any()))
      .thenReturn(Future.succeededFuture(new Suffix().withName("test")));
    when(restClient.delete(any(RequestEntry.class), any())).thenReturn(Future.succeededFuture(null));
    when(purchaseOrderStorageService.getPurchaseOrders(anyString(), anyInt(), anyInt(), any()))
      .thenReturn(Future.succeededFuture(new PurchaseOrderCollection().withTotalRecords(1)));

    String id = UUID.randomUUID().toString();
    Future<Void> future  = suffixService.deleteSuffix(id, requestContext);
    vertxTestContext.assertFailure(future)
      .onComplete(expectedException -> {
        HttpException httpException = (HttpException) expectedException.cause();
        assertEquals(400, httpException.getCode());
        assertEquals(SUFFIX_IS_USED.toError(), httpException.getError());

        verify(restClient).get(any(RequestEntry.class), any(), any());
        verify(restClient, never()).delete(any(RequestEntry.class), any());
        verify(purchaseOrderStorageService).getPurchaseOrders(eq("poNumberSuffix==test"), eq(0), eq(0), any());
        vertxTestContext.completeNow();
      });

  }

  @Test
  void testSetSuffixFailedIfSuffixNotAvailable(VertxTestContext vertxTestContext) {
    //given
    when(restClient.get(any(RequestEntry.class), any(), any()))
      .thenReturn(Future.succeededFuture(new SuffixCollection().withTotalRecords(0)));

    Future<Void> future = suffixService.validateSuffixAvailability("Test", requestContext);
    vertxTestContext.assertFailure(future)
      .onComplete(expectedException -> {
        HttpException httpException = (HttpException) expectedException.cause();
        assertEquals(404, httpException.getCode());
        assertEquals(SUFFIX_NOT_FOUND.toError(), httpException.getError());

        verify(restClient).get(any(RequestEntry.class), any(), any());
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testDeleteSuffixSuccessIfNotUsed(VertxTestContext vertxTestContext) {
    //given
    when(restClient.get(any(RequestEntry.class), any(), any())).thenReturn(Future.succeededFuture(new Suffix().withName("test")));
    when(restClient.delete(any(RequestEntry.class), any())).thenReturn(Future.succeededFuture(null));
    when(purchaseOrderStorageService.getPurchaseOrders(anyString(), anyInt(), anyInt(), any()))
      .thenReturn(Future.succeededFuture(new PurchaseOrderCollection().withTotalRecords(0)));

    String id = UUID.randomUUID().toString();
    Future<Void> future = suffixService.deleteSuffix(id, requestContext);
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        verify(restClient).get(any(RequestEntry.class), any(), any());
        verify(restClient).delete(any(RequestEntry.class), any());
        verify(purchaseOrderStorageService).getPurchaseOrders(eq("poNumberSuffix==test"), eq(0), eq(0), any());
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testGetSuffixById(VertxTestContext vertxTestContext) {
    Suffix suffix = new Suffix().withName("suf").withId(UUID.randomUUID().toString()).withDescription("Test suffix");
    when(restClient.get(any(RequestEntry.class), any(), any())).thenReturn(Future.succeededFuture(suffix));

    Future<Suffix> future = suffixService.getSuffixById(suffix.getId(), requestContext);
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        Suffix resultSuffix = result.result();
        assertEquals(suffix, resultSuffix);

        verify(restClient).get(any(RequestEntry.class), any(), any());
        vertxTestContext.completeNow();
      });

  }

  @Test
  void testGetSuffixesByQuery(VertxTestContext vertxTestContext) {
    String query = "name==suf";
    Suffix suffix = new Suffix().withName("suf").withId(UUID.randomUUID().toString()).withDescription("Test suffix");
    SuffixCollection suffixCollection = new SuffixCollection().withTotalRecords(1).withSuffixes(Collections.singletonList(suffix));
    when(restClient.get(any(RequestEntry.class), any(), any())).thenReturn(Future.succeededFuture(suffixCollection));

    Future<SuffixCollection> future = suffixService.getSuffixes(query, 1, 0, requestContext);
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        SuffixCollection resultSuffixCollection = result.result();

        assertEquals(suffixCollection, resultSuffixCollection);
        verify(restClient).get(any(RequestEntry.class), any(), any());
        vertxTestContext.completeNow();
      });

  }

  @Test
  void testUpdateSuffix(VertxTestContext vertxTestContext) {
    Suffix suffix = new Suffix().withId(UUID.randomUUID().toString())
      .withName("suff");

    when(restClient.put(any(RequestEntry.class), eq(suffix), any())).thenReturn(Future.succeededFuture(null));

    Future<Void> future = suffixService.updateSuffix(suffix.getId(), suffix, requestContext);
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        verify(restClient).put(any(RequestEntry.class), eq(suffix), any());
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testUpdateSuffixWithoutIdInBody(VertxTestContext vertxTestContext) {
    Suffix suffix = new Suffix()
      .withName("suff");

    String id = UUID.randomUUID().toString();
    when(restClient.put(any(RequestEntry.class), eq(suffix), any())).thenReturn(Future.succeededFuture(null));

    Future<Void> future = suffixService.updateSuffix(id, suffix, requestContext);
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        assertEquals(id, suffix.getId());
        verify(restClient).put(any(RequestEntry.class), eq(suffix), any());
        vertxTestContext.completeNow();
      });

  }

  @Test
  void testUpdateSuffixWithIdMismatchFails(VertxTestContext vertxTestContext) {
    Suffix suffix = new Suffix().withId(UUID.randomUUID().toString());
    Future<Void> future = suffixService.updateSuffix(UUID.randomUUID().toString(), suffix, requestContext);

    vertxTestContext.assertFailure(future)
      .onComplete(completionException -> {
        assertTrue(completionException.failed());

        HttpException httpException = (HttpException) completionException.cause();
        assertEquals(422, httpException.getCode());
        assertEquals(MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY.toError(), httpException.getError());
        vertxTestContext.completeNow();
      });

  }

  @Test
  void testCreateSuffix(VertxTestContext vertxTestContext) {
    Suffix suffix = new Suffix().withName("suf").withId(UUID.randomUUID().toString()).withDescription("Test suffix");

    when(restClient.post(any(RequestEntry.class), eq(suffix), any(), any())).thenReturn(Future.succeededFuture(suffix));

    Future<Suffix> future = suffixService.createSuffix(suffix, requestContext);
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        Suffix resultSuffix = result.result();
        assertEquals(suffix, resultSuffix);

        verify(restClient).post(any(RequestEntry.class), eq(suffix), any(), any());
        vertxTestContext.completeNow();
      });


  }

}
