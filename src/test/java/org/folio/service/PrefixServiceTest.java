package org.folio.service;

import static org.folio.rest.core.exceptions.ErrorCodes.*;
import static org.folio.rest.core.exceptions.ErrorCodes.PREFIX_NOT_FOUND;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
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

import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.Prefix;
import org.folio.rest.jaxrs.model.PrefixCollection;
import org.folio.rest.jaxrs.model.PurchaseOrderCollection;
import org.folio.rest.jaxrs.model.SuffixCollection;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import io.vertx.core.Future;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;

@ExtendWith(VertxExtension.class)
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
  void testDeletePrefixFailedIfSuffixUsedByOrder(VertxTestContext vertxTestContext) {
    //given
    when(restClient.get(any(RequestEntry.class), any(), any())).thenReturn(Future.succeededFuture(new Prefix().withName("test")));
    when(restClient.delete(anyString(), any())).thenReturn(Future.succeededFuture(null));
    when(purchaseOrderStorageService.getPurchaseOrders(anyString(), anyInt(), anyInt(), any())).thenReturn(Future.succeededFuture(new PurchaseOrderCollection().withTotalRecords(1)));

    String id = UUID.randomUUID().toString();
    Future<Void> future = prefixService.deletePrefix(id, requestContext);
    vertxTestContext.assertFailure(future)
      .onComplete(completionException -> {
        HttpException httpException = (HttpException) completionException.cause();
        assertEquals(400, httpException.getCode());
        assertEquals(PREFIX_IS_USED.toError(), httpException.getError());

        verify(restClient).get(any(RequestEntry.class), any(), any());
        verify(restClient, never()).delete(anyString(), any());
        verify(purchaseOrderStorageService).getPurchaseOrders(eq("poNumberPrefix==test"), eq(0), eq(0), any());
        vertxTestContext.completeNow();
      });


  }

  @Test
  void testSetPrefixFailedIfSuffixIsNotAvailable(VertxTestContext vertxTestContext) {
    //given
    when(restClient.get(any(RequestEntry.class), any(), any())).thenReturn(Future.succeededFuture(new PrefixCollection().withTotalRecords(0)));

    String id = UUID.randomUUID().toString();
    Future<Void> future  = prefixService.validatePrefixAvailability("Test", requestContext);
    vertxTestContext.assertFailure(future)
      .onComplete(expectedException ->{
        HttpException httpException = (HttpException) expectedException.cause();
        assertEquals(404, httpException.getCode());
        assertEquals(PREFIX_NOT_FOUND.toError(), httpException.getError());

        verify(restClient).get(any(RequestEntry.class), any(), any());
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testDeletePrefixSuccessIfNotUsed(VertxTestContext vertxTestContext) {
    //given
    when(restClient.get(any(RequestEntry.class), any(), any())).thenReturn(Future.succeededFuture(new Prefix().withName("test")));
    when(restClient.delete(any(RequestEntry.class), any())).thenReturn(Future.succeededFuture(null));
    when(purchaseOrderStorageService.getPurchaseOrders(anyString(), anyInt(), anyInt(), any())).thenReturn(Future.succeededFuture(new PurchaseOrderCollection().withTotalRecords(0)));

    String id = UUID.randomUUID().toString();
    Future<Void> future = prefixService.deletePrefix(id, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        verify(restClient).get(any(RequestEntry.class), any(), any());
        verify(restClient).delete(any(RequestEntry.class), any());
        verify(purchaseOrderStorageService).getPurchaseOrders(eq("poNumberPrefix==test"), eq(0), eq(0), any());
        vertxTestContext.completeNow();
      });

  }

  @Test
  void testGetPrefixById(VertxTestContext vertxTestContext) throws InterruptedException {
    Prefix prefix = new Prefix().withName("suf").withId(UUID.randomUUID().toString()).withDescription("Test prefix");
    when(restClient.get(any(RequestEntry.class), any(), any())).thenReturn(Future.succeededFuture(prefix));
    var future = prefixService.getPrefixById(prefix.getId(), requestContext);
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertEquals(prefix, result.result());
        vertxTestContext.completeNow();
      });

  }

  @Test
  void testGetSuffixesByQuery(VertxTestContext vertxTestContext) {
    String query = "name==pref";
    Prefix prefix = new Prefix().withName("suf").withId(UUID.randomUUID().toString()).withDescription("Test prefix");
    PrefixCollection prefixCollection = new PrefixCollection().withTotalRecords(1).withPrefixes(Collections.singletonList(prefix));
    when(restClient.get(any(RequestEntry.class), any(), any())).thenReturn(Future.succeededFuture(prefixCollection));

    Future<PrefixCollection> future = prefixService.getPrefixes(query, 1, 0, requestContext);
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());

        PrefixCollection resultPrefixCollection = result.result();

        assertEquals(prefixCollection, resultPrefixCollection);
        verify(restClient).get(any(RequestEntry.class), any(), any());
        vertxTestContext.completeNow();
      });

  }

  @Test
  void testUpdateSuffix(VertxTestContext vertxTestContext) {
    Prefix prefix = new Prefix().withId(UUID.randomUUID().toString())
      .withName("pref");

    when(restClient.put(any(RequestEntry.class), any(), any())).thenReturn(Future.succeededFuture(null));

    Future<Void> future = prefixService.updatePrefix(prefix.getId(), prefix, requestContext);
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testUpdateSuffixWithoutIdInBody(VertxTestContext vertxTestContext) {
    Prefix prefix = new Prefix()
      .withName("pref");

    String id = UUID.randomUUID().toString();
    when(restClient.put(any(RequestEntry.class), any(), any())).thenReturn(Future.succeededFuture(null));

    Future<Void> future = prefixService.updatePrefix(id, prefix, requestContext);
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        assertEquals(id, prefix.getId());
        verify(restClient).put(any(RequestEntry.class), eq(prefix), any());
        vertxTestContext.completeNow();
      });

  }

  @Test
  void testUpdateSuffixWithIdMismatchFails(VertxTestContext vertxTestContext) {
    Prefix prefix = new Prefix().withId(UUID.randomUUID().toString());

    Future<Void> future = prefixService.updatePrefix(UUID.randomUUID().toString(), prefix, requestContext);
    vertxTestContext.assertFailure(future)
      .onComplete(completionException ->{
        HttpException httpException = (HttpException) completionException.cause();
        assertEquals(422, httpException.getCode());
        assertEquals(MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY.toError(), httpException.getError());
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testCreatePrefix(VertxTestContext vertxTestContext) {
    Prefix prefix = new Prefix().withName("suf").withId(UUID.randomUUID().toString()).withDescription("Test prefix");

    when(restClient.post(any(RequestEntry.class), any(), any(), any())).thenReturn(Future.succeededFuture(prefix));

    Future<Prefix> future = prefixService.createPrefix(prefix, requestContext);
    vertxTestContext.assertComplete(future)
       .onComplete(result -> {
        assertTrue(result.succeeded());
        Prefix resultPrefix = result.result();
        assertEquals(prefix, resultPrefix);

        verify(restClient).post(any(RequestEntry.class), any(), any(), any());
        vertxTestContext.completeNow();
      });

  }

}
