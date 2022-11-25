package org.folio.service;

import static org.folio.rest.core.exceptions.ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY;
import static org.folio.rest.core.exceptions.ErrorCodes.PREFIX_IS_USED;
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
import java.util.UUID;
import java.util.concurrent.CompletionException;
import java.util.concurrent.TimeUnit;

import io.vertx.core.Future;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Prefix;
import org.folio.rest.jaxrs.model.PrefixCollection;
import org.folio.rest.jaxrs.model.PurchaseOrderCollection;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

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
  void testDeletePrefixFailedIfSuffixUsedByOrder() {
    //given
    when(restClient.get(anyString(), any(), any())).thenReturn(Future.succeededFuture(new Prefix().withName("test")));
    when(restClient.delete(anyString(), any())).thenReturn(Future.succeededFuture(null));
    when(purchaseOrderStorageService.getPurchaseOrders(anyString(), anyInt(), anyInt(), any())).thenReturn(Future.succeededFuture(new PurchaseOrderCollection().withTotalRecords(1)));

    String id = UUID.randomUUID().toString();
    Future<Void> result = prefixService.deletePrefix(id, requestContext);
    CompletionException expectedException = assertThrows(CompletionException.class, result::result);

    HttpException httpException = (HttpException) expectedException.getCause();
    assertEquals(400, httpException.getCode());
    assertEquals(PREFIX_IS_USED.toError(), httpException.getError());

    verify(restClient).get(anyString(), any(), any());
    verify(restClient, never()).delete(anyString(), any());
    verify(purchaseOrderStorageService).getPurchaseOrders(eq("poNumberPrefix==test"), eq(0), eq(0), any());
  }

  @Test
  void testDeletePrefixSuccessIfNotUsed() {
    //given
    when(restClient.get(anyString(), any(), any())).thenReturn(Future.succeededFuture(new Prefix().withName("test")));
    when(restClient.delete(anyString(), any())).thenReturn(Future.succeededFuture(null));
    when(purchaseOrderStorageService.getPurchaseOrders(anyString(), anyInt(), anyInt(), any())).thenReturn(Future.succeededFuture(new PurchaseOrderCollection().withTotalRecords(0)));

    String id = UUID.randomUUID().toString();
    Future<Void> result = prefixService.deletePrefix(id, requestContext);
    assertFalse(result.failed());
    result.result();

    verify(restClient).get(anyString(), any(), any());
    verify(restClient).delete(anyString(), any());
    verify(purchaseOrderStorageService).getPurchaseOrders(eq("poNumberPrefix==test"), eq(0), eq(0), any());
  }

  @Test
  void testGetPrefixById(VertxTestContext vertxTestContext) throws InterruptedException {
    Prefix prefix = new Prefix().withName("suf").withId(UUID.randomUUID().toString()).withDescription("Test prefix");
    when(restClient.get(anyString(), any(), any())).thenReturn(Future.succeededFuture(prefix));
    Prefix resultPrefix = prefixService.getPrefixById(prefix.getId(), requestContext).toCompletionStage().toCompletableFuture().join();
    assertEquals(prefix, resultPrefix);
    verify(restClient).get(anyString(), any(), any());
  }

  @Test
  void testGetSuffixesByQuery() {
    String query = "name==pref";
    Prefix prefix = new Prefix().withName("suf").withId(UUID.randomUUID().toString()).withDescription("Test prefix");
    PrefixCollection prefixCollection = new PrefixCollection().withTotalRecords(1).withPrefixes(Collections.singletonList(prefix));
    when(restClient.get(anyString(), any(), any())).thenReturn(Future.succeededFuture(prefixCollection));

    Future<PrefixCollection> result = prefixService.getPrefixes(query, 1, 0, requestContext);
    assertFalse(result.failed());

    PrefixCollection resultPrefixCollection = result.result();

    assertEquals(prefixCollection, resultPrefixCollection);
    verify(restClient).get(anyString(), any(), any());
  }

  @Test
  void testUpdateSuffix() {
    Prefix prefix = new Prefix().withId(UUID.randomUUID().toString())
      .withName("pref");

    when(restClient.put(anyString(), any(), any())).thenReturn(Future.succeededFuture(null));

    Future<Void> result = prefixService.updatePrefix(prefix.getId(), prefix, requestContext);
    assertFalse(result.failed());
    result.result();

    verify(restClient).put(anyString(), eq(prefix), any());
  }

  @Test
  void testUpdateSuffixWithoutIdInBody() {
    Prefix prefix = new Prefix()
      .withName("pref");

    String id = UUID.randomUUID().toString();
    when(restClient.put(anyString(), any(), any())).thenReturn(Future.succeededFuture(null));

    Future<Void> result = prefixService.updatePrefix(id, prefix, requestContext);
    assertFalse(result.failed());
    result.result();

    assertEquals(id, prefix.getId());
    verify(restClient).put(anyString(), eq(prefix), any());
  }

  @Test
  void testUpdateSuffixWithIdMismatchFails() {
    Prefix prefix = new Prefix().withId(UUID.randomUUID().toString());

    Future<Void> result = prefixService.updatePrefix(UUID.randomUUID().toString(), prefix, requestContext);
    CompletionException expectedException = assertThrows(CompletionException.class, result::result);

    HttpException httpException = (HttpException) expectedException.getCause();
    assertEquals(422, httpException.getCode());
    assertEquals(MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY.toError(), httpException.getError());
  }

  @Test
  void testCreateSuffix() {
    Prefix prefix = new Prefix().withName("suf").withId(UUID.randomUUID().toString()).withDescription("Test prefix");

    when(restClient.post(anyString(), any(), any(), any())).thenReturn(Future.succeededFuture(prefix));

    Future<Prefix> result = prefixService.createPrefix(prefix, requestContext);
    assertFalse(result.failed());
    Prefix resultPrefix = result.result();
    assertEquals(prefix, resultPrefix);

    verify(restClient).post(anyString(), any(), any(), any());

  }

}
