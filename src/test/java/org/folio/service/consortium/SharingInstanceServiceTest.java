package org.folio.service.consortium;

import io.vertx.core.Future;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import org.apache.commons.lang3.StringUtils;
import org.folio.models.consortium.ConsortiumConfiguration;
import org.folio.models.consortium.SharingInstance;
import org.folio.models.consortium.SharingStatus;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.ConsortiumException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;

@ExtendWith({VertxExtension.class, MockitoExtension.class})
public class SharingInstanceServiceTest {

  @InjectMocks
  private SharingInstanceService sharingInstanceService;

  @Mock
  private RestClient restClient;

  @Mock
  private RequestContext requestContext;

  @Test
  void testCreateShadowInstance(VertxTestContext vertxTestContext) {
    String instanceId = UUID.randomUUID().toString();
    ConsortiumConfiguration consortiumConfiguration = new ConsortiumConfiguration("diku", "consortium");

    Mockito.when(restClient.post(any(RequestEntry.class), any(), any(), any()))
      .thenReturn(Future.succeededFuture(new SharingInstance(UUID.randomUUID(), StringUtils.EMPTY, StringUtils.EMPTY)));

    Future<SharingInstance> future = sharingInstanceService.createShadowInstance(instanceId, consortiumConfiguration, requestContext);
    vertxTestContext.assertComplete(future)
      .onComplete(ar -> {
        Assertions.assertTrue(ar.succeeded());
        verify(restClient).post(any(RequestEntry.class), any(), any(), any());
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testCreateShadowInstanceFailure(VertxTestContext vertxTestContext) {
    SharingInstanceService service = new SharingInstanceService(restClient);
    String instanceId = UUID.randomUUID().toString();
    SharingInstance response = new SharingInstance(UUID.randomUUID(), UUID.randomUUID(), "test", "consortium", SharingStatus.ERROR, StringUtils.EMPTY);
    ConsortiumConfiguration consortiumConfiguration = new ConsortiumConfiguration("diku", "consortium");

    Mockito.when(restClient.post(any(RequestEntry.class), any(), any(), any())).thenReturn(Future.succeededFuture(response));

    Future<SharingInstance> future = service.createShadowInstance(instanceId, consortiumConfiguration, requestContext);
    vertxTestContext.assertFailure(future)
      .onComplete(completionException -> {
        assertEquals(ConsortiumException.class, completionException.cause().getClass());
        vertxTestContext.completeNow();
      });
  }

}
