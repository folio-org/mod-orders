package org.folio.service;

import static org.folio.TestConfig.mockPort;
import static org.folio.TestConstants.X_OKAPI_TOKEN;
import static org.folio.TestConstants.X_OKAPI_USER_ID;
import static org.folio.rest.RestConstants.OKAPI_URL;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;

import io.restassured.http.Header;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import org.folio.rest.acq.model.tag.Tag;
import org.folio.rest.acq.model.tag.TagCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

@ExtendWith(VertxExtension.class)
public class TagServiceTest {
  public static final String TENANT_ID = "ordertest";
  public static final Header X_OKAPI_TENANT = new Header(OKAPI_HEADER_TENANT, TENANT_ID);
  Map<String, String> okapiHeadersMock = new HashMap<>();
  @InjectMocks
  TagService tagService;
  @Mock
  private RestClient restClient;
  @Mock
  private RequestContext requestContextMock;

  @BeforeEach
  public void initMocks() {
    okapiHeadersMock.put(OKAPI_URL, "http://localhost:" + mockPort);
    okapiHeadersMock.put(X_OKAPI_TOKEN.getName(), X_OKAPI_TOKEN.getValue());
    okapiHeadersMock.put(X_OKAPI_TENANT.getName(), X_OKAPI_TENANT.getValue());
    okapiHeadersMock.put(X_OKAPI_USER_ID.getName(), X_OKAPI_USER_ID.getValue());
    MockitoAnnotations.openMocks(this);
  }

  @Test
  void createTagsIfMissing(VertxTestContext vertxTestContext) {
    String sampleTag = "CAseSenSeTivE";
    doReturn(Future.succeededFuture(new TagCollection().withTags(Collections.singletonList(new Tag().withLabel(sampleTag)))))
      .when(restClient).get(any(RequestEntry.class), eq(TagCollection.class), any(RequestContext.class));
    Future<Void> future = tagService.createTagsIfMissing(Collections.singleton(sampleTag), requestContextMock);
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        Assertions.assertTrue(result.succeeded());
        vertxTestContext.completeNow();
      });
  }
}
