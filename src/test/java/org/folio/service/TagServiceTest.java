package org.folio.service;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.TestConfig.mockPort;
import static org.folio.TestConstants.X_OKAPI_TOKEN;
import static org.folio.TestConstants.X_OKAPI_USER_ID;
import static org.folio.rest.RestConstants.OKAPI_URL;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doReturn;

import io.restassured.http.Header;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import org.folio.rest.acq.model.tag.Tag;
import org.folio.rest.acq.model.tag.TagCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.junit.BeforeClass;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

@ExtendWith(VertxExtension.class)
class TagServiceTest {
  private static final String ORDER_ID = "1ab7ef6a-d1d4-4a4f-90a2-882aed18af14";
  private static final String ORDER_PATH = BASE_MOCK_DATA_PATH + "compositeOrders/" + ORDER_ID + ".json";
  public static final String TENANT_ID = "ordertest";
  public static final Header X_OKAPI_TENANT = new Header(OKAPI_HEADER_TENANT, TENANT_ID);
  private Collections collections;
  private RequestContext requestContext;
  public Context ctxMock = Vertx.vertx().getOrCreateContext();

  Map<String, String> okapiHeadersMock = new HashMap<>();
  @InjectMocks
  TagService tagService1 ;
  @Spy
  private RestClient restClient2;
  @Mock
  private RequestContext requestContextMock;
  @Mock
  private Context ctxMocks = Vertx.vertx().getOrCreateContext();

  @BeforeEach
  public void initMocks(){
    okapiHeadersMock.put(OKAPI_URL, "http://localhost:" + mockPort);
    okapiHeadersMock.put(X_OKAPI_TOKEN.getName(), X_OKAPI_TOKEN.getValue());
    okapiHeadersMock.put(X_OKAPI_TENANT.getName(), X_OKAPI_TENANT.getValue());
    okapiHeadersMock.put(X_OKAPI_USER_ID.getName(), X_OKAPI_USER_ID.getValue());
    MockitoAnnotations.openMocks(this);
  }

  @Test
  void createTagsIfMissing(VertxTestContext vertxTestContext) throws NoSuchFieldException, IllegalAccessException {
    String sampleTag = "CAseSenSeTivE";
    Tag postTagResponse = new Tag().withLabel(sampleTag);
    TagCollection emptyTagCollection = new TagCollection()
      .withTags(new ArrayList<>())
      .withTotalRecords(0);
    TagService tagService1 = new TagService(restClient2);
    doReturn(succeededFuture(new TagCollection())).when(restClient2).get(anyString(), any(), (RequestContext) any());
    doReturn(succeededFuture(emptyTagCollection)).when(restClient2).get(anyString(), any(), any());
    doReturn(succeededFuture(postTagResponse)).when(restClient2).post(anyString(), any(), any(), any());
    Field restClientField = TagService.class.getDeclaredField("restClient");
    Future<Void> future = tagService1.createTagsIfMissing(Collections.singleton(sampleTag), requestContextMock);
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        Assertions.assertTrue(result.succeeded());
        vertxTestContext.completeNow();
      });
  }
}
