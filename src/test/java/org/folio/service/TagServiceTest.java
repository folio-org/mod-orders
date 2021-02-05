package org.folio.service;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.TestConfig.mockPort;
import static org.folio.TestConstants.X_OKAPI_TOKEN;
import static org.folio.TestConstants.X_OKAPI_USER_ID;
import static org.folio.rest.RestConstants.OKAPI_URL;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import org.folio.rest.acq.model.tag.Tag;
import org.folio.rest.acq.model.tag.TagCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import io.restassured.http.Header;
import io.vertx.core.Context;
import io.vertx.core.Vertx;

class TagServiceTest {
  private static final String ORDER_ID = "1ab7ef6a-d1d4-4a4f-90a2-882aed18af14";
  private static final String ORDER_PATH = BASE_MOCK_DATA_PATH + "compositeOrders/" + ORDER_ID + ".json";
  public static final String TENANT_ID = "ordertest";
  public static final Header X_OKAPI_TENANT = new Header(OKAPI_HEADER_TENANT, TENANT_ID);

  @InjectMocks
  TagService tagService;

  @Mock
  private RestClient restClient;

  private RequestContext requestContextMock;

  @BeforeEach
  public void initMocks(){
    Context ctxMock = Vertx.vertx().getOrCreateContext();
    Map<String, String> okapiHeadersMock = new HashMap<>();
    okapiHeadersMock.put(OKAPI_URL, "http://localhost:" + mockPort);
    okapiHeadersMock.put(X_OKAPI_TOKEN.getName(), X_OKAPI_TOKEN.getValue());
    okapiHeadersMock.put(X_OKAPI_TENANT.getName(), X_OKAPI_TENANT.getValue());
    okapiHeadersMock.put(X_OKAPI_USER_ID.getName(), X_OKAPI_USER_ID.getValue());
    requestContextMock = new RequestContext(ctxMock, okapiHeadersMock);
    MockitoAnnotations.openMocks(this);
  }

  @Test
  void createTagsIfMissing() {
    String sampleTag = "CAseSenSeTivE";
    Tag postTagResponse = new Tag().withLabel(sampleTag);
    TagCollection emptyTagCollection = new TagCollection()
      .withTags(new ArrayList<>())
      .withTotalRecords(0);

    doReturn(completedFuture(emptyTagCollection)).when(restClient).get(any(), any(),  any());
    doReturn(completedFuture(postTagResponse)).when(restClient).post(any(), any(),  any(), any());

    CompletableFuture<Void> response = tagService.createTagsIfMissing(Collections.singleton(sampleTag), requestContextMock);
    response.join();

    Assertions.assertTrue(response.isDone());

  }

}
