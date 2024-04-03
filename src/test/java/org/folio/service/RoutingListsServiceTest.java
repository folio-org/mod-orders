package org.folio.service;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.TestConstants.ROUTING_LIST_ID;
import static org.folio.TestUtils.getMockData;
import static org.folio.rest.impl.MockServer.ROUTING_LIST_MOCK_DATA_PATH;
import static org.folio.rest.impl.MockServer.USERS_MOCK_DATA_PATH;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;

import java.io.IOException;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import org.folio.models.UserCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.RoutingList;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

@ExtendWith(VertxExtension.class)
public class RoutingListsServiceTest {

  @InjectMocks
  RoutingListsService routingListsService;
  @Mock
  private RestClient restClient;
  @Mock
  private UserService userService;
  @Mock
  private RequestContext requestContextMock;
  private AutoCloseable mockitoMocks;

  @BeforeEach
  public void initMocks() throws Exception {
    mockitoMocks = MockitoAnnotations.openMocks(this);
  }

  @AfterEach
  void afterEach() throws Exception {
    mockitoMocks.close();
  }

  @Test
  void processTemplate(VertxTestContext vertxTestContext) throws IOException {
    var routingList = new JsonObject(getMockData(ROUTING_LIST_MOCK_DATA_PATH + ROUTING_LIST_ID + ".json")).mapTo(RoutingList.class);
    var users = new JsonObject(getMockData(USERS_MOCK_DATA_PATH + "user_collection.json")).mapTo(UserCollection.class);
    var expectedTemplateRequest = new JsonObject(getMockData(ROUTING_LIST_MOCK_DATA_PATH + ROUTING_LIST_ID + "-expected-template-request.json"));
    doReturn(succeededFuture(routingList)).when(restClient).get(any(RequestEntry.class), eq(RoutingList.class), any(RequestContext.class));
    doReturn(succeededFuture(users)).when(userService).getUsersByIds(eq(routingList.getUserIds()), any(RequestContext.class));
    doReturn(succeededFuture(new JsonObject())).when(restClient).postJsonObject(any(RequestEntry.class), eq(expectedTemplateRequest), any());

    Future<JsonObject> future = routingListsService.processTemplateRequest(ROUTING_LIST_ID, requestContextMock);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        Assertions.assertTrue(result.succeeded());
        vertxTestContext.completeNow();
      });
  }
}
