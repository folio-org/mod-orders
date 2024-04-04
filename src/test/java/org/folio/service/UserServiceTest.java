package org.folio.service;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.TestUtils.getMockData;
import static org.folio.rest.impl.MockServer.USERS_MOCK_DATA_PATH;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;

import java.io.IOException;
import java.util.List;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import org.folio.models.UserCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

@ExtendWith(VertxExtension.class)
public class UserServiceTest {

  @Mock
  private RestClient restClient;
  @InjectMocks
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
  void getUsersByIds(VertxTestContext vertxTestContext) throws IOException {
    var users = new JsonObject(getMockData(USERS_MOCK_DATA_PATH + "user_collection.json")).mapTo(UserCollection.class);
    List<String> userIds = users.getUsers().stream().map(user -> String.valueOf(user.getId())).toList();
    doReturn(succeededFuture(users)).when(restClient).get(any(RequestEntry.class), eq(UserCollection.class), any());

    Future<UserCollection> future = userService.getUsersByIds(userIds, requestContextMock);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        vertxTestContext.completeNow();
      });
  }
}
