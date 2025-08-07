package org.folio.rest.impl;

import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.jaxrs.model.Errors;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import javax.ws.rs.core.Response;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

public class BaseApiTest {

  private AutoCloseable mockitoMocks;
  private BaseApi baseApi;

  @Mock
  Handler<AsyncResult<Response>> asyncResultHandler;
  @Captor
  private ArgumentCaptor<Future<Response>> asyncResultCaptor;

  @BeforeEach
  void beforeEach() {
    mockitoMocks = MockitoAnnotations.openMocks(this);
    baseApi = new BaseApi();
  }

  @AfterEach
  void resetMocks() throws Exception {
    mockitoMocks.close();
  }

  @Test
  @DisplayName("Test handleErrorResponse")
  void testHandleErrorResponse() {
    // Given
    Throwable t = new HttpException(123, "test");
    doNothing()
      .when(asyncResultHandler).handle(any());

    // When
    baseApi.handleErrorResponse(asyncResultHandler, t);

    // Then
    verify(asyncResultHandler, times(1)).handle(asyncResultCaptor.capture());
    Response response = asyncResultCaptor.getAllValues().getFirst().result();
    assertThat(response.getStatus(), equalTo(123));
    Errors errors = (Errors)response.getEntity();
    assertThat(errors.getErrors().getFirst().getMessage(), equalTo("test"));
  }
}
