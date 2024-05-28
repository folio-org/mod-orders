package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;
import static javax.ws.rs.core.HttpHeaders.CONTENT_TYPE;
import static javax.ws.rs.core.HttpHeaders.LOCATION;
import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static javax.ws.rs.core.Response.Status.CREATED;
import static javax.ws.rs.core.Response.Status.INTERNAL_SERVER_ERROR;
import static org.folio.rest.core.exceptions.ExceptionUtil.convertToErrors;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;

import javax.ws.rs.core.Response;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;

import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;

public class BaseApi {
  private final Logger logger = LogManager.getLogger();
  private final Errors processingErrors = new Errors();

  public Response buildOkResponse(Object body) {
    return Response.ok(body, APPLICATION_JSON)
      .build();
  }

  public Response buildNoContentResponse() {
    return Response.noContent()
      .build();
  }

  public Response buildResponseWithLocation(String okapi, String endpoint, Object body) {
    try {
      return Response.created(new URI(okapi + endpoint))
        .header(CONTENT_TYPE, APPLICATION_JSON)
        .entity(body)
        .build();
    } catch (URISyntaxException e) {
      return Response.created(URI.create(endpoint))
        .header(CONTENT_TYPE, APPLICATION_JSON)
        .header(LOCATION, endpoint)
        .entity(body)
        .build();
    }
  }

  public Void handleErrorResponse(Handler<AsyncResult<Response>> asyncResultHandler, Throwable t) {
    asyncResultHandler.handle(succeededFuture(buildErrorResponse(t)));
    return null;
  }

  public Response buildErrorResponse(Throwable throwable) {
    logger.error("Exception encountered", throwable);
    if (throwable instanceof HttpException exception) {
      return buildErrorResponseFromHttpException(exception);
    }
    final int code = defineErrorCode(throwable);
    final Errors errors = convertToErrors(throwable);
    final Response.ResponseBuilder responseBuilder = createResponseBuilder(code);
    return responseBuilder.header(CONTENT_TYPE, APPLICATION_JSON)
      .entity(errors)
      .build();
  }

  public List<Error> getErrors() {
    return processingErrors.getErrors();
  }

  protected Errors getProcessingErrors() {
    processingErrors.setTotalRecords(processingErrors.getErrors()
      .size());
    return processingErrors;
  }

  public void addProcessingError(Error error) {
    processingErrors.getErrors().add(error);
  }

  public Response buildErrorResponse(int code) {
    final Response.ResponseBuilder responseBuilder = switch (code) {
      case 400, 403, 404, 422 -> Response.status(code);
      default -> Response.status(INTERNAL_SERVER_ERROR);
    };

    return responseBuilder.header(CONTENT_TYPE, APPLICATION_JSON)
      .entity(getProcessingErrors())
      .build();
  }

  private static Response buildErrorResponseFromHttpException(HttpException exception) {
    return Response.status(exception.getCode())
      .header(CONTENT_TYPE, APPLICATION_JSON)
      .entity(exception.getErrors())
      .build();
  }

  public Response buildCreatedResponse(Object body) {
    return Response.status(CREATED).header(CONTENT_TYPE, APPLICATION_JSON).entity(body).build();
  }

  public static javax.ws.rs.core.Response.ResponseBuilder createResponseBuilder(int code) {
    return switch (code) {
      case 400, 403, 404, 409, 422 -> Response.status(code);
      default -> Response.status(INTERNAL_SERVER_ERROR);
    };
  }

  public static int defineErrorCode(Throwable throwable) {
    final Throwable cause = throwable.getCause() == null ? throwable : throwable.getCause();
    if (cause instanceof HttpException httpException) {
      return httpException.getCode();
    }
    return INTERNAL_SERVER_ERROR.getStatusCode();
  }
}

