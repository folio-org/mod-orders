package org.folio.helper;

import static javax.ws.rs.core.HttpHeaders.CONTENT_TYPE;
import static javax.ws.rs.core.HttpHeaders.LOCATION;
import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static javax.ws.rs.core.Response.Status.CREATED;
import static javax.ws.rs.core.Response.Status.INTERNAL_SERVER_ERROR;
import static org.folio.rest.RestConstants.ERROR_CAUSE;
import static org.folio.rest.core.exceptions.ErrorCodes.GENERIC_ERROR_CODE;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;
import java.util.Map;

import javax.ws.rs.core.Response;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.spring.SpringContextUtil;

import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.eventbus.Message;
import io.vertx.core.json.JsonObject;

public abstract class BaseHelper {
  public static final String ORDER_ID = "orderId";
  public static final String EVENT_PAYLOAD = "eventPayload";
  public static final String ID = HelperUtils.ID;
  public static final String OKAPI_HEADERS = "okapiHeaders";
  public static final String OKAPI_URL = "x-okapi-url";
  public static final int MAX_REPEAT_ON_FAILURE = 5;


  protected final Logger logger = LogManager.getLogger();

  private final Errors processingErrors = new Errors();

  protected Map<String, String> okapiHeaders;
  protected final Context ctx;

  protected BaseHelper(Map<String, String> okapiHeaders, Context ctx) {
    this.okapiHeaders = okapiHeaders;
    this.ctx = ctx;
    SpringContextUtil.autowireDependencies(this, ctx);
  }
  protected BaseHelper(Context ctx) {
    this.okapiHeaders = null;
    this.ctx = ctx;
  }
  public List<Error> getErrors() {
    return processingErrors.getErrors();
  }

  protected <T> void completeAllFutures(List<Future<T>> futures, Message<JsonObject> message) {
    // Now wait for all operations to be completed and send reply
    GenericCompositeFuture.join(futures)
      .onSuccess(v -> message.reply(Response.Status.OK.getReasonPhrase()))
      .onFailure(e -> message.fail(handleProcessingError(e), getErrors().get(0).getMessage()));
  }

  protected Errors getProcessingErrors() {
    processingErrors.setTotalRecords(processingErrors.getErrors().size());
    return processingErrors;
  }

  public void addProcessingError(Error error) {
    processingErrors.getErrors().add(error);
  }


  public RequestContext getRequestContext() {
    return new RequestContext(ctx, okapiHeaders);
  }
  protected int handleProcessingError(Throwable throwable) {
    logger.error("Exception encountered", throwable);
    final Error error;
    final int code;

    if (throwable instanceof HttpException httpException) {
      code = httpException.getCode();
      error = httpException.getError();
    } else {
      code = INTERNAL_SERVER_ERROR.getStatusCode();
      error = GENERIC_ERROR_CODE.toError().withAdditionalProperty(ERROR_CAUSE, throwable.getMessage());
    }

    if (getErrors().isEmpty()) {
      addProcessingError(error);
    }

    return code;
  }

  public Response buildErrorResponse(Throwable throwable) {
    return buildErrorResponse(handleProcessingError(throwable));
  }

  public Response buildErrorResponse(int code) {
    final Response.ResponseBuilder responseBuilder = switch (code) {
      case 400, 403, 404, 422, 409 -> Response.status(code);
      default -> Response.status(INTERNAL_SERVER_ERROR);
    };

    return responseBuilder
      .header(CONTENT_TYPE, APPLICATION_JSON)
      .entity(getProcessingErrors())
      .build();
  }

  public Response buildOkResponse(Object body) {
    return Response.ok(body, APPLICATION_JSON).build();
  }

  public Response buildNoContentResponse() {
    return Response.noContent().build();
  }

  public Response buildResponseWithLocation(String endpoint, Object body) {
    try {
      return Response.created(new URI(okapiHeaders.get(OKAPI_URL) + endpoint))
        .header(CONTENT_TYPE, APPLICATION_JSON).entity(body).build();
    } catch (URISyntaxException e) {
      return Response.status(CREATED).location(URI.create(endpoint))
        .header(CONTENT_TYPE, APPLICATION_JSON)
        .header(LOCATION, endpoint).entity(body).build();
    }
  }

}
