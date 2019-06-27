package org.folio.rest.impl;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static javax.ws.rs.core.HttpHeaders.CONTENT_TYPE;
import static javax.ws.rs.core.HttpHeaders.LOCATION;
import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static javax.ws.rs.core.MediaType.TEXT_PLAIN;
import static javax.ws.rs.core.Response.Status.INTERNAL_SERVER_ERROR;
import static me.escoffier.vertx.completablefuture.VertxCompletableFuture.allOf;
import static javax.ws.rs.core.Response.Status.CREATED;
import static org.apache.commons.lang3.ObjectUtils.defaultIfNull;
import static org.folio.orders.utils.ErrorCodes.GENERIC_ERROR_CODE;
import static org.folio.orders.utils.HelperUtils.*;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.FULLY_RECEIVED;

import io.vertx.core.Context;
import io.vertx.core.eventbus.DeliveryOptions;
import io.vertx.core.eventbus.Message;
import io.vertx.core.http.HttpHeaders;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;

import javax.ws.rs.core.Response;

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import org.folio.orders.events.handlers.MessageAddress;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PoLine.ReceiptStatus;
import org.folio.rest.tools.client.HttpClientFactory;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import org.folio.rest.tools.utils.TenantTool;

public abstract class AbstractHelper {
  public static final String ID = "id";
  public static final String ORDER_IDS = "orderIds";
  public static final String OKAPI_HEADERS = "okapiHeaders";
  public static final String ERROR_CAUSE = "cause";

  protected final Logger logger = LoggerFactory.getLogger(this.getClass());

  private final Errors processingErrors = new Errors();

  protected final HttpClientInterface httpClient;
  protected Map<String, String> okapiHeaders;
  protected final Context ctx;
  protected final String lang;
  private JsonObject tenantConfiguration;

  AbstractHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders, Context ctx, String lang) {
    setDefaultHeaders(httpClient);
    this.httpClient = httpClient;
    this.okapiHeaders = okapiHeaders;
    this.ctx = ctx;
    this.lang = lang;
  }

  AbstractHelper(Map<String, String> okapiHeaders, Context ctx, String lang) {
    this.httpClient = getHttpClient(okapiHeaders, true);
    this.okapiHeaders = okapiHeaders;
    this.ctx = ctx;
    this.lang = lang;
  }

  protected AbstractHelper(Context ctx) {
    this.httpClient = null;
    this.okapiHeaders = null;
    this.lang = null;
    this.ctx = ctx;
  }

  public static HttpClientInterface getHttpClient(Map<String, String> okapiHeaders, boolean setDefaultHeaders) {
    final String okapiURL = okapiHeaders.getOrDefault(OKAPI_URL, "");
    final String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_HEADER_TENANT));

    HttpClientInterface httpClient = HttpClientFactory.getHttpClient(okapiURL, tenantId);

    // Some requests do not have body and in happy flow do not produce response body. The Accept header is required for calls to storage
    if (setDefaultHeaders) {
      setDefaultHeaders(httpClient);
    }
    return httpClient;
  }

  public static HttpClientInterface getHttpClient(Map<String, String> okapiHeaders) {
    return getHttpClient(okapiHeaders, false);
  }

  public void closeHttpClient() {
    httpClient.closeClient();
  }

  public List<Error> getErrors() {
    return processingErrors.getErrors();
  }

  protected boolean isCheckin(PoLine poLine) {
    return defaultIfNull(poLine.getCheckinItems(), false);
  }

  public CompletableFuture<String> updatePoLineReceiptStatus(PoLine poLine, ReceiptStatus status, HttpClientInterface httpClient,
      Context ctx, Map<String, String> okapiHeaders, Logger logger) {
    
    if (status == null || poLine.getReceiptStatus() == status) {
      return completedFuture(null);
    }

    // Update receipt date and receipt status
    if (status == FULLY_RECEIVED) {
      poLine.setReceiptDate(new Date());
    } else if (isCheckin(poLine) && poLine.getReceiptStatus()
      .equals(ReceiptStatus.AWAITING_RECEIPT) && status == ReceiptStatus.PARTIALLY_RECEIVED) {
      // if checking in, set the receipt date only for the first piece
      poLine.setReceiptDate(new Date());
    } else {
      poLine.setReceiptDate(null);
    }

    poLine.setReceiptStatus(status);

    // Update PO Line in storage
    return handlePutRequest(resourceByIdPath(PO_LINES, poLine.getId()), JsonObject.mapFrom(poLine), httpClient, ctx, okapiHeaders,
        logger).thenApply(v -> poLine.getId())
          .exceptionally(e -> {
            logger.error("The PO Line '{}' cannot be updated with new receipt status", e, poLine.getId());
            return null;
          });
  }

  protected <T> void completeAllFutures(Context ctx, HttpClientInterface httpClient, List<CompletableFuture<T>> futures, Message<JsonObject> message) {
    // Now wait for all operations to be completed and send reply
    allOf(ctx, futures.toArray(new CompletableFuture[0])).thenAccept(v -> {
      // Sending reply message just in case some logic requires it
      message.reply(Response.Status.OK.getReasonPhrase());
      httpClient.closeClient();
    })
      .exceptionally(e -> {
        message.fail(handleProcessingError(e), getErrors().get(0)
          .getMessage());
        httpClient.closeClient();
        return null;
      });
  }
  
  /**
   * Some requests do not have body and in happy flow do not produce response body. The Accept header is required for calls to storage
   */
  private static void setDefaultHeaders(HttpClientInterface httpClient) {
    Map<String,String> customHeader = new HashMap<>();
    customHeader.put(HttpHeaders.ACCEPT.toString(), APPLICATION_JSON  + ", " + TEXT_PLAIN);
    httpClient.setDefaultHeaders(customHeader);
  }

  protected Errors getProcessingErrors() {
    processingErrors.setTotalRecords(processingErrors.getErrors().size());
    return processingErrors;
  }

  protected void addProcessingError(Error error) {
    processingErrors.getErrors().add(error);
  }

  protected void addProcessingErrors(List<Error> errors) {
    processingErrors.getErrors().addAll(errors);
  }

  /**
   * A common method to create a new entry in the storage based on the Json Object.
   *
   * @param recordData json to post
   * @return completable future holding id of newly created entity Record or an exception if process failed
   */
  protected CompletableFuture<String> createRecordInStorage(JsonObject recordData, String endpoint) {
    CompletableFuture<String> future = new VertxCompletableFuture<>(ctx);
    try {
      if (logger.isDebugEnabled()) {
        logger.debug("Sending 'POST {}' with body: {}", endpoint, recordData.encodePrettily());
      }
      httpClient
        .request(HttpMethod.POST, recordData.toBuffer(), endpoint, okapiHeaders)
        .thenApply(this::verifyAndExtractRecordId)
        .thenAccept(id -> {
          future.complete(id);
          logger.debug("'POST {}' request successfully processed. Record with '{}' id has been created", endpoint, id);
        })
        .exceptionally(throwable -> {
          future.completeExceptionally(throwable);
          logger.error("'POST {}' request failed. Request body: {}", throwable, endpoint, recordData.encodePrettily());
          return null;
        });
    } catch (Exception e) {
      future.completeExceptionally(e);
    }

    return future;
  }

  private String verifyAndExtractRecordId(org.folio.rest.tools.client.Response response) {
    logger.debug("Validating received response");

    JsonObject body = verifyAndExtractBody(response);

    String id;
    if (body != null && !body.isEmpty() && body.containsKey(ID)) {
      id = body.getString(ID);
    } else {
      String location = response.getHeaders().get(LOCATION);
      id = location.substring(location.lastIndexOf('/') + 1);
    }
    return id;
  }

  protected int handleProcessingError(Throwable throwable) {
    final Throwable cause = throwable.getCause();
    logger.error("Exception encountered", cause);
    final Error error;
    final int code;

    if (cause instanceof HttpException) {
      code = ((HttpException) cause).getCode();
      error = ((HttpException) cause).getError();
    } else {
      code = INTERNAL_SERVER_ERROR.getStatusCode();
      error = GENERIC_ERROR_CODE.toError().withAdditionalProperty(ERROR_CAUSE, cause.getMessage());
    }

    addProcessingError(error);

    return code;
  }

  public Response buildErrorResponse(Throwable throwable) {
    return buildErrorResponse(handleProcessingError(throwable));
  }

  public Response buildErrorResponse(int code) {
    final Response.ResponseBuilder responseBuilder;
    switch (code) {
      case 400:
      case 404:
      case 422:
        responseBuilder = Response.status(code);
        break;
      default:
        responseBuilder = Response.status(INTERNAL_SERVER_ERROR);
    }
    closeHttpClient();

    return responseBuilder
      .header(CONTENT_TYPE, APPLICATION_JSON)
      .entity(getProcessingErrors())
      .build();
  }

  public Response buildOkResponse(Object body) {
    closeHttpClient();
    return Response.ok(body, APPLICATION_JSON).build();
  }

  public Response buildNoContentResponse() {
    closeHttpClient();
    return Response.noContent().build();
  }

  public Response buildCreatedResponse(Object body) {
    closeHttpClient();
    return Response.status(CREATED).header(CONTENT_TYPE, APPLICATION_JSON).entity(body).build();
  }

  public CompletableFuture<JsonObject> getTenantConfiguration() {
    if (this.tenantConfiguration != null) {
      return CompletableFuture.completedFuture(this.tenantConfiguration);
    } else {
      return loadConfiguration(okapiHeaders, ctx, logger)
        .thenApply(config -> {
          this.tenantConfiguration = config;
          return config;
        });
    }
  }

  protected void sendEvent(MessageAddress messageAddress, JsonObject data) {
    DeliveryOptions deliveryOptions = new DeliveryOptions();

    // Add okapi headers
    if (okapiHeaders != null) {
      okapiHeaders.forEach(deliveryOptions::addHeader);
    } else {
      Map<String, String> okapiHeaders = new HashMap<>();
      JsonObject okapiHeadersObject = data.getJsonObject("okapiHeaders");
      okapiHeaders.put("x-okapi-url", okapiHeadersObject.getString("x-okapi-url"));
      this.okapiHeaders = okapiHeaders;
    }

    data.put(LANG, lang);

    ctx.owner()
      .eventBus()
      .send(messageAddress.address, data, deliveryOptions);
  }
}
