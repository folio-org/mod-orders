package org.folio.rest.impl;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;
import org.folio.HttpStatus;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.ErrorCodes;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.ErrorCodes.GENERIC_ERROR_CODE;
import static org.folio.orders.utils.ErrorCodes.ORDER_VENDOR_IS_INACTIVE;
import static org.folio.orders.utils.ErrorCodes.ORDER_VENDOR_NOT_FOUND;
import static org.folio.orders.utils.ErrorCodes.POL_ACCESS_PROVIDER_IS_INACTIVE;
import static org.folio.orders.utils.ErrorCodes.POL_ACCESS_PROVIDER_NOT_FOUND;
import static org.folio.orders.utils.HelperUtils.convertIdsToCqlQuery;
import static org.folio.orders.utils.HelperUtils.encodeQuery;
import static org.folio.orders.utils.HelperUtils.getAccessProvidersList;
import static org.folio.orders.utils.HelperUtils.handleGetRequest;


public class VendorHelper {

  private static final Logger logger = LoggerFactory.getLogger(VendorHelper.class);

  private final HttpClientInterface httpClient;
  private final Map<String, String> okapiHeaders;
  private final Context ctx;
  private final String lang;

  private static final String ID = "id";
  private static final String VENDORS = "vendors";
  private static final String VENDOR_STATUS = "vendor_status";
  private static final String VENDOR_STORAGE_VENDORS = "/vendor-storage/vendors/";
  private static final String VENDORS_WITH_QUERY_ENDPOINT = "/vendor-storage/vendors?limit=%d&lang=%s&query=%s";

  public VendorHelper(String lang, HttpClientInterface httpClient, Map<String, String> okapiHeaders, Context ctx) {
    this.lang = lang;
    this.httpClient = httpClient;
    this.okapiHeaders = okapiHeaders;
    this.ctx = ctx;
  }

  /**
   * Checks if vendor in {@link CompositePurchaseOrder} exists and has status Active.
   * If any false, adds corresponding error to {@link Errors} object.
   *
   * @param compPO composite purchase order
   * @return CompletableFuture with {@link Errors} object
   */
  public CompletableFuture<Errors> validateVendor(CompositePurchaseOrder compPO) {
    String id = compPO.getVendor();
    CompletableFuture<Errors> future = new VertxCompletableFuture<>(ctx);
    List<Error> errors = new ArrayList<>();
    getVendorJsonById(id)
      .thenApply(vendorJson -> {
        VendorStatus status = VendorStatus.valueOf(vendorJson.getString(VENDOR_STATUS).toUpperCase());

        if(status != VendorStatus.ACTIVE) {
          errors.add(createErrorWithId(ORDER_VENDOR_IS_INACTIVE, id));
        }
        return buildErrorsObject(errors);
      })
      .thenAccept(future::complete)
      .exceptionally(t -> {
        if(HttpStatus.HTTP_NOT_FOUND.toInt() == (((HttpException) t.getCause()).getCode())) {
          errors.add(createErrorWithId(ORDER_VENDOR_NOT_FOUND, id));
        } else {
          logger.error("Failed to validate vendor's status", t);
          errors.add(GENERIC_ERROR_CODE.toError());
        }
        future.complete(buildErrorsObject(errors));
        return null;
      });
    return future;
  }

  /**
   * Checks if access providers in exist and have status Active. If any false, adds corresponding error to {@link Errors} object.
   * @param compPO composite purchase order
   * @return CompletableFuture with {@link Errors} object
   */
  public CompletableFuture<Errors> validateAccessProviders(CompositePurchaseOrder compPO) {
    CompletableFuture<Errors> future = new VertxCompletableFuture<>(ctx);
    List<String> ids = getAccessProvidersList(compPO);
    List<Error> errors = new ArrayList<>();
    getAccessProvidersByIds(ids).thenApply(vendors -> {
      // Validate access provider status Active
      vendors.forEach(vendorJson -> {
        if(VendorStatus.valueOf(vendorJson.getString(VENDOR_STATUS).toUpperCase()) != VendorStatus.ACTIVE) {
          errors.add(createErrorWithId(POL_ACCESS_PROVIDER_IS_INACTIVE, vendorJson.getString(ID)));
        }
      });

      // Validate access provider existence
      List<String> vendorsIds = vendors.stream()
        .map(jsonObject -> jsonObject.getString(ID))
        .collect(toList());

      ids.stream()
        .filter(id -> !vendorsIds.contains(id))
        .forEach(id -> errors.add(POL_ACCESS_PROVIDER_NOT_FOUND.toError().withAdditionalProperty(ID, id)));

      return buildErrorsObject(errors);
    }).thenAccept(future::complete)
      .exceptionally(t -> {
        logger.error("Failed to validate access provider's status", t);
        future.completeExceptionally(t);
        return null;
      });
    return future;
  }

  /**
   * Builds {@link Errors} object based on list of {@link Error}
   *
   * @param errors list of {@link Error}
   * @return {@link Errors} object with list of {@link Error}
   */
  private Errors buildErrorsObject(List<Error> errors) {
    return new Errors()
      .withErrors(errors)
      .withTotalRecords(errors.size());
  }

  /**
   * Creates {@link Error} with id of corresponding vendor/access provider
   *
   * @param id vendor's/access provider's identifier
   * @param errorCodes error code
   * @return {@link Error} with id of failed vendor/access provider
   */
  private Error createErrorWithId(ErrorCodes errorCodes, String id) {
    return errorCodes.toError()
      .withAdditionalProperty(ID, id);
  }

  /**
   * Retrieves vendor as json file
   *
   * @param vendorId vendor's id
   * @return CompletableFuture with {@link JsonObject} representation of vendor
   */
  private CompletableFuture<JsonObject> getVendorJsonById(String vendorId) {
    return handleGetRequest(VENDOR_STORAGE_VENDORS + vendorId, httpClient, ctx, okapiHeaders, logger);
  }

  /**
   * Retrieves list of access providers
   *
   * @param accessProviderIds - list of access providers id
   * @return CompletableFuture with {@link List<JsonObject>} representation of vendor
   */
  private CompletableFuture<List<JsonObject>> getAccessProvidersByIds(List<String> accessProviderIds) {
    CompletableFuture<List<JsonObject>> future = new CompletableFuture<>();
    String query = convertIdsToCqlQuery(accessProviderIds);
    String endpoint = String.format(VENDORS_WITH_QUERY_ENDPOINT, accessProviderIds.size(), lang, encodeQuery(query, logger));
    handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
      .thenApply(vendors -> vendors.getJsonArray(VENDORS).stream().map(obj -> (JsonObject) obj).collect(toList())).thenAccept(future::complete);
    return future;
  }

  public enum VendorStatus {
    INACTIVE, ACTIVE, PENDING,
  }

}
