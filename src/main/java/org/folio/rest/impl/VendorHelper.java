package org.folio.rest.impl;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;
import org.folio.HttpStatus;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.ErrorCodes;
import org.folio.rest.acq.model.Vendor;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.Parameter;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CompletableFuture;

import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toSet;
import static org.folio.orders.utils.ErrorCodes.ORDER_VENDOR_IS_INACTIVE;
import static org.folio.orders.utils.ErrorCodes.ORDER_VENDOR_NOT_FOUND;
import static org.folio.orders.utils.ErrorCodes.POL_ACCESS_PROVIDER_IS_INACTIVE;
import static org.folio.orders.utils.ErrorCodes.POL_ACCESS_PROVIDER_NOT_FOUND;
import static org.folio.orders.utils.ErrorCodes.VENDOR_ISSUE;
import static org.folio.orders.utils.HelperUtils.convertIdsToCqlQuery;
import static org.folio.orders.utils.HelperUtils.encodeQuery;
import static org.folio.orders.utils.HelperUtils.handleGetRequest;


public class VendorHelper extends AbstractHelper {

  static final String VENDORS = "vendors";
  private static final String VENDOR_STORAGE_VENDORS = "/vendor-storage/vendors/";
  private static final String VENDORS_WITH_QUERY_ENDPOINT = "/vendor-storage/vendors?limit=%d&lang=%s&query=%s";
  private static final String PO_LINE_NUMBER = "poLineNumber";

  public VendorHelper(Map<String, String> okapiHeaders, Context ctx, String lang) {
    super(okapiHeaders, ctx, lang);
  }

  /**
   * Checks if vendor in {@link CompositePurchaseOrder} exists and has status Active.
   * If any false, adds corresponding error to {@link Errors} object.
   *
   * @param compPO composite purchase order
   * @return CompletableFuture with {@link Errors} object
   */
  public CompletableFuture<Errors> validateVendor(CompositePurchaseOrder compPO) {
    CompletableFuture<Errors> future = new VertxCompletableFuture<>(ctx);
    String id = compPO.getVendor();

    logger.debug("Validating vendor with id={}", id);

    List<Error> errors = new ArrayList<>();
    if (id != null) {
      getVendorById(id)
        .thenApply(vendor -> {
          VendorStatus status = VendorStatus.valueOf(vendor.getVendorStatus().toUpperCase());
          if(status != VendorStatus.ACTIVE) {
            errors.add(createErrorWithId(ORDER_VENDOR_IS_INACTIVE, id));
          }
          return handleAndReturnErrors(errors);
        })
        .thenAccept(future::complete)
        .exceptionally(t -> {
          Throwable cause = t.getCause();
          if (cause instanceof HttpException && HttpStatus.HTTP_NOT_FOUND.toInt() == (((HttpException) cause).getCode())) {
            errors.add(createErrorWithId(ORDER_VENDOR_NOT_FOUND, id));
          } else {
            logger.error("Failed to validate vendor's status", cause);
            errors.add(createErrorWithId(VENDOR_ISSUE, id).withAdditionalProperty(ERROR_CAUSE, cause.getMessage()));
          }
          future.complete(handleAndReturnErrors(errors));
          return null;
        });
    } else {
      future.complete(handleAndReturnErrors(errors));
    }
    return future;
  }

  /**
   * Checks if access providers in exist and have status Active. If any false, adds corresponding error to {@link Errors} object.
   * @param compPO composite purchase order
   * @return CompletableFuture with {@link Errors} object
   */
  public CompletableFuture<Errors> validateAccessProviders(CompositePurchaseOrder compPO) {
    CompletableFuture<Errors> future = new VertxCompletableFuture<>(ctx);
    Set<String> ids = verifyAndGetAccessProvidersIdsList(compPO);
    List<CompositePoLine> poLines = compPO.getCompositePoLines();
    List<Error> errors = new ArrayList<>();
    if (!ids.isEmpty()) {
      logger.debug("Validating {} access provider(s) for order with id={}", ids.size(), compPO.getId());

      getAccessProvidersByIds(ids).thenApply(vendors -> {
        // Validate access provider status Active
        vendors.forEach(vendor -> {
          if(VendorStatus.valueOf(vendor.getVendorStatus().toUpperCase()) != VendorStatus.ACTIVE) {
            errors.add(createErrorWithId(POL_ACCESS_PROVIDER_IS_INACTIVE, vendor.getId(), poLines));
          }
        });
        // Validate access provider existence
        List<String> vendorsIds = vendors.stream()
          .map(Vendor::getId)
          .collect(toList());
        ids.stream()
          .filter(id -> !vendorsIds.contains(id))
          .forEach(id -> errors.add(createErrorWithId(POL_ACCESS_PROVIDER_NOT_FOUND, id, poLines)));
        return handleAndReturnErrors(errors);
      }).thenAccept(future::complete)
        .exceptionally(t -> {
          Throwable cause = t.getCause();
          logger.error("Failed to validate access provider's status", cause);
          errors.add(VENDOR_ISSUE.toError().withAdditionalProperty(ERROR_CAUSE, cause.getMessage()));
          future.complete(handleAndReturnErrors(errors));
          return null;
        });
    } else {
      logger.debug("Order with id={} does not have any access provider to validate", compPO.getId());
      future.complete(handleAndReturnErrors(errors));
    }
    return future;
  }

  /**
   * Builds {@link Errors} object based on list of {@link Error}
   *
   * @param errors list of {@link Error}
   * @return {@link Errors} object with list of {@link Error}
   */
  private Errors handleAndReturnErrors(List<Error> errors) {
    // Add errors to the helper processing errors
    addProcessingErrors(errors);
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
    Error error = errorCodes.toError();
    error.getParameters().add(new Parameter().withKey(ID).withValue(id));
    return error;
  }

  /**
   * Creates {@link Error} with id of corresponding vendor/access provider and poLines ids
   *
   * @param id vendor's/access provider's identifier
   * @param errorCodes error code
   * @param poLines list of composite PoLines
   * @return {@link Error} with id of failed vendor/access provider
   */
  private Error createErrorWithId(ErrorCodes errorCodes, String id, List<CompositePoLine> poLines) {
    Error error = createErrorWithId(errorCodes, id);
    for (CompositePoLine poLine : poLines) {
      if (poLine.getPoLineNumber() != null) {
        error.getParameters().add(new Parameter().withKey(PO_LINE_NUMBER).withValue(poLine.getPoLineNumber()));
      }
    }
    return error;
  }

  /**
   * Retrieves vendor by id
   *
   * @param vendorId vendor's id
   * @return CompletableFuture with {@link Vendor} object
   */
  private CompletableFuture<Vendor> getVendorById(String vendorId) {
    return handleGetRequest(VENDOR_STORAGE_VENDORS + vendorId, httpClient, ctx, okapiHeaders, logger)
      .thenApply(json -> json.mapTo(Vendor.class));
  }

  /**
   * Retrieves set of access providers
   *
   * @param accessProviderIds - {@link Set<String>} of access providers id
   * @return CompletableFuture with {@link List<Vendor>} of vendors
   */
  private CompletableFuture<List<Vendor>> getAccessProvidersByIds(Set<String> accessProviderIds) {
    String query = convertIdsToCqlQuery(new ArrayList<>(accessProviderIds));
    String endpoint = String.format(VENDORS_WITH_QUERY_ENDPOINT, accessProviderIds.size(), lang, encodeQuery(query, logger));
    return handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
      .thenApply(jsons -> jsons.getJsonArray(VENDORS)
        .stream()
        .map(obj -> ((JsonObject) obj).mapTo(Vendor.class))
        .collect(toList())
      );
  }

  /**
   * Returns set of access providers ids for composite purchase order
   *
   * @param compPO composite purchase order
   * @return {@link Set<String>} of access providers ids
   */
  private Set<String> verifyAndGetAccessProvidersIdsList(CompositePurchaseOrder compPO) {
    return compPO.getCompositePoLines().stream()
      .filter(p -> (p.getEresource() != null && p.getEresource().getAccessProvider() != null))
      .map(p -> p.getEresource().getAccessProvider())
      .collect(toSet());
  }

  public enum VendorStatus {
    INACTIVE, ACTIVE, PENDING,
  }

}
