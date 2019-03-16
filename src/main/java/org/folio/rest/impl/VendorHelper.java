package org.folio.rest.impl;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;
import org.folio.HttpStatus;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.ErrorCodes;
import org.folio.rest.acq.model.Vendor;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

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
import static org.folio.orders.utils.HelperUtils.convertIdsToCqlQuery;
import static org.folio.orders.utils.HelperUtils.encodeQuery;
import static org.folio.orders.utils.HelperUtils.handleGetRequest;


public class VendorHelper extends AbstractHelper {

  private static final String ID = "id";
  private static final String VENDORS = "vendors";
  private static final String VENDOR_STORAGE_VENDORS = "/vendor-storage/vendors/";
  private static final String VENDORS_WITH_QUERY_ENDPOINT = "/vendor-storage/vendors?limit=%d&lang=%s&query=%s";

  public VendorHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders, Context ctx, String lang) {
    super(httpClient, okapiHeaders, ctx, lang);
  }

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
    String id = compPO.getVendor();
    CompletableFuture<Errors> future = new VertxCompletableFuture<>(ctx);
    if(id != null) {
      getVendorById(id)
        .thenApply(vendor -> {
          VendorStatus status = VendorStatus.valueOf(vendor.getVendorStatus().toUpperCase());
          if(status != VendorStatus.ACTIVE) {
            addProcessingError(createErrorWithId(ORDER_VENDOR_IS_INACTIVE, id));
          }
          return getProcessingErrors();
        })
        .thenAccept(future::complete)
        .exceptionally(t -> {
          if(HttpStatus.HTTP_NOT_FOUND.toInt() == (((HttpException) t.getCause()).getCode())) {
            addProcessingError(createErrorWithId(ORDER_VENDOR_NOT_FOUND, id));
            future.complete(getProcessingErrors());
          } else {
            logger.error("Failed to validate vendor's status", t);
            future.completeExceptionally(t);
          }
          return null;
        });
    } else {
      future.complete(getProcessingErrors());
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
    if (!ids.isEmpty()) {
      getAccessProvidersByIds(ids).thenApply(vendors -> {
        // Validate access provider status Active
        vendors.forEach(vendor -> {
          if(VendorStatus.valueOf(vendor.getVendorStatus().toUpperCase()) != VendorStatus.ACTIVE) {
            addProcessingError(createErrorWithId(POL_ACCESS_PROVIDER_IS_INACTIVE, vendor.getId()));
          }
        });
        // Validate access provider existence
        List<String> vendorsIds = vendors.stream()
          .map(Vendor::getId)
          .collect(toList());
        ids.stream()
          .filter(id -> !vendorsIds.contains(id))
          .forEach(id -> addProcessingError(POL_ACCESS_PROVIDER_NOT_FOUND.toError().withAdditionalProperty(ID, id)));
        return getProcessingErrors();
      }).thenAccept(future::complete)
        .exceptionally(t -> {
          logger.error("Failed to validate access provider's status", t);
          future.completeExceptionally(t);
          return null;
        });
    } else {
      future.complete(getProcessingErrors());
    }
    return future;
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
  private CompletableFuture<Set<Vendor>> getAccessProvidersByIds(Set<String> accessProviderIds) {
    CompletableFuture<Set<Vendor>> future = new CompletableFuture<>();
    String query = convertIdsToCqlQuery(new ArrayList<>(accessProviderIds));
    String endpoint = String.format(VENDORS_WITH_QUERY_ENDPOINT, accessProviderIds.size(), lang, encodeQuery(query, logger));
    handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
      .thenApply(jsons -> jsons.getJsonArray(VENDORS).stream().map(obj -> ((JsonObject) obj).mapTo(Vendor.class)).collect(toSet())).thenAccept(future::complete);
    return future;
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
      .map(p -> p.getEresource().getAccessProvider()).collect(toSet());
  }

  public enum VendorStatus {
    INACTIVE, ACTIVE, PENDING,
  }

}
