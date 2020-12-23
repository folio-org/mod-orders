package org.folio.helper;

import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.ErrorCodes.ORDER_VENDOR_IS_INACTIVE;
import static org.folio.orders.utils.ErrorCodes.ORDER_VENDOR_NOT_FOUND;
import static org.folio.orders.utils.ErrorCodes.ORGANIZATION_NOT_A_VENDOR;
import static org.folio.orders.utils.ErrorCodes.POL_ACCESS_PROVIDER_IS_INACTIVE;
import static org.folio.orders.utils.ErrorCodes.POL_ACCESS_PROVIDER_NOT_FOUND;
import static org.folio.orders.utils.ErrorCodes.VENDOR_ISSUE;
import static org.folio.orders.utils.HelperUtils.convertIdsToCqlQuery;
import static org.folio.orders.utils.HelperUtils.encodeQuery;
import static org.folio.orders.utils.HelperUtils.handleGetRequest;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import org.folio.HttpStatus;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.ErrorCodes;
import org.folio.rest.acq.model.Organization;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;


public class VendorHelper extends AbstractHelper {


  static final String ORGANIZATIONS = "organizations";
  private static final String ORGANIZATIONS_STORAGE_VENDORS = "/organizations-storage/organizations/";
  private static final String ORGANIZATIONS_WITH_QUERY_ENDPOINT = "/organizations-storage/organizations?limit=%d&lang=%s&query=%s";
  private static final String PO_LINE_NUMBER = "poLineNumber";


  public VendorHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders, Context ctx, String lang) {
    super(httpClient, okapiHeaders, ctx, lang);
  }

  /**
   * Checks if vendor in {@link CompositePurchaseOrder} exists in Organizations
   * and has status "Active" with isVendor flag enabled. If not, adds
   * corresponding error to {@link Errors} object.
   *
   * @param compPO
   *          composite purchase order
   * @return CompletableFuture with {@link Errors} object
   */
  public CompletableFuture<Errors> validateVendor(CompositePurchaseOrder compPO) {
    CompletableFuture<Errors> future = new VertxCompletableFuture<>(ctx);
    String id = compPO.getVendor();

    logger.debug("Validating vendor with id={}", id);

    List<Error> errors = new ArrayList<>();
    if (id != null) {
      getVendorById(id)
        .thenApply(organization -> {
          if(!organization.getStatus().equals(Organization.Status.ACTIVE)) {
            errors.add(createErrorWithId(ORDER_VENDOR_IS_INACTIVE, id));
          }
          if (null == organization.getIsVendor() || !organization.getIsVendor()) {
            errors.add(createErrorWithId(ORGANIZATION_NOT_A_VENDOR, id));
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
   * @param poLines list of composite purchase order lines
   * @return CompletableFuture with {@link Errors} object
   */
  public CompletableFuture<Errors> validateAccessProviders(List<CompositePoLine> poLines) {
    CompletableFuture<Errors> future = new VertxCompletableFuture<>(ctx);

    Map<String, List<CompositePoLine>> poLinesMap =
      poLines.stream()
        .filter(p -> (p.getEresource() != null && p.getEresource().getAccessProvider() != null))
        .collect(Collectors.groupingBy(p -> p.getEresource().getAccessProvider()));

    Set<String> ids = poLinesMap.keySet();

    List<Error> errors = new ArrayList<>();
    if (!ids.isEmpty()) {
      logger.debug("Validating {} access provider(s) for order with id={}", ids.size(), poLines.get(0).getPurchaseOrderId());

      getAccessProvidersByIds(ids).thenApply(organizations -> {
        // Validate access provider status Active
        organizations.forEach(organization -> {
          if(!organization.getStatus().equals(Organization.Status.ACTIVE)) {
            errors.add(createErrorWithId(POL_ACCESS_PROVIDER_IS_INACTIVE, organization.getId(), poLinesMap.get(organization.getId())));
          }
        });
        // Validate access provider existence
        List<String> vendorsIds = organizations.stream()
          .map(Organization::getId)
          .collect(toList());
        ids.stream()
          .filter(id -> !vendorsIds.contains(id))
          .forEach(id -> errors.add(createErrorWithId(POL_ACCESS_PROVIDER_NOT_FOUND, id, poLinesMap.get(id))));
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
      logger.debug("Order does not have any access provider to validate");
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
    poLines.stream()
      .filter(p -> p.getPoLineNumber() != null)
      .forEach(p -> error.getParameters().add(new Parameter().withKey(PO_LINE_NUMBER).withValue(p.getPoLineNumber())));
    return error;
  }

  /**
   * Retrieves vendor by id
   *
   * @param vendorId vendor's id
   * @return CompletableFuture with {@link Organization} object
   */
  private CompletableFuture<Organization> getVendorById(String vendorId) {
    return handleGetRequest(ORGANIZATIONS_STORAGE_VENDORS + vendorId, httpClient, ctx, okapiHeaders, logger)
      .thenApply(json -> json.mapTo(Organization.class));
  }

  /**
   * Retrieves set of access providers
   *
   * @param accessProviderIds - {@link Set<String>} of access providers id
   * @return CompletableFuture with {@link List<Organization>} of vendors
   */
  private CompletableFuture<List<Organization>> getAccessProvidersByIds(Set<String> accessProviderIds) {
    String query = convertIdsToCqlQuery(new ArrayList<>(accessProviderIds));
    String endpoint = String.format(ORGANIZATIONS_WITH_QUERY_ENDPOINT, accessProviderIds.size(), lang, encodeQuery(query, logger));
    return handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
      .thenApply(jsons -> jsons.getJsonArray(ORGANIZATIONS)
        .stream()
        .map(obj -> ((JsonObject) obj).mapTo(Organization.class))
        .collect(toList())
      );
  }
}
