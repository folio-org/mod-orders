package org.folio.service.organization;

import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.HelperUtils.convertIdsToCqlQuery;
import static org.folio.orders.utils.HelperUtils.encodeQuery;
import static org.folio.rest.RestConstants.ERROR_CAUSE;
import static org.folio.rest.core.exceptions.ErrorCodes.ORDER_VENDOR_IS_INACTIVE;
import static org.folio.rest.core.exceptions.ErrorCodes.ORDER_VENDOR_NOT_FOUND;
import static org.folio.rest.core.exceptions.ErrorCodes.ORGANIZATION_NOT_A_VENDOR;
import static org.folio.rest.core.exceptions.ErrorCodes.POL_ACCESS_PROVIDER_IS_INACTIVE;
import static org.folio.rest.core.exceptions.ErrorCodes.POL_ACCESS_PROVIDER_NOT_FOUND;
import static org.folio.rest.core.exceptions.ErrorCodes.VENDOR_ISSUE;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.HttpStatus;
import org.folio.rest.acq.model.Organization;
import org.folio.rest.acq.model.OrganizationCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.service.UserService;
import org.springframework.stereotype.Component;

import com.github.benmanes.caffeine.cache.AsyncCache;
import com.github.benmanes.caffeine.cache.Caffeine;

import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.Vertx;

@Component
public class OrganizationService {
  private static final Logger log = LogManager.getLogger(OrganizationService.class);
  private final RestClient restClient;
  static final String ORGANIZATIONS = "organizations";
  private static final String ORGANIZATIONS_STORAGE_VENDORS = "/organizations-storage/organizations/";
  private static final String ORGANIZATIONS_WITH_QUERY_ENDPOINT = "/organizations-storage/organizations?limit=%d&&query=%s";
  private static final String PO_LINE_NUMBER = "poLineNumber";
  private final AsyncCache<String, Organization> asyncCache;
  private static final String UNIQUE_CACHE_KEY_PATTERN = "%s_%s_%s";

  private final Errors processingErrors = new Errors();
  public OrganizationService(RestClient restClient) {
    asyncCache = Caffeine.newBuilder()
      .expireAfterWrite(30, TimeUnit.SECONDS)
      .executor(task -> Vertx.currentContext()
        .runOnContext(v -> task.run()))
      .buildAsync();
    this.restClient = restClient;
  }


  /**
   * Checks if vendor in {@link CompositePurchaseOrder} exists in Organizations
   * and has status "Active" with isVendor flag enabled. If not, adds
   * corresponding error to {@link Errors} object.
   *
   * @param vendorId organization id
   *
   * @return CompletableFuture with {@link Errors} object
   */
  public Future<Errors> validateVendor(String vendorId, RequestContext requestContext) {
    Promise<Errors> promise = Promise.promise();

    log.debug("Validating vendor with id={}", vendorId);

    List<Error> errors = new ArrayList<>();
    getAndCacheVendorById(vendorId, requestContext)
      .map(organization -> {
        if(!organization.getStatus().equals(Organization.Status.ACTIVE)) {
          errors.add(createErrorWithId(ORDER_VENDOR_IS_INACTIVE, organization.getId()));
        }
        if (null == organization.getIsVendor() || !organization.getIsVendor()) {
          errors.add(createErrorWithId(ORGANIZATION_NOT_A_VENDOR, organization.getId()));
        }
        return handleAndReturnErrors(errors);
      })
      .onSuccess(promise::complete)
      .onFailure(cause -> {
        if (cause instanceof HttpException && HttpStatus.HTTP_NOT_FOUND.toInt() == (((HttpException) cause).getCode())) {
          errors.add(createErrorWithId(ORDER_VENDOR_NOT_FOUND, vendorId));
        } else {
          log.error("Failed to validate vendor's status", cause);
          errors.add(createErrorWithId(VENDOR_ISSUE, vendorId).withAdditionalProperty(ERROR_CAUSE, cause.getMessage()));
        }
        promise.complete(handleAndReturnErrors(errors));
      });
    return promise.future();
  }

  /**
   * Checks if access providers in exist and have status Active. If any false, adds corresponding error to {@link Errors} object.
   * @param poLines list of composite purchase order lines
   * @return CompletableFuture with {@link Errors} object
   */public Future<Errors> validateAccessProviders(List<CompositePoLine> poLines, RequestContext requestContext) {
    Promise<Errors> promise = Promise.promise();

    Map<String, List<CompositePoLine>> poLinesMap =
      poLines.stream()
        .filter(p -> (p.getEresource() != null && p.getEresource().getAccessProvider() != null))
        .collect(Collectors.groupingBy(p -> p.getEresource().getAccessProvider()));

    Set<String> ids = poLinesMap.keySet();

    List<Error> errors = new ArrayList<>();
    if (!ids.isEmpty()) {
      log.debug("Validating {} access provider(s) for order with id={}", ids.size(), poLines.get(0).getPurchaseOrderId());

      getAccessProvidersByIds(ids, requestContext).map(organizations -> {
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
        })
        .onSuccess(promise::complete)
        .onFailure(t -> {
          Throwable cause = t.getCause();
          log.error("Failed to validate access provider's status", cause);
          errors.add(VENDOR_ISSUE.toError().withAdditionalProperty(ERROR_CAUSE, cause.getMessage()));
          promise.complete(handleAndReturnErrors(errors));
        });
    } else {
      log.debug("Order does not have any access provider to validate");
      promise.complete(handleAndReturnErrors(errors));
    }
    return promise.future();
  }

  public Future<Organization> getAndCacheVendorById(String vendorId, RequestContext requestContext) {
    try {
      if (vendorId == null) {
        return Future.succeededFuture(null);
      }
      var tenantId = TenantTool.tenantId(requestContext.getHeaders());
      var userId = UserService.getCurrentUserId(requestContext.getHeaders());
      var cacheKey = String.format(UNIQUE_CACHE_KEY_PATTERN, tenantId, userId, vendorId);

      return Future
        .fromCompletionStage(asyncCache.get(cacheKey, (key, executor) -> getVendorById(vendorId, requestContext)
          .toCompletionStage()
          .toCompletableFuture()));
    } catch (Exception e) {
      log.error("getAndCacheVendorById:: Error loading organization from cache, tenantId: '{}'", TenantTool.tenantId(requestContext.getHeaders()), e);
      return Future.failedFuture(e);
    }
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
    error.getParameters().add(new Parameter().withKey("id").withValue(id));
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
  public Future<Organization> getVendorById(String vendorId, RequestContext requestContext) {
    return restClient.get(ORGANIZATIONS_STORAGE_VENDORS + vendorId, Organization.class, requestContext);
  }

  /**
   * Retrieves set of access providers
   *
   * @param accessProviderIds - {@link Set<String>} of access providers id
   * @return CompletableFuture with {@link List<Organization>} of vendors
   */
  private Future<List<Organization>> getAccessProvidersByIds(Set<String> accessProviderIds, RequestContext requestContext) {
    String query = convertIdsToCqlQuery(accessProviderIds);
    String endpoint = String.format(ORGANIZATIONS_WITH_QUERY_ENDPOINT, accessProviderIds.size(), encodeQuery(query));
    return restClient.get(endpoint, OrganizationCollection.class, requestContext)
      .map(OrganizationCollection::getOrganizations);
  }

  protected Errors getProcessingErrors() {
    processingErrors.setTotalRecords(processingErrors.getErrors().size());
    return processingErrors;
  }

  public void addProcessingError(Error error) {
    processingErrors.getErrors().add(error);
  }

  protected void addProcessingErrors(List<Error> errors) {
    processingErrors.getErrors().addAll(errors);
  }

}
